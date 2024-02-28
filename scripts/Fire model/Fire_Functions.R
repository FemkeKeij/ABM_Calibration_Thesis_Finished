# for confusion matrices
library(caret)
# for matthew's correlation coefficient
library(mltools)
# for dynamic time warping
library(dtw)

##--------------- COMPUTE ERRORS -------------------##
# function to calculate errors for the fire model
FireComputeErrors <- function(predictions,
                              sample_size, sample_method,
                              summarise_runs, datapoints){
  # predictions: dataframe with results from fitting
  # requires the columns density, density_pred,
  # directions, directions_pred
  
  # sample_size, sample_method, summarise_runs, datapoints: experiment info to be added to output data row
  
  noise_levels <- unique(predictions$noise)
  
  # to store results
  errors <- tibble(perc_correct_params = numeric(),
                   perc_density_correct = numeric(),
                   perc_directions_correct = numeric(),
                   perc_correct_cat_density = numeric(),
                   RMSE_density = numeric(),
                   NRMSE_density = numeric(),
                   point_pred_performance_density = numeric(),
                   directions_kappa = numeric(),
                   directions_f1 = numeric(),
                   directions_mcc = numeric(),
                   #RMSE_burn = numeric(),
                   #pcc_burn = numeric(),
                   #dtw_burn = numeric(),
                   fold = numeric(),
                   noise = character())
  
  # for each noise level in the data (clean & output)
  for(j in noise_levels){
    # retrieve data with appropriate noise level
    dat_noise <- predictions %>%
      filter(noise == j)
    # for each of the 5 folds
    for(i in 1:5){
      # retrieve correct rows
      dat <- dat_noise %>%
        filter(fold == i)
      
      # compute mean density and burn percentage in training data
      mean_density <- predictions %>%
        filter(fold != i) %>%
        summarise(mean(density))
      #mean_burn <- predictions %>%
      #  filter(fold != i) %>%
      #  summarise(mean(burn_percentage))
      
      # compute errors
      errors_new <- dat %>%
        # % correctly predicted direction + density in test set
        summarise(perc_correct_params = sum(
          directions == directions_pred &
            density == round(density_pred)) / nrow(dat) * 100,
          # % correctly predicted density in test set
          perc_density_correct = sum(density == round(density_pred)) /
            nrow(dat) * 100,
          # % correctly predicted direction in test set
          perc_directions_correct = sum(directions == directions_pred) /
            nrow(dat) * 100,
          # % predicted density within 10% of true density in test set
          perc_correct_cat_density = sum(density_pred >= density - 5 &
                                           density_pred <= density + 5) /
            nrow(dat) * 100,
          # RMSE of predicted vs. true density
          RMSE_density = sqrt(sum((density_pred - density)^2))
          / nrow(dat),
          # NRMSE of predicted vs. true density
          NRMSE_density = (sqrt(sum((density_pred - density)^2))
                           / nrow(dat)) / sd(density),
          # point prediction performance for density
          point_pred_performance_density = 1 - 
            sum(sqrt((density_pred - density)^2)) /
            sum(sqrt((density - mean_density)^2)))
      
      # add columns for the metrics for directions
      errors_new$directions_kappa <- NA
      errors_new$directions_f1 <- NA
      errors_new$directions_mcc <- NA
      
      # add the error measures for the directions
      # compute confusion matrix
      cm <- confusionMatrix(as.factor(dat_noise$directions_pred),
                            as.factor(dat_noise$directions),
                            mode = 'everything')
      # retrieve kappa, F1, and MCC
      errors_new$directions_kappa = cm$overall[2]
      errors_new$directions_f1 = cm$byClass[7]
      errors_new$directions_mcc =
        mcc(as.factor(dat_noise$directions_pred),
            as.factor(dat_noise$directions))
      
      # add columns for the metrics for burn_percentage
      #errors_new$RMSE_burn <- NA
      #errors_new$pcc_burn <- NA
      #errors_new$dtw_burn <- NA
      
      # compute RMSE for burn percentage
      #if(any(dat$time_label == 'middle')){
      #  RMSE_burn <- dat %>%
      #    group_by(id) %>%
      #    mutate(RMSE_b = sqrt(sum(burn_percentage_pred - 
      #                               burn_percentage)^2) / 2,
      #           RMSE_A = sqrt(sum(burn_percentage_pred -
      #                               mean_burn)^2) / 2) %>%
      #    mutate(RMSE_burn_ind = RMSE_A / RMSE_b) %>%
      #    ungroup(id) %>%
      #    summarise(RMSE_burn = mean(RMSE_burn_ind))
      #  errors_new$RMSE_burn <- RMSE_burn
        
      #  pcc_burn <- dat %>%
      #    group_by(id) %>%
      #    mutate(rho = cor(burn_percentage_pred, burn_percentage)) %>%
      #    mutate(Z = 0.5 * log(1 + rho) - log(1 - rho)) %>%
      #    ungroup() %>%
      #    summarise(meanZ = mean(Z)) %>%
      #    summarise(mean_rho = (exp(2 * meanZ) - 1)/
      #                (exp(2 * meanZ) - 1))
      #  errors_new$pcc_burn <- pcc_burn
        
      #  # compute DTW score
      #  dtws <- numeric()
      #  for(k in unique(dat$id)){
      #    x_mat <- dat %>%
      #      filter(id == k) %>%
      #      select(burn_percentage)
      #    y_mat <- dat %>%
      #      filter(id == k) %>%
      #      select(burn_percentage_pred)
      #    dtws[k] <- dtw(x_mat, y_mat,
      #                   distance.only = TRUE)$normalizedDistance
      #  }
      #  errors_new$dtw_burn <- mean(dtws)
        
      #} else {
        # there's just 1 timepoint, so I can use the regular RMSE
        #errors_new$RMSE_burn <- sqrt(sum((burn_percentage_pred -
        #                                    burn_percentage)^2)) /
        #  nrow(dat)
      #}
      
      # add fold and noise levels & add to errors data frame
      errors_new$fold <- i
      errors_new$noise <- j
      errors <- errors %>%
        add_row(errors_new)
    }
  }
  
  
  # compute the mean statistics for each fold
  errors <- errors %>%
    group_by(noise) %>%
    summarise_all(mean) %>%
    select(- fold) %>%
    # add the experiment info to the df
    add_column(sample_size = sample_size,
               sample_method = sample_method,
               summarise_runs = summarise_runs,
               datapoints = datapoints)
  
  # return data row
  return(errors)
}

##--------------- PLOT ERRORS -------------------##
# plot predicted vs. true density
PlotPredTrueDensity <- function(results, n_plot){
  # results: data frame with fitting results
  # n: sample size for which to plot
  supp_labs <- c('without ticks', 'with ticks')
  names(supp_labs) <- c('no', 'yes')
  
  plot <- results %>%
    filter(n %in% n_plot) %>%
    ggplot(mapping = aes(x = density_pred,
                         y = density_true,
                         colour = direction_correct)) +
    facet_grid(. ~ ticks_included,
               labeller = labeller(ticks_included = supp_labs)) +
    geom_point() +
    theme_minimal() +
    labs(x = 'predicted density',
         y = 'true density',
         colour = 'Number of directions \n predicted correctly') +
    theme(panel.grid.minor.y = element_blank(),
          panel.grid.minor.x = element_blank())
  
  return(plot)
}

# plot predicted vs. true directions (confusion matrix)
PlotPredTrueDirection <- function(results, n_plot){
  # results: data frame with fitting results
  # n: sample size for which to plot
  
  # split data based on ticks or no, and extract correct sample size
  results_ticks <- results %>%
    filter(ticks_included == 'yes',
           n == n_plot)
  results_noticks <- results %>%
    filter(ticks_included == 'no',
           n == n_plot)
  
  # compute confusion matrices
  cm_ticks <- confusionMatrix(factor(results_ticks$direction_pred),
                              factor(results_ticks$direction_true),
                              dnn = c('prediction', 'test'))
  cm_noticks <- confusionMatrix(factor(results_noticks$direction_pred),
                                factor(results_noticks$direction_true),
                                dnn = c('prediction', 'test'))
  dat_ticks <- as.data.frame(cm_ticks$table)
  dat_noticks <- as.data.frame(cm_noticks$table)
  
  p1 <- dat_noticks %>%
    ggplot(mapping = aes(x = prediction, y = test, fill = Freq)) +
    geom_tile() +
    geom_text(aes(label = Freq)) +
    scale_fill_gradient(low = "white", high = "#009194") +
    labs(y = 'predicted') +
    theme_minimal() +
    theme(legend.position = 'none',
          panel.border = element_rect(colour = 'lightgrey', fill = NA)) +
    scale_x_discrete(labels = c('4 directions', '8 directions')) +
    scale_y_discrete(labels = c('4 directions', '8 directions')) +
    ggtitle('without ticks')
  
  p2 <- dat_ticks %>%
    ggplot(mapping = aes(x = prediction, y = test, fill = Freq)) +
    geom_tile() +
    geom_text(aes(label = Freq)) +
    scale_fill_gradient(low = "white", high = "#009194") +
    labs(y = 'predicted') +
    theme_minimal() +
    theme(legend.position = 'none',
          panel.border = element_rect(colour = 'lightgrey', fill = NA)) +
    scale_x_discrete(labels = c('4 directions', '8 directions')) +
    scale_y_discrete(labels = c('4 directions', '8 directions')) +
    ggtitle('with ticks')
  
  plot <- p1 + p2
  
  return(plot)
}

# plot predicted vs. true burn percentage
PlotPredTrueBurn <- function(results, n_plot){
  # results: data frame with fitting results
  # n: sample size for which to plot
  supp_labs <- c('without ticks', 'with ticks')
  names(supp_labs) <- c('no', 'yes')
  
  plot <- results %>%
    filter(n %in% n_plot) %>%
    ggplot(mapping = aes(x = burn_pred,
                         y = burn_true,
                         colour = direction_correct)) +
    facet_grid(. ~ ticks_included,
               labeller = labeller(ticks_included = supp_labs)) +
    geom_point() +
    theme_minimal() +
    labs(x = 'predicted burn percentage',
         y = 'true burn percentage',
         colour = 'Number of directions \n predicted correctly') +
    theme(panel.grid.minor.y = element_blank(),
          panel.grid.minor.x = element_blank())
  
  return(plot)
}

PrepErrorsForPlotting <- function(errors){
  errors_long <- errors %>%
    rename(perc_all = perc_correct_params,
           perc_density = perc_density_correct,
           perc_directions = perc_directions_correct,
           perc_densitycat = perc_correct_cat_density,
           ppp_density = point_pred_performance_density,
           kappa_directions = directions_kappa,
           f1_directions = directions_f1,
           mcc_directions = directions_mcc) %>%
    pivot_longer(cols = - c(noise, sample_size, sample_method,
                            summarise_runs, datapoints, algorithm),
                 names_to = 'metric',
                 values_to = 'value') %>%
    rowwise %>%
    mutate(metric_general = str_split(metric, '_', 2)[[1]][1],
           entity = str_split(metric, '_', 2)[[1]][2]) %>%
    select(- metric) %>%
    rename(metric = metric_general)
  
  return(errors_long)
}

# % of correct predictions of the parameters
PlotPercCorrectParams <- function(errors){
  entity_labs <- c('all' = 'density & directions',
                   'density' = 'tree density (%)',
                   'directions' = 'fire spreading \n in # directions',
                   'densitycat' = 'density within 10% \n of true density')
  
  plot_errors_perc <- errors_long %>%
    filter(metric == 'perc') %>%
    ggplot(mapping = aes(x = fct_relevel(datapoints,
                                         'endpoints',
                                         'timepoints'),
                         y = value,
                         fill = fct_relevel(noise,
                                            'clean',
                                            'noise'))) +
    geom_bar(position = 'dodge', stat = 'identity') +
    geom_text(aes(label = round(value, digits = 1)),
              position = position_dodge(width = 0.9),
              size = 2) +
    facet_grid(entity ~ summarise_runs,
               scales = 'free_y',
               labeller = labeller(entity = entity_labs)) +
    theme_minimal() +
    labs(x = NULL,
         fill = 'test noise',
         y = '% correctly predicted parameters') +
    theme(axis.text.x = element_text(angle = 90),
          strip.text.y.right = element_text(angle = 0),
          panel.border = element_rect(fill = NA,
                                      colour = 'lightgrey')) +
    scale_fill_brewer(palette ='Set2')
  
  return(plot_errors_perc)
}

# RMSE etc. of density
PlotErrorsDensity <- function(errors){
  entity_labs <- c('all' = 'density & directions',
                   'density' = 'tree density (%)',
                   'directions' = 'fire spreading \n in # directions',
                   'densitycat' = 'density within 10% \n of true density')
  
  plot_errors_rmse <- errors_long %>%
    filter(metric %in% c('RMSE', 'ppp')) %>%
    ggplot(mapping = aes(x = fct_relevel(datapoints,
                                         'endpoints',
                                         'timepoints'),
                         y = value,
                         fill = fct_relevel(noise,
                                            'clean',
                                            'noise'))) +
    geom_bar(position = 'dodge', stat = 'identity') +
    geom_text(aes(label = round(value, digits = 1)),
              position = position_dodge(width = 0.9),
              size = 2) +
    facet_grid(metric ~ summarise_runs,
               scales = 'free_y',
               labeller = labeller(entity = entity_labs)) +
    theme_minimal() +
    labs(x = NULL,
         fill = 'test noise',
         y = NULL,
         subtitle = 'tree density (%)') +
    theme(axis.text.x = element_text(angle = 90),
          strip.text.y.right = element_text(angle = 0),
          panel.border = element_rect(fill = NA,
                                      colour = 'lightgrey')) +
    scale_fill_brewer(palette ='Set2')
  
  return(plot_errors_rmse)
}

# plot kappa score, F1 score, and MCC for directions
PlotErrorsDirections <- function(errors, n_plot){
  metric_labs <- c('f1' = 'F1 score',
                   'kappa' = 'kappa score',
                   'mcc' = 'MCC')
  
  plot_errors_directions <- errors_long %>%
    filter(metric %in% c('kappa', 'f1', 'mcc')) %>%
    ggplot(mapping = aes(x = fct_relevel(datapoints,
                                         'endpoints',
                                         'timepoints'),
                         y = value,
                         fill = fct_relevel(noise,
                                            'clean',
                                            'noise'))) +
    geom_bar(position = 'dodge', stat = 'identity') +
    geom_text(aes(label = round(value, digits = 1)),
              position = position_dodge(width = 0.9),
              size = 2) +
    facet_grid(metric ~ summarise_runs,
               scales = 'free_y',
               labeller = labeller(metric = metric_labs)) +
    theme_minimal() +
    labs(x = NULL,
         fill = 'test noise',
         y = NULL,
         subtitle = 'fire spreading in 4/8 directions') +
    theme(axis.text.x = element_text(angle = 90),
          strip.text.y.right = element_text(angle = 0),
          panel.border = element_rect(fill = NA,
                                      colour = 'lightgrey')) +
    scale_fill_brewer(palette ='Set2')
  
  return(plot_errors_directions)
}

# RMSE etc. of burn percentage
PlotErrorsBurnpercentage(errors) <- function(errors){
  p1 <- errors %>%
    ggplot(mapping = aes(x = noise, y = RMSE_burn,
                         fill = summarise_runs)) +
    geom_bar(position = 'dodge', stat = 'identity') +
    theme_minimal() +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          axis.text.x = element_text(angle = 90)) +
    labs(y = 'RMSE burn percentage', fill = 'runs summarised y/n') +
    facet_grid(~ datapoints)
  
  p2 <- errors %>%
    ggplot(mapping = aes(x = noise, y = pcc_burn,
                         fill = summarise_runs)) +
    geom_bar(position = 'dodge', stat = 'identity') +
    theme_minimal() +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          axis.text.x = element_text(angle = 90)) +
    labs(y = 'Pearsons correlation coefficient burn percentage',
         fill = 'runs summarised') +
    lims(y = c(0, NA)) +
    facet_grid(~ datapoints)
  
  p3 <- errors %>%
    ggplot(mapping = aes(x = noise, y = dtw_burn,
                         fill = summarise_runs)) +
    geom_bar(position = 'dodge', stat = 'identity') +
    theme_minimal() +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          axis.text.x = element_text(angle = 90)) +
    labs(y = 'Dynamic Time Warping Score burn percentage',
         fill = 'runs summarised') +
    lims(y = c(0, NA)) +
    facet_grid(~ datapoints)
  
  plot <- p1 + p2 + p3 +
    plot_layout(guides = 'collect') +
    plot_annotation(tag_levels = 'A') &
    theme(plot.tag = element_text(size = 8)) &
    xlab(NULL)
  
  return(plot)
}