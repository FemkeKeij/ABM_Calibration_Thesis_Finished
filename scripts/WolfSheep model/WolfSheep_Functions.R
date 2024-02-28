################### ERROR COMPUTATION ########################
# Function to compute errors for continuous parameters:
WSComputeErrors <- function(predictions,
                            sample_size, sample_method,
                            summarise_runs, datapoints,
                            double_noise = TRUE){
  # predictions: dataframe with results from fitting
  # sample_size, sample_method, summarise_runs, datapoints: info to
  #     pass on to output data frame
  # double_noise: indicates whether the dataframe includes temporal
  #     noise as well as output noise
  
  # to store the results:
  errors <- tibble(perc_correct_params = numeric(),
                   perc_initial_sheep_correct = numeric(),
                   perc_initial_wolves_correct = numeric(),
                   perc_food_sheep_correct = numeric(),
                   perc_food_wolves_correct = numeric(),
                   perc_sheep_rep_correct = numeric(),
                   perc_wolves_rep_correct = numeric(),
                   perc_grass_correct = numeric(),
                   RMSE_initial_sheep = numeric(),
                   RMSE_initial_wolves = numeric(),
                   RMSE_food_sheep = numeric(),
                   RMSE_food_wolves = numeric(),
                   RMSE_sheep_rep = numeric(),
                   RMSE_wolves_rep = numeric(),
                   RMSE_grass = numeric(),
                   NRMSE_initial_sheep = numeric(),
                   NRMSE_initial_wolves = numeric(),
                   NRMSE_food_sheep = numeric(),
                   NRMSE_food_wolves = numeric(),
                   NRMSE_sheep_rep = numeric(),
                   NRMSE_wolves_rep = numeric(),
                   NRMSE_grass = numeric(),
                   ppp_initial_sheep = numeric(),
                   ppp_initial_wolves = numeric(),
                   ppp_food_sheep = numeric(),
                   ppp_food_wolves = numeric(),
                   ppp_sheep_rep = numeric(),
                   ppp_wolves_rep = numeric(),
                   ppp_grass = numeric(),
                   #RMSE_sheep = numeric(),
                   #pcc_sheep = numeric(),
                   #dtw_sheep = numeric(),
                   #RMSE_wolves = numeric(),
                   #pcc_wolves = numeric(),
                   #dtw_wolves = numeric(),
                   #RMSE_grass = numeric(),
                   #pcc_grass = numeric(),
                   #dtw_grass = numeric(),
                   fold = numeric(),
                   noise = character())
  
  # for each noise level
  if(double_noise){
    noise_levels <- c('clean', 'output noise', 'double noise')
  } else {
    noise_levels <- c('clean', 'output noise')
  }
  
  for(i in noise_levels){
    data_noise <- predictions %>%
      filter(noise == i)
    for(j in 1:5){
      # compute mean values in training data
      mean_vals <- data_noise %>%
        filter(fold != j) %>%
        summarise(mean_initial_number_sheep =
                    mean(initial_number_sheep),
                  mean_initial_number_wolves = 
                    mean(initial_number_wolves),
                  mean_sheep_gain_from_food = 
                    mean(sheep_gain_from_food),
                  mean_wolves_gain_from_food = 
                    mean(wolves_gain_from_food),
                  mean_sheep_reproduce = mean(sheep_reproduce),
                  mean_wolves_reproduce = mean(wolves_reproduce),
                  mean_grass_regrowth = mean(grass_regrowth),
                  #mean_sheep = mean(sheep),
                  #mean_wolves = mean(wolves),
                  #mean_grass = mean(grass)
                  )
      
      data <- data_noise %>%
        filter(fold == j)
      
      new_errors <- data %>%
        summarise(perc_correct_params =
                    sum(initial_number_sheep_pred ==
                          initial_number_sheep &
                          initial_number_wolves_pred ==
                          initial_number_wolves & 
                          sheep_gain_from_food_pred ==
                          sheep_gain_from_food &
                          wolves_gain_from_food_pred ==
                          wolves_gain_from_food &
                          sheep_reproduce_pred ==
                          sheep_reproduce &
                          wolves_reproduce_pred == 
                          wolves_reproduce & 
                          grass_regrowth_pred ==
                          grass_regrowth) / nrow(data) * 100,
                  perc_initial_sheep_correct =
                    sum(initial_number_sheep ==
                          initial_number_sheep_pred) /
                    nrow(data) * 100,
                  perc_initial_wolves_correct =
                    sum(initial_number_wolves ==
                          initial_number_wolves_pred) /
                    nrow(data) * 100,
                  perc_food_sheep_correct =
                    sum(sheep_gain_from_food ==
                          sheep_gain_from_food_pred) /
                    nrow(data) * 100,
                  perc_food_wolves_correct =
                    sum(wolves_gain_from_food ==
                          wolves_gain_from_food_pred) /
                    nrow(data) * 100,
                  perc_sheep_rep_correct =
                    sum(sheep_reproduce == sheep_reproduce_pred) /
                    nrow(data) * 100,
                  perc_wolves_rep_correct =
                    sum(wolves_reproduce == wolves_reproduce_pred) /
                    nrow(data) * 100,
                  perc_grass_correct =
                    sum(grass_regrowth == grass_regrowth_pred) /
                    nrow(data) * 100,
                  RMSE_initial_sheep =
                    sqrt(sum((initial_number_sheep_pred -
                                initial_number_sheep)^2)) /
                    nrow(data),
                  RMSE_initial_wolves =
                    sqrt(sum((initial_number_wolves_pred -
                                initial_number_wolves)^2)) /
                    nrow(data),
                  RMSE_food_sheep =
                    sqrt(sum((sheep_gain_from_food_pred -
                                sheep_gain_from_food)^2)) /
                    nrow(data),
                  RMSE_food_wolves =
                    sqrt(sum((wolves_gain_from_food_pred -
                                wolves_gain_from_food)^2)) /
                    nrow(data),
                  RMSE_sheep_rep =
                    sqrt(sum((sheep_reproduce_pred -
                                sheep_reproduce)^2)) / nrow(data),
                  RMSE_wolves_rep =
                    sqrt(sum((wolves_reproduce_pred -
                                wolves_reproduce)^2)) / nrow(data),
                  RMSE_grass =
                    sqrt(sum((grass_regrowth_pred -
                                grass_regrowth)^2)) / nrow(data),
                  NRMSE_initial_sheep =
                    (sqrt(sum((initial_number_sheep_pred -
                                 initial_number_sheep)^2)) /
                       nrow(data)) / sd(initial_number_sheep),
                  NRMSE_initial_wolves =
                    (sqrt(sum((initial_number_wolves_pred -
                                 initial_number_wolves)^2)) /
                       nrow(data)) / sd(initial_number_wolves),
                  NRMSE_food_sheep =
                    (sqrt(sum((sheep_gain_from_food_pred -
                                 sheep_gain_from_food)^2)) /
                       nrow(data)) / sd(sheep_gain_from_food),
                  NRMSE_food_wolves =
                    (sqrt(sum((wolves_gain_from_food_pred -
                                 wolves_gain_from_food)^2)) /
                       nrow(data)) / sd(wolves_gain_from_food),
                  NRMSE_sheep_rep =
                    (sqrt(sum((sheep_reproduce_pred -
                                 sheep_reproduce)^2)) / nrow(data)) /
                    sd(sheep_reproduce),
                  NRMSE_wolves_rep =
                    (sqrt(sum((wolves_reproduce_pred -
                                 wolves_reproduce)^2)) /
                       nrow(data)) / sd(wolves_reproduce),
                  NRMSE_grass =
                    (sqrt(sum((grass_regrowth_pred -
                                 grass_regrowth)^2)) /
                       nrow(data)) / sd(grass_regrowth),
                  ppp_initial_sheep =
                    1 - sum(sqrt((initial_number_sheep_pred -
                                    initial_number_sheep)^2)) /
                    sum(sqrt((initial_number_sheep  - 
                                mean_vals$mean_initial_number_sheep)^2)),
                  ppp_initial_wolves =
                    1 - sum(sqrt((initial_number_wolves_pred -
                                    initial_number_wolves)^2)) /
                    sum(sqrt((initial_number_wolves -
                                mean_vals$mean_initial_number_wolves)^2)),
                  ppp_food_sheep =
                    1 - sum(sqrt((sheep_gain_from_food_pred -
                                    sheep_gain_from_food)^2)) /
                    sum(sqrt((sheep_gain_from_food -
                                mean_vals$mean_sheep_gain_from_food)^2)),
                  ppp_food_wolves =
                    1 - sum(sqrt((wolves_gain_from_food_pred -
                                    wolves_gain_from_food)^2)) /
                    sum(sqrt((wolves_gain_from_food -
                                mean_vals$mean_wolves_gain_from_food)^2)),
                  ppp_sheep_rep =
                    1 - sum(sqrt((sheep_reproduce_pred -
                                    sheep_reproduce)^2)) /
                    sum(sqrt((sheep_reproduce -
                                mean_vals$mean_sheep_reproduce)^2)),
                  ppp_wolves_rep =
                    1 - sum(sqrt((wolves_reproduce_pred -
                                    wolves_reproduce)^2)) /
                    sum(sqrt((wolves_reproduce -
                                mean_vals$mean_wolves_reproduce)^2)),
                  ppp_grass =
                    1 - sum(sqrt((grass_regrowth_pred -
                                    grass_regrowth)^2)) /
                    sum(sqrt((grass_regrowth -
                                mean_vals$mean_grass_regrowth)^2)),
                  fold = j,
                  noise = i)
      
      # compute and add RMSE
      #new_errors$RMSE_sheep <- NA
      #new_errors$RMSE_wolves <- NA
      #new_errors$RMSE_grass <- NA
      
      # compute and add Pearson's correlation coefficient
      #new_errors$pcc_sheep <- NA
      #new_errors$pcc_wolves <- NA
      #new_errors$pcc_grass <- NA
      
      # compute and add DTW
      #new_errors$dtw_sheep <- NA
      #new_errors$dtw_wolves <- NA
      #new_errors$dtw_grass <- NA
      
      #if(datapoints == 'endpoints'){
        # there's just 1 timepoint, so use the regular RMSE
        #errors_new$RMSE_sheep <- sqrt(sum((sheep_pred -
         #                                   sheep)^2)) / nrow(dat)
        #errors_new$RMSE_wolves <- sqrt(sum((wolves_pred -
         #                                    wolves)^2)) / nrow(dat)
        #errors_new$RMSE_grass <- sqrt(sum((grass_pred -
         #                                    grass)^2)) / nrow(dat)
      #} else {
        #RMSE_out <- dat %>%
         # group_by(id) %>%
          #mutate(RMSE_b_sheep = sqrt(sum(sheep_pred - 
           #                          sheep)^2) / 2,
            #     RMSE_A_sheep = sqrt(sum(sheep_pred -
             #                        mean_vals$mean_sheep )^2) / 2,
              #   RMSE_b_wolves = sqrt(sum(wolves_pred - 
               #                       wolves)^2) / 2,
                # RMSE_A_wolves = sqrt(sum(wolves_pred -
                 #                     mean_vals$mean_wolves )^2) / 2,
                 #RMSE_b_grass = sqrt(sum(grass_pred - 
                  #                      grass)^2) / 2,
                 #RMSE_A_grass = sqrt(sum(grass_pred -
                  #                      mean_vals$mean_grass )^2) / 2) %>%
          #mutate(RMSE_ind_sheep = RMSE_A_sheep / RMSE_b_sheep,
           #      RMSE_ind_wolves = RMSE_A_wolves / RMSE_b_wolves,
            #     RMSE_ind_grass = RMSE_A_grass / RMSE_b_grass) %>%
          #ungroup(id) %>%
          #summarise(RMSE_sheep = mean(RMSE_ind_sheep),
           #         RMSE_wolves = mean(RMSE_ind_wolves),
            #        RMSE_grass = mean(RMSE_ind_grass))
        #errors_new$RMSE_sheep <- RMSE_out$RMSE_sheep
        #errors_new$RMSE_wolves <- RMSE_out$RMSE_wolves
        #errors_new$RMSE_grass <- RMSE_out$RMSE_grass
        
        #pcc_out <- dat %>%
         # group_by(id) %>%
          #mutate(rho_sheep = cor(sheep_pred, sheep),
           #      rho_wolves = cor(wolves_pred, wolves),
            #     rho_grass = cor(grass_pred, grass)) %>%
          #mutate(Z_sheep = 0.5 * log(1 + rho_sheep)
           #      - log(1 - rho_sheep),
            #     Z_wolves = 0.5 * log(1 + rho_wolves)
             #    - log(1 - rho_wolves),
              #   Z_grass = 0.5 * log(1 + rho_grass)
               #  - log(1 - rho_grass)) %>%
          #ungroup() %>%
          #summarise(meanZ_sheep = mean(Z_sheep),
           #         meanZ_wolves = mean(Z_wolves),
            #        meanZ_grass = mean(Z_grass),) %>%
          #summarise(mean_rho_sheep = (exp(2 * meanZ_sheep) - 1)/
           #           (exp(2 * meanZ_sheep) - 1),
            #        mean_rho_wolves = (exp(2 * meanZ_wolves) - 1)/
             #         (exp(2 * meanZ_wolves) - 1),
              #      mean_rho_grass = (exp(2 * meanZ_grass) - 1)/
               #       (exp(2 * meanZ_grass) - 1))
        #errors_new$pcc_sheep <- pcc_out$mean_rho_sheep
        #errors_new$pcc_wolves <- pcc_out$mean_rho_wolves
        #errors_new$pcc_grass <- pcc_out$mean_rho_grass
        
        #dtws_sheep <- numeric()
        #dtws_wolves <- numeric()
        #dtws_grass <- numeric()
        #for(k in unique(dat$id)){
         # x_mat_sheep <- dat %>%
          #  filter(id == k) %>%
           # select(sheep)
          #y_mat_sheep <- dat %>%
           # filter(id == k) %>%
            #select(sheep_pred)
          #dtws_sheep[k] <- dtw(x_mat_sheep, y_mat_sheep,
           #              distance.only = TRUE)$normalizedDistance
          
          #x_mat_wolves <- dat %>%
            #filter(id == k) %>%
            #select(wolves)
          #y_mat_wolves <- dat %>%
           # filter(id == k) %>%
            #select(wolves_pred)
          #dtws_wolves[k] <- dtw(x_mat_wolves, y_mat_wolves,
           #                    distance.only = TRUE)$normalizedDistance
        #errors_new$dtw_wolves <- mean(dtws_wolves)
        
        #x_mat_grass <- dat %>%
          #filter(id == k) %>%
          #select(grass)
        #y_mat_grass <- dat %>%
          #filter(id == k) %>%
          #select(grass_pred)
        #dtws_grass[k] <- dtw(x_mat_grass, y_mat_grass,
         #                    distance.only = TRUE)$normalizedDistance
      #}
      errors <- errors %>%
        add_row(new_errors)
      #}
    }
  }
  # compute mean over folds (per noise type)
  errors <- errors %>%
    group_by(noise) %>%
    summarise_all(mean) %>%
    select(- fold) %>%
    # add the experiment info to the df
    add_column(sample_size = sample_size,
               sample_method = sample_method,
               summarise_runs = summarise_runs,
               datapoints = datapoints)
  
  return(errors)  
}


################### VISUALISE ERRORS ########################
PrepErrorsForPlotting <- function(errors_df){
  errors_long <- errors_df %>%
    pivot_longer(cols = - c(noise, sample_size, sample_method,
                            summarise_runs, datapoints),
                 names_to = 'metric',
                 values_to = 'value') %>%
    rowwise %>%
    mutate(metric_general = str_split(metric, '_', 2)[[1]][1],
           entity = str_split(metric, '_', 2)[[1]][2]) %>%
    select(- metric) %>%
    rename(metric = metric_general) %>%
    mutate(entity = ifelse(entity == 'initial_sheep_correct',
                           'initial_sheep', entity),
           entity = ifelse(entity == 'initial_wolves_correct',
                           'initial_wolves', entity),
           entity = ifelse(entity == 'food_sheep_correct',
                           'food_sheep', entity),
           entity = ifelse(entity == 'food_wolves_correct',
                           'food_wolves', entity),
           entity = ifelse(entity == 'sheep_rep_correct',
                           'sheep_rep', entity),
           entity = ifelse(entity == 'wolves_rep_correct',
                           'wolves_rep', entity),
           entity = ifelse(entity == 'grass_correct',
                           'grass', entity),
           entity = ifelse(entity == 'correct_params',
                           'all parameters', entity))
  
  return(errors_long)
}

PlotRMSE <- function(errors_long){
  entity_labs <- c('initial_sheep' = 'initial number \n of sheep',
                   #'initial_wolves' = 'initial number \n of wolves',
                   'food_sheep' = 'sheep gain \n from food'#,
                   #'food_wolves' = 'wolves gain \n from food',
                   #'sheep_rep' = 'sheep reproduction \n prob.',
                   #'wolves_rep' = 'wolves reproduction \n prob.',
                   #'grass' = 'grass \n regrowth time'
                   )
  
  plot_errors_RMSE <- errors_long %>%
    filter(metric == 'RMSE') %>%
    ggplot(mapping = aes(x = fct_relevel(datapoints,
                                         'endpoints',
                                         'timesteps 10',
                                         'timesteps 20',
                                         'timesteps 50',
                                         'timeseries'),
                         y = value,
                         fill = fct_relevel(noise,
                                            'clean',
                                            'output noise',
                                            'double noise'))) +
    geom_bar(position = 'dodge', stat = 'identity') +
    geom_text(aes(label = round(value, 1)),
              position = position_dodge(width = 0.9),
              size = 2) +
    facet_grid(entity ~ summarise_runs,
               scales = 'free_y',
               labeller = labeller(entity = entity_labs)) +
    theme_minimal() +
    labs(x = NULL,
         fill = ' test noise',
         y = 'RMSE') +
    theme(axis.text.x = element_text(angle = 90),
          strip.text.y.right = element_text(angle = 0),
          panel.border = element_rect(fill = NA,
                                      colour = 'lightgrey')) +
    scale_fill_brewer(palette ='Set2')
  
  return(list(plot_errors_RMSE))
}

PlotPPP <- function(errors_long){
  entity_labs <- c('initial_sheep' = 'initial number \n of sheep',
                   #'initial_wolves' = 'initial number \n of wolves',
                   'food_sheep' = 'sheep gain \n from food'#,
                   #'food_wolves' = 'wolves gain \n from food',
                   #'sheep_rep' = 'sheep reproduction \n prob.',
                   #'wolves_rep' = 'wolves reproduction \n prob.',
                   #'grass' = 'grass \n regrowth time'
                   )
  
  plot_errors_ppp <- errors_long %>%
    filter(metric == 'ppp') %>%
    ggplot(mapping = aes(x = fct_relevel(datapoints,
                                         'endpoints',
                                         'timesteps 10',
                                         'timesteps 20',
                                         'timesteps 50',
                                         'timeseries'),
                         y = value,
                         fill = fct_relevel(noise,
                                            'clean',
                                            'output noise',
                                            'double noise'))) +
    geom_bar(position = 'dodge', stat = 'identity') +
    geom_text(aes(label = round(value, 2)),
              position = position_dodge(width = 0.9),
              size = 2) +
    facet_grid(entity ~ summarise_runs,
               scales = 'free_y',
               labeller = labeller(entity = entity_labs)) +
    theme_minimal() +
    labs(x = NULL,
         fill = ' test noise',
         y = 'point prediction performance') +
    theme(axis.text.x = element_text(angle = 90),
          strip.text.y.right = element_text(angle = 0),
          panel.border = element_rect(fill = NA,
                                      colour = 'lightgrey')) +
    scale_fill_brewer(palette ='Set2')
  
  return(plot_errors_ppp)
}

PlotPercCorrect <- function(errors_long){
  entity_labs <- c('initial_sheep' = 'initial number \n of sheep',
                   'initial_wolves' = 'initial number \n of wolves',
                   'food_sheep' = 'sheep gain \n from food',
                   'food_wolves' = 'wolves gain \n from food',
                   'sheep_rep' = 'sheep reproduction \n prob.',
                   'wolves_rep' = 'wolves reproduction \n prob.',
                   'grass' = 'grass \n regrowth time',
                   'all parameters' = 'all input \n parameters')
  
  plot_errors_ppp <- errors_long %>%
    filter(metric == 'perc') %>%
    ggplot(mapping = aes(x = fct_relevel(datapoints,
                                         'endpoints',
                                         'timesteps 10',
                                         'timesteps 20',
                                         'timesteps 50',
                                         'timeseries'),
                         y = value,
                         fill = fct_relevel(noise,
                                            'clean',
                                            'output noise',
                                            'double noise'))) +
    geom_bar(position = 'dodge', stat = 'identity') +
    geom_text(aes(label = round(value, digits = 2)),
              position = position_dodge(width = 0.9),
              size = 2) +
    facet_grid(entity ~ summarise_runs,
               scales = 'free_y',
               labeller = labeller(entity = entity_labs)) +
    theme_minimal() +
    labs(x = NULL,
         fill = ' test noise',
         y = '% correctly predicted parameters') +
    theme(axis.text.x = element_text(angle = 90),
          strip.text.y.right = element_text(angle = 0),
          panel.border = element_rect(fill = NA,
                                      colour = 'lightgrey')) +
    scale_fill_brewer(palette ='Set2')
  
  return(plot_errors_ppp)
}