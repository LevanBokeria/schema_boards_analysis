# Description ############################


# Global setup ###########################

rm(list=ls())

source('./scripts/utils/load_all_libraries.R')
source('./scripts/utils/load_transform_data.R')
source('./scripts/utils/functions_for_fitting_learning_curves.R')

# Flags
saveData <- T

# Start combining data ######################################################

ml_learning_rate <- import('./results/learning_rate_fits_matlab.csv')

# If data summary already has the model fitting columns, remove those
data_summary <- data_summary %>%
        select(-c(starts_with('sse_'),
                  starts_with('asymptote_'),
                  starts_with('intercept_'),
                  starts_with('learning_rate_')))

data_summary <- merge(data_summary,
                      ml_learning_rate,
                      by = c('ptp',
                             'condition',
                             'hidden_pa_img_type'),
                      all.x = T) 

## Calculate AIC ----------------------------
data_summary %>%
        mutate(aic_two_param = get_aic(sse_two_param,
                                       2)) %>% View()



## Calculate predicted y values  ------------
y_hat_three_param <-
        ml_learning_rate %>%
        group_by(ptp,
                 condition,
                 hidden_pa_img_type) %>% 
        mutate(y_hat_three_param = list(fit_learning(c(intercept_three_param,
                                                       learning_rate_three_param,
                                                       intercept_three_param - asymptote_three_param),
                                                     seq(1:8),
                                                     seq(1:8),
                                                     ret = 'fit',
                                                     print_output = FALSE,
                                                     which_model = 'three_param')),
               hidden_pa_img_row_number_across_blocks = list(seq(1:8))) %>%
        unnest(c(y_hat_three_param,
                 hidden_pa_img_row_number_across_blocks)) %>% 
        select(c(ptp,
                 condition,
                 hidden_pa_img_type,
                 y_hat_three_param,
                 hidden_pa_img_row_number_across_blocks)) %>%
        ungroup() 

y_hat_two_param <-
        ml_learning_rate %>%
        group_by(ptp,
                 condition,
                 hidden_pa_img_type) %>% 
        mutate(y_hat_two_param = list(fit_learning(c(intercept_two_param,
                                                     learning_rate_two_param),
                                                   seq(1:8),
                                                   seq(1:8),
                                                   ret = 'fit',
                                                   print_output = FALSE,
                                                   which_model = 'two_param')),
               hidden_pa_img_row_number_across_blocks = list(seq(1:8))) %>%
        unnest(c(y_hat_two_param,
                 hidden_pa_img_row_number_across_blocks)) %>% 
        select(c(ptp,
                 condition,
                 hidden_pa_img_type,
                 y_hat_two_param,
                 hidden_pa_img_row_number_across_blocks)) %>%
        ungroup()

y_hat_both_models <- merge(y_hat_three_param,
                           y_hat_two_param,
                           by = c('ptp',
                                  'condition',
                                  'hidden_pa_img_type',
                                  'hidden_pa_img_row_number_across_blocks')) %>%
        mutate(border_dist_closest = 'all')

# If mean_by_rep already has the predicted data as columns, remove those
mean_by_rep_long_all_types <- mean_by_rep_long_all_types %>%
        select(-starts_with('y_hat_'))

## Now merge with predicted data -------------------
mean_by_rep_long_all_types <- merge(mean_by_rep_long_all_types,
                                    y_hat_both_models,
                                    by = c('ptp',
                                           'condition',
                                           'hidden_pa_img_type',
                                           'hidden_pa_img_row_number_across_blocks',
                                           'border_dist_closest'),
                                    all.x = TRUE)

# Save the data #################################
if (saveData){
        write_csv(mean_by_rep_long_all_types,
                  file = './results/mean_by_rep_long_all_types.csv')
        
        write_csv(data_summary,file = './results/data_summary.csv')
}
