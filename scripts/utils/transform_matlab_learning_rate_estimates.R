# Description ##############################

# Transform matlab learning rate estimates with 1.5IQR checks.
# Also, mark as to-be-excluded if missed the 1st trial
# 
# transform_matlab_learning_rate_estimates <- function(ml_learning_rate,
#                                                      mean_by_rep_long_all_types){
#         
         # Global setup #############################
        rm(list=ls())
        source('./scripts/utils/load_all_libraries.R')       
        

        ## Import the data frames #####################
        # if (missing('ml_learning_rate')){
                ml_learning_rate <- import('./results/learning_rate_fits_matlab.csv')
        # }
        
        
        # if (missing('mean_by_rep_long_all_types')){
                mean_by_rep_long_all_types <- import('./results/mean_by_rep_long_all_types.csv')
        # }
        
        qc_table <- import('./results/qc_check_sheets/qc_table.csv')
                
        # Mark if missed first rep #######################
        mean_by_rep_long_all_types <- mean_by_rep_long_all_types %>%
        filter(border_dist_closest == 'all',
               hidden_pa_img_row_number_across_blocks == 1) %>% 
        mutate(no_data_first_rep = is.na(mouse_error_mean))
        
        # Add this columnt to matlab estimates
        ml_learning_rate <- merge(ml_learning_rate,
                                  select(mean_by_rep_long_all_types,
                                         ptp,
                                         condition,
                                         border_dist_closest,
                                         hidden_pa_img_type,
                                         no_data_first_rep))
        
        
        # Mark if outside 1.5IQR rule ###############################
        
        # Get the list of people passing QC till now
        good_ptp <- qc_table %>%
                filter(qc_fail_break_rt == F,
                       qc_fail_missing_or_fast == F,
                       qc_fail_manual == F,
                       qc_fail_mouse_error == F,
                       qc_fail_instructions_rt == F,
                       qc_fail_display_issues == F) %>%
                select(ptp) %>% .[[1]]
        
        # Get data from only these participants 
        good_ptp_data <- ml_learning_rate %>%
                filter(ptp %in% good_ptp) %>%
                droplevels()
        
        # Calculate the Q1, Q2, IQR, and Q1-1.5xIQR and Q3+1.5xIQR
        learning_rate_check_iqr <- good_ptp_data %>%
                group_by(condition,
                         hidden_pa_img_type) %>%
                mutate(Q1_two_param = quantile(.$learning_rate_two_param, probs = 0.25),
                       Q3_two_param = quantile(.$learning_rate_two_param, probs = 0.75),
                       IQR_two_param = Q3_two_param - Q1_two_param,
                       lower_boundary_two_param = Q1_two_param - IQR_two_param * 1.5,
                       upper_boundary_two_param = Q3_two_param + IQR_two_param * 1.5,
                       outside_iqr_two_param = learning_rate_two_param < lower_boundary_two_param | 
                               learning_rate_two_param > upper_boundary_two_param,
                       Q1_three_param = quantile(.$learning_rate_three_param, probs = 0.25),
                       Q3_three_param = quantile(.$learning_rate_three_param, probs = 0.75),
                       IQR_three_param = Q3_three_param - Q1_three_param,
                       lower_boundary_three_param = Q1_three_param - IQR_three_param * 1.5,
                       upper_boundary_three_param = Q3_three_param + IQR_three_param * 1.5,
                       outside_iqr_three_param = learning_rate_three_param < lower_boundary_three_param | 
                               learning_rate_three_param > upper_boundary_three_param,
                       ) %>%
                ungroup()
        
        # learning_rate_check_iqr %>% 
        #         filter(condition == 'no_schema',hidden_pa_img_type == 'all_pa') %>% 
        #         select(learning_rate_two_param,outside_iqr_two_param) -> a
        # 
        # # Does mutate work on grouped data?
        # a %>% filter(outside_iqr_two_param == F) %>% select(learning_rate_two_param) %>% .[[1]] %>% mean()
        # 
        # a %>% mutate(group_average_data_two_param = mean(learning_rate_two_param[outside_iqr_two_param == F]))
        # 
        
        # Calculate group specific averages #########################################
        
        # What is the group-specific learning rate average, for those within 1.5IQR rule?
        learning_rate_check_iqr <- learning_rate_check_iqr %>%
                group_by(condition,
                         hidden_pa_img_type) %>%
                mutate(group_average_data_two_param   = mean(learning_rate_two_param[outside_iqr_two_param == F]),
                       group_average_data_three_param = mean(learning_rate_three_param[outside_iqr_three_param == F])) %>%
                ungroup()
        
        # Create new columns without outlier data ###################################
        # Finally, create new columns, with the learning rate being substituted by group average rate for
        learning_rate_check_iqr <- learning_rate_check_iqr %>%
                mutate(learning_rate_two_param_no_outlier = case_when(
                        outside_iqr_two_param == T ~ group_average_data_two_param,
                        TRUE ~ learning_rate_two_param
                ),
                learning_rate_three_param_no_outlier = case_when(
                        outside_iqr_three_param == T ~ group_average_data_three_param,
                        TRUE ~ learning_rate_three_param
                ))
                
        
        # Merge with ml_learning_rate
        ml_learning_rate <- merge(ml_learning_rate,
                                  learning_rate_check_iqr,
                                  by = c('ptp',
                                         'condition',
                                         'hidden_pa_img_type'),
                                  all.x = T)
# }



