# Description ############################

# This code will load the long form data and analyze the following variables:
# - average block 2 mouse error
# - Average mouse error for each of the 8 repetitions of the PAs. 

# The analysis is done separately for all hiddent-PAs and far-PAs and near-PAs.

# The data will be saved as a csv file

# Global setup ###########################

rm(list=ls())

source('./scripts/utils/load_all_libraries.R')
source('./scripts/utils/load_transform_data.R')

# Flags
saveData <- T


# Start analysis ##########################################

## Mean by PA rep -------------------------------------------

### All PAs =================================================

mean_by_rep_long_all_pa <- long_data %>%
        group_by(ptp,
                 counterbalancing,
                 condition,
                 hidden_pa_img_row_number_across_blocks) %>%
        summarise(mouse_error_mean = mean(mouse_error, na.rm = T),
                  mouse_error_sd   = sd(mouse_error, na.rm = T),
                  mouse_error_n    = n()) %>%
        ungroup() %>%
        mutate(border_dist_closest = 'all',
               hidden_pa_img_type = 'all_pa')


### Near/Far ================================================

mean_by_rep_long_near_far <- long_data %>%
        filter(hidden_pa_img_type %in% c('far','near')) %>%
        droplevels() %>%
        group_by(ptp,
                 counterbalancing,
                 condition,
                 hidden_pa_img_type,
                 hidden_pa_img_row_number_across_blocks) %>%
        summarise(mouse_error_mean = mean(mouse_error, na.rm = T),
                  mouse_error_sd   = sd(mouse_error, na.rm = T),
                  mouse_error_n    = n()) %>%
        ungroup() %>%
        mutate(border_dist_closest = 'all')

### Border distances =========================================

mean_by_rep_long_border_dist <- long_data %>%
        group_by(ptp,
                 counterbalancing,
                 condition,
                 border_dist_closest,
                 hidden_pa_img_row_number_across_blocks) %>%
        summarise(mouse_error_mean = mean(mouse_error, na.rm = T),
                  mouse_error_sd   = sd(mouse_error, na.rm = T),
                  mouse_error_n    = n()) %>%
        ungroup() %>%
        mutate(hidden_pa_img_type = 'all_pa')

### Combine all these ====================================

mean_by_rep_long_all_types <- bind_rows(mean_by_rep_long_all_pa,
                                        mean_by_rep_long_near_far,
                                        mean_by_rep_long_border_dist)


## Block 2 average -----------------------------------------

### All PAs ========================================

data_summary_all_pas <- long_data %>%
        filter(block == 2) %>%
        group_by(ptp,
                 counterbalancing,
                 condition) %>%
        summarise(n_trials = n(),
                  block_2_mouse_error_mean = mean(mouse_error, na.rm = T),
                  block_2_mouse_error_sd   = sd(mouse_error, na.rm = T),
                  block_2_rt_mean = mean(rt, na.rm = T),
                  block_2_rt_sd   = sd(rt, na.rm = T),                  
                  hidden_pa_img_type = 'all_pa') %>% 
        ungroup()

### Near and Far separately ========================================

data_summary_near_far_pas <- long_data %>%
        filter(block == 2,
               hidden_pa_img_type %in% c('near','far')) %>% 
        group_by(ptp,
                 counterbalancing,
                 condition,
                 hidden_pa_img_type) %>%
        summarise(n_trials = n(),
                  block_2_mouse_error_mean = mean(mouse_error, na.rm = T),
                  block_2_mouse_error_sd   = sd(mouse_error, na.rm = T),
                  block_2_rt_mean = mean(rt, na.rm = T),
                  block_2_rt_sd   = sd(rt, na.rm = T)) %>% 
        ungroup()

### Combine =============================
data_summary <- NULL
data_summary <- bind_rows(data_summary_all_pas,
                          data_summary_near_far_pas)

# Clean the extra variables
remove(data_summary_all_pas,data_summary_near_far_pas)

# Save the data #####################################

if (saveData){
        
        write_csv(data_summary,
                  './results/data_summary.csv')
        
        write_csv(mean_by_rep_long_all_types,
                  './results/mean_by_rep_long_all_types.csv')
        
}