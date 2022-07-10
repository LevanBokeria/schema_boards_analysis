# Description ############################

# This code will load up data submission modules for each participant, and 
# preprocess those JSON files. It will save bunch of dataframes in a long-format
# of the data: feedbacks, trial-by-trial-results, etc.

# Global setup ###########################

rm(list=ls())

source('./scripts/utils/load_all_libraries.R')

saveDataCSV <- T



# Start reading the files ##################

# Get a list of all files in the folder
incoming_files <- list.files('./data/',pattern = '*.RDS')

# Pre-assign variables
block_results_all_ptp <- NULL
listings_all_ptp      <- NULL
feedback_all_ptp      <- NULL

break_rt_all_ptp      <- NULL
instructions_rt_all_ptp <- NULL
int_fb_all_ptp          <- NULL

for (iFile in incoming_files){
        
        ## Preparing -------------------------------------
        print(iFile)
        
        # Parse it
        json_decoded <- import(paste0('./data/',iFile))
        
        print(json_decoded$prolific_ID)  
        
        ## Get the performance output df -----------------------

        block_results <- as_tibble(
                json_decoded$outputData$block_results
        )

        block_results <- block_results %>%
                select(!c('internal_node_id','trial_index','trial_type','trial_stage'))
        
        # Sanity check
        n_trials_per_block <- block_results %>%
                filter(!condition %in% c('practice')) %>% 
                group_by(condition,learning_stage,block) %>%
                summarise(n = n())
        if (any(n_trials_per_block$n != 24)){
                stop('n trials per block is wrong!')
        }
        
        # All the mutations
        block_results <- block_results %>%
                mutate(across(.cols = c(learning_stage,
                                        block,
                                        condition,
                                        arrangement,
                                        pa_img,
                                        pa_img_type),as.factor),
                       correct = as.numeric(correct),
                       mouse_dist_cb = abs(mouse_clientX - pa_center_x) +
                               abs(mouse_clientY - pa_center_y),
                       mouse_error = sqrt(
                               (mouse_clientX - pa_center_x)^2 +
                                       (mouse_clientY - pa_center_y)^2
                       ),                       
                       )
        
        block_results <- block_results %>%
                mutate(ptp = json_decoded$prolific_ID, .before = rt,
                       ptp = as.factor(ptp))
        
        block_results <- block_results %>%
                mutate(counterbalancing = json_decoded$inputData$counterbalancing,
                       .before = rt,
                       counterbalancing = as.factor(counterbalancing))        
        
        
        
        # Add a row counter for each occurrence of a pa_img, 
        # within that condition and within that block
        block_results <- block_results %>%
                group_by(condition,learning_stage,block,pa_img) %>%
                mutate(pa_img_row_number = row_number()) %>%
                ungroup()
        
        # Add a row counter for each occurrence of a pa_img, 
        # within that condition, across the two blocks
        block_results <- block_results %>%
                group_by(condition,learning_stage,pa_img) %>%
                mutate(pa_img_row_number_across_blocks = row_number()) %>%
                ungroup()
        
        
        # Add trial index counter for each block
        block_results <- block_results %>%
                group_by(condition,learning_stage,block) %>%
                mutate(block_trial_idx = row_number(),
                       .after = block) %>%
                ungroup()
        
        
        # Mark which ones are close to the border of the board
        block_results <- block_results %>%
                mutate(dist_border_l = corr_col - 1,
                       dist_border_r = 12 - corr_col,
                       dist_border_t = corr_row - 1,
                       dist_border_b = 12 - corr_row) %>%
                rowwise() %>%
                mutate(border_dist_closest = min(dist_border_l,
                                                 dist_border_r,
                                                 dist_border_t,
                                                 dist_border_b),
                       border_dist_summed = min(dist_border_l,dist_border_r) + 
                                            min(dist_border_t,dist_border_b)) %>%
                ungroup()
        
        block_results <- block_results %>%
                relocate(starts_with('display_information'), 
                         .after = 'border_dist_summed')
        
        # We have several nested dataframes within the display_information column
        # so we must flatten the whole thing. 2 levels of hierarchy, so call this twice:
        block_results <- do.call(data.frame,block_results)
        block_results <- do.call(data.frame,block_results)
        
        # Combine the data across participants
        block_results_all_ptp <- bind_rows(block_results_all_ptp,block_results)
        
        
        ## Intermediate feedback ----------------------------------------
        
        curr_ptp_int_fb <- json_decoded$outputData$break_results[[1]][1,]
        
        for (iBreak in seq(2,11)){
                curr_ptp_int_fb <- bind_rows(curr_ptp_int_fb,json_decoded$outputData$break_results[[iBreak]][1,])
        }
        
        curr_ptp_int_fb <- curr_ptp_int_fb %>%
                mutate(response = response$Q0) %>%
                mutate(ptp = json_decoded$prolific_ID, .before = rt) %>%
                select(c(ptp,rt,response,time_elapsed))
        
        int_fb_all_ptp <- bind_rows(int_fb_all_ptp,curr_ptp_int_fb)
        
        ## Final Feedback -----------------------
        
        # Add up feedback
        curr_ptp_feedback <- 
                as_tibble(
                        json_decoded[["outputData"]][["debriefing"]][["response"]]
                )
        
        # Remove the NA row, from the experiment explanation page
        curr_ptp_feedback <- curr_ptp_feedback %>%
                filter(rowSums(is.na(curr_ptp_feedback)) != ncol(curr_ptp_feedback))
        
        # Add the participant ID
        curr_ptp_feedback <- curr_ptp_feedback %>%
                mutate(ptp = json_decoded$prolific_ID, .before = Q0,
                       ptp = as.factor(ptp))        
        # Concatenate
        feedback_all_ptp <- bind_rows(feedback_all_ptp,curr_ptp_feedback)
        
        
        ## Hidden/Visible item names ----------------------
        
        # Congregate listing of the hidden and visible items
        curr_ptp_listings <- bind_rows(
                json_decoded$outputData$break_results[[1]][3,],
                json_decoded$outputData$break_results[[3]][3,],
                json_decoded$outputData$break_results[[5]][3,],
                json_decoded$outputData$break_results[[7]][3,],
                json_decoded$outputData$break_results[[9]][3,]
        )
        curr_ptp_listings <- curr_ptp_listings %>%
                select(rt,response) %>%
                mutate(response.Q0 = response$Q0,
                       response.Q1 = response$Q1) %>% 
                select(-response) %>% 
                mutate(ptp = json_decoded$prolific_ID, .before = rt) %>% 
                mutate(boards = unique(block_results$condition)[3:7], .before = rt)

        listings_all_ptp <- bind_rows(listings_all_ptp,curr_ptp_listings)
        
        
        ## Times spent at each break --------------------
        curr_ptp_break_rt <- NULL
        # curr_ptp_break_rt[1] <- json_decoded$prolific_ID
        for (iBreak in seq(1,11)){
                curr_ptp_break_rt[iBreak] <- sum(json_decoded$outputData$break_results[[iBreak]]$rt,na.rm=T)
        }
        
        curr_ptp_break_rt <- curr_ptp_break_rt %>%
                as_tibble() %>% 
                rename(time_spent_msec = value) %>%
                mutate(time_spent_mins = time_spent_msec/1000/60) %>%
                mutate(break_idx = 1:nrow(.), .before = time_spent_msec) %>%
                mutate(ptp = json_decoded$prolific_ID, .before = break_idx)
                
        
        
        break_rt_all_ptp <- bind_rows(break_rt_all_ptp,curr_ptp_break_rt)
        
        ## Times spent at instructions ---------------------------
        
        # For the first instructions
        inst_1 <- do.call(rbind, 
                          json_decoded$outputData$instructions_results$instructions_1)
        inst_2 <- do.call(rbind, 
                          json_decoded$outputData$instructions_results$instructions_2)
        
        # Combine these
        curr_ptp_instructions_rt <- rbind(inst_1,inst_2)
        
        # Add the participant id
        curr_ptp_instructions_rt <- curr_ptp_instructions_rt %>%
                mutate(ptp = json_decoded$prolific_ID, .before = page_index)
        
        instructions_rt_all_ptp <- bind_rows(instructions_rt_all_ptp, curr_ptp_instructions_rt)
        
}

block_results_all_ptp <- block_results_all_ptp %>%
        reorder_levels(condition, order = c('practice',
                                            'practice2',
                                            'schema_c',
                                            'schema_ic',
                                            'schema_l',
                                            'random_loc',
                                            'no_schema'))

names(feedback_all_ptp) <- c('ptp',
                             'Clear instructions?',
                             'Notice schema_C',
                             'Notice schema_IC',
                             'Notice landmarks',
                             'Notice random',
                             'Strategy?',
                             'Did visible ones help or hinder?',
                             'Anything else')

# Create condition orders ################
condition_orders <- tibble(.rows = 7)

all_ptp <- unique(block_results_all_ptp$ptp)

for (iPtp in as.vector(all_ptp)){
        iPtp
        condition_orders[iPtp] <-
                unique(
                        block_results_all_ptp$condition[
                                block_results_all_ptp$ptp==iPtp
                        ])
}

# Save everything #######################
if (saveDataCSV){
        
        print('Overwriting data...')
        
        write_csv(block_results_all_ptp,'./results/preprocessed_data/block_results_long_form.csv')
        write_csv(feedback_all_ptp,'./results/preprocessed_data/feedback_all_ptp.csv')
        write_csv(int_fb_all_ptp,'./results/preprocessed_data/intermediate_feedback_all_ptp.csv')
        write_csv(listings_all_ptp,'./results/preprocessed_data/listings_all_ptp.csv')
        write.csv(break_rt_all_ptp,'./results/preprocessed_data/break_rt_all_ptp.csv',
                  row.names = F)
        write.csv(instructions_rt_all_ptp,
                  './results/preprocessed_data/instructions_rt_all_ptp.csv',
                  row.names = FALSE)
        
        print('Data overwritten.')
}
