# Description ############################

# This code loads the batch data downloaded from JATOS, and extracts prolific 
# IDs, so then I can manually assign random IDs to them and substitute them
# in the data files.

# 1. Download the data from JATOS.
# 2. Run this code.
# 3. Manually copy to prolific IDs into the prol_id_to_rand_id.csv file, and 
# assign new random IDs.
# 4. Run 2_save_individual_files.R code.


# Global setup ###########################

rm(list=ls())

source('./scripts/utils/load_all_libraries.R')

# Start reading the files ##################

# Get a list of all files in the folder
incoming_files <- list.files('./data/incoming_data/jatos_gui_downloads/')

prol_ids <- c()

for (iFile in incoming_files){
        
        print(iFile)
        
        # Parse it
        my_data <- read_file(paste0('./data/incoming_data/jatos_gui_downloads/',iFile))
        
        # Find the data submission module
        start_loc <- str_locate_all(my_data, 'data_submission_start---')[[1]]
        end_loc   <- str_locate_all(my_data, '---data_submission_end]')[[1]]
        
        # If no data submission module, skip
        if (is.na(start_loc)){
                
                print(print0('No data submission module. Skipping file',
                             iFile))
                next
                
        } else {
                
                
                for (iPtp in seq(nrow(start_loc))){
                        
                        json_content <- substr(my_data,start_loc[iPtp,2]+1,end_loc[iPtp,1]-1)
                        
                        json_decoded <- fromJSON(json_content)
                        
                        print(json_decoded$prolific_ID)  
                        
                        prol_ids <- append(prol_ids,json_decoded$prolific_ID)      
                        
                        
                }
          
                
        }

}

# Now here, save these as CSV
write_csv(as.data.frame(prol_ids),
          paste0('../../../',
                 Sys.getenv("USERNAME"),
                 '/ownCloud/Cambridge/PhD/projects/fast_schema_mapping/prolific_metadata/incoming_prol_ids.csv'))