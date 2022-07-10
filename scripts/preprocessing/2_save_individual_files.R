# Description ############################

# This script will load the group data file downloaded from JATOS, find the 
# 'data submission' modules for each participant, and save those as separate
# RData files. Prolific IDs are substituted with sub_00* strings.

# Global setup ###########################

rm(list=ls())

source('./scripts/utils/load_all_libraries.R')


file_location <- 'jatos_gui_downloads'

# Start reading the files ##################

# Get the file mapping prolific IDs with randID
prol_to_rand <- read_csv(paste0('../../../',
                                Sys.getenv("USERNAME"),
                                '/ownCloud/Cambridge/PhD/projects/fast_schema_mapping/prolific_metadata/prol_id_to_rand_id.csv'))

# Get a list of all files in the folder
incoming_files <- list.files(paste0('./data/incoming_data/',file_location,'/'))

prol_ids <- c()

for (iFile in incoming_files){
        
        print(iFile)
        
        # Parse it
        my_data <- read_file(paste0('./data/incoming_data/',file_location,'/',iFile))
        
        # Find the data submission module
        start_loc <- str_locate_all(my_data, 'data_submission_start---')[[1]]
        end_loc   <- str_locate_all(my_data, '---data_submission_end]')[[1]]
        
        for (iPtp in seq(nrow(start_loc))){
                
                json_content <- substr(my_data,start_loc[iPtp,2]+1,end_loc[iPtp,1]-1)
                
                json_decoded <- fromJSON(json_content)
                
                print(json_decoded$prolific_ID)  
                
                # Find the rand_id of this person
                iRand_id <- prol_to_rand$rand_id[prol_to_rand$prol_id == json_decoded$prolific_ID]
        
                
                # Re-decode the content
                json_decoded$prolific_ID <- iRand_id
                
                # Save the data submission module output
                export(json_decoded,paste0('./data/',iRand_id,'.RDS'))
                
                print(paste0('Saved ', json_decoded$prolific_ID))
                
        }        
        
        
}