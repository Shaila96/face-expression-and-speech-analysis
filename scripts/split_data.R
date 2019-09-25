#-------------------------#
#--------LIBRARIES--------#
#-------------------------#
library(tidyr)
library(dplyr)
library(readr)
library(plotly)
library(data.table)
library(reshape2)
library(ggnewscale)
library(cowplot)
library(cowplot)




#-------------------------#
#-----GLOBAL VARIABLES----#
#-------------------------#
script_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
source(file.path(script_dir, 'us-common-functions.R'))

current_dir <- dirname(script_dir)
setwd(current_dir)



group_list <- c('B', 'C')


#-------------------------#
#---FUNCTION DEFINITION---#
#-------------------------#
split_data <- function() {
  ## Making the df as global variable, as it takes a lot of time to load.
  ## So, during one R session we load this df once
  osf_facs_df <<- custom_read_csv(file.path(current_dir, data_dir, "Physiology+FACS+Gaze+Speech+Judges_v2.csv"))
  
  for (group in group_list) {
    
    group_facs_df <<- osf_facs_df %>%
      filter(Group %in% paste0(group, c('H', 'L')))
    
    print(unique(group_facs_df$Participant_ID))
    convert_to_csv(group_facs_df, file.path(current_dir, curated_data_dir, paste0(group, "_facs.csv")))
  }
}



#-------------------------#
#-------Main Program------#
#-------------------------#
split_data()


