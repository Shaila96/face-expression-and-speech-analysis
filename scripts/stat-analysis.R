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
library(nonpar)
library(cooccur)
library(EMT)




#-------------------------#
#-----GLOBAL VARIABLES----#
#-------------------------#
script_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
source(file.path(script_dir, 'us-common-functions.R'))

current_dir <- dirname(script_dir)
setwd(current_dir)



facs_size <- 7
geom_text_size <- 4.5

emotion_cols <- c('F_Angry',
                  'F_Disgusted',
                  'F_Afraid',
                  'F_Happy',
                  'F_Sad',
                  'F_Surprised',
                  'F_Neutral')

plot_emotion_cols <- c('Angry',
                       'Disgusted',
                       'Afraid',
                       'Happy',
                       'Sad',
                       'Surprised',
                       'Neutral')

group_list <- c('B', 'C')


#-------------------------#
#---FUNCTION DEFINITION---#
#-------------------------#
read_data <- function() {
  ## Making the df as global variable, as it takes a lot of time to load.
  ## So, during one R session we load this df once
  facs_df <- custom_read_csv(file.path(current_dir, data_dir, facs_file_name)) %>% 
    mutate(Treatment_Time_New = Treatment_Time + F_Seconds%%1)  ## F_Seconds%%1 gives the deicmal point
  # print(str(facs_df))
  
  ques_df <<- custom_read_csv(file.path(current_dir, data_dir, "Questionnaire Data.csv"))
  
  return(facs_df)
}

get_stats <- function(facs_df) {
  temp_facs_df <- facs_df %>% 
    select(Participant_ID, Group) %>% 
    group_by(Participant_ID, Group) %>%
    summarize(n()) %>%
    group_by(Group) %>%
    summarise(Total_Group=n())
  print(temp_facs_df)
  
  subj_df <- facs_df %>% 
    select(Participant_ID) %>% 
    group_by(Participant_ID) %>% 
    summarize(n())
  convert_to_csv(subj_df, file.path(current_dir, curated_data_dir, "subject_list.csv"))
  
  # View(subj_df)
  # View(ques_df)
  
  ques_df <- ques_df %>%
    merge(subj_df, by="Participant_ID")
  
  View(ques_df)
  
  
  print(paste0("Mean age: ", mean(ques_df$Age)))
  print(paste0("SD age: ", sd(ques_df$Age)))
  
  
  ques_df_1 <- ques_df %>% 
    group_by(Gender) %>% 
    summarize(n())
  print(ques_df_1)
  # View(ques_df_1)
  
  
  ques_df_2 <- ques_df %>% 
    mutate(email_grp=ifelse(Group %in% c("BH", "BL"), "B", "C")) %>% 
    group_by(email_grp, Gender) %>% 
    summarize(grp_gender=n())
  print(ques_df_2)
  # View(ques_df_2)
  
  
  ques_df_3 <- ques_df %>% 
    mutate(email_grp=ifelse(Group %in% c("BH", "BL"), "B", "C")) %>% 
    group_by(email_grp) %>% 
    summarize(grp_mean_age=mean(Age),
              grp_sd_age=sd(Age))
  print(ques_df_3)
  # View(ques_df_3)
}


#-------------------------#
#-------Main Program------#
#-------------------------#
facs_df <<- read_data()


get_stats(facs_df)


















