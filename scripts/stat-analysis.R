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

get_mean <- function(x, na.rm = T) (mean(x, na.rm = na.rm))

get_mean_vals <- function(x, na.rm = T) (mean(x, na.rm = na.rm))*get_total_frame(x) / 50*60*10

get_total_frame <- function(x) length(x)




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



generate_mean_values <- function(facs_df) {
  mean_facs_df <- facs_df %>%
    select(Participant_ID, Group, Treatment, emotion_cols) %>%
    filter(Treatment=='DT') %>%
    filter_at(vars(emotion_cols), all_vars(!is.na(.))) %>%
    group_by(Participant_ID, Group) %>%
    summarise_at(vars(emotion_cols), funs(mean=get_mean,
                                          w_mean=get_mean_vals,
                                          sum=sum,
                                          frames=get_total_frame))
  #   mutate(Group_Type=str_extract(Group, "[^.]"))

  View(mean_facs_df)
  convert_to_csv(mean_facs_df, file.path(current_dir, curated_data_dir, "mean_facs_dt.csv"))
}


get_mean_values <- function(facs_df) {
  
  generate_mean_values(facs_df)
  
  mean_facs_df <- custom_read_csv(file.path(current_dir, curated_data_dir, "mean_facs_dt.csv"))
  return(mean_facs_df)
}

is_normal <- function(df) {
  if (shapiro.test(df)$p.value>=0.05) {
    return(T)
  }
  
  return(F)
}

check_normality_and_log_transform <- function(mean_facs_df, exp) {
  batch_df <- mean_facs_df %>% filter(Group %in% c('BH', 'BL'))
  continual_df <- mean_facs_df %>% filter(Group %in% c('CH', 'CL'))
  
  print(batch_df)
  print(continual_df)
  
  batch_data <- batch_df[[exp]]
  continual_data <- continual_df[[exp]]
  
  
  if (!is_normal(batch_data) | !is_normal(continual_data)) {
    print('-------- NOT NORMAL DISTRIBUTION')
    batch_data <- log(batch_data)
    continual_data <- log(continual_data)
    print('-------- LOG TRANSFORMED')
  }
  
  ###############################################
  # TAMU PEOPLE LOG TRANSFORMED ALTHOUGH NORMAL #
  ###############################################
  # batch_data <- log(batch_data)
  # continual_data <- log(continual_data)
  ###############################################
  
  if (is_normal(batch_data) & is_normal(continual_data)) {
    print('BOTH NORMAL DISTRIBUTION')
    print('Performing T-Test')
    t_test <- t.test(batch_data, continual_data)
    print(t_test)
    
  } else {
    print('-------- NOT NORMAL DISTRIBUTION AFTER LOG TRANSFORMATION')
    print('Performing U Rank Test')
    w_test <- wilcox.test(batch_data, continual_data)
    print(w_test)
  }
  
  
  
  
  box_plot_df <- mean_facs_df
  
  
  
  
}




get_t_test_result <- function(mean_facs_df, type, remove_small_dataset=F) {
  if (remove_small_dataset==T) {
    mean_facs_df <- mean_facs_df %>%
      filter(!(Participant_ID %in% c('T077', 
                                     'T139',
                                     
                                     'T063',
                                     'T092',
                                     'T094',
                                     'T124',
                                     'T157'
                                     )))
    # convert_to_csv(mean_facs_df, file.path(current_dir, curated_data_dir, "mean_facs_dt_filtered.csv"))
  }

  
  for (exp in paste0(c(emotion_cols), '_', type)) {
    print('----------------------------')
    print(exp)
    print('----------------------------')
    
    exp_mean_df <- mean_facs_df %>%
      select(Group, exp)

    check_normality_and_log_transform(exp_mean_df, exp)
  }
  

  #   for (subj in levels(factor(group_facs_df$Participant_ID))) {
  # 
  #     subj_facs_df <- group_facs_df %>%
  #       filter(Participant_ID==subj)
  # 
  # }
}


#-------------------------#
#-------Main Program------#
#-------------------------#
# facs_df <<- read_data()


# get_stats(facs_df)


mean_facs_df <- get_mean_values(facs_df)


get_t_test_result(mean_facs_df, 'sum')
# get_t_test_result(mean_facs_df, 'mean')
# get_t_test_result(mean_facs_df, 'w_mean')



# get_t_test_result(mean_facs_df, 'mean', remove_small_dataset=T)













