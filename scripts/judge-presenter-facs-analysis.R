#-------------------------#
#--------LIBRARIES--------#
#-------------------------#
library(tidyr)
library(dplyr)
library(readr)
library(plotly)
library(data.table)





#-------------------------#
#-----GLOBAL VARIABLES----#
#-------------------------#
script_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
source(file.path(script_dir, 'us-common-functions.R'))

current_dir <- dirname(script_dir)
setwd(current_dir)




#-------------------------#
#---FUNCTION DEFINITION---#
#-------------------------#
get_subj_facs_plot <- function(subj_facs_df, subj, treatment) {
  plot <- plot_ly(subj_facs_df, 
                  x = ~TreatmentTime, 
                  y = ~F_Angry, 
                  type = 'scatter', 
                  mode = 'lines',
                  line = list(color = light_red),
                  name = 'Anger') %>% 
    add_trace(y = ~F_Disgusted, name = 'Disgust', line = list(color = brown)) %>%
    add_trace(y = ~F_Afraid, name = 'Afraid', line = list(color = light_violet)) %>%
    add_trace(y = ~F_Happy, name = 'Happy', line = list(color = light_green)) %>%
    add_trace(y = ~F_Sad, name = 'Sad', line = list(color = light_blue)) %>%
    add_trace(y = ~F_Surprised, name = 'Surprised', line = list(color = violet)) %>%
    add_trace(y = ~F_Neutral, name = 'Neutral', line = list(color = light_grey)) %>% 
    layout(title = paste0(subj, ' - ', treatment),
           xaxis = get_axis_label('Time [s]'), 
           yaxis = get_axis_label('Emotion'))
  
  
  plot <- layout(plot, 
              # title = 'Highlighting with Rectangles',
              shapes = list(
                list(type = "rect",
                     fillcolor = "blue", line = list(color = "blue"), opacity = 0.4,
                     x0 = 0.0, x1 = 0.3, xref = "x",
                     y0 = 0, y1 = 1, yref = "y"),
                list(type = "rect",
                     fillcolor = "green", line = list(color = "green"), opacity = 0.4,
                     x0 = 0.4, x1 = 3, xref = "x",
                     y0 = 0, y1 = 1, yref = "y"),
                list(type = "rect",
                     fillcolor = "green", line = list(color = "green"), opacity = 0.4,
                     x0 = 0.4, x1 = 3, xref = "x",
                     y0 = 0, y1 = 1, yref = "y")
                ))
  
  return(plot)
}

get_judge_facs_plot <- function(temp_judge_facs_df, subj, judge_no) {
  # print(str(temp_judge_facs_df))
  plot <- plot_ly(temp_judge_facs_df, 
                  x = ~TreatmentTime, 
                  y = temp_judge_facs_df[[paste0('judge', judge_no, '_surprised')]],
                  type = 'scatter', 
                  mode = 'lines', 
                  line = list(color = light_red),
                  name = paste0('judge', judge_no, '_anger')) %>% 
    add_trace(y = temp_judge_facs_df[[paste0('judge', judge_no, '_disgusted')]], name = paste0('judge', judge_no, '_disgust'), line = list(color = brown)) %>%
    add_trace(y = temp_judge_facs_df[[paste0('judge', judge_no, '_afraid')]], name = paste0('judge', judge_no, '_afraid'), line = list(color = light_violet)) %>%
    add_trace(y = temp_judge_facs_df[[paste0('judge', judge_no, '_happy')]], name = paste0('judge', judge_no, '_happy'), line = list(color = light_green)) %>%
    add_trace(y = temp_judge_facs_df[[paste0('judge', judge_no, '_sad')]], name = paste0('judge', judge_no, '_sad'), line = list(color = light_blue)) %>%
    add_trace(y = temp_judge_facs_df[[paste0('judge', judge_no, '_surprised')]], name = paste0('judge', judge_no, '_surprised'), line = list(color = violet)) %>%
    add_trace(y = temp_judge_facs_df[[paste0('judge', judge_no, '_neutral')]], name = paste0('judge', judge_no, '_neutral'), line = list(color = light_grey)) %>%
    layout(title = paste0(subj, ' - ', 'PR'),
           xaxis = get_axis_label('Time [s]'),
           yaxis = get_axis_label('Emotion'))
  

  return(plot)
}

draw_session_plots <- function() {
  plot_list <- list()

  # for (subj in levels(factor(facs_df$Participant_ID))) {
  for (subj in c('T005')) {

    # for (treatment in c( "RB", "ST", "PM", "DT", "PR")) {
    for (treatment in c( "RB", "ST", "PM", "PR")) {
    # for (treatment in c( "PR")) {

      temp_facs_df <- facs_df %>%
        filter(Participant_ID==subj) %>%
        filter(Treatment==treatment) %>%
        group_by(Participant_ID) %>%
        mutate(Time=as.POSIXct(Time),
               TreatmentTime=as.numeric(Time)-as.numeric(head(Time, 1)))
      # print(str(temp_facs_df))
      print(paste0(subj, ' - ', treatment))

      plot_list[[length(plot_list)+1]] <- get_subj_facs_plot(temp_facs_df, subj, treatment)
      # print(subj_plot)


      # options(browser = 'false')
      # api_create(subj_plot, filename = paste0(subj, ' - ', treatment))
    }
  }

  combined_plot <- subplot(plot_list, nrows=length(plot_list))
  print(combined_plot)
}




get_presentation_df <- function(facs_df, facs_judges_df) {
  # facs_judges_df <- custom_read_csv(file.path(current_dir, data_dir, facs_judges_file_name)) 
  # print(str(facs_judges_df))
  # print(levels(factor(facs_judges_df$Sector)))

  
  facs_judges_df <- facs_judges_df %>% 
    select(-Frame, -Time) %>% 
    filter(Seconds > 117) %>%  ## Here the judges shows thumbs up
    filter(!is.na(Sector)) %>%
    mutate_if(is.integer, as.numeric)
    
  # View(facs_judges_df)
  # print(str(facs_judges_df))
  # print(levels(factor(facs_judges_df$Sector)))
  
  # facs_judges_df <- dcast(setDT(facs_judges_df),
  #                         Seconds ~ Sector,
  #                         value.var = c("angry",
  #                                       "disgusted",
  #                                       "afraid",
  #                                       "happy",
  #                                       "sad",
  #                                       "surprised",
  #                                       "neutral")) %>%
  #   mutate(TreatmentTime=Seconds-head(Seconds, 1))
  # View(facs_judges_df)
  # print(str(facs_judges_df))
  # print(facs_judges_df$Seconds)
  
  facs_judges_df <- facs_judges_df %>%
    gather(variable, value, -(Seconds:Sector)) %>%
    unite(judge_facs, Sector, variable) %>%
    # slice(c(1:240, 192000:192120)) %>%
    # slice(c(1:2400)) %>%
    distinct(Seconds, judge_facs, .keep_all=T) %>% 
    spread(judge_facs, value) %>% 
    mutate(TreatmentTime=Seconds-head(Seconds, 1))
  # View(facs_judges_df)
  # print(str(facs_judges_df))
  # print(levels(factor(facs_judges_df$judge_facs)))
 

  
  facs_df <- facs_df %>% 
    filter(Treatment=='PR') %>% 
    group_by(Participant_ID) %>% 
    mutate(TreatmentTime=Seconds-head(Seconds, 1))
  # print(str(facs_df))
  
  
  return(list(facs_df, facs_judges_df))
    
    
    
  # 'data.frame':	1044378 obs. of  21 variables:
  # $ Participant_ID: chr  "T005" "T005" "T005" "T005" ...
  # $ Group         : chr  "CH" "CH" "CH" "CH" ...
  # $ Treatment     : chr  "RB" "RB" "RB" "RB" ...
  # $ Frame         : num  420 421 422 423 424 425 426 427 428 429 ...
  # $ Seconds       : num  42 42.1 42.2 42.3 42.4 42.5 42.6 42.7 42.8 42.9 ...
  # $ Time          : chr  "2018-06-27 14:15:41" "2018-06-27 14:15:41" "2018-06-27 14:15:41" "2018-06-27 14:15:41" ...
  # $ Task          : chr  "" "" "" "" ...
  # $ F_Angry       : num  0.00831 0.00852 0.00903 0.01251 0.0054 ...
  # $ F_Disgusted   : num  7.11e-07 7.16e-07 2.87e-07 3.17e-07 1.03e-06 ...
  # $ F_Afraid      : num  0.249 0.285 0.206 0.105 0.287 ...
  # $ F_Happy       : num  0.00278 0.00194 0.00137 0.00163 0.00364 ...
  # $ F_Sad         : num  0.0374 0.0588 0.05 0.0459 0.0346 ...
  # $ F_Surprised   : num  1.78e-05 1.57e-05 3.47e-06 3.92e-06 2.10e-05 ...
  # $ F_Neutral     : num  0.702 0.646 0.734 0.835 0.669 ...
  # $ G_Ratio       : num  NA NA NA NA NA NA NA NA NA NA ...
  # $ G_Direction   : chr  "" "" "" "" ...
  # $ S_Angry       : num  NA NA NA NA NA NA NA NA NA NA ...
  # $ S_Afraid      : num  NA NA NA NA NA NA NA NA NA NA ...
  # $ S_Happy       : num  NA NA NA NA NA NA NA NA NA NA ...
  # $ S_Sad         : num  NA NA NA NA NA NA NA NA NA NA ...
  # $ S_Neutral     : num  NA NA NA NA NA NA NA NA NA NA ...
}


add_judges_plots <- function(judge_facs_df, subj_facs_df) {
  # print(colnames(judge_facs_df))
  colnames(judge_facs_df) <- c('video_time',
                               
                               'judge1_afraid',
                               'judge1_angry',
                               'judge1_disgusted',
                               'judge1_happy',
                               'judge1_neutral',
                               'judge1_sad',
                               'judge1_surprised',
                               
                               'judge2_afraid',
                               'judge2_angry',
                               'judge2_disgusted',
                               'judge2_happy',
                               'judge2_neutral',
                               'judge2_sad',
                               'judge2_surprised',
                               
                               'judge3_afraid',
                               'judge3_angry',
                               'judge3_disgusted',
                               'judge3_happy',
                               'judge3_neutral',
                               'judge3_sad',
                               'judge3_surprised',
                               
                               'TreatmentTime'
                               )
  
  for (judge_no in c(1:3)) {
    # print(judge_no)
    temp_judge_facs_df <- judge_facs_df %>%
      # select(paste0('judge', judge_no, '_afraid'))
      select(paste0('judge', judge_no, '_afraid'),
             paste0('judge', judge_no, '_angry'),
             paste0('judge', judge_no, '_disgusted'),
             paste0('judge', judge_no, '_happy'),
             paste0('judge', judge_no, '_neutral'),
             paste0('judge', judge_no, '_sad'),
             paste0('judge', judge_no, '_surprised'),
             TreatmentTime) %>% 
      filter(TreatmentTime <= max(subj_facs_df$TreatmentTime, na.rm=T))
    plot_list[[length(plot_list)+1]] <- get_judge_facs_plot(temp_judge_facs_df, subj, judge_no)
  }
  
}


make_gaze_data <- function(subj_facs_df) {
  gaze_df <- as.data.frame(subj_facs_df) %>% 
    select(G_Direction, TreatmentTime) %>% 
    # filter(G_Direction %in% c('LEFT', 'CENTER', 'RIGHT')) %>%  ## Don't delete missing dataset
    mutate(G_Direction_ID = rleid(G_Direction))
  View(gaze_df)
  
  
  gaze_df <- gaze_df %>% 
    group_by(G_Direction, G_Direction_ID) %>%
    mutate(StartTime=min(TreatmentTime),
           EndTime=max(TreatmentTime)) %>% 
    ungroup() %>% 
    select(G_Direction, StartTime, EndTime) %>% 
    distinct(G_Direction, StartTime, EndTime) %>% 
    mutate(DiffTime=EndTime-StartTime)
  
  # print(str(subj_facs_df))
  # print(str(gaze_df))
  View(gaze_df)
}



draw_pr_facs_plots <- function(facs_df_list) {
  subj_facs_df <- as.data.frame(facs_df_list[1])
  judge_facs_df <- as.data.frame(facs_df_list[2])

  treatment <- 'PR'
  
  # for (subj in levels(factor(facs_df$Participant_ID))) {
  for (subj in c('T005')) {
    plot_list <<- list()
    
    subj_facs_df <- subj_facs_df %>%
      filter(Participant_ID==subj) %>%
      group_by(Participant_ID) %>%
      mutate(Time=as.POSIXct(Time))
    # print(paste0(subj, ' - ', treatment))
    
    
    ###############################################################
    # plot_list[[length(plot_list)+1]] <<- get_subj_facs_plot(subj_facs_df, subj, treatment)
    ###############################################################
    
    
    make_gaze_data(subj_facs_df)
    
    
    ###############################################################
    # add_judges_plots(judge_facs_df, subj_facs_df)
    # 
    # combined_plot <- subplot(plot_list, nrows=length(plot_list))
    # print(combined_plot)
    ###############################################################
    
    
    
    # options(browser = 'false')
    # api_create(combined_plot, filename = paste0(subj, ' - ', treatment))
  }
}



draw_plots <- function() {
  ## Making the df as global variable, as it takes a lot of time to load.
  ## So, during one R session we load this df once
  # facs_df <<- custom_read_csv(file.path(current_dir, data_dir, facs_gaze_speech_file_name))
  # facs_judges_df <<- custom_read_csv(file.path(current_dir, data_dir, facs_judges_file_name))

  
  
  
  ##############################################################
  facs_df_list <- get_presentation_df(facs_df, facs_judges_df)
  draw_pr_facs_plots(facs_df_list)
  ##############################################################


  #######################################
  # draw_session_plots(facs_df)
  #######################################
}



#-------------------------#
#-------Main Program------#
#-------------------------#
draw_plots()




