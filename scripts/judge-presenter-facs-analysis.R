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


temp_gaze <<- ''

facs_frequency <- 0.1
gaze_threshold <- 1
gaze_opacity <- 0.3

judge_df_new_col_names <- c('video_time',
                            
                            'judge_l_afraid',
                            'judge_l_angry',
                            'judge_l_disgusted',
                            'judge_l_happy',
                            'judge_l_neutral',
                            'judge_l_sad',
                            'judge_l_surprised',
                            
                            'judge_c_afraid',
                            'judge_c_angry',
                            'judge_c_disgusted',
                            'judge_c_happy',
                            'judge_c_neutral',
                            'judge_c_sad',
                            'judge_c_surprised',
                            
                            'judge_r_afraid',
                            'judge_r_angry',
                            'judge_r_disgusted',
                            'judge_r_happy',
                            'judge_r_neutral',
                            'judge_r_sad',
                            'judge_r_surprised',
                            
                            'TreatmentTime'
                            )



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
           xaxis = list(title=get_axis_label('Time [s]'),
                        range=c(0, max(subj_facs_df$TreatmentTime))), 
           yaxis = list(title=get_axis_label('Emotion'),
                        range=c(0, 1)))
  
  
  # plot <- layout(plot, 
  #             # title = 'Highlighting with Rectangles',
  #             shapes = list(
  #               list(type = "rect",
  #                    fillcolor = "blue", line = list(color = "blue"), opacity = 0.4,
  #                    x0 = 0.0, x1 = 0.3, xref = "x",
  #                    y0 = 0, y1 = 1, yref = "y"),
  #               list(type = "rect",
  #                    fillcolor = "green", line = list(color = "green"), opacity = 0.4,
  #                    x0 = 0.4, x1 = 3, xref = "x",
  #                    y0 = 0, y1 = 1, yref = "y"),
  #               list(type = "rect",
  #                    fillcolor = "green", line = list(color = "green"), opacity = 0.4,
  #                    x0 = 0.4, x1 = 3, xref = "x",
  #                    y0 = 0, y1 = 1, yref = "y")
  #               ))
  
  return(plot)
}

# get_judge_direction_abbr <- function(judge_no) {
#   return(switch(judge_no, "l", "c", "r"))
# }

get_judge_direction <- function(judge_no) {
  # return(switch(judge_no, "LEFT", "CENTER", "RIGHT"))
  if (judge_no=='l') {
    return("LEFT")
  } else if (judge_no=='c') {
    return("CENTER")
  }  else if (judge_no=='r') {
    return("RIGHT")
  } 
}

get_gaze_shape <- function(temp_subj_gaze_df) {
  gaze_shape_list <- list()
  
  for(i in rownames(temp_subj_gaze_df)) {
    single_gaze_shape <- list(type = "rect",
                              fillcolor = "yellow", 
                              line = list(color = "yellow"), 
                              opacity = gaze_opacity,
                              x0 = temp_subj_gaze_df[i, ]$Start_Time_New, 
                              x1 = temp_subj_gaze_df[i, ]$End_Time_New, 
                              xref = "x",
                              y0 = 0, 
                              y1 = 1, 
                              yref = "y")
    gaze_shape_list <- c (gaze_shape_list, list(single_gaze_shape))
  }
  
  return(gaze_shape_list)
  
  
  
  
  # shape <- list(
  #   list(type = "rect",
  #        fillcolor = "yellow", line = list(color = "yellow"), opacity = 0.4,
  #        x0 = 34.6, x1 = 42.5, xref = "x",
  #        y0 = 0, y1 = 1, yref = "y"),
  #   list(type = "rect",
  #        fillcolor = "yellow", line = list(color = "yellow"), opacity = 0.4,
  #        x0 = 100.7, x1 = 105.6, xref = "x",
  #        y0 = 0, y1 = 1, yref = "y"),
  #   list(type = "rect",
  #        fillcolor = "yellow", line = list(color = "yellow"), opacity = 0.4,
  #        x0 = 178.2, x1 = 179.1, xref = "x",
  #        y0 = 0, y1 = 1, yref = "y")
  #   )
  # 
  # return(shape)

}

get_judge_facs_plot <- function(temp_judge_facs_df, temp_subj_gaze_df, subj_facs_df, subj, judge_no) {
  print(str(temp_subj_gaze_df))
  
  plot <- plot_ly(temp_judge_facs_df, 
                  x = ~TreatmentTime, 
                  y = temp_judge_facs_df[[paste0('judge_', judge_no, '_surprised')]],
                  type = 'scatter', 
                  mode = 'lines', 
                  line = list(color = light_red),
                  name = paste0('judge_', judge_no, '_anger')) %>% 
    add_trace(y = temp_judge_facs_df[[paste0('judge_', judge_no, '_disgusted')]], name = paste0('judge_', judge_no, '_disgust'), line = list(color = brown)) %>%
    add_trace(y = temp_judge_facs_df[[paste0('judge_', judge_no, '_afraid')]], name = paste0('judge_', judge_no, '_afraid'), line = list(color = light_violet)) %>%
    add_trace(y = temp_judge_facs_df[[paste0('judge_', judge_no, '_happy')]], name = paste0('judge_', judge_no, '_happy'), line = list(color = light_green)) %>%
    add_trace(y = temp_judge_facs_df[[paste0('judge_', judge_no, '_sad')]], name = paste0('judge_', judge_no, '_sad'), line = list(color = light_blue)) %>%
    add_trace(y = temp_judge_facs_df[[paste0('judge_', judge_no, '_surprised')]], name = paste0('judge_', judge_no, '_surprised'), line = list(color = violet)) %>%
    add_trace(y = temp_judge_facs_df[[paste0('judge_', judge_no, '_neutral')]], name = paste0('judge_', judge_no, '_neutral'), line = list(color = light_grey)) %>%
    layout(title = paste0(subj, ' - ', 'PR'),
           xaxis = list(title=get_axis_label('Time [s]'),
                        range=c(0, max(subj_facs_df$TreatmentTime))), 
           yaxis = list(title=get_axis_label('Emotion'),
                        range=c(0, 1)))
  

  plot <- layout(plot, shapes = get_gaze_shape(temp_subj_gaze_df))
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

add_judges_plots <- function(judge_facs_df, subj_facs_df, subj_gaze_df, subj) {
  # print(colnames(judge_facs_df))
  colnames(judge_facs_df) <- judge_df_new_col_names
  
  for (judge_no in c('l', 'c', 'r')) {
    temp_judge_facs_df <- judge_facs_df %>%
      select(paste0('judge_', judge_no, '_afraid'),
             paste0('judge_', judge_no, '_angry'),
             paste0('judge_', judge_no, '_disgusted'),
             paste0('judge_', judge_no, '_happy'),
             paste0('judge_', judge_no, '_neutral'),
             paste0('judge_', judge_no, '_sad'),
             paste0('judge_', judge_no, '_surprised'),
             TreatmentTime) %>% 
      filter(TreatmentTime <= max(subj_facs_df$TreatmentTime, na.rm=T))
    
    temp_subj_gaze_df <- subj_gaze_df %>%
      filter(G_Direction_New==get_judge_direction(judge_no))
    
    plot_list[[length(plot_list)+1]] <<- get_judge_facs_plot(temp_judge_facs_df, temp_subj_gaze_df, subj_facs_df, subj, judge_no)
  }
  
}

replace_gaze <- function(gaze_df, row_no) {
  if(gaze_df[row_no, ]$Diff_Time < gaze_threshold) {
    gaze_df[row_no, ]$G_Direction_New <- temp_gaze
  } else {
    temp_gaze <<- gaze_df[row_no, ]$G_Direction
  }
  
  return(gaze_df)
}

get_curated_gaze_data <- function(subj_facs_df) {
  gaze_df_1 <- as.data.frame(subj_facs_df) %>% 
    select(G_Direction, TreatmentTime) %>% 
    # filter(G_Direction %in% c('LEFT', 'CENTER', 'RIGHT')) %>%  ## Don't delete missing dataset
    mutate(G_Direction_ID = rleid(G_Direction)) %>% 
    group_by(G_Direction_ID) %>% 
    mutate(G_Direction_ID_Count=n(),
           Time=G_Direction_ID_Count * facs_frequency)
  # View(gaze_df_1)
  
  
  gaze_df_2 <- gaze_df_1 %>% 
    group_by(G_Direction, G_Direction_ID) %>%
    mutate(Start_Time=min(TreatmentTime),
           End_Time=max(TreatmentTime)) %>% 
    ungroup() %>% 
    select(G_Direction, Start_Time, End_Time) %>% 
    distinct(G_Direction, Start_Time, End_Time) %>% 
    mutate(Diff_Time=End_Time-Start_Time + facs_frequency,
           G_Direction_New = G_Direction)
  # View(gaze_df_2)
  
  
  gaze_df_3 <- gaze_df_2
  for(i in rownames(gaze_df_3)) {
    gaze_df_3 <- replace_gaze(gaze_df_3, i)
  }
  
  gaze_df_3 <- gaze_df_3 %>% 
    mutate(G_Direction_ID_New = rleid(G_Direction_New)) %>%
    group_by(G_Direction_ID_New) %>% 
    mutate(Diff_Time_New=sum(Diff_Time),
           Start_Time_New=min(Start_Time),
           End_Time_New=max(End_Time)) %>% 
    ungroup()
  # View(gaze_df_3)
  
  
  gaze_df_4 <- gaze_df_3 %>%
    distinct(G_Direction_New, Start_Time_New, End_Time_New, Diff_Time_New)
  # View(gaze_df_4)
  
  return(gaze_df_4)
}



draw_pr_facs_plots <- function(facs_df_list) {
  subj_facs_df <- as.data.frame(facs_df_list[1])
  judge_facs_df <- as.data.frame(facs_df_list[2])

  treatment <- 'PR'
  
  
  # print(levels(factor(facs_df$Participant_ID)))
  # "T005" "T016" "T021" "T051" "T063" "T064" "T068"  
  # "T077" "T079" "T083" "T085" "T092" "T094" "T098"
  # "T099" "T106" "T121" "T124" "T132" "T139" "T144" 
  # "T151" "T157" "T166" "T175" "T178"
  
  
  # for (subj in levels(factor(facs_df$Participant_ID))) {
  for (subj in c('T021')) {
    plot_list <<- list()
    
    subj_facs_df <- subj_facs_df %>%
      filter(Participant_ID==subj) %>%
      group_by(Participant_ID) %>%
      mutate(Time=as.POSIXct(Time))
    # print(paste0(subj, ' - ', treatment))
    
    
    ###############################################################
    plot_list[[length(plot_list)+1]] <<- get_subj_facs_plot(subj_facs_df, subj, treatment)
    ###############################################################
    
    
    subj_gaze_df <- get_curated_gaze_data(subj_facs_df)
    
    
    ###############################################################
    add_judges_plots(judge_facs_df, subj_facs_df, subj_gaze_df, subj)

    combined_plot <- subplot(plot_list, nrows=length(plot_list))
    print(combined_plot)
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




