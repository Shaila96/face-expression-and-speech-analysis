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



facs_size <- 7
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
  facs_df <<- custom_read_csv(file.path(current_dir, data_dir, facs_file_name)) %>% 
    mutate(Treatment_Time_New = Treatment_Time + F_Seconds%%1)  ## F_Seconds%%1 gives the deicmal point
  
  return(facs_df)
}

draw_heat_map_plot <- function(heat_map_df, plot_title) {
  
  heatmap_plot <- ggplot(heat_map_df, aes(x=row_name, y=col_name)) +
    
    geom_tile(aes(fill=diagonal_val)) +
    geom_text(aes(label=diagonal_val)) +
    scale_fill_gradientn(colours = c("lightgray","dimgray"), name = "") +
    # scale_fill_gradientn(colours = c("white", "lightgray","dimgray"), name = "") +
    # scale_fill_gradientn(colours = c("yellow", "orange", "tomato2", "red", "red4", "black"), name = "") +
    
    new_scale("fill") +
    geom_tile(aes(fill=non_diagonal_upper_matrix_val), data = subset(heat_map_df, non_diagonal_upper_matrix_val >= 0)) +
    geom_text(aes(label=non_diagonal_upper_matrix_val), data = subset(heat_map_df, non_diagonal_upper_matrix_val >= 0)) +
    scale_fill_gradientn(colours = c("white", "lightblue", "darkblue"), name = "") +
    # scale_fill_gradientn(colours = c("lightblue", "darkblue"), name = "") +
    
    new_scale("fill") +
    geom_tile(aes(fill=non_diagonal_lower_matrix_val), data = subset(heat_map_df, non_diagonal_lower_matrix_val < 0), show.legend = FALSE) +
    # geom_text(aes(label=non_diagonal_val), data = subset(heat_map_df, non_diagonal_val < -1), show.legend = FALSE) +
    scale_fill_gradientn(colours = c("white")) +

    # ggtitle(plot_title) +
    xlab("") +
    ylab("") +
    theme_bw() +
    theme(text = element_text(size=20),
          panel.background = element_blank(),
          panel.grid = element_blank(),
          plot.title = element_text(hjust = 0.5, size=24)) +
    labs(fill="") +
    scale_x_discrete(position = "top")
  
  return(heatmap_plot)
}


get_heat_map_df <- function(subj_facs_df, group='no_group') {
  ## Initializing matrix with all 0
  final_matrix = matrix(0, facs_size, facs_size) 
  
  # print(subj_facs_df[c(1:10), emotion_cols])
  # convert_to_csv(subj_facs_df[c(1:10), emotion_cols], 'facs_test.csv')
  
  for(i in 1:nrow(subj_facs_df)){
  # for(i in 1:500){
    
    emotion_vals_by_row <- as.vector(unlist(subj_facs_df[i, emotion_cols]))
    # emotion_vals_by_row <- as.vector(c(0, 0, 1, 1, 0, 7, 1))
    
    if (!any(is.na(emotion_vals_by_row))) {
      ## Outer product
      current_matrix <- outer(emotion_vals_by_row, emotion_vals_by_row)
      # print(current_matrix)
      
      ## Convert into upper triangle matrix
      current_matrix[lower.tri(current_matrix)] <- 0
      # print(current_matrix)
      
      ## Normalize with the sum of the elements of the matrix
      current_matrix <- current_matrix/sum(current_matrix)
      # print(current_matrix)
      
      ## Add to the final matrix
      final_matrix <- final_matrix + current_matrix
      # print(paste0('Step ', i, ': Sum Matrix -->'))
      # print(final_matrix)
    }
  }
  
  ## Dividing matrix using 1000 and taking until 2 decimal
  final_matrix = round(final_matrix, 2)
  final_matrix[lower.tri(final_matrix)] <- NA
  dimnames(final_matrix) = list(plot_emotion_cols, plot_emotion_cols)
  
  heat_map_df <- melt(final_matrix, varnames=c('row_name', 'col_name')) %>% 
    mutate(row_name=as.factor(row_name),
           col_name=as.factor(col_name),
           diagonal_val = ifelse(row_name==col_name, value, NA),
           non_diagonal_upper_matrix_val = ifelse(row_name==col_name, -1, value),
           non_diagonal_lower_matrix_val = ifelse(is.na(non_diagonal_upper_matrix_val), -1, 0)) 
  # %>% 
  #   mutate(col_name = recode(col_name,
  #                            'WordCount'='Word Count',
  #                            'CharCount'='Char Count'))
  
  # View(heat_map_df)
  # convert_to_csv(heat_map_df, paste0('heat_map_dual_task_', group, '.csv'))
  return(heat_map_df)
}

draw_signal_plot <- function(subj_facs_df, subj, treatment) {
  signal_plot <- subj_facs_df %>% 
    select(Treatment_Time_New, F_Angry, F_Disgusted, F_Afraid, F_Happy, F_Sad, F_Surprised, F_Neutral) %>% 
    gather(key = "Expression", value = "Value", -Treatment_Time_New) %>% 
    
    ggplot(aes(x=Treatment_Time_New, y=Value, color=Expression)) +
    geom_line(alpha = 0.7) +
    
    ggtitle(paste0(subj, ' - ', treatment, ' - ', subj_facs_df$Group)) +
    xlab("Time [s]") +
    ylab("Facial Expression Probability") +
    
    theme_bw() +
    theme(text=element_text(size=18),
          axis.text = element_text(size=16),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"),
          # legend.position='bottom',
          legend.title = element_blank(),
          plot.title = element_text(hjust = 0.5,
                                    size=20,
                                    margin=margin(t=0, r=0, b=15, l=0)), ##top, right, bottom, left
          plot.margin = unit(c(0.5, 2, 0.5, 0.5), "lines")) ##top, right, bottom, left
  
    
  return(signal_plot)
}


draw_signal_heatmap_plots <- function(facs_df) {
  # for (subj in levels(factor(facs_df$Participant_ID))) {
  for (subj in c('T005')) {
    
    # for (treatment in c('RB', 'ST', 'PM', 'DT', 'PR')) {
    for (treatment in c('PR')) {
    # for (treatment in c('DT')) {
      
      subj_facs_df <- facs_df %>%
        filter(Participant_ID==subj & Treatment==treatment) %>% 
        slice(1:100)
      
      heat_map_df <- get_heat_map_df(subj_facs_df)
      heatmap_plot <- draw_heat_map_plot(heat_map_df, paste0(subj, ' - ', treatment))
      # save_plot(paste0(subj, '_', treatment), heatmap_plot)
      
      signal_plot <- draw_signal_plot(subj_facs_df, subj, treatment)
      
      combined_dt_plot <- plot_grid(signal_plot,
                                    heatmap_plot,
                                    labels=c('A', 'B'),
                                    # label_size=18,
                                    # align='v',
                                    # rel_height=c(2, 10),
                                    ncol=1)
      save_plot(paste0(subj, '_', treatment, '_combined'), combined_dt_plot, width=20)
    }
  }
}

get_group_abbr <- function(group_name_full) {
  # print(class(group_name_full))
  # print(group_name_full)
  
  if (group_name_full %in% c('CH', 'CL')) {
    return('Continual')
  } else if (group_name_full %in% c('BH', 'BL')) {
    return('Batch')
  }
  

}

generate_dt_heat_map_df <- function(facs_df) {

  dt_heat_map_df <- data.frame()
  
  for (subj in levels(factor(facs_df$Participant_ID))) {
  # for (subj in c('T005')) {
  # for (subj in c('T005', 'T016')) {
    
    # for (treatment in c('RB', 'ST', 'PM', 'DT', 'PR')) {
    for (treatment in c('DT')) {
      
      subj_facs_df <- facs_df %>%
        filter(Participant_ID==subj & Treatment==treatment) 
      # %>%
      #   slice(1:10)
      
      print(subj)
      # print(subj_facs_df$Group[1])
      
      # View(heat_map_df)
      subj_heat_map_df <- get_heat_map_df(subj_facs_df) %>% 
        mutate(facs_combination=ifelse(row_name==col_name, as.character(col_name), paste0(row_name, '_', col_name))) %>% 
        select(facs_combination, diagonal_val, non_diagonal_upper_matrix_val) %>% 
        gather(direction, value, -facs_combination) %>% 
        filter(value>=0) %>% 
        mutate(Subject=subj,
               Group=get_group_abbr(subj_facs_df$Group[1]),
               Sum_All=sum(value),
               Percentage_All=value/Sum_All) %>%
        group_by(direction) %>%
        mutate(Sum_By_Direction=sum(value),
               Percentage_By_Direction=value/Sum_By_Direction)
        
        
      # View(subj_heat_map_df)
      dt_heat_map_df <- bind_rows(dt_heat_map_df, subj_heat_map_df)
    }
  }
  
  View(dt_heat_map_df)
  convert_to_csv(dt_heat_map_df, file.path(curated_data_dir, 'dt_heat_map_df.csv'))

  return(dt_heat_map_df)
}


get_dt_heat_map_df <- function(facs_df) {
  # dt_heat_map_df <- generate_dt_heat_map_df(facs_df)
  
  dt_heat_map_df <- read.csv(file.path(curated_data_dir, 'dt_heat_map_df.csv'))
  
  return(dt_heat_map_df)
}

get_alternative_for_expression <- function(expression) {
  if (expression %in% c('Afraid_Sad', 'Angry_Sad', 'Sad')) {
    return('less')
  }
  if (expression %in% c('Angry_Neutral', 'Neutral')) {
    return('greater')
  }
}

get_t_test_percentage_all <- function(dt_heat_map_df) {
  
  for(expression in c('Afraid_Sad', 'Angry_Sad', 'Angry_Neutral', 'Neutral', 'Sad')) {
  # for(expression in c('Afraid_Sad')) {
    temp_dt_heat_map_df <- dt_heat_map_df %>% 
      filter(facs_combination==expression) %>% 
      select(Subject, Group, facs_combination, Percentage_All)
    
    # View(temp_dt_heat_map_df)
    print('---------------------------------------------------------------------')
    t_test = t.test(temp_dt_heat_map_df[temp_dt_heat_map_df$Group=="Batch", ]$Percentage_All,
                    temp_dt_heat_map_df[temp_dt_heat_map_df$Group=="Continual", ]$Percentage_All,
                    alternative=get_alternative_for_expression(expression))
    print(paste0(expression, ' - ', get_alternative_for_expression(expression)))
    print(t_test)
    print('---------------------------------------------------------------------')
    
  }
}


get_t_test_percentage_by_direction <- function(dt_heat_map_df) {
  
  for(expression in c('Afraid_Sad', 'Angry_Sad', 'Angry_Neutral', 'Neutral', 'Sad')) {
    # for(expression in c('Afraid_Sad')) {
    temp_dt_heat_map_df <- dt_heat_map_df %>% 
      filter(facs_combination==expression) %>% 
      select(Subject, Group, facs_combination, Percentage_By_Direction)
    
    # View(temp_dt_heat_map_df)
    print('############################################################################')
    t_test = t.test(temp_dt_heat_map_df[temp_dt_heat_map_df$Group=="Batch", ]$Percentage_By_Direction,
                    temp_dt_heat_map_df[temp_dt_heat_map_df$Group=="Continual", ]$Percentage_By_Direction,
                    alternative=get_alternative_for_expression(expression))
    print(paste0(expression, ' - ', get_alternative_for_expression(expression)))
    print(t_test)
    print('############################################################################')
    
  }
}

# get_t_test_percentage <- function(dt_heat_map_df) {
#   
# }


get_t_test_result <- function(dt_heat_map_df) {
  # get_t_test_percentage(dt_heat_map_df)
  
  # get_t_test_summative_value(dt_heat_map_df)
  get_t_test_percentage_all(dt_heat_map_df)
  get_t_test_percentage_by_direction(dt_heat_map_df)
  
}


#-------------------------#
#-------Main Program------#
#-------------------------#
# facs_df <- read_data()

dt_heat_map_df <- get_dt_heat_map_df(facs_df)

get_t_test_result(dt_heat_map_df)

# draw_signal_heatmap_plots(facs_df)








