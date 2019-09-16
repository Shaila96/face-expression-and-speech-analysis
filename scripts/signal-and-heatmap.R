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
  facs_df <- custom_read_csv(file.path(current_dir, data_dir, facs_file_name)) %>% 
    mutate(Treatment_Time_New = Treatment_Time + F_Seconds%%1)  ## F_Seconds%%1 gives the deicmal point
  
  return(facs_df)
}

get_group_abbr <- function(group_name_full) {
  if (group_name_full %in% c('CH', 'CL')) {
    return('Continual')
  } else if (group_name_full %in% c('BH', 'BL')) {
    return('Batch')
  }
}

draw_heat_map_plot <- function(heat_map_df, type, plot_title) {
  
  heatmap_plot <- ggplot(heat_map_df, aes(x=row_name, y=col_name)) +
    
    geom_tile(aes(fill=diagonal_val)) +
    # geom_text(aes(label=diagonal_val)) +
    # geom_text(aes(label=diagonal_percentage)) +
    scale_fill_gradientn(colours = c("lightgray","dimgray"), name = "") +
    
    new_scale("fill") +
    geom_tile(aes(fill=non_diagonal_upper_matrix_val), 
              data = subset(heat_map_df, non_diagonal_upper_matrix_val >= 0)) +
    # geom_text(aes(label=non_diagonal_upper_matrix_val), data = subset(heat_map_df, non_diagonal_upper_matrix_val >= 0)) +
    # geom_text(aes(label=non_diagonal_upper_matrix_percentage), data = subset(heat_map_df, non_diagonal_upper_matrix_percentage >= 0)) +
    scale_fill_gradientn(colours = c("white", "yellow", "red"), name = "") +
    
    new_scale("fill") +
    geom_tile(aes(fill=non_diagonal_lower_matrix_val), 
              data = subset(heat_map_df, non_diagonal_lower_matrix_val < 0), show.legend = FALSE) +
    scale_fill_gradientn(colours = c("white")) +
    
    # ggtitle(plot_title) +
    xlab("") +
    ylab("") +
    theme_bw() +
    theme(text = element_text(size=20),
          panel.background = element_blank(),
          panel.grid = element_blank(),
          legend.position = 'left',
          plot.margin=unit(c(t = 0, r = 1.8, b = 2, l = 0), "lines"),
          plot.title = element_text(hjust = 0.5, size=24)) +
    labs(fill="") +
    scale_x_discrete(position = "top")
  
  
  if (type=='summative') {
    heatmap_plot <- heatmap_plot + 
      geom_text(aes(label=diagonal_val)) +
      geom_text(aes(label=non_diagonal_upper_matrix_val), data = subset(heat_map_df, non_diagonal_upper_matrix_val >= 0))
  } else if (type=='percentage') {
    heatmap_plot <- heatmap_plot + 
      geom_text(aes(label=diagonal_percentage)) +
      geom_text(aes(label=non_diagonal_upper_matrix_percentage), data = subset(heat_map_df, non_diagonal_upper_matrix_percentage >= 0))
  } 
  
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
           non_diagonal_lower_matrix_val = ifelse(is.na(non_diagonal_upper_matrix_val), -1, 0),
           diagonal_percentage=round(100*diagonal_val/sum(value, na.rm=T), 2),
           non_diagonal_upper_matrix_percentage=round(100*non_diagonal_upper_matrix_val/sum(value, na.rm=T), 2))
  
  View(heat_map_df)
  # convert_to_csv(heat_map_df, paste0('heat_map_dual_task_', group, '.csv'))
  return(heat_map_df)
}



draw_signal_plot <- function(subj_facs_df, subj, treatment) {
  signal_plot <- subj_facs_df %>% 
    select(Treatment_Time_New, F_Angry, F_Disgusted, F_Afraid, F_Happy, F_Sad, F_Surprised, F_Neutral) %>% 
    gather(key = "Expression", value = "Value", -Treatment_Time_New) %>% 
    mutate(Expression = recode_factor(Expression,
                                      'F_Neutral'='Neutral',
                                      'F_Surprised'='Surprised',
                                      'F_Sad'='Sad',
                                      'F_Happy'='Happy',
                                      'F_Afraid'='Afraid',
                                      'F_Disgusted'='Disgusted',
                                      'F_Angry'='Angry')) %>% 
    
    ggplot(aes(x=Treatment_Time_New, y=Value, color=Expression)) +
    geom_line(alpha = 0.5, size=1.2) +
    
    # ggtitle(paste0(subj, ' - ', treatment, ' - ', subj_facs_df$Group)) +
    xlab("Time [s]") +
    ylab("Probability") +
    
    scale_color_manual(values = c("Neutral"="Gray", 
                                  "Surprised"="Violet",
                                  "Sad"="Blue",
                                  "Happy"="Green",
                                  "Afraid"="Orange",
                                  "Disgusted"="Brown",
                                  "Angry"="Red"
                                  )) +
    
    theme_bw() +
    theme(text=element_text(size=18),
          axis.text = element_text(size=16),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"),
          legend.position='left',
          legend.title = element_blank(),
          plot.title = element_text(hjust = 0.5,
                                    size=20,
                                    margin=margin(t=0, r=0, b=15, l=0)), ##top, right, bottom, left
          plot.margin = unit(c(0.5, 2, 0.5, 0.5), "lines")) ##top, right, bottom, left
  
    
  return(signal_plot)
}


draw_signal_heatmap_plots <- function(facs_df, type) {
  # for (subj in levels(factor(facs_df$Participant_ID))) {
  # for (subj in c('T005')) {
  for (subj in c('T085')) {
    
    # for (treatment in c('RB', 'ST', 'PM', 'DT', 'PR')) {
    # for (treatment in c('PR')) {
    for (treatment in c('DT')) {
      
      subj_facs_df <- facs_df %>%
        filter(Participant_ID==subj & Treatment==treatment) 
      # %>% 
      #   slice(1:1000)
      
      heat_map_df <- get_heat_map_df(subj_facs_df)
      heatmap_plot <- draw_heat_map_plot(heat_map_df, type, paste0(subj, ' - ', treatment))
      # save_plot(paste0(subj, '_', treatment), heatmap_plot)
      
      signal_plot <- draw_signal_plot(subj_facs_df, subj, treatment)
      
      
      title <- ggdraw() + 
        draw_label(paste0(subj, ' - ', treatment, ' - ', get_group_abbr(subj_facs_df$Group[1])), 
                   # fontface='bold',
                   size=20)
      combined_dt_plot <- plot_grid(title,
                                    NULL,
                                    signal_plot,
                                    heatmap_plot,
                                    # labels=c('', '', 'A', 'B'),
                                    # label_size=18,
                                    rel_heights=c(0.1, 0.4, 1, 1),
                                    ncol=1)
      save_plot(paste0(subj, '_', treatment, '_combined_', type), combined_dt_plot, width=20)
    }
  }
}




#-------------------------#
#-------Main Program------#
#-------------------------#
# facs_df <<- read_data()

draw_signal_heatmap_plots(facs_df, 'summative')
# draw_signal_heatmap_plots(facs_df, 'percentage')








