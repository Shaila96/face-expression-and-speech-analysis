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
  
  # print(str(facs_df))
  
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
    
    ggtitle(plot_title) +
    xlab("") +
    ylab("") +
    theme_bw() +
    theme(text = element_text(size=20),
          panel.background = element_blank(),
          panel.grid = element_blank(),
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



draw_dual_task_group_plots <- function(facs_df, type) {
  plot_list <- list()
  
  for (group in group_list) {
    dt_facs_df <- facs_df %>%
      filter(Treatment=='DT' & Group %in% paste0(group, c('H', 'L'))) %>% 
      slice(1:1000)

    heat_map_df <- get_heat_map_df(dt_facs_df, group)
    heatmap_plot <- draw_heat_map_plot(heat_map_df, type, paste0('Dual Task - ', group))
    plot_list[[length(plot_list)+1]] <- heatmap_plot
  } 
  
  combined_dt_plot <- plot_grid(plotlist=plot_list, 
                          labels=c('A', 'B'), 
                          label_size=18,
                          ncol=2)
  
  save_plot(paste0('dual_task_group_', type), combined_dt_plot, width=20)
}




#-------------------------#
#-------Main Program------#
#-------------------------#
# facs_df <- read_data()

# get_stats(facs_df)

draw_dual_task_group_plots(facs_df, 'summative')
draw_dual_task_group_plots(facs_df, 'percentage')










