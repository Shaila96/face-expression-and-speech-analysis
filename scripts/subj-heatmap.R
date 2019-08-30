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




#-------------------------#
#-----GLOBAL VARIABLES----#
#-------------------------#
script_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
source(file.path(script_dir, 'us-common-functions.R'))

current_dir <- dirname(script_dir)
setwd(current_dir)



facs_size <- 7
emotion_list <- c('F_Angry',
                  'F_Disgusted',
                  'F_Afraid',
                  'F_Happy',
                  'F_Sad',
                  'F_Surprised',
                  'F_Neutral')


#-------------------------#
#---FUNCTION DEFINITION---#
#-------------------------#
read_data <- function() {
  ## Making the df as global variable, as it takes a lot of time to load.
  ## So, during one R session we load this df once
  facs_df <<- custom_read_csv(file.path(current_dir, data_dir, facs_file_name)) %>% 
    mutate(Treatment_Time_New = Treatment_Time + F_Seconds%%1)  ## F_Seconds%%1 gives the deicmal point
  
  print(str(facs_df))
  
  return(facs_df)
}

make_matrix_data <- function(facs_df) {
  final_matrix = matrix(0, facs_size, facs_size)  ## initialize matrix with all 0

  # for (subj in levels(factor(facs_df$Participant_ID))) {
  for (subj in c('T005')) {
    
      # for (treatment in c('RB', 'ST', 'PM', 'DT', 'PR')) {
      for (treatment in c('PM')) {
        
        subj_facs_df <- facs_df %>%
          filter(Participant_ID==subj & Treatment==treatment)
        
        # for(i in 1:nrow(subj_facs_df)){
        for(i in 1:3){
          emotion_vals_by_row <- as.vector(unlist(subj_facs_df[i, emotion_list]))
          
          ## Outer product
          current_matrix <- outer(emotion_vals_by_row, emotion_vals_by_row)
          # print(outer_matrix)
          
          ## Convert into upper triangle matrix
          current_matrix[lower.tri(current_matrix)] <- 0
          
          ## Normalize with the sum of the elements of the matrix
          current_matrix <- current_matrix/sum(current_matrix)
          
          
          print(final_matrix)
          print(current_matrix)
          ## Add to the final matrix
          final_matrix <- final_matrix + current_matrix
      }
    }
  }
  
  # dividing matrix using 1000 and taking until 2 decimal
  final_matrix = round(final_matrix)
  final_matrix[lower.tri(final_matrix)] <- NA
  
  print(final_matrix)
  return(final_matrix)
}

draw_plots <- function(facs_matrix) {
  heat_map_df = melt(facs_matrix)
  heat_map_df$Var1 = as.factor(heat_map_df$Var1)
  heat_map_df$Var2 = as.factor(heat_map_df$Var2)
  heat_map_df = heat_map_df %>% mutate(value2 = ifelse(Var1==Var2, value, NA))
  heat_map_df = heat_map_df %>% mutate(value1 = ifelse(Var1==Var2, -1, value))
  heat_map_df$value1[is.na(heat_map_df$value1)] <- -2
  
  
  plot1 = ggplot(heat_map_df, aes(x=Var1, y=Var2))+
    geom_tile(aes(fill=value2))+
    scale_fill_gradientn(
      colours = c("lightgray","dimgray"), name = "X 1000")+
    new_scale("fill") +
    geom_tile(aes(fill=value1), data = subset(heat_map_df, value1 > -1))+
    scale_fill_gradientn(
      colours = c("white","yellow", "red"), name = "X 1000")+
    new_scale("fill") +
    geom_tile(aes(fill=value1), data = subset(heat_map_df, value1 < -1 ), show.legend = FALSE)+
    scale_fill_gradientn(
      colours = c("white"))+
    xlab("")+
    ylab("")+
    theme_bw()+
    theme(text = element_text(size=20),
          panel.background = element_blank(),
          panel.grid = element_blank()) +
    labs(fill=" X 1000") +
    scale_x_discrete(position = "top")
  
  print(plot1)
}


#-------------------------#
#-------Main Program------#
#-------------------------#
# facs_df <- read_data()
facs_matrix <- make_matrix_data(facs_df)
draw_plots(facs_matrix)




