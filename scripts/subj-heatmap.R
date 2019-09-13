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

draw_heat_map_plot <- function(heat_map_df, plot_title) {
  
  heatmap_plot <- ggplot(heat_map_df, aes(x=row_name, y=col_name)) +
    
    geom_tile(aes(fill=diagonal_val)) +
    # geom_text(aes(label=diagonal_val)) +
    geom_text(aes(label=100*diagonal_val/sum(heat_map_df$diagonal_val))) +
    scale_fill_gradientn(colours = c("lightgray","dimgray"), name = "") +
    # scale_fill_gradientn(colours = c("white", "lightgray","dimgray"), name = "") +
    # scale_fill_gradientn(colours = c("yellow", "orange", "tomato2", "red", "red4", "black"), name = "") +
    
    new_scale("fill") +
    geom_tile(aes(fill=non_diagonal_upper_matrix_val), 
              data = subset(heat_map_df, non_diagonal_upper_matrix_val >= 0)) +
    # geom_text(aes(label=non_diagonal_upper_matrix_val), data = subset(heat_map_df, non_diagonal_upper_matrix_val >= 0)) +
    geom_text(aes(label=100*non_diagonal_upper_matrix_val/sum(heat_map_df$non_diagonal_upper_matrix_val)), 
              data = subset(heat_map_df, non_diagonal_upper_matrix_val >= 0)) +
    scale_fill_gradientn(colours = c("white", "lightblue", "darkblue"), name = "") +
    # scale_fill_gradientn(colours = c("lightblue", "darkblue"), name = "") +
    
    new_scale("fill") +
    geom_tile(aes(fill=non_diagonal_lower_matrix_val), 
              data = subset(heat_map_df, non_diagonal_lower_matrix_val < 0), show.legend = FALSE) +
    # geom_text(aes(label=non_diagonal_val), data = subset(heat_map_df, non_diagonal_val < -1), show.legend = FALSE) +
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
           non_diagonal_lower_matrix_val = ifelse(is.na(non_diagonal_upper_matrix_val), -1, 0)) %>% 
    mutate(col_name = recode(col_name,
                             'WordCount'='Word Count',
                             'CharCount'='Char Count'))
  
  # print(heat_map_df)
  # convert_to_csv(heat_map_df, paste0('heat_map_dual_task_', group, '.csv'))
  return(heat_map_df)
}



get_new_heat_map_df <- function(subj_facs_df, group='no_group') {
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
           non_diagonal_upper_matrix_val = ifelse(row_name==col_name, NA, value),
           non_diagonal_lower_matrix_val = ifelse(is.na(non_diagonal_upper_matrix_val), NA, 0),
           diagonal_percentage=diagonal_val/sum(diagonal_val, na.rm=T),
           non_diagonal_percentage=non_diagonal_upper_matrix_val/sum(non_diagonal_upper_matrix_val, na.rm=T)) %>% 
    mutate(col_name = recode(col_name,
                             'WordCount'='Word Count',
                             'CharCount'='Char Count'))
  
  View(heat_map_df)
  # convert_to_csv(heat_map_df, paste0('heat_map_dual_task_', group, '.csv'))
  return(heat_map_df)
}

draw_subj_session_plots <- function(facs_df) {

  # for (subj in levels(factor(facs_df$Participant_ID))) {
  # for (subj in c('T166', 'T175', 'T178')) {
  for (subj in c('T005')) {
    
    # for (treatment in c('RB', 'ST', 'PM', 'DT', 'PR')) {
    for (treatment in c('PR')) {
      
      subj_facs_df <- facs_df %>%
        filter(Participant_ID==subj & Treatment==treatment)

      heat_map_df <- get_heat_map_df(subj_facs_df)
      heatmap_plot <- draw_heat_map_plot(heat_map_df, paste0(subj, ' - ', treatment))
      save_plot(paste0(subj, '_', treatment), heatmap_plot)
    }
  }
  
}


draw_dual_task_group_plots <- function(facs_df) {
  plot_list <- list()
  for (group in group_list) {
    dt_facs_df <- facs_df %>%
      filter(Treatment=='DT' & Group %in% paste0(group, c('H', 'L'))) %>% 
      slice(1:100)

    heat_map_df <- get_heat_map_df(dt_facs_df, group)
    heatmap_plot <- draw_heat_map_plot(heat_map_df, paste0('Dual Task - ', group))
    # save_plot(paste0('dual_task_', group), heatmap_plot)
    plot_list[[length(plot_list)+1]] <- heatmap_plot
  } 
  
  combined_dt_plot <- plot_grid(plotlist=plot_list, 
                          labels=c('A', 'B'), 
                          label_size=18,
                          ncol=2)
  save_plot('dual_task_group', combined_dt_plot, width=20)
}

draw_new_dual_task_group_plots <- function(facs_df) {
  plot_list <- list()
  for (group in group_list) {
    dt_facs_df <- facs_df %>%
      filter(Treatment=='DT' & Group %in% paste0(group, c('H', 'L'))) %>% 
      slice(1:100)
    
    heat_map_df <- get_new_heat_map_df(dt_facs_df, group)
    heatmap_plot <- draw_new_heat_map_plot(heat_map_df, paste0('Dual Task - ', group))
    # save_plot(paste0('dual_task_', group), heatmap_plot)
    plot_list[[length(plot_list)+1]] <- heatmap_plot
  } 
  
  combined_dt_plot <- plot_grid(plotlist=plot_list, 
                                labels=c('A', 'B'), 
                                label_size=18,
                                ncol=2)
  save_plot('dual_task_group', combined_dt_plot, width=20)
}


draw_dual_task_group_email_report_plots <- function(facs_df) {
  plot_list <- list()
  for (group in group_list) {
    for (task in c('Email', 'Report')) {
      dt_task_facs_df <- facs_df %>%
        filter(Treatment=='DT' & 
               Task==task &
               Group %in% paste0(group, c('H', 'L')))
      
      heat_map_df <- get_new_heat_map_df(dt_task_facs_df)
      heatmap_plot <- draw_heat_map_plot(heat_map_df, paste0('Dual Task - ', group, ' - ', task))
      save_plot(paste0('dual_task_', group, '_', task), heatmap_plot)
      plot_list[[length(plot_list)+1]] <- heatmap_plot
    }
  } 
  
  combined_dt_plot <- plot_grid(plotlist=plot_list, 
                                labels=c('A', 'B', 'C', 'D'), 
                                label_size=18,
                                ncol=2)
  
  save_plot('dual_task_group_email_report', combined_dt_plot, width=30, height=20)
}


draw_dual_task_email_report_plots <- function(facs_df) {
  plot_list <- list()
  for (task in c('Email', 'Report')) {
    dt_task_facs_df <- facs_df %>%
      filter(Treatment=='DT' & Task==task)
    
    heat_map_df <- get_heat_map_df(dt_task_facs_df)
    heatmap_plot <- draw_heat_map_plot(heat_map_df, paste0('Dual Task - ', task))
    save_plot(paste0('dual_task_', task), heatmap_plot)
    plot_list[[length(plot_list)+1]] <- heatmap_plot
  }
  
  combined_dt_plot <- plot_grid(plotlist=plot_list, 
                                labels=c('A', 'B'), 
                                label_size=18,
                                ncol=2)
  
  save_plot('dual_task_email_report', combined_dt_plot, width=20)
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

get_proportion_test <- function() {
  # file_name_list <- c('heat_map_dual_task_B.csv', 'heat_map_dual_task_C.csv')
  
  # diagonal_df_list <- vector()
  # upper_diagonal_df_list <- vector()
  
  diagonal_df_list <- list()
  upper_diagonal_df_list <- list()
  
  for (group in group_list) {
    
    file_name <- paste0('heat_map_dual_task_', group, '.csv')
    prop_test_df <- custom_read_csv(file.path(current_dir, curated_data_dir, file_name)) %>% 
      mutate(facs_combination=ifelse(row_name==col_name, row_name, paste0(row_name, '_', col_name)))
    
    diagonal_df <- prop_test_df %>% 
      select(facs_combination, diagonal_val) %>% 
      na.omit() %>% 
      # mutate(sum = sum(diagonal_val),
      #        proportion = diagonal_val/sum) %>% 
      mutate(!!paste0('sum_', group):=sum(diagonal_val),
             !!paste0('diagonal_val_', group):=diagonal_val) %>%
      select(-diagonal_val)
    
    upper_diagonal_df <- prop_test_df %>% 
      select(facs_combination, non_diagonal_upper_matrix_val) %>% 
      filter(non_diagonal_upper_matrix_val>=0) %>% 
      na.omit() %>% 
      # mutate(sum = sum(non_diagonal_upper_matrix_val),
      #        proportion = non_diagonal_upper_matrix_val/sum) %>% 
      mutate(!!paste0('sum_', group):=sum(non_diagonal_upper_matrix_val),
             !!paste0('non_diagonal_upper_matrix_val_', group):=non_diagonal_upper_matrix_val) %>%
      select(-non_diagonal_upper_matrix_val)
    
    
    # View(diagonal_df)
    # View(upper_diagonal_df)
    
    
    # diagonal_df_list <- c(diagonal_df_list, diagonal_df)
    # upper_diagonal_df_list <- c(upper_diagonal_df_list, upper_diagonal_df)
    
    diagonal_df_list[[length(diagonal_df_list)+1]] <- diagonal_df
    upper_diagonal_df_list[[length(upper_diagonal_df_list)+1]] <- upper_diagonal_df

    
    
    # prop_test = prop.test(diagonal_df$diagonal_val, diagonal_df$sum)
    # print(prop_test)
    # 
    # prop_test = prop.test(upper_diagonal_df$non_diagonal_upper_matrix_val, upper_diagonal_df$sum)
    # print(prop_test)
  }
  
  # View(diagonal_df_list)
  # View(upper_diagonal_df_list)
  
  # View(Map(cbind, diagonal_df_list))
  
  diagonal_df <- merge(diagonal_df_list[1], diagonal_df_list[2], by='facs_combination') %>% 
    mutate(percentage_B=100*diagonal_val_B/sum_B,
           percentage_C=100*diagonal_val_C/sum_C,
           percentage_diff=percentage_C-percentage_B)
  upper_diagonal_df <- merge(upper_diagonal_df_list[1], upper_diagonal_df_list[2], by='facs_combination') %>% 
    mutate(percentage_B=100*non_diagonal_upper_matrix_val_B/sum_B,
           percentage_C=100*non_diagonal_upper_matrix_val_C/sum_C,
           percentage_diff=percentage_C-percentage_B)
  
  
  
  View(diagonal_df)
  View(upper_diagonal_df)
  
  
  temp_diagonal_df <- diagonal_df %>%
    mutate(percentage_B=round(percentage_B),
           percentage_C=round(percentage_C),
           percentage_B=case_when(percentage_B==0~1,
                                  TRUE~percentage_B),
           percentage_C=case_when(percentage_C==0~1,
                                  TRUE~percentage_C)
           )

  print(temp_diagonal_df$percentage_B)
  print(temp_diagonal_df$percentage_C)

  print(sum(temp_diagonal_df$percentage_B))
  print(sum(temp_diagonal_df$percentage_C))

  diagonal.data <- as.table(matrix(c(temp_diagonal_df$percentage_B, temp_diagonal_df$percentage_C),
                          nrow=2,
                          ncol=nrow(temp_diagonal_df),
                          byrow=TRUE,
                          dimnames = list(c("Batch", "Continual"),
                                          temp_diagonal_df$facs_combination)))

  print(chisq.test(diagonal.data, correct=FALSE))
  
  
  
  
  
  
  
  
  
  temp_upper_diagonal_df <- upper_diagonal_df %>%
    mutate(percentage_B=round(percentage_B),
           percentage_C=round(percentage_C),
           percentage_B=case_when(percentage_B==0~1,
                                  TRUE~percentage_B),
           percentage_C=case_when(percentage_C==0~1,
                                  TRUE~percentage_C))

  print(temp_upper_diagonal_df$percentage_B)
  print(temp_upper_diagonal_df$percentage_C)

  print(sum(temp_upper_diagonal_df$percentage_B))
  print(sum(temp_upper_diagonal_df$percentage_C))

  diagonal.data <- as.table(matrix(c(temp_upper_diagonal_df$percentage_B, temp_upper_diagonal_df$percentage_C),
                                   nrow=2,
                                   ncol=nrow(temp_upper_diagonal_df),
                                   byrow=TRUE,
                                   dimnames = list(c("Batch", "Continual"),
                                                   temp_upper_diagonal_df$facs_combination)))

  print(chisq.test(diagonal.data, correct=FALSE))
  
  
  
  
  
  
  temp_upper_diagonal_df <- upper_diagonal_df %>%
    mutate(percentage_B=round(percentage_B),
           percentage_C=round(percentage_C),
           percentage_B=case_when(percentage_B==0~1,
                                  TRUE~percentage_B),
           percentage_C=case_when(percentage_C==0~1,
                                  TRUE~percentage_C)) %>% 
    filter(
      percentage_diff>0
      # !(facs_combination %in% c('Surprised_Neutral', 
      #                              'Angry_Surprised', 
      #                              'Angry_Happy', 
      #                              'Disgusted_Afraid',
      #                              'Sad_Surprised',
      #                              'Happy_Surprised',
      #                              'Disgusted_Neutral',
      #                              'Disgusted_Surprised',
      #                              'Disgusted_Happy',
      #                              'Disgusted_Sad',
      #                              'Angry_Disgusted',
      #                              'Happy_Sad',
      #                              'Afraid_Happy',
      #                              'Happy_Neutral',
      #                              'Angry_Afraid',
      #                              'Afraid_Neutral',
      #                              'Angry_Sad'))
      )
  
  
  View(temp_upper_diagonal_df)
  
  print(temp_upper_diagonal_df$percentage_B)
  print(temp_upper_diagonal_df$percentage_C)
  
  print(sum(temp_upper_diagonal_df$percentage_B))
  print(sum(temp_upper_diagonal_df$percentage_C))
  
  diagonal.data <- as.table(matrix(c(temp_upper_diagonal_df$percentage_B, temp_upper_diagonal_df$percentage_C),
                                   nrow=2,
                                   ncol=nrow(temp_upper_diagonal_df),
                                   byrow=TRUE,
                                   dimnames = list(c("Batch", "Continual"),
                                                   temp_upper_diagonal_df$facs_combination)))
  
  print(chisq.test(diagonal.data, correct=FALSE))
  
  
  
  
  
  
  # print(sum(round(diagonal_df$percentage_C)))
  # print(sum(round(diagonal_df$percentage_B/100, 2)))
  # 
  # print(round(diagonal_df$percentage_C))
  # print(round(diagonal_df$percentage_B/100, 2))
  
  
  
  
  

  
  # observed <- round(upper_diagonal_df$percentage_C)
  # prob <- round(upper_diagonal_df$percentage_B/100, 2)
  # 
  # print(observed)
  # print(prob)
  # 
  # print(sum(observed))
  # print(sum(prob))
  # 
  # print(chisq.test(observed, prob))
  # 
  # 
  # upper_diagonal_df <- upper_diagonal_df %>%
  #     filter(!(facs_combination %in% c('Afraid_Sad', 'Angry_Sad')))
  # 
  # 
  # observed <- round(upper_diagonal_df$percentage_C)
  # prob <- round(upper_diagonal_df$percentage_B/100, 2)

  
  
  
  
  
  
  
  
  
  
  
  
  
  # observed <- round(diagonal_df$percentage_C)
  # prob <- round(diagonal_df$percentage_B/100, 2)
  # 
  # 
  # print(observed)
  # print(prob)
  # 
  # print(sum(observed))
  # print(sum(prob))
  
  # observed <- c(8,  5,  1,  1, 32, 54,  1)
  # prob <- c(0.11, 0.18, 0.01, 0.03, 0.45, 0.22, 0.01)
  
  # print(chisq.test(observed, prob))
  # multinomial.test(observed, prob)
  
  # observed <- c(5, 2, 1)   		
  # prob <- c(0.25, 0.5, 0.25) 	
  
  # observed <- c(8,  5,  0,  1, 32, 54,  0)   		
  # prob <- c(0.11, 0.18, 0.00, 0.03, 0.45, 0.22, 0.01) 
  
  
  
  
  
  
  
  
  
  
  
  
  
  ## Calculate p-value using default options:
  # multinomial.test(observed, prob, useChisq = T)
  
  # multinomial.test(observed,
  #                  prob,
  #                  useChisq = F,
  #                  MonteCarlo = F,
  #                  ntrial = 1,
  #                  atOnce = 1)
  #                  # ntrial = 100000,
  #                  # atOnce = 1000000)
  
  
  
  
  
  
  
  
  
  
  
  
  # t_test <- t.test(upper_diagonal_df$percentage_diff, mu = 0, alternative = "greater")
  # print(t_test)
  

  
  
  # prop_test <- prop.test(c(upper_diagonal_df$non_diagonal_upper_matrix_val_B, upper_diagonal_df$non_diagonal_upper_matrix_val_C),
  #                        c(upper_diagonal_df$sum_B, upper_diagonal_df$sum_C))
  # print(prop_test)
  # 
  # 
  # new_upper_diagonal_df <- upper_diagonal_df %>%
  #   filter(!(facs_combination %in% c('Afraid_Sad', 'Angry_Sad')))
  # 
  # 
  # # View(upper_diagonal)
  # prop_test <- prop.test(c(new_upper_diagonal_df$non_diagonal_upper_matrix_val_B, new_upper_diagonal_df$non_diagonal_upper_matrix_val_C),
  #                        c(new_upper_diagonal_df$sum_B, new_upper_diagonal_df$sum_C))
  # print(prop_test)
  
  
  
  
  
  
  # prop_test <- prop.test(c(upper_diagonal_df$percentage_B, upper_diagonal_df$percentage_C),
  #                        c(rep(100, 2*nrow(upper_diagonal_df))))
  # print(prop_test)
  # 
  # 
  # new_upper_diagonal_df <- upper_diagonal_df %>%
  #   filter(!(facs_combination %in% c('Afraid_Sad', 'Angry_Sad')))
  # 
  # 
  # prop_test <- prop.test(c(new_upper_diagonal_df$percentage_B, new_upper_diagonal_df$percentage_C),
  #                        c(rep(100, 2*nrow(new_upper_diagonal_df))))
  # print(prop_test)
  
  
  
  
  
  
  
  # prop_test <- pairwise.prop.test(c(upper_diagonal_df$non_diagonal_upper_matrix_val_B, upper_diagonal_df$non_diagonal_upper_matrix_val_C),
  #                                 c(upper_diagonal_df$sum_B, upper_diagonal_df$sum_C),
  #                                 p.adjust.method = p.adjust.methods)
  # print(prop_test)
  
  
  
  
  
  
  
  
  
  
  ##### diagonal_matrix <- as.matrix(round(c(c(90, 0, 0, 0, 0, 0, 0, 0, 10), diagonal_df[, c('percentage_C')])), 2, 9)
  # diagonal_matrix <- as.matrix(round(diagonal_df[, c('percentage_B', 'percentage_C')]))
  # non_diagonal_matrix <- as.matrix(round(upper_diagonal_df[, c('percentage_B', 'percentage_C')]))
  # 
  # print(diagonal_matrix)
  # print(non_diagonal_matrix)
  # 
  # print(cochrans.q(diagonal_matrix, alpha=NULL))
  # print(cochrans.q(non_diagonal_matrix, alpha=NULL))
  # 
  # mcnemar.test(diagonal_matrix)
  
  
  
  # Performance <-
  #   matrix(c(794, 86, 150, 570),
  #          nrow = 2,
  #          dimnames = list("1st Survey" = c("Approve", "Disapprove"),
  #                          "2nd Survey" = c("Approve", "Disapprove")))
  # print(Performance)
  # mcnemar.test(Performance)
  
  
  
  # cooccur(diagonal_matrix, type = "spp_site")
  # chisq.test(tbl) 
  
  
  
  
  
  
#   new_upper_diagonal_df <- data.frame()
# 
#   for (fac in levels(factor(upper_diagonal_df$facs_combination))) {
#     temp_upper_diagonal_df <- upper_diagonal_df %>%
#       filter(facs_combination==fac)
# 
#     prop_test = prop.test(c(temp_upper_diagonal_df$non_diagonal_upper_matrix_val_B, temp_upper_diagonal_df$non_diagonal_upper_matrix_val_C),
#                           c(temp_upper_diagonal_df$sum_B, temp_upper_diagonal_df$sum_C),
#                           p = NULL,
#                           alternative = 'less',
#                           correct = T)
#     # print(fac)
#     # print(prop_test$p.value)
#     temp_upper_diagonal_df$p_val=prop_test$p.value
# 
#     new_upper_diagonal_df <- rbind(new_upper_diagonal_df, temp_upper_diagonal_df)
#   }
# 
#   View(new_upper_diagonal_df)
#   
}


#-------------------------#
#-------Main Program------#
#-------------------------#
# facs_df <- read_data()

# get_stats(facs_df)

# draw_dual_task_group_plots(facs_df)

# draw_new_dual_task_group_plots(facs_df)

get_proportion_test()


# draw_subj_session_plots(facs_df)
# draw_dual_task_group_email_report_plots(facs_df)
# draw_dual_task_email_report_plots(facs_df)









