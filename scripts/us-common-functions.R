scripts_dir <- 'scripts'
plots_dir <- 'plots'
log_dir <- 'log-files'

data_dir <- 'data'
curated_data_dir <- 'curated-data'

# subj_data_dir <- 'subject-data'
# utility_data_dir <- 'utility-data'
# physiological_data_dir <- 'physiological-data'
# questionnaire_data_dir <- 'questionnaire-data'
# performance_data_dir <- 'performance-data'
# index_data_dir <- 'index-data'
# final_data_dir <- 'final-data'

default_plot_width <- 12
default_plot_height <- 10

one_hour_sec <- 3600

facs_file_name <- 'Physiology+FACS+Gaze+Speech+Judges.csv'
facs_gaze_speech_file_name <- 'facs_gaze_speech_data.csv'
facs_judges_file_name <- 'facs_judges.csv'



font_style <- list(
  family = "Courier New, monospace",
  size = 18,
  color = "#7f7f7f"
)


brown <- 'rgb(202, 111, 30)'
violet <- 'rgb(91, 44, 111)'
light_red <- 'rgb(231, 76, 60)'
light_violet <- 'rgb(210, 180, 222)'
light_green <- 'rgb(39, 174, 96)'
light_blue <- 'rgb(133, 193, 233)'
light_grey <- 'rgb(204, 209, 209)'

# subj_list_file_name <- 'subj_list.csv'


# grp_dir <- 'Group1'
# session_list <- c('Baseline', 'WorkingSession')
# signal_list <- c('PP', 'E4_HR', 'E4_EDA', 'iWatch_HR')
# 
# 
# pp_file_pattern <- '.*_pp.csv'
# nr_pp_file_pattern <- '.*_nr.csv'
# rb_marker_file_pattern <- '.*Baseline_sessionmarkers.csv'
# ws_marker_file_pattern <- '.*WorkingSession_sessionmarkers.csv'
# activity_file_pattern <- '.*Activity.csv'
# mac_app_usage_file_pattern <- '.*Monitor.*log'
# win_app_usage_file_pattern <- '.*MonitorLog.csv'
# e4_file_pattern <- 'HR.csv|EDA.csv'
# iWatch_file_pattern <- '.*iWatch.csv'



# computer_usage_pattern = '^CR$|^CW$|^Computer - Reading$|^Computer - Writing$|^C - Reading$|^C - Writing$|^C - Writing/Reading$'
# computer_usage_pattern = 'CR|CW|Computer - Reading|Computer - Writing|C - Reading|C - Writing|C - Writing/Reading|Working'
# computer_usage_pattern = 'CR|CW'



# qc0_file_name <- 'qc0_all_subj.csv'
# qc1_file_name <- 'qc1_all_subj.csv'
# qc2_file_name <- 'qc2_all_subj.csv'
# 
# 
# qc0_session_mean_file_name <- 'qc0_session_mean.csv'
# qc1_session_mean_file_name <- 'qc1_session_mean.csv'
# qc2_session_mean_file_name <- 'qc2_session_mean.csv'
# 
# qc0_activity_mean_file_name <- 'qc0_activity_mean.csv'
# qc1_activity_mean_file_name <- 'qc1_activity_mean.csv'
# qc2_activity_mean_file_name <- 'qc2_activity_mean.csv'


## We don't know until now how many filtering we will do :P
## Hope it doesn't exixts 99 filtering
# qc99_file_name <- 'qc99_all_subj.csv'




## 1. Timestamp -->    Extract whatever inside []                 -->   \\[(.*)\\]
## 2. Application -->  Extract whatever after the space of []     -->   (.*)
# mac_data_pattern <- '\\[(.*)\\] (.*)'

# s_interface_date_format <- '%a %b %d %H:%M:%S'


decorator_hash <- '###########################################################'




custom_read_csv <- function(file_name) {
  return(read.csv(file_name, stringsAsFactors=F))
}

convert_to_csv <- function(df, file_path) {
  write.table(df, file = file_path, row.names=F, sep = ',')
}

save_plot <- function(plot_name, plot, width=default_plot_width, height=default_plot_height) {
  plot_path <- file.path(plots_dir, paste0(plot_name, '.png'))
  ggsave(plot_path, plot, width=width, height=height)
  
  plot_path <- file.path(plots_dir, paste0(plot_name, '.pdf'))
  ggsave(plot_path, plot, device=cairo_pdf, width=width, height=height)
}




#--------------------------#
#-------   String   -------#
#--------------------------#
print_msg <- function(msg) {
  print(msg)
  message(msg)
}

write_log_msg <- function(msg, file_name) {
  message(msg)
  write(msg, file=file_name, append=TRUE)
}

is_match <- function(str, pattern) { 
  return(grepl(pattern, str)) 
} 

replace_to_underscore <- function(str) {
  gsubfn('.', list('.' = '_', ' ' = '_', '-' = '_'), tolower(str))
}

replace_to_space <- function(str) {
  gsubfn('.', list('_' = ' ', '-' = ' '), str)
}

trim <- function( x ) {
  gsub("(^[[:space:]]+|[[:space:]]+$)", "", x)
}

is_null <- function(cell_val) {
  print(cell_val)
  if (is.na(cell_val)) {
    return(T)
  } else if (length(trim(cell_val))==0) {
    return(T)
  } 
  
  # else if (cell_val=="") {
  #   return(T)
  # }
  
  return(F)
}



#---------------------------------#
#-------   Date and Time   -------#
#---------------------------------#
convert_date <- function(data, date_format) {
  return(as.POSIXct(data, format=date_format))
}

convert_s_interface_date <- function(data) {
  convert_date(data, s_interface_date_format)
}


#--------------------------#
#--- File and Directory ---#
#--------------------------#
is_empty <- function(item) {
  return(length(item)==0)
}

get_dir_list <- function(directory) {
  return(list.dirs(path=directory, full.names=F, recursive=F))
}

get_matched_file_names <- function(directory, file_pattern) {
  return(list.files(path=directory, pattern=file_pattern, recursive=F))
}

get_matched_file_names_recursively <- function(directory, file_pattern) {
  return(list.files(path=directory, pattern=file_pattern, recursive=T))
}


#--------------------------#
#---   Miscellaneous    ---#
#--------------------------#
get_axis_label <- function(label) {
  return(list(
    title = label,
    titlefont = font_style
  ))
}

