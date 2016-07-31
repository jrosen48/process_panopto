#-----------------------------
# 1. Setting up
#-----------------------------

rm(list = ls())

setwd("~/Dropbox/research/Engagement")

library(dplyr)
library(lubridate)

the_dir <- "~/Dropbox/research/Engagement/data/f15data/csv"
the_key <-"~/Dropbox/research/Engagement/data/f15data/key.csv"

#-----------------------------
# 2. The functions
#-----------------------------

file_loader <- function(the_key, the_dir, file_type){
  key <- read.csv(the_key, stringsAsFactors = FALSE) # loads key
  # key <- dplyr::select(key, ID, Username) # for spring
  key <- dplyr::select(key, ID = Study.ID, Username) # for fall
  file_names <- list.files(the_dir)
  file_names <- sort(file_names)
  print(file_names)
  file_list <- list()
  error_log <- list()
  for (i in 1:length(file_names)) { 
    print(paste("Processing ", file_names[[i]], sep = ""))
    if(file_type == "txt"){
      file <- read.delim(paste0(the_dir, "/", file_names[i]), header = T, stringsAsFactors = F, 
                         fileEncoding = "UCS-2LE")
    } else{
      file <- read.csv(paste0(the_dir, "/", file_names[i]), header = T, stringsAsFactors = F)
    }
    file_list[[i]] <- file # saves file to list
    file_list[[i]][, 4] <- sub(".*\\\\", "", file_list[[i]][, 4]) # extracts username

    file_list[[i]][, 5] <- file_names[[i]]
    
    names(file_list[[i]]) <- c("date", "start_time_seconds", "time_watched_minutes", "username", "video_id") # renames columns for each file
  } 
  return(file_list)
}

file_list <- file_loader(the_key = the_key, 
                         the_dir = the_dir,
                         file_type = "txt")

make_new_variables <- function(file_list){
  max_view <- vector()
  for (i in 1:length(file_list)){
    max_view[[i]] <- max(file_list[[i]]$time_watched_minutes)
    file_list[[i]] <- mutate(file_list[[i]], # creates new variables for each file
                             time_watched_seconds = time_watched_minutes * 60,
                             end_time_seconds = start_time_seconds + time_watched_seconds,
                             end_time_minutes = end_time_seconds /60,
                             date = lubridate::parse_date_time(date, c("%m/%d/%Y %I:%M:%S %p","%m/%d/%Y %H%M"), tz = "EST5EDT"))
  }
  out <- list(file_list, max_view)
  return(out)
}

tmp_out <- make_new_variables(file_list)
mod_file_list <- tmp_out[[1]]
max_view <- tmp_out[[2]]

# final processing

mod_file_df <- plyr::ldply(mod_file_list)

key <- read.csv(the_key, stringsAsFactors = F, header = T)
# key <- select(key, ID, username = Username) # for spring
key <- select(key, ID = Study.ID, username = Username) # for fall

mod_file_df <- dplyr::left_join(mod_file_df, key, by = "username")
mod_file_df <- filter(mod_file_df, !is.na(ID))

out <- mod_file_df %>%
  group_by(ID, video_id) %>%
  summarize(total_time = sum(time_watched_minutes)) %>%
  tidyr::spread(video_id, total_time, fill = 0) %>%
  ungroup()

colMeans(out[, 2:ncol(out)])

prop <- out[, 2:ncol(out)] / max_view
prop$ID <- out$ID

# write.csv(prop, "~/Dropbox/research/Engagement/data/f15data/fall_prop.csv", row.names = F)

colMeans(prop[, 1:(ncol(prop) - 1)])
mean(colMeans(prop[, 1:(ncol(prop) - 1)]))

# write.csv(colMeans(prop[, 1:(ncol(prop) - 1)]), "~/Dropbox/research/Engagement/data/s15data/spring_col_means.csv", row.names = F)

#-----------------------------
# 3. Subsequent analysis - proportion
#-----------------------------

remove(list = ls())

spring_prop <- read.csv("~/Dropbox/research/Engagement/data/s15data/spring_prop.csv")
fall_prop <- read.csv("~/Dropbox/research/Engagement/data/f15data/fall_prop.csv")

str(spring_prop)
str(fall_prop)

spring_col_means <- read.csv("~/Dropbox/research/Engagement/data/s15data/spring_col_means.csv")
fall_col_means <- read.csv("~/Dropbox/research/Engagement/data/f15data/fall_col_means.csv")

str(spring_col_means)
mean(spring_col_means$x)
str(fall_col_means)
mean(fall_col_means$x)

# waves

spring_prop_proc <- as.data.frame(t(zoo::rollapply(t(spring_prop[, -ncol(spring_prop)]), width = 8, by = 8, mean, partial = T)))
str(spring_prop_proc)
names(spring_prop_proc) <- paste0("wave_", 1:8)
str(spring_prop_proc)
spring_prop_proc$ID <- spring_prop$ID
str(spring_prop_proc)
write.csv(spring_prop_proc, "spring_prop_processed.csv", row.names = F)

fall_prop_proc <- as.data.frame(t(zoo::rollapply(t(fall_prop[, -ncol(fall_prop)]), width = 8, by = 8, mean, partial = T)))
str(fall_prop_proc)
names(fall_prop_proc) <- paste0("wave_", 1:8)
str(fall_prop_proc)
fall_prop_proc$ID <- fall_prop$ID
str(fall_prop_proc)
write.csv(fall_prop_proc, "fall_prop_processed.csv", row.names = F)

# new analysis with all three semesters
str(spring_prop_proc)
str(fall_prop_proc)
all_prop_proc <- rbind(spring_prop_proc, fall_prop_proc)

spring_prop_proc_ss <- mutate(spring_prop_proc, semester = "spring")
fall_prop_proc_ss <- mutate(fall_prop_proc, semester = "fall")

all_prop_proc_ss <- rbind(spring_prop_proc_ss, fall_prop_proc_ss)

out1 <- all_prop_proc_ss %>% 
  group_by(semester) %>% 
  summarize(mw1 = mean(wave_1, na.rm = T),
            mw2 = mean(wave_2, na.rm = T),
            mw3 = mean(wave_3, na.rm = T),
            mw4 = mean(wave_4, na.rm = T),
            mw5 = mean(wave_5, na.rm = T),
            mw6 = mean(wave_6, na.rm = T),
            mw7 = mean(wave_7, na.rm = T),
            mw8 = mean(wave_8, na.rm = T))

out2 <- all_prop_proc_ss %>% 
  group_by(semester) %>% 
  summarize(mw1 = sd(wave_1, na.rm = T),
            mw2 = sd(wave_2, na.rm = T),
            mw3 = sd(wave_3, na.rm = T),
            mw4 = sd(wave_4, na.rm = T),
            mw5 = sd(wave_5, na.rm = T),
            mw6 = sd(wave_6, na.rm = T),
            mw7 = sd(wave_7, na.rm = T),
            mw8 = sd(wave_8, na.rm = T))

write.csv(out1, "flipped_m-flipped_mean.csv")
write.csv(out2, "flipped_m-flipped_sd.csv")

data <- haven::read_sav("~/Dropbox/research/Engagement/data/all_three_semesters.sav")
data$ID <- as.integer(data$ID)
data <- left_join(data, all_prop_proc, by = "ID")

str(data)

attributes(data)$names

data_ss <- filter(data, T1Q126 != 6)

mean(data_ss$T1Q126, na.rm = T)
mean(data_ss$T1Q134, na.rm = T)

write.csv(data, "~/Dropbox/research/Engagement/data/all_three_semesters.csv", row.names = F)
haven::write_sav(data, "~/Dropbox/research/Engagement/data/AllThreeSemesters_Dropped_Without 66_2016.06.04_with_waves.sav")
