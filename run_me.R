#Load libraries
library(tidyverse)
library(lubridate)
library(janitor)

#Import data
#note: These datasets were huge, if you want re-run this script, you must first download these dataset and put
#under /input/data/2018/ folder, and also keep the file name same as thye are, then good to run.

Q1_2018 <- read.csv("./input/data/2018/Divvy_Trips_2018_Q1.csv", stringsAsFactors = F) %>% clean_names() %>% 
  select(duration_sec = x01_rental_details_duration_in_seconds_uncapped, user_type,
         started_at = x01_rental_details_local_start_time, ended_at = x01_rental_details_local_end_time
         )
Q1_2018$quarter <- "Quarter 1"

Q2_2018 <- read.csv("./input/data/2018/Divvy_Trips_2018_Q2.csv", stringsAsFactors = F) %>%
  select(started_at = start_time, ended_at = end_time, duration_sec = tripduration, user_type = usertype)
Q2_2018$quarter <- "Quarter 2"

Q3_2018 <- read.csv("./input/data/2018/Divvy_Trips_2018_Q3.csv", stringsAsFactors = F) %>% 
  select(started_at = start_time, ended_at = end_time, duration_sec = tripduration, user_type = usertype)
Q3_2018$quarter <- "Quarter 3"

Q4_2018 <- read.csv("./input/data/2018/Divvy_Trips_2018_Q4.csv", stringsAsFactors = F) %>% 
  select(started_at = start_time, ended_at = end_time, duration_sec = tripduration, user_type = usertype)
Q4_2018$quarter <- "Quarter 4"

#bind datasets together
data_18 <- bind_rows(Q1_2018, Q2_2018, Q3_2018, Q4_2018)

#clean ride length variable and add weekday
data_18$duration_sec <- str_replace_all(data_18$duration_sec, ",", "") %>% as.numeric()
data_18$ride_length <- round(data_18$duration_sec/60, 1)
data_18$day_of_week <- weekdays(as.Date(data_18$started_at))

##EDA
# Outlier check
data_18 %>% ggplot(aes(ride_length))+
  geom_boxplot()+
  facet_wrap(~quarter)

#remove outliers
data_18 <- data_18 %>% mutate(
  ride_length = case_when(
    quarter == "Quarter 1" & ride_length > 50000 ~ NA_integer_,
    quarter == "Quarter 2" & ride_length > 130000 ~ NA_integer_,
    quarter == "Quarter 3" & ride_length > 150000 ~ NA_integer_,
    quarter == "Quarter 4" & ride_length > 50000 ~ NA_integer_,
    TRUE ~ as.integer(ride_length)
  )
) %>% filter(!is.na(ride_length))
  
data_18_NAs <- data_18 %>% group_by(quarter) %>% 
  summarise(
    started_at_na = sum(is.na(started_at)),
    ended_at_na = sum(is.na(ended_at)),
    ride_length_na = sum(is.na(ride_length)),
    user_type_na = sum(is.na(user_type)),
  )

data_18_smr <- data_18 %>% group_by(quarter) %>% 
  summarise(
    ride_length_mean = mean(ride_length, na.rm = T),
    ride_length_min = min(ride_length, na.rm = T),
    ride_length_max = max(ride_length, na.rm = T),
    ride_length_md = median(ride_length, na.rm = T)
  )


#Descriptive analysis
weekday_smry <- data_18 %>% group_by(user_type, day_of_week) %>% 
  summarize(
    n_value = n(),
    len_min_mean = mean(ride_length, na.rm = T)
  )

quarter_smry <- data_18 %>% group_by(quarter, user_type, day_of_week) %>% 
  summarize(
    n_value = n(),
    len_min_mean = mean(ride_length, na.rm = T)
  )

##Visualizations
weekday_smry %>% ggplot(aes(x = day_of_week, y = len_min_mean, fill = user_type))+
  geom_bar(stat = "identity", position = 'dodge')+
  labs(title = "Ride length comparison", x = "Day of week", y = "Ride length average (minute)", fill = "User type")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#By quarter
quarter_smry %>% ggplot(aes(x = day_of_week, y = len_min_mean, fill = user_type))+
  geom_bar(stat = "identity", position = 'dodge')+
  labs(title = "Ride length comparison", x = "Day of week", y = "Ride length average  (minute)", fill = "User type")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  facet_wrap(~quarter)

#Taking the log of n_value because it's too long to display
weekday_smry %>% ggplot(aes(x = log(n_value), y = len_min_mean))+
  geom_point()+
  geom_smooth(method = "lm", se = F)+
  labs(title = "Ride length by number of ride", x = "Log of ride frequency", y = "Ride length average  (minute)", color = "User type")+
  facet_wrap(~user_type)


#Export
write.csv(weekday_smry, "./output/summary of ride per weekday.csv", row.names = F)
write.csv(quarter_smry, "./output/summary of ride per weekday by quarter.csv", row.names = F)

