# Installing Packages

install.packages('tidyverse')
install.packages('janitor')
install.packages('lubridate')
install.packages('skimr')
install.packages('ggplot2')
install.packages('dlpry')
install.packages('readr')

# Load Packages

library(tidyverse)
library(janitor)
library(lubridate)
library(skimr)
library(ggplot2)
library(dplyr)
library(readr)


# Import the Datasets

daily_activity <- read_csv('dailyActivity_merged.csv')
daily_sleep <- read_csv('sleepDay_merged.csv')
daily_steps <- read_csv('dailySteps_merged.csv')
daily_intensities <- read_csv('dailyIntensities_merged.csv')
hourly_steps <- read_csv('hourlySteps_merged.csv')


# daily_activity

head(daily_activity) 
colnames(daily_activity) 
n_unique(daily_activity$Id) 
sum(duplicated(daily_activity)) 

# daily_sleep

head(daily_sleep) 
colnames(daily_sleep) 
n_unique(daily_sleep$Id) 
sum(duplicated(daily_sleep)) 

# daily_steps

head(daily_steps) 
colnames(daily_steps) 
n_unique(daily_steps$Id) 
sum(duplicated(daily_steps)) 

# daily_intensities

head(daily_intensities) 
colnames(daily_intensities) 
n_unique(daily_intensities$Id) 
sum(duplicated(daily_intensities)) 

# hourly_steps

head(hourly_steps) 
colnames(hourly_steps) 
n_unique(hourly_steps$Id) 
sum(duplicated(hourly_steps)) 


# Cleaning column names

clean_names(daily_activity)
daily_activity <- rename_with(daily_activity, tolower)

clean_names(daily_sleep)
daily_sleep <- rename_with(daily_sleep, tolower)

clean_names(daily_steps)
daily_steps <- rename_with(daily_steps, tolower)

clean_names(daily_intensities)
daily_intensities <- rename_with(daily_intensities, tolower)

clean_names(hourly_steps)
hourly_steps <- rename_with(hourly_steps, tolower)

# Removing duplicates

daily_sleep <- distinct(daily_sleep)

# Checking if all duplicates are removed

sum(duplicated(daily_sleep))

# Correcting consistency of dates

daily_activity <- daily_activity %>% 
  rename(date = activitydate) %>%
  mutate(date = as_date(date, format = "%m/%d/%Y"))

daily_sleep <- daily_sleep %>% 
  rename(date = sleepday) %>%
  mutate(date = as_date(date, format = "%m/%d/%Y %I:%M:%S %p"))

daily_steps <- daily_steps %>% 
  rename(date = activityday) %>%
  mutate(date = as_date(date, format = "%m/%d/%Y"))

daily_intensities <- daily_intensities %>% 
  rename(date = activityday) %>%
  mutate(date = as_date(date, format = "%m/%d/%Y"))

hourly_steps <- hourly_steps %>%
  rename(date_time = activityhour) %>%
  mutate(date_time = as.POSIXct(date_time, format = "%m/%d/%Y %I:%M:%S %p", tz = Sys.timezone()))

# Merging data

daily_activity_sleep <- merge(daily_activity, daily_sleep, by = c('id','date'))

# Adding a column for weekdays

daily_activity_sleep <- daily_activity_sleep %>%
  mutate(week_day = weekdays(date))

# Preview of Clean data

head(daily_activity)
head(daily_sleep)
head(daily_steps)
head(daily_intensities)
head(hourly_steps)
head(daily_activity_sleep)

# Calculate how often users use their devices in a month

colnames(daily_activity_sleep)

user_type <- daily_activity %>% 
  group_by(id) %>% 
  summarise(days_used = n())

user_type <- user_type %>% 
  mutate(usage = case_when(
    days_used >= 0 & days_used < 11 ~ "rarely"
    ,days_used >= 11 & days_used < 21 ~ "often"
    ,days_used >= 21 ~ "regularly"))


# Converting to percentage for easier visualization

user_type_percent <- user_type %>% 
  group_by(usage) %>% 
  summarise(total = n()) %>% 
  mutate(totals = sum(total)) %>% 
  group_by(usage) %>% 
  summarise(total_percent = total / totals) %>% 
  mutate(labels = scales::percent(total_percent))

user_type_percent$usage <- factor(user_type_percent$usage, levels = c("regularly", "very often", 
                                                                      "often", "rarely"))
# Visualizing how often users use their devices in a month

plot1 <- ggplot(user_type_percent, aes(x="", y= total_percent, fill = usage))+
  geom_bar(stat = "identity", width = 1)+
  coord_polar("y", start = 0)+
  theme_void()+
  geom_text(aes(label = labels),
            position = position_stack(vjust = 0.5))+
  scale_fill_manual(values = c("#00BFC4","#FF00FF","#00BE67"),
                    labels = c("Regularly - 22 to 31 days",
                               "Often - 11 to 21 days",
                               "Rarely - 0 to 11 days"))+
  labs(title = "Device usage in a Month")

plot1

# Calculate the time it takes for users to fall asleep

time_to_sleep <- daily_sleep %>%
  mutate(time_taken = (totaltimeinbed - totalminutesasleep)- 10)

time_to_sleep <- time_to_sleep %>%
  group_by(id) %>%
  summarise(avg_time_taken = mean(time_taken))

# Categorizing users based on amount of minutes it takes to fall asleep

time_to_sleep <- time_to_sleep %>%
  mutate(fel_asleep = case_when(
    avg_time_taken >= 0 & avg_time_taken < 15 ~ "very quickly"
    ,avg_time_taken >= 15 & avg_time_taken < 30 ~ "quickly"
    ,avg_time_taken >= 30 & avg_time_taken < 50 ~ "slowly"
    ,avg_time_taken >= 50 ~ "very slowly"))
time_to_sleep$fel_asleep <- factor(time_to_sleep$fel_asleep, levels = c("very quickly","quickly","slowly","very slowly" ))
time_to_sleep <- drop_na(time_to_sleep)

# Converting to percentages to visualize easier

time_to_sleep_percent <- time_to_sleep %>%
  group_by(fel_asleep) %>%
  summarise(total = n()) %>%
  mutate(totals = sum(total)) %>%
  group_by(fel_asleep) %>%
  summarise(total_percent = total/totals) %>%
  mutate(labels = scales::percent(total_percent))


# Visualizing time it takes for users to fall asleep

plot2 <- ggplot(time_to_sleep_percent, aes(x="", y= total_percent, fill = fel_asleep))+
  geom_bar(stat = "identity", width = 1)+
  coord_polar("y", start = 0)+
  theme_void()+
  geom_text(aes(label = labels),
            position = position_stack(vjust = 0.5))+
  scale_fill_manual(values = c("#0CB702","#00e673","#80ffbf", "#E68613"),
                    labels = c("Very quickly - 0 to 14 minutes",
                               "Quickly - 15 to 29 minutes",
                               "Slowly - 30 to 49 minutes",
                               "Very slowly - more than 50 minutes"))+
  labs(title = "Time taken to fall Asleep")

plot2

# Visualization of time spent in bed and time sleeping

plot3 <- ggplot(daily_sleep, aes(x = totalminutesasleep, y = totaltimeinbed)) + geom_point() + geom_smooth() + labs(title = "Time actually sleeping v.s Time spent in bed")
plot3

# Correlation between Steps walked and amount of sleep

daily_steps_sleep <- daily_activity_sleep %>% 
  group_by(id) %>% 
  summarise(total_steps = sum(totalsteps), total_sleepminutes = sum(totalminutesasleep))

# Visualizing Correlation between steps and sleep

plot4 <- ggplot(daily_steps_sleep, aes(x= total_steps, y= total_sleepminutes))+
  geom_point(fill = "green")+
  geom_smooth(color = "blue")+
  labs(title = "Steps walked Vs Minutes asleep", x= "Total Steps", y= "Total Sleep(minutes)")

plot4

# Which days of the week are users most active

weekday_steps_sleep <- daily_activity_sleep 

weekday_steps_sleep$week_day <- ordered(weekday_steps_sleep$week_day, levels = c("Monday", "Tuesday", "Wednesday", "Thursday",
                                                                                 "Friday", "Saturday", "Sunday"))

weekday_steps_sleep <- weekday_steps_sleep %>%
  group_by(week_day) %>%
  summarise(daily_steps = mean(totalsteps), daily_sleep = mean(totalminutesasleep))

# Visualizing which days of the week users were most active

plot5 <-  ggplot(weekday_steps_sleep) +
  geom_col(mapping = aes(week_day, daily_steps), fill = "#00A9FF")+
  labs(title = "Steps per day of the week", x= "Weekday", y= "Daily Steps")+
  geom_hline(yintercept = 7500)
plot5

# Visualizing sleep per day of the week

plot6 <-  ggplot(weekday_steps_sleep)+
  geom_col(aes(x= week_day, y= daily_sleep), fill = "#C77CFF")+
  geom_hline(yintercept = 420)+
  labs(title = "Sleep per Weekday", x= "Weekday", y= "Daily Sleep")

plot6

# Correlation between steps and calories

daily_steps_calories <- daily_activity %>% 
  group_by(id) %>% 
  summarise(total_steps = sum(totalsteps), total_calories = sum(calories))

# Visualization of the correlation between steps and calories

plot7 <- ggplot(daily_steps_calories, aes(x= total_steps, y= total_calories), fill = blue)+
  geom_point()+
  geom_smooth(color = "green")+
  labs(title = "Steps walked Vs Calories burnt", x= "Total Steps", y= "Total Calories")

plot7


# Calculate which time of day users are most active by separating date and time

hourly_steps <- hourly_steps %>%
  separate(date_time, into = c("date", "time"), sep= " ") %>%
  mutate(date = ymd(date))


# Visualizing what time of day users are most active

plot8 <- hourly_steps %>%
  group_by(time) %>%
  summarise(avg_steps = mean(steptotal)) %>%
  ggplot(aes(x= time, y= avg_steps, fill = avg_steps))+
  geom_col()+
  labs(title = "Steps walked throughout the Day", x= 'Time of the day', y= "Steps")+
  theme(axis.text.x = element_text(angle = 90))

plot8











