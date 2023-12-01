#Install and open environment
library(readxl)
library(skimr)
library(tidyverse)

# save all files on separate dataframes
df_05 <- read_excel("G:/My Drive/Google Data Analytics Certificate/Module 8 Capstone/2023-cyclist_xlsx_files/202305-divvy-tripdata.xlsx")
df_06 <- read_excel("G:/My Drive/Google Data Analytics Certificate/Module 8 Capstone/2023-cyclist_xlsx_files/202306-divvy-tripdata.xlsx")
df_07 <- read_excel("G:/My Drive/Google Data Analytics Certificate/Module 8 Capstone/2023-cyclist_xlsx_files/202307-divvy-tripdata.xlsx")
df_08 <- read_excel("G:/My Drive/Google Data Analytics Certificate/Module 8 Capstone/2023-cyclist_xlsx_files/202308-divvy-tripdata.xlsx")
df_09 <- read_excel("G:/My Drive/Google Data Analytics Certificate/Module 8 Capstone/2023-cyclist_xlsx_files/202309-divvy-tripdata.xlsx")
df_10 <- read_excel("G:/My Drive/Google Data Analytics Certificate/Module 8 Capstone/2023-cyclist_xlsx_files/202310-divvy-tripdata.xlsx")

#Merge all dataframes into one and structure summary 
df_complete <- rbind(df_05, df_06, df_07, df_08, df_09, df_10)
skim_without_charts(df_complete)

#Create trip_duration column by subtracting the start trip time from the end trip time and convert to numeric
df_complete$ride_length <- difftime(df_complete$ended_at, df_complete$started_at)
df_complete$ride_length <- as.numeric(as.character(df_complete$ride_length))

#summarize and analyze df structure
skim_without_charts(df_complete)
glimpse(df_complete)
str(df_complete)
colnames(df_complete)

#data verification
table(df_complete$member_casual)
table(df_complete$rideable_type)

#create new aggregation columns such as date, month, and day
df_complete$date <- as.Date(df_complete$started_at)
df_complete$month <- format(as.Date(df_complete$started_at), format = "%m")
df_complete$day <- format(as.Date(df_complete$started_at), format = "%d")
df_complete$day_of_week <- format(as.Date(df_complete$started_at), "%A")
 
#create a new v2 df with cleaned data and removed negative erroneous values (167 records removed)
df_complete_v2 <- df_complete[!df_complete$ride_length<0,]

#descriptive analysis of data
summary(df_complete_v2$ride_length)

#comparison between members and casual riders
aggregate(df_complete_v2$ride_length ~ df_complete_v2$member_casual, FUN = mean)
aggregate(df_complete_v2$ride_length ~ df_complete_v2$member_casual, FUN = median)
aggregate(df_complete_v2$ride_length ~ df_complete_v2$member_casual, FUN = max)
aggregate(df_complete_v2$ride_length ~ df_complete_v2$member_casual, FUN = min)

#average ride time by each day for members vs casual users
aggregate(df_complete_v2$ride_length ~ df_complete_v2$member_casual + df_complete_v2$day_of_week, FUN = mean)

#order the days of the week in sequential order
df_complete_v2$day_of_week <- ordered(df_complete_v2$day_of_week, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

#average ride time by each day for members vs casual users
aggregate(df_complete_v2$ride_length ~ df_complete_v2$member_casual + df_complete_v2$day_of_week, FUN = mean)

# analyze ridership data by type and weekday
df_complete_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>%    #creates weekday field using wday()
  group_by(member_casual, weekday) %>%                    #groups by usertype and weekday
  summarise(number_of_rides = n()							            #calculates the number of rides and average duration 
            ,average_duration = mean(ride_length)) %>% 		#calculates the average duration
  arrange(member_casual, weekday)		


# alternative script, not necessary to use wday(), day_of_week already exists
df_complete_v2 %>% 
  group_by(member_casual, day_of_week) %>%                #groups by usertype and weekday
  summarise(number_of_rides = n()							            #calculates the number of rides and average duration 
            ,average_duration = mean(ride_length)) %>% 		#calculates the average duration
  arrange(member_casual, day_of_week)


# Let's visualize the number of rides by rider type
df_complete_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")

# Let's create a visualization for average duration
df_complete_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")


#output file to start visualizations
write_csv(df_complete_v2, "G:/My Drive/Google Data Analytics Certificate/Module 8 Capstone/2023-cyclist_xlsx_files/2023-cleaned-final-trip-dataset.csv")