df_Q1 <- read.csv("Divvy_Trips_2019_Q1.csv")
df_Q2 <- read.csv("Divvy_Trips_2019_Q2.csv")
df_Q3 <- read.csv("Divvy_Trips_2019_Q3.csv")
df_Q4 <- read.csv("Divvy_Trips_2019_Q4.csv")
head(df_Q1)
colnames(df_Q1)
View(df_Q1)

for(i in 1:ncol(df_Q1)){
  cat(sprintf("%s : %d\n", colnames(df_Q1)[i], sum(is.na(df_Q1[,i]))))
}
sum(is.na(df_Q1$to_station_id))
#needs to clean df_Q2 column names cause its inconsistent.
#df_Q2_cleaned <- df_Q2
#colnames(df_Q2_cleaned) <- colnames(df_Q1)
#head(df_Q2_cleaned)
colnames(df_Q2) <- colnames(df_Q1)
#head(df_Q2_cleaned)

merged <- rbind(df_Q1, df_Q2, df_Q3, df_Q4)
head(merged)

for(i in 1:ncol(merged)){
  cat(sprintf("%s : %d\n", colnames(merged)[i], sum(is.na(merged[,i]))))
}

merged$gender[merged$gender==""] <- "Not Given"

levels(merged$gender) <- c(levels(merged$gender), "Not Given")
str(merged$gender)



#get trip length in seconds USELESS, already exists trip_duration
print(as.numeric(as.POSIXct(merged$end_time[1]))- as.numeric(as.POSIXct(merged$start_time[1])))

merged$day <- format(as.Date(merged$start_time,format="%Y-%m-%d"), format = "%u")
merged$day_of_week <- format(as.Date(merged$start_time,format="%Y-%m-%d"), format = "%A")
merged$month <- format(as.Date(merged$start_time,format="%Y-%m-%d"), format = "%m")
merged$year <- format(as.Date(merged$start_time,format="%Y-%m-%d"), format = "%Y")

summary(merged)

library(stringr)
merged$tripduration <- str_replace_all(merged$tripduration, ",", "")
merged$tripduration <- as.double(merged$tripduration)


aggregate(merged$tripduration ~ merged$usertype, FUN = mean)
aggregate(merged$tripduration ~ merged$usertype, FUN = median)
aggregate(merged$tripduration ~ merged$usertype, FUN = max)
aggregate(merged$tripduration ~ merged$usertype, FUN = min)

aggregate(merged$tripduration ~ merged$usertype + merged$day_of_week, FUN = mean)


merged$day_of_week <- ordered(merged$day_of_week, levels=c("domenica", "lunedì", "martedì", "mercoledì", "giovedì", "venerdì", "sabato"))


library("dplyr")
library("lubridate")
# analyze ridership data by type and weekday
merged %>% 
  mutate(weekday = wday(start_time, label = TRUE)) %>%  #creates weekday field using wday()
  group_by(usertype, weekday) %>%  #groups by usertype and weekday
  summarise(number_of_rides = n()							#calculates the number of rides and average duration 
            ,average_duration = mean(tripduration)) %>% 		# calculates the average duration
  arrange(usertype, weekday)			

#day of week
library("ggplot2")
merged %>%
  mutate(weekday = wday(start_time, label = TRUE)) %>% 
  group_by(usertype, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(tripduration)) %>% 
  arrange(usertype, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = usertype)) +
  geom_col(position = "dodge")

#months
library("ggplot2")
merged %>%
  mutate(month_name = month(start_time, label = TRUE)) %>% 
  group_by(usertype, month_name) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(tripduration)) %>% 
  arrange(usertype, month_name)  %>% 
  ggplot(aes(x = month_name, y = number_of_rides, fill = usertype)) +
  geom_col(position = "dodge")


file_to_save <- aggregate(merged$tripduration ~ merged$usertype + merged$day_of_week, FUN = mean)
write.csv(file_to_save, file = 'avg_ride_length.csv')
write.csv(merged, file="merged.csv")




