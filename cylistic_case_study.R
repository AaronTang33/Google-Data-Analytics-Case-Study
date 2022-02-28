#Three questions will guide the future marketing program:
#1. How do annual members and casual riders use Cyclistic bikes differently?
# 2. Why would casual riders buy Cyclistic annual memberships?
#3. How can Cyclistic use digital media to influence casual riders to become members?

setwd("/Users/aaron_ooi/Documents/R_examples/Cyclistic_Case_Study")
install.packages("tidyverse")
library(tidyverse)

q1_2020 <- read_csv(unz('./Divvy_Trips_2020_Q1.zip','Divvy_Trips_2020_Q1.csv'))
q4_2019 <- read_csv(unz('./Divvy_Trips_2019_Q4.zip','Divvy_Trips_2019_Q4.csv'))
q3_2019 <- read_csv(unz('./Divvy_Trips_2019_Q3.zip','Divvy_Trips_2019_Q3.csv'))
q2_2019 <- read_csv(unz('./Divvy_Trips_2019_Q2.zip','Divvy_Trips_2019_Q2.csv'))

str(q1_2020)
str(q4_2019)
str(q3_2019)
str(q2_2019)

#Follow column naming convention from the most recent data set (q1_2020)

(q4_2019 <- rename(q4_2019
                   ,ride_id = trip_id
                   ,rideable_type = bikeid 
                   ,started_at = start_time  
                   ,ended_at = end_time  
                   ,start_station_name = from_station_name 
                   ,start_station_id = from_station_id 
                   ,end_station_name = to_station_name 
                   ,end_station_id = to_station_id 
                   ,member_casual = usertype))

(q3_2019 <- rename(q3_2019
                   ,ride_id = trip_id
                   ,rideable_type = bikeid 
                   ,started_at = start_time  
                   ,ended_at = end_time  
                   ,start_station_name = from_station_name 
                   ,start_station_id = from_station_id 
                   ,end_station_name = to_station_name 
                   ,end_station_id = to_station_id 
                   ,member_casual = usertype))

(q2_2019 <- rename(q2_2019
                   ,ride_id = "01 - Rental Details Rental ID"
                   ,rideable_type = "01 - Rental Details Bike ID" 
                   ,started_at = "01 - Rental Details Local Start Time"  
                   ,ended_at = "01 - Rental Details Local End Time"  
                   ,start_station_name = "03 - Rental Start Station Name" 
                   ,start_station_id = "03 - Rental Start Station ID"
                   ,end_station_name = "02 - Rental End Station Name" 
                   ,end_station_id = "02 - Rental End Station ID"
                   ,member_casual = "User Type"))

str(q1_2020)
str(q4_2019)
str(q3_2019)
str(q2_2019)


#Convert all ride_id and rideable_type to character data type 
q4_2019 <-  mutate(q4_2019, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type)) 
q3_2019 <-  mutate(q3_2019, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type)) 
q2_2019 <-  mutate(q2_2019, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type)) 

#Combine all quarters into one data frame 
all_trips <- bind_rows(q2_2019, q3_2019, q4_2019, q1_2020)

#Remove columns related to gender, birthyear, lat, long 
all_trips <- all_trips %>%  
  select(-c(start_lat, start_lng, end_lat, end_lng, birthyear, gender, "05 - Member Details Member Birthday Year", "Member Gender", "01 - Rental Details Duration In Seconds Uncapped", "tripduration"))

#Inspect "all_trips" to clean and further analysis
summary(all_trips)
str(all_trips)

# In "member_casual" "member" and "Subscriber" both represent members
# and "Customer" and "casual" represent casual riders
# Standardize "member_casual" column values to fit q1_2020 nomenclature
# Replace "Subscriber" with "member"
# Replace "Customer" with "casual"
all_trips <-  all_trips %>% 
  mutate(member_casual = recode(member_casual,"Subscriber" = "member", "Customer" = "casual"))

#Ensure all values for member_casual were properly assigned 
table(all_trips$member_casual)


#Create columns for data, year, month and day of every trip start time*
all_trips$date <- as.Date(all_trips$started_at) #The default format is yyyy-mm-dd
all_trips$month <- format(as.Date(all_trips$date), "%m")
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$day_name <- format(as.Date(all_trips$date), "%a")
all_trips$month_name <- format(as.Date(all_trips$date), "%b")

#Fix the order of the days
all_trips$day_name <- ordered(all_trips$day_name, levels=c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"))

#Fix the order of the months
all_trips$month_name <- ordered(all_trips$month_name, levels=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

#Add "ride_length" column (in seconds)
all_trips$ride_length <- difftime(all_trips$ended_at,all_trips$started_at, units="secs")

#Inspect data types of columns 
str(all_trips)

#Convert "ride_length" type from difftime to numeric
is.numeric(all_trips$ride_length)
all_trips$ride_length <- as.numeric(all_trips$ride_length)
is.numeric(all_trips$ride_length)

#Inspect "ride_length", "start_station_name" and "end_station_name"
#Check for "ride_length" that is less than or equal to zero.  
#"ride_length" values less than or equal to zero are invalid and will be removed.  
range(all_trips$ride_length)

#Check for anomalies in start_station_name and end_station_name  
unique(sort(all_trips$start_station_name))
unique(sort(all_trips$end_station_name))
  
#Findings
#- Values in "ride_length" less than or equal to zero was found
#- Found several station names in "start_station_name" and "end_station_name" that represented repairs, tests, or special events done by Cyclistic.
#For example, "DIVVY CASSETTE REPAIR MOBILE STATION", "DIVVY Map Frame B/C Station" and more.
#- Some station names include "(\*)" and "(Temp)" at the end of the station names.
#At times, the station names are repeated but with "(\*)" or "(Temp)" at the end of the station name.
#For example, "Sangamon St & Washington Blvd" and "Sangamon St & Washington Blvd (\*)"  

#Remove all instances where "start_station_name" or "end_station_name" contains a value that indicates repairs, 
#tests or special events done by Cyclistic and "ride_length" values less than or equal to zero

#Create a new data frame since data is being removed
all_trips_v2 <- all_trips[!(all_trips$start_station_name == "HQ QR" | all_trips$end_station_name == "HQ QR"| 
                              all_trips$start_station_name == "DIVVY CASSETTE REPAIR MOBILE STATION" | all_trips$end_station_name == "DIVVY CASSETTE REPAIR MOBILE STATION"|
                              all_trips$start_station_name == "DIVVY Map Frame B/C Station" | all_trips$end_station_name == "DIVVY Map Frame B/C Station"| 
                              all_trips$start_station_name == "HUBBARD ST BIKE CHECKING (LBS-WH-TEST)" | all_trips$end_station_name == "HUBBARD ST BIKE CHECKING (LBS-WH-TEST)"| 
                              all_trips$start_station_name == "LBS - BBB La Magie" | all_trips$end_station_name == "LBS - BBB La Magie"| 
                              all_trips$start_station_name == "MTL-ECO5.1-01" | all_trips$end_station_name == "MTL-ECO5.1-01"| 
                              all_trips$start_station_name == "Special Events" | all_trips$end_station_name == "Special Events"| 
                              all_trips$ride_length<=0),]

#Remove substrings " (\*)" and " (Temp)" in all station names
all_trips_v2$start_station_name = gsub(" (*)","",all_trips_v2$start_station_name, fixed = TRUE)
all_trips_v2$start_station_name = gsub(" (Temp)","", all_trips_v2$start_station_name, fixed = TRUE)
all_trips_v2$end_station_name = gsub(" (*)","",all_trips_v2$end_station_name, fixed = TRUE)
all_trips_v2$end_station_name = gsub(" (Temp)","", all_trips_v2$end_station_name, fixed = TRUE)

#Check for missing values
sum(is.na(all_trips_v2)) 

#Analyze

#Compare Min, Max, Median, Mean
#Min
aggregate(ride_length ~ member_casual, data = all_trips_v2 ,FUN = min)
#Max
aggregate(ride_length ~ member_casual, data = all_trips_v2 ,FUN = max)
#Median
aggregate(ride_length ~ member_casual, data = all_trips_v2 ,FUN = median)
#Mean
aggregate(ride_length ~ member_casual, data = all_trips_v2 ,FUN = mean)

#Compare average ride times between casual and member riders
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_name, data = all_trips_v2, FUN = mean)

#Compare "number_of_rides" and "average_duration" type of rider and day of the week
all_trips_v2 %>% 
  group_by(member_casual, day_name) %>%  
  summarise(number_of_rides = n()	 
            ,average_duration = mean(ride_length)) 

#Visualize 
#Total number of riders by Month and Rider Type
all_trips_v2 %>% 
  group_by(member_casual, month_name) %>%
  summarise(number_of_rides = n()) %>%
  ggplot(aes(x = month_name, y = number_of_rides/1000, fill = member_casual)) + geom_col(position = "dodge") +
  labs(title = "Total number of riders by Month and Rider Type", fill = "Rider Type", x = "Month", y = "Number of Rides (1000s)") +
  theme(axis.text.x = element_text(angle = 90, size = 8)) +
  scale_fill_manual(values=c("#005FCF","#008B71"))


#Total number of riders by Day of Week and Rider Type
all_trips_v2 %>% 
  group_by(member_casual, day_name) %>%
  summarise(number_of_rides = n()) %>%
  ggplot(aes(x = day_name, y = number_of_rides/10000, fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(title = "Total number of riders by Day of Week and Rider Type", fill = "Rider Type", x = "Day of the Week", y = "Number of Rides (1000s)") +
  theme(axis.text.x = element_text(angle = 90, size = 8)) +
  scale_fill_manual(values=c("#005FCF","#008B71"))


#Total number of riders by Hour and Rider Type
all_trips_v2 %>% 
  mutate(hour_started_at = format(as.POSIXct(started_at), format = "%H")) %>%
  group_by(member_casual, hour_started_at) %>%
  summarise(number_of_rides = n()) %>%
  ggplot(aes(x = hour_started_at, y = number_of_rides/1000, fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(title = "Total number of riders by Hour and Rider Type", fill = "Rider Type", x = "Hour of the Day (24-Hour Format)", y = "Number of Rides (1000s)") +
  theme(axis.text.x = element_text(angle = 90, size = 8)) +
  scale_fill_manual(values=c("#005FCF","#008B71"))

#Average ride length by Day of Week and Rider Type
all_trips_v2 %>% 
  group_by(member_casual, day_name) %>%
  summarise(average_duration = mean(ride_length)) %>%
  ggplot(aes(x = day_name, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(title = "Average ride length by Day of Week and Rider Type", fill = "Rider Type", x = "Day of the Week", y = "Average Ride Length (Seconds)") +
  theme(axis.text.x = element_text(angle = 45)) +
  scale_fill_manual(values=c("#005FCF","#008B71"))

#Determine the Top 5 most popular start stations for "member" riders (Most -> Least)
#"Canal St & Adams St", "Clinton St & Madison St", "Clinton St & Washington Blvd", "Columbus Dr & Randolph St", "Franklin St & Monroe St"
sort(table(subset(all_trips_v2, member_casual=="member")$start_station_name), decreasing = TRUE)[1:5] 

#Determine the Top 5 most popular start stations for "casual" riders (Most -> Least)
#"Streeter Dr & Grand Ave", "Lake Shore Dr & Monroe St", "Millennium Park", "Michigan Ave & Oak St", "Shedd Aquarium"
sort(table(subset(all_trips_v2, member_casual=="casual")$start_station_name), decreasing = TRUE)[1:5] 


#Most popular start stations for "member" and "casual" riders
all_trips_v2 %>% 
  group_by(member_casual, start_station_name) %>%
  summarise(number_of_rides = n()) %>%
  ggplot(aes(x = start_station_name, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge") +
  scale_x_discrete(limits=c("Canal St & Adams St", "Clinton St & Madison St", "Clinton St & Washington Blvd", "Columbus Dr & Randolph St", "Franklin St & Monroe St", "Streeter Dr & Grand Ave", "Lake Shore Dr & Monroe St", "Millennium Park", "Michigan Ave & Oak St", "Shedd Aquarium"))+ 
  labs(title = "Most popular start stations for \"member\" and \"casual\" riders", fill = "Rider Type", x = "Station Name", y = "Number of Rides") +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_fill_manual(values=c("#005FCF","#008B71"))

##Determine the Top 5 most end popular stations for member riders (Most -> Least)
#Canal St & Adams St, Clinton St & Washington Blvd, Clinton St & Madison St, Daley Center Plaza, Kingsbury St & Kinzie St 
sort(table(subset(all_trips_v2, member_casual=="member")$end_station_name), decreasing = TRUE)[1:5] 

##Determine the Top 5 most end popular stations for casual riders (Most -> Least)
#"Streeter Dr & Grand Ave", "Lake Shore Dr & Monroe St", "Millennium Park", "Michigan Ave & Oak St", "Lake Shore & North Blvd"
sort(table(subset(all_trips_v2, member_casual=="casual")$end_station_name), decreasing = TRUE)[1:5] 


#Top 5 most popular stations for member riders and Top 5 most popular stations for casual riders comparison
all_trips_v2 %>% 
  group_by(member_casual, end_station_name) %>%
  summarise(number_of_rides = n()) %>%
  ggplot(aes(x = end_station_name, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge") +
  scale_x_discrete(limits=c("Canal St & Adams St", "Clinton St & Washington Blvd", "Clinton St & Madison St", "Daley Center Plaza", "Kingsbury St & Kinzie St", "Streeter Dr & Grand Ave", "Lake Shore Dr & Monroe St", "Millennium Park", "Michigan Ave & Oak St", "Lake Shore Dr & North Blvd")) + 
  labs(title = "Most popular end stations for \"member\" and \"casual\" riders", fill = "Rider Type", x = "Station Name", y = "Number of Rides") +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_fill_manual(values=c("#005FCF","#008B71"))
