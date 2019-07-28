install.packages("ggplot2")
install.packages("tidyverse")
library(ggplot2)
library(dplyr)

  
# Production Question 1 - What is the trip duration distribution across the 3 cities?

# Install requirement packages  
library(dplyr)
library(ggplot2)

# Import CSV files
ny = read.csv('new-york-city.csv')
chi = read.csv('chicago.csv')
wash = read.csv('washington.csv')

# Add city to each dataframe
city="New York"
q1_ny <- cbind(ny,city)

city="Washington"
q1_wh <- cbind(wash,city)

city="Chicago"
q1_ch <- cbind(chi,city)

# Only keep required variables to enable data frame stacking
q1_ny2 <- select(q1_ny,c(Trip.Duration,city))
q1_wh2 <- select(q1_wh,c(Trip.Duration,city))
q1_ch2 <- select(q1_ch,c(Trip.Duration,city))

# Create one aggregate dataframe
q1_all <- rbind(q1_ny2,q1_ch2,q1_wh2)

# Generate counts by city
table(q1_all$city)

# Summary statistics of trip duration by city in seconds
by(q1_all$Trip.Duration, q1_all$city, summary)

# Summary statistics of trip duration by city in minutes as more meaningful
by(q1_all$Trip.Duration/60, q1_all$city, summary)

#  Summary statistics of trip duration by city in hours to investigate outliers
by(q1_all$Trip.Duration/3600, q1_all$city, summary)


# Plot a histogram of trip duration in minutes by city up go 4 hours  
qplot(x = Trip.Duration/60, data = subset(q1_all, !is.na(city)), binwidth = 1) +
  scale_x_continuous(limits=c(0,240), breaks = seq(0,240,50)) +
  facet_wrap(~city) +
  labs(title="Up to 4 Hours")
 
# Plot a histogram of trip duration in minutes by city up go 2 hours  
qplot(x = Trip.Duration/60, data = subset(q1_all, !is.na(city)), binwidth = 1) +
  scale_x_continuous(limits=c(0,120), breaks = seq(0,120,20)) +
  facet_wrap(~city) +
  labs(title="Up to 2 Hours")

# Plot a histogram of trip duration in minutes by city up go 1.5 hours  
qplot(x = Trip.Duration/60, data = subset(q1_all, !is.na(city)), binwidth = 1) +
  scale_x_continuous(limits=c(0,90), breaks = seq(0,90,10)) +
  facet_wrap(~city) +
  labs(title="Up to 1.5 Hours")

# Plot a histogram of trip duration in minutes by city up go 1 hours  
qplot(x = Trip.Duration/60, data = subset(q1_all, !is.na(city)), binwidth = 1) +
  scale_x_continuous(limits=c(0,60), breaks = seq(0,60,10)) +
  facet_wrap(~city) +
  labs(title="Up to 1 Hour")

# Plot a histogram of trip duration in minutes by city up go 0.5 hours  
qplot(x = Trip.Duration/30, data = subset(q1_all, !is.na(city)), binwidth = 1) +
  scale_x_continuous(limits=c(0,30), breaks = seq(0,30,10)) +
  facet_wrap(~city) +
  labs(title="Up to 0.5 Hour")

# Investigate data further with a boxplot of trip duration in minutes by city up to 1.5 hours
ggplot(q1_all, aes(x=city,y=Trip.Duration/60)) +
  geom_boxplot() +
  ylim(0,90) +
  xlab("City") + 
  ylab("Trip Duration")

# Investigate data further with a boxplot of trip duration in minutes by city up to 1 hours
ggplot(q1_all, aes(x=city,y=Trip.Duration/60)) +
  geom_boxplot() +
  ylim(0,60) +
  xlab("City") + 
  ylab("Trip Duration")

# Investigate data further with a boxplot of trip duration in minutes by city up to 40 minutes
ggplot(q1_all, aes(x=city,y=Trip.Duration/60)) +
  geom_boxplot() +
  ylim(0,40) +
  xlab("City") + 
  ylab("Trip Duration")

# Investigate data further with a boxplot of trip duration in minutes by city up to 30 minutes
ggplot(q1_all, aes(x=city,y=Trip.Duration/60)) +
  geom_boxplot() +
  ylim(0,30) +
  xlab("City") + 
  ylab("Trip Duration")


# Create final boxplot of trip duration in minutes by city up to 30 minutes
# With average added
# Titles and colours added
ggplot(q1_all, aes(x=city,y=Trip.Duration/60)) +
  geom_boxplot(fill="skyblue", alpha=0.2) +
  stat_summary(fun.y=mean, geom="point",shape=20, size=5, color="red",fill="red") +
  ylim(0,30) +
  xlab("City") + 
  ylab("Trip Duration (Minutes)") +
  ggtitle("Trip Duration in Minutes by City")

# Production Question 2 - What are the counts of each user type across the 3 cities?
# Add city to each dataframe
city="New York"
q2_ny <- cbind(ny,city)

city="Washington"
q2_wh <- cbind(wash,city)

city="Chicago"
q2_ch <- cbind(chi,city)

# Only keep required variables to enable data frame stacking
q2_ny2 <- select(q2_ny,c(User.Type,city))
q2_wh2 <- select(q2_wh,c(User.Type,city))
q2_ch2 <- select(q2_ch,c(User.Type,city))

# Stack datasets
q2_all <- rbind(q2_ny2,q2_ch2,q2_wh2)

# Add value to enable barchart generation
q2_all2 <- mutate(q2_all,value=1)

# Generate counts by city
table(q2_all$city, q2_all$User.Type)

# Generate rough bar chart of user type counts
ggplot(q2_all, aes(x=User.Type)) +
  geom_bar()

# Generate final proportional bar chart of User Type by City
ggplot(q2_all2, aes(x=city,y=value,fill=User.Type)) +
  geom_bar(stat="identity", position="fill") +
  ggtitle("Proportional Bar Chart of User Type by City") +
  labs(x="City", y = "Proportion")

# Production Question 3 - What is the most common day of the week?
library(lubridate)

# Add city to each dataframe
city="New York"
q3_ny <- cbind(ny,city)
q3_ny2 <- mutate(q3_ny,day = wday(Start.Time, label = TRUE))

city="Washington"
q3_wh <- cbind(wash,city)
q3_wh2 <- mutate(q3_wh,day = wday(as.Date(Start.Time), label = TRUE))

city="Chicago"
q3_ch <- cbind(chi,city)
q3_ch2 <- mutate(q3_ch,day = wday(Start.Time, label = TRUE))

# Only keep required variables to enable data frame stacking
q3_ny3 <- select(q3_ny2,c(Start.Time,city,day))
q3_wh3 <- select(q3_wh2,c(Start.Time,city,day))
q3_ch3 <- select(q3_ch2,c(Start.Time,city,day))

# Create one aggregate dataframe
q3_all2 <- rbind(q3_ny3,q3_ch3,q3_wh3)

# Determine day counts by city
table(q3_all2$city, q3_all2$day)

# Determine most common day of the week overall
ggplot(q3_all2, aes(x=day)) +
  geom_bar(color="skyblue", fill="skyblue")

# Determine most common day of the week by city
ggplot(q3_all2, aes(x=day)) +
  geom_bar(color="skyblue", fill="skyblue")  +
  facet_wrap(~city) +
  labs(x="Day of Week", y = "Count") +
  ggtitle("Barchart of Number of Trips per Week Day by City")

# Conclusion: Overall across the 3 cities Wednesday has the largest number of trips with the weekend (Sat and Sun have the lowest)
# However, when looking at the individual cities this is not the uniform case. Both New York and Washington following this trend.
# But Chicago does noit. Chicago has a relatively flat,


