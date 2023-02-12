install.packages("tidyverse")
library(tidyverse)
install.packages("ggthemes")
library(ggplot2)
library(ggthemes)
library(lubridate)
library(dplyr)
library(tidyr)
library(DT)
library(scales)


colors <- c("red", "green", "blue", "pink", "cyan", "yellow", "#ffa600") # nolint
apr_data <- read.csv("uber-raw-data-apr14.csv")
may_data <- read.csv("uber-raw-data-may14.csv")
jun_data <- read.csv("uber-raw-data-jun14.csv")
jul_data <- read.csv("uber-raw-data-jul14.csv")
aug_data <- read.csv("uber-raw-data-aug14.csv")
sep_data <- read.csv("uber-raw-data-sep14.csv")

data_2014 <- rbind(apr_data, may_data, jun_data, jul_data, aug_data, sep_data)

#DATAFORMATTING
data_2014$Date.Time <- as.POSIXct(data_2014$Date.Time, format = "%m/%d/%Y %H:%M:%S") # nolint
data_2014$Time <- format(as.POSIXct(data_2014$Date.Time, format = "%m/%d/%Y %H:%M:%S"), format = "%H:%M:%S") #nolint
data_2014$Date.Time <- ymd_hms(data_2014$Date.Time)
data_2014$day <- factor(day(data_2014$Date.Time))
data_2014$month <- factor(month(data_2014$Date.Time, label = TRUE))
data_2014$year <- factor(year(data_2014$Date.Time))
data_2014$dayofweek <- factor(wday(data_2014$Date.Time, label = TRUE))

data_2014$hour <- factor(hour(hms(data_2014$Time)))
data_2014$minute <- factor(minute(hms(data_2014$Time)))
data_2014$second <- factor(second(hms(data_2014$Time)))

#GRAPH PLOT FOR TRIPS/HOUR
hour_data <- data_2014 %>% group_by(hour) %>% dplyr::summarise(Total = n())
ggplot(hour_data, aes(hour, Total)) +
    geom_bar(stat = "identity", fill = "yellow", color = "red") +
    ggtitle("TRIPS BY HOUR") +
    theme(legend.position = "none")
    scale_y_continuous(labels = comma)
# ggsave("graphplot.png") nolint


#GRAPH PLOT FOR HOUR/MONTH
month_hour <- data_2014 %>% group_by(month,hour) %>% dplyr::summarize(Total = n()) #nolint
ggplot(month_hour, aes(hour, Total, fill = month)) +
geom_bar(stat = "identity") +
ggtitle("Trips by hour and month") +
scale_y_continuous(label = comma)
# ggsave("Trips by hour and month.png") nolint


#GRAPH PLOT FOR TRIPS BY DAY
day_group <- data_2014 %>% group_by(day) %>% dplyr::summarize(Total = n())
datatable(day_group)
ggplot(day_group, aes(day, Total)) +
geom_bar(stat = "identity", fill = "#00c3ff") +
ggtitle("TRIPS BY DAY ") +
scale_y_continuous(labels = comma)
# ggsave("TRIPS BY DAY.png") nolint


#GRAPH PLOT FOR TRIPS BY MONTH
month_group <- data_2014  %>% group_by(month) %>% dplyr::summarize(Total = n())
# datatable(month_group) NOLINT 
ggplot(month_group, aes(month, Total, fill = month)) +
geom_bar(stat = "identity") +
ggtitle("Trips by month") +
theme(legend.position = "none") +
scale_y_continuous(labels = comma) +
scale_fill_manual(values = colors)
# ggsave("Trips by month.png") nolint 


#BAR PLOT BY DAY AND MONTH
month_weekday <- data_2014 %>% group_by(month, dayofweek) %>% dplyr::summarise(Total = n()) #nolint 
ggplot(month_weekday, aes(month, Total, fill = dayofweek)) +
geom_bar(stat = "identity", position = "dodge") +
ggtitle("Trips by day and month") +
scale_y_continuous(labels = comma) +
scale_fill_manual(values = colors)
# ggsave("Trips by day and month.png") nolint


#TRIPS BY BASES
ggplot(data_2014, aes(Base)) +
geom_bar(fill = "blue") +
scale_y_continuous(labels = comma) +
ggtitle("Trips by bases")


#TRIPS BY BASES AND MONTH
ggplot(data_2014, aes(Base, fill = month)) +
geom_bar(position = "dodge") +
scale_y_continuous(labels = comma) +
ggtitle("trips by base and month") +
scale_fill_manual(values = colors)
# ggsave("trips by base and month.png") nolint


#TRIPS BY BASE AND DAY OF WEEK
ggplot(data_2014, aes(Base, fill = dayofweek)) +
geom_bar(position = "dodge") +
ggtitle("trips by base and day of week") +
scale_y_continuous(labels = comma) +
scale_fill_manual(values = colors)
# ggsave("trips by base and day of week.png") nolint


#HEAT MAP BY HOUR AND DAY
day_hour <- data_2014 %>% group_by(day, hour) %>% dplyr::summarise(Total = n())
# datatable(day_hour) NO LINT 
ggplot(day_hour, aes(day, hour, fill = Total)) +
geom_tile(color = "white") +
ggtitle("heat map by hour and day")
# ggsave("Heat map by hour and day.png") nolint 


#HEAT MAP BY MONTH AND DAY
ggplot(day_month_group, aes(day, month, fill = Total))+
geom_tile(color = "white") +
ggtitle("heat map by month and day")
# ggsave("heat map by month and day.png") no lint 


#HEAT MAP BY MONTH AND DAY OF WEEK
ggplot(month_weekday, aes(dayofweek, month, fill = Total)) +
geom_tile(color = "white") +
ggtitle("Heat Map by month and day of week")
# ggsave("heat map by month and day of week.png") nolint 


#HEAT MAP BY MONTH AND BASE 
month_base <- data_2014 %>% group_by(Base, month) %>% dplyr::summarise(Total = n()) #nolint 
# options(scipen = 999) nolint 
ggplot(month_base, aes(Base, month, fill = Total)) +
geom_tile(color = "white") +
ggtitle("Heat map month and bases")
# ggsave("heat map by month and bases.png") nolint


#HEAT MAP BY DAY OF WEEK AND BASE
base_weekday <- data_2014 %>% group_by(Base, dayofweek) %>% dplyr::summarise(Total = n()) #nolint 
ggplot(base_weekday, aes(Base, dayofweek, fill = Total)) +
geom_tile(color = "white") +
ggtitle("Heat Map by day of week and Base")
# ggsave("Heat map by day of week and base.png") nolint



#PLOT FOR NYC MAP BASED ON UBER RIDES
min_lat <- 40.5774
max_lat <- 40.9176
min_long <- -74.15
max_long <- -73.7004
ggplot(data_2014, aes(x=Lon, y=Lat)) +
  geom_point(size=1, color = "blue") +
     scale_x_continuous(limits=c(min_long, max_long)) +
      scale_y_continuous(limits=c(min_lat, max_lat)) +
        theme_map() +
           ggtitle("NYC MAP BASED ON UBER RIDES DURING 2014 (APR-SEP)")



#NYC MAP BASED ON UBER RIDES BY BASES 
min_lat <- 40.5774
max_lat <- 40.9176
min_long <- -74.15
max_long <- -73.7004
ggplot(data_2014, aes(x=Lon, y=Lat, color = Base)) +
  geom_point(size=1) +
     scale_x_continuous(limits=c(min_long, max_long)) +
      scale_y_continuous(limits=c(min_lat, max_lat)) +
       theme_map() +
          ggtitle("NYC MAP BASED ON UBER RIDES DURING 2014 (APR-SEP) by BASE")