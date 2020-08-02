## BikeShare Project EDA
library(tidyverse) 
library(lubridate)
library(gridExtra)

## Set up and check input data sets 
# Set wd and load files
setwd("/Users/sarah/Desktop")
ch<-read.csv('chicago.csv')
ny<-read.csv('new_york_city.csv')
wa<-read.csv('washington.csv')

cities <- c('ch','ny','wa')
city.name <- c("Chicago", "New York", "Washington")

# 1: Date information - Using the start time, what is the most common month, day and start time for each city? 
# Set up holder lists for histograms
plot.m = list() 
plot.d = list() 
plot.h = list() 

# Loop through each city and return plots 
for (ii in 1:length(cities)){ 
  i=data.frame()
  i <- get(cities[ii])
  
  # Return month, day and hour 
  i <- mutate(i, mnth = month(as.Date(Start.Time)))
  i <- mutate(i, dy = day(as.Date(Start.Time)))
  i <- mutate(i, hr = hour(as.Date(Start.Time)))
  
  # Histogram month 
  plot.m[[ii]] <- ggplot(data = i, aes(x = mnth)) +
    theme_bw() +
    geom_histogram(binwidth = 1, color = 'blue') +
    scale_x_continuous(breaks = 1:6) +
    ggtitle(city.name[ii]) +
    xlab("Month") + 
    ylab("Count")
  
  # Histogram day
  plot.m[[ii]] <- ggplot(data = i, aes(x = dy)) +
    theme_bw() +
    geom_histogram(binwidth = 1, color = 'yellow') +
    scale_x_continuous(breaks = 1:31) +
    ggtitle(city.name[ii]) +
    xlab("Day") + 
    ylab("Count")
  
  # Histogram time
  plot.m[[ii]] <- ggplot(data = i, aes(x = hr)) +
    theme_bw() +
    geom_histogram(binwidth = 1, color = 'green') +
    scale_x_continuous(breaks = 1:24) +
    ggtitle(city.name[ii]) +
    xlab("Start time (24hr)") + 
    ylab("Count")
}

# Plot the histograms for each city in a single plot 
# What is the most common month?
grid.arrange(plot.m[[1]], plot.m[[2]],plot.m[[3]], ncol=3)

# What is the most common day of week?
grid.arrange(plot.d[[1]], plot.d[[2]],plot.d[[3]], ncol=3)

# What is the most common hour of day? 
grid.arrange(plot.h[[1]], plot.h[[2]],plot.h[[3]], ncol=3)

# 2. Popular stations and trip 
# Create a function to return summary stats of popular trips, including the most common start station, end station, and trip.
# For the most common trip for each city, what is the average trip duration? 

station.stats = function(city){ 
  # Most common start 
  top.start <- city %>% 
    count(Start.Station, sort = TRUE) %>% 
    head(1)
  
  # Most common end 
  top.end <- city %>% 
    count(End.Station, sort = TRUE) %>% 
    head(1) 
  
  # Most common trip and average duration
  top.trip <- city %>% 
    group_by(Start.Station, End.Station) %>%
    summarise(Count.trips = n(), Avg.time = mean(Trip.Duration)) %>% 
    arrange(desc(Count.trips)) %>% 
    head(10)

  top.trip.s <- list(top.start,top.end,top.trip)
  return(top.trip.s)
} 

# Calculate for each city 
ch.top.trip <- station.stats(ch) 
ny.top.trip <- station.stats(ny)
wa.top.trip <- station.stats(wa)

# Holder list for plots 
plot.t = list() 

paste("The most common start station is",ch.top.trip[[1]][1],"with",ch.top.trip[[1]][2],"trips") 

# What is the distribution of trip duration for the top trip? 
# Chicago 
ch.cut <- ch %>% 
  filter(Start.Station %in% ch.top.trip[[3]][1,1] & End.Station %in% ch.top.trip[[3]][1,2])

plot.t[[1]] <- ggplot(data = ch.cut, aes(y = Trip.Duration/60)) +
  theme_bw() +
  geom_boxplot() +
  ylab("Trip Duration (minutes)") +
  xlab("Top Trip") +
  ggtitle("Chicago")

# New York 
ny.cut <- ny %>% 
  filter(Start.Station %in% ny.top.trip[[3]][1,1] & End.Station %in% ny.top.trip[[3]][1,2])

plot.t[[2]] <- ggplot(data = ny.cut, aes(y = Trip.Duration/60)) +
  theme_bw() +
  geom_boxplot() +
  ylab("Trip Duration (minutes)") +
  xlab("Top Trip") +
  ggtitle("New York")

# Washington 
wa.cut <- wa %>% 
  filter(Start.Station %in% wa.top.trip[[3]][1,1] & End.Station %in% wa.top.trip[[3]][1,2])

plot.t[[3]] <- ggplot(data = wa.cut, aes(y = Trip.Duration/60)) +
  theme_bw() +
  geom_boxplot() +
  ylab("Trip Duration (minutes)") +
  xlab("Top Trip") +
  ggtitle("Washington")

# Plot as a single plot 
grid.arrange(plot.t[[1]], plot.t[[2]],plot.t[[3]], ncol=3)

# 3. User information 
# For NYC and Chicago, what is the distribution of subscribers for birth year and gender?
ch.id <- ch %>% 
  filter(User.Type %in% 'Subscriber') %>%
  group_by(Birth.Year, Gender) %>% 
  tally()

# Plot results 
ggplot(aes(x = Birth.Year, y=n), data = na.omit(ch.id)) +
  theme_minimal() +
  geom_jitter(color = 'blue') + 
  facet_wrap(~Gender) +
  scale_y_continuous(limits = c(0, 500)) +
  ylab("User count") +
  xlab("Birth year") + 
  ggtitle("Chicago User Information")
  
ny.id <- ny %>% 
  filter(User.Type %in% 'Subscriber') %>%
  group_by(Birth.Year, Gender) %>% 
  tally()

# Plot results 
ggplot(aes(x = Birth.Year, y=n), data = na.omit(ny.id)) +
  theme_minimal() +
  geom_jitter(color = 'blue') + 
  facet_wrap(~Gender) +
  scale_y_continuous(limits = c(0, 2000)) +
  ylab("User count") +
  xlab("Birth year") + 
  ggtitle("New York User Information")





