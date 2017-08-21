library(RCurl)
library(XML)

get_data.f <- function(l){
  folder <- './raws'
  dir.create(file.path(folder), showWarnings = FALSE)
  for(i in 1:length(links)){
    x <- links[i]
    filename <- paste(folder, strsplit(x, '/')[[1]][5], sep = '/')
    download.file(url = x,destfile = filename, method = "wget")
    unzip(filename, exdir = "./raws")
    file.remove(filename)
  }
}

script <- getURL('https://s3.amazonaws.com/tripdata/index.html')
doc <- htmlParse(script)
links <- getHTMLLinks(doc)
links <- links[grepl('zip', links)]
lapply(links, get_data.f)

dir.create(file.path('./data'), showWarnings = FALSE)
db <- list.files('raws')
for(i in 1:length(db)){
  n <- db[i]
  df <- fread(n)
  n <- strsplit(n, '[.]')[[1]][1]
}
setnames(df, c('duration', 'starttime', 'stoptime', 'startid', 'startname', 'startlat', 'startlon', 'endid', 'endname', 'endlat', 'endlon', 'bikeid', 'usertype', 'birth', 'gender'))

library(reshape2)
library(plyr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(ggthemes)
library(ggmap)
library(leaflet)
library(shiny)
library(googleVis)
library(data.table)
library(scales)
library(plotly)

#setwd("H:/RProgramming/citibike/citishiny")
#getwd()
#rawdf<- read.csv(file = "H:/RProgramming/citibike/citishiny/201707-citibike-tripdata.csv",header=TRUE, stringsAsFactors=FALSE)
#setnames(rawdf, c('duration', 'starttime', 'stoptime', 'startid', 'startname', 'startlat', 'startlon', 'endid', 'endname', 'endlat', 'endlon', 'bikeid', 'usertype', 'birth', 'gender'))
#head(rawdf)


setwd("C:/citibike-analysis")
rawdf<- read.csv(file = "C:/citibike-analysis/201707-citibike-tripdata.csv",header=TRUE, stringsAsFactors=FALSE)
setnames(rawdf, c('duration', 'starttime', 'stoptime', 'startid', 'startname', 'startlat', 'startlon', 'endid', 'endname', 'endlat', 'endlon', 'bikeid', 'usertype', 'birth', 'gender'))
head(rawdf)






## Data Analysis

# 1- Identify variables' type
str(rawdf)

# 2- Find missing values in dataset
# NA values:
summary(rawdf)
# only the column birth.year and gender (expressed as 0) have missing values. These variables are relevant for the first part of our analysis but irrelevant to the second part. 
# We'll remove the observations with missing values for the first part of our analysis.
# We'll keep all the observations (and ignore the missing values) in the second part of our analysis.
# the starttime and stoptime columns are treated as characters so we can't detect any potential missing values. We'll convert them.

# 3- Convert & combine/split values
# a- split date and time in the starttime and endtime columns
df = separate(rawdf, starttime, into=c("startday","starttime"), sep=" ")
df = separate(df, stoptime, into=c("stopday","stoptime"), sep=" ")

# b- convert dates to proper format
df$startday = as.Date(df$startday, format = '%Y-%m-%d')
df$stopday = as.Date(df$stopday, format = '%Y-%m-%d')

# c- check for missing values in the time and date columns
summary(df)
# there is no unusual dates.

# e- extract month and day from dates (month and day are similar for start and stop date so only grabbing the values from startday)
df$month = as.numeric(format(df$startday, format = "%m"))
df$day = as.numeric(format(df$startday, format = "%d"))
df$monthname = month.abb[df$month]
df$dayofweek = weekdays(as.Date(df$startday))

df = as.data.frame(df)
names(df)[names(df)=="start station id"] = "start.station.id"
names(df)[names(df)=="start station name"] = "start.station.name"
names(df)[names(df)=="start station latitude"] = "start.station.latitude"
names(df)[names(df)=="start station longitude"] = "start.station.longitude"
names(df)[names(df)=="end station id"] = "end.station.id"
names(df)[names(df)=="end station name"] = "end.station.name"
names(df)[names(df)=="end station latitude"] = "end.station.latitude"
names(df)[names(df)=="end station longitude"] = "end.station.longitude"
names(df)[names(df)=="birth year"] = "birth.year"


# f- convert start and stop times to proper format and group by intervals of 15 mins 
df$starttime = as.POSIXlt(round(as.double(strptime(df$starttime, '%H:%M:%S'))/(15*60))*(15*60),origin=(as.POSIXlt('1970-01-01')))
df$starttime = format(df$starttime,"%H:%M")

df$stoptime = as.POSIXlt(round(as.double(strptime(df$stoptime, '%H:%M:%S'))/(15*60))*(15*60),origin=(as.POSIXlt('1970-01-01')))
df$stoptime = format(df$stoptime,"%H:%M")

# add a column where we group the start and stop time by hour
tStart = strptime(paste("2001-01-01", df$starttime), format="%Y-%m-%d %H:%M")
df$starthour = format(round(tStart), format="%H:%M")

tStop = strptime(paste("2001-01-01", df$stoptime), format="%Y-%m-%d %H:%M")
df$stophour = format(round(tStop), format="%H:%M")


# g- convert the time duration to minutes (instead of seconds) to make it more user friendly. 
df$tripduration.min = round(as.numeric(df$duration)/60)

# h- add a column with time duration grouped by chunks of 10 min.
df$tripduration.mingroup = 10*floor((df$tripduration.min + 9)/10)

# i- convert the gender values to gender names (i.e. 1 = male, 2 = female)
df$gender = ifelse(df$gender %in% '0', NA, ifelse(df$gender %in% '1', 'male','female'))

# j- convert the birth.year to age
df$age = as.integer(format(Sys.Date(), "%Y")) - as.integer(df$birth)

# k- create age groups
df$agegroup = cut(df$age, 
                  breaks = c(17, 25, 35, 45, 55, 65, 75, Inf), 
                  labels = c("17-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75 or older"), 
                  right = FALSE)

########## Handle missing values
row.has.na <- apply(df, 1, function(x){any(is.na(x))})
sum(row.has.na)
dim(df)
df <- df[!row.has.na,]
dim(df)


# l- create group for start and stop hour
df$starthour = strtrim(df$starthour, 2)
df$starthour = as.numeric(df$starthour)
df$startrange = cut(df$starthour, 
                    breaks = c(00, 04, 08, 12, 16, 20, 24), 
                    labels = c("before 4am", "4am-8am", "8am-12pm", "12pm-4pm", "4pm-8pm", "after 8pm"), 
                    right = FALSE)

df$stophour = strtrim(df$stophour, 2)
df$stophour = as.numeric(df$stophour)
df$stoprange = cut(df$stophour, 
                   breaks = c(00, 04, 08, 12, 16, 20, 24), 
                   labels = c("before 4am", "4am-8am", "8am-12pm", "12pm-4pm", "4pm-8pm", "after 8pm"), 
                   right = FALSE)

# m - convert latitude and longitude to numeric
df$start.station.longitude = as.numeric(df$startlon)
df$end.station.longitude = as.numeric(df$endlon)
df$start.station.latitude = as.numeric(df$startlat)
df$end.station.latitude = as.numeric(df$endlat)

## 4- Understand distribution of numerical values and detect outliers
# outliers in trip duration
df$dayofweek = factor(df$dayofweek, levels = c("Sunday", "Saturday", "Friday", "Thursday", "Wednesday", "Tuesday", "Monday"))
library(ggplot2)
ggplot(data = df, aes(x=dayofweek, y= tripduration.min)) + 
  geom_boxplot(outlier.colour = "red", alpha = 0.1) + 
  labs(x = "day of the week", y = "trip duration (min)", title = 'trip duration: identifying outliers') +
  coord_flip() + 
  theme_pander()

#df$dayofweek = factor(df$dayofweek, levels = c("Sunday", "Saturday", "Friday", "Thursday", "Wednesday", "Tuesday", "Monday"))

ggplot(data = df, aes(x=dayofweek, y= tripduration.min)) +
  geom_boxplot(outlier.colour = "dodgerblue3", alpha = 0.1) +
  labs(x = "", y = "", title = 'Identifying Outliers : Trip Duration (minutes)') +
  coord_flip(ylim=c(0,60)) + 
  theme_pander() +
  theme(axis.line=element_blank(),
        axis.ticks=element_blank(),
        legend.title = element_blank(),
        plot.title = element_text(size = 22, 
                                  face = 'bold',
                                  color = 'grey28',
                                  margin = margin(10,0,10,0),
                                  family = 'Helvetica',
                                  hjust = 0.25,
                                  vjust = 5))

#median duration time per age and gender
duration_table = df %>% 
  select(gender, agegroup, tripduration.min) %>% 
  group_by(gender, agegroup) %>% 
  mutate(Median = round(median(tripduration.min),1))

# Shows the minimum tripduration by gender
dcast(duration_table, agegroup ~ gender)


# ggplot(data = df, aes(x=tripduration.min, fill = gender, color = gender)) + 
#        geom_density() + 
#        labs(x = "Trip duration (min)", y = "", title = '') +
#        coord_cartesian(xlim=c(0,120)) +
#        theme_pander() +
#        theme(axis.line=element_blank(),
#              axis.text.y=element_blank(),
#              axis.ticks=element_blank(),
#              legend.title = element_blank(),
#              strip.text.x = element_blank()) +
#        facet_grid(agegroup~gender)

density = ggplot(data = df, aes(x=tripduration.min, fill = gender, color = gender)) + 
  geom_density(alpha = 0.1) + 
  labs(x = "", y = "", title = 'Trip Duration (minutes) per Age Group') +
  coord_cartesian(xlim=c(0,90)) +
  theme_pander() +
  theme(axis.line=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        legend.title = element_blank(),
        plot.title = element_text(size = 22, 
                                  face = 'bold',
                                  color = 'grey28',
                                  margin = margin(10,0,10,0),
                                  family = 'Helvetica',
                                  hjust = 0.025)) +
  facet_wrap(~agegroup, strip.position = 'left')
density

# there are a lot of outliers. We'll keep only the data for trips of 120 min or under.

# outlier in bikeid. We're checking if bikeid is occuring too many times in the table which could signal potential anomaly
table(df$bikeid) # no anomaly

# outliers in age distribution
histo = ggplot(data = df, aes(x=agegroup, fill= gender)) + 
  geom_histogram(stat='count') + 
  labs(x = "", y = "Num of trips", title = 'Num of riders per age group') +
  theme_pander() +
  theme(axis.line=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        plot.title = element_text(size = 22, 
                                  face = 'bold',
                                  color = 'grey28',
                                  margin = margin(10,0,10,0),
                                  family = 'Helvetica',
                                  hjust = 0.025),
        legend.title = element_blank(),
        strip.text.x = element_blank()) +
  facet_grid(~gender)
histo

# ggplotly(histo) %>% config(displayModeBar = F)

# there are a few outliers (over 100 years old), which I will remove

# analyse citibike trip by hour of day
weekdays = df %>% 
  dplyr::filter(!dayofweek %in% c('Saturday', 'Sunday')) %>% 
  dplyr::group_by(starthour) %>% 
  dplyr::summarise(count = n())
  
  
ggplot(data = weekdays, aes(x=starthour, y = count)) + 
  geom_histogram(stat = 'identity', color = "lightblue", fill = "lightblue") +
  labs(x = "", y = "", title = 'No of riders at rush hours (Weekdays)') +
  theme_pander() +
  theme(axis.line=element_blank(),
        axis.ticks=element_blank(),
        plot.title = element_text(size = 18, 
                                  face = 'bold',
                                  color = 'grey28',
                                  margin = margin(10,0,10,0),
                                  family = 'Helvetica',
                                  hjust = 0.025),
        legend.title = element_blank(),
        strip.text.x = element_blank()) +
  scale_x_continuous(breaks = seq(min(weekdays$starthour), 
                                  max(weekdays$starthour), by = 4))

weekends = df %>% 
  filter(dayofweek %in% c('Saturday', 'Sunday')) %>% 
  group_by(starthour) %>% 
  dplyr::summarise(count = n())

ggplot(data = weekends, aes(x=starthour, y = count)) + 
  geom_histogram(stat = 'identity', color = "dodgerblue3", fill = "dodgerblue3") +
  labs(x = "", y = "", title = 'No of riders at by hour (Weekends)') +
  theme_pander() +
  theme(axis.line=element_blank(),
        axis.ticks=element_blank(),
        plot.title = element_text(size = 18, 
                                  face = 'bold',
                                  color = 'grey28',
                                  margin = margin(10,0,10,0),
                                  family = 'Helvetica',
                                  hjust = 0.025),
        legend.title = element_blank(),
        strip.text.x = element_blank()) +
  scale_x_continuous(breaks = seq(min(weekends$starthour), 
                                  max(weekends$starthour), by = 4))

# split by age group
weekdays_agegroup = df %>% 
  filter(!dayofweek %in% c('Saturday', 'Sunday')) %>% 
  group_by(starthour, agegroup) %>% 
  dplyr::summarise(count = n())

ggplot(data = weekdays_agegroup, aes(x=starthour, y = count, fill = agegroup)) + 
  geom_histogram(stat = 'identity', position = 'fill') +
  labs(x = "", y = "", title = 'No of riders per Hour of the Day (Weekdays)') +
  scale_fill_economist() +
  theme_pander() +
  theme(axis.line=element_blank(),
        axis.ticks=element_blank(),
        plot.title = element_text(size = 18, 
                                  face = 'bold',
                                  color = 'grey28',
                                  margin = margin(10,0,10,0),
                                  family = 'Helvetica',
                                  hjust = 0.025),
        legend.title = element_blank(),
        strip.text.x = element_blank()) +
  scale_x_continuous(breaks = seq(min(weekdays_agegroup$starthour), 
                                  max(weekdays_agegroup$starthour), by = 4)) 


weekdends_agegroup = df %>% 
  filter(dayofweek %in% c('Saturday', 'Sunday')) %>% 
  group_by(starthour, agegroup) %>% 
  dplyr::summarise(count = n())

ggplot(data = weekdends_agegroup, aes(x=starthour, y = count, fill = agegroup)) + 
  geom_histogram(stat = 'identity', position = 'fill') +
  labs(x = "", y = "", title = '# of Riders per Hour of the Day (Weekends)') +
  theme_pander() +
  theme(axis.line=element_blank(),
        axis.ticks=element_blank(),
        plot.title = element_text(size = 18, 
                                  face = 'bold',
                                  color = 'grey28',
                                  margin = margin(10,0,10,0),
                                  family = 'Helvetica',
                                  hjust = 0.025),
        legend.title = element_blank(),
        strip.text.x = element_blank()) +
  scale_x_continuous(breaks = seq(min(weekdends_agegroup$starthour), 
                                  max(weekdends_agegroup$starthour), by = 4))

# looking at median age per area (start station)
manhattan_bounds = 
  data.frame(south = c(40.694528782),
             east = c(-73.9661407471),
             west = c(-74.0155663569),
             north = c(40.7692806668))

# [-73.9942932129,40.6918325826],
# [-74.0380501416,40.70695852],
# [-73.9723205566,40.8161477453],
# [-73.9285636279,40.8010466317],
# [-73.9942932129,40.6918325826]
# 
# [-73.9790763856,40.694528782],
# [-74.0155663569,40.6983520502],
# [-74.0026307184,40.7692806668],
# [-73.9661407471,40.7654614721],
# [-73.9790763856,40.694528782]

medianage_df = df %>% 
  select(age,startname,startlon,startlat) %>% 
  dplyr::filter(startlat > manhattan_bounds$south & startlat < manhattan_bounds$north & 
                  startlon < manhattan_bounds$east & startlon > manhattan_bounds$west) %>%
  #filter(start.station.longitude >= -74.0380501416 & start.station.longitude <= -73.9285636279) %>%
  #filter(start.station.latitude >= 40.6918325826 & start.station.latitude <= 40.8161477453) %>%
  #dplyr::filter(start.station.longitude == -74.04385) %>%
  group_by(startname, startlon,startlat) %>%
  dplyr::summarize(median = median(age))

#medianage_df = medianage_df[sample(nrow(medianage_df),replace=F,size=0.5*nrow(medianage_df)),]
medianage_df
medianage_start = ggmap(ggmap::get_map("New York City", zoom = 14)) + 
  geom_point(data=medianage_df, aes(x=startlon,
                                    y=startlat, 
                                    color = median), size=2, alpha=0.8) + 
  theme_map() +
  labs(x = "", y = "", title = 'Median Age per Departure Station') +
  theme(axis.line=element_blank(),
        axis.ticks=element_blank(),
        plot.title = element_text(size = 22, 
                                  face = 'bold',
                                  color = 'grey28',
                                  margin = margin(10,0,10,0),
                                  family = 'Helvetica',
                                  hjust = 0.025),
        legend.title = element_blank(),
        strip.text.x = element_blank()) + 
  scale_color_gradient(low = "#ffffff", high = "#133145")

jpeg('medianage_start.jpg')
medianage_start
dev.off()




#median age per area(stop)
medianage_df = df %>% 
  select(age,endname, endlon, endlat) %>% 
  dplyr::filter(endlat > manhattan_bounds$south & endlat < manhattan_bounds$north & 
                  endlon < manhattan_bounds$east & endlon > manhattan_bounds$west) %>%
  group_by(endname, endlon, endlat) %>%
  dplyr::summarize(median = median(age)) %>%
  top_n(20)
# medianage_df = medianage_df[sample(nrow(medianage_df),replace=F,size=0.5*nrow(medianage_df)),]

medianage_stop = ggmap(ggmap::get_map("New York City", zoom = 13)) + 
  geom_point(data=medianage_df, aes(x=endlon,
                                    y=endlat, 
                                    color = median), size=2, alpha=0.8) + 
  theme_map() +
  labs(x = "", y = "", title = 'Median Age per Arrival Station') +
  theme(axis.line=element_blank(),
        axis.ticks=element_blank(),
        plot.title = element_text(size = 18, 
                                  face = 'bold',
                                  color = 'grey28',
                                  margin = margin(10,0,10,0),
                                  family = 'Helvetica',
                                  hjust = 0.025),
        legend.title = element_blank(),
        strip.text.x = element_blank()) + 
  scale_color_gradient(c(0,60))
jpeg('medianage_stop.jpg')
medianage_stop
dev.off()



# concentration of Citi Bike rides
medianage_df = df %>% 
  select(age, startid, startlon, startlat) %>% 
  dplyr::filter(startlat > manhattan_bounds$south & startlat < manhattan_bounds$north & 
                  startlon < manhattan_bounds$east & startlon > manhattan_bounds$west) %>%
  group_by (startid, startlon, startlat) %>%
  summarize(median = median(age)) %>%
  ungroup()
medianage_df

medianage_concentration = ggmap(get_map('New York',zoom=13, maptype='terrain')) +
  stat_density2d(data= medianage_df, aes(x=startlon,y = startlat, alpha=.25, fill=..level..),bins = 10, geom = 'polygon') +
  guides(fill = guide_colorbar(barwidth = 1, barheight = 8)) +
  scale_alpha(guide = FALSE)+ 
  theme_map() +
  labs(x = "", y = "", title = 'Concentration of Citi Bike Rides') +
  theme(axis.line=element_blank(),
        axis.ticks=element_blank(),
        plot.title = element_text(size = 22, 
                                  face = 'bold',
                                  color = 'grey28',
                                  margin = margin(10,0,10,0),
                                  family = 'Helvetica',
                                  hjust = 0.025),
        legend.title = element_blank(),
        strip.text.x = element_blank()) +
  scale_color_gradient(c(0,60))

jpeg('medianage_concentration.jpg')
medianage_concentration
dev.off()




## STEP 3- DATA CLEANING
# remove outliers & keep only the relevant variables. The table will be filtered a bit more in part 1 and part 2.
df = filter(df, age <= 85 & tripduration.min <= 120)
df = df %>% filter(!grepl(".*TEMP.*", startname))
df = df %>% filter(!grepl(".*TEMP.*", endname))

# df = df %>% select(gender, age, agegroup, monthname, dayofweek,
#                    tripduration, tripduration.min, tripduration.mingroup,
#                    month, day, monthname, dayofweek,
#                    startday, starttime, starthour, startrange, start.station.id, 
#                    start.station.name, start.station.latitude, start.station.longitude,
#                    stopday, stoptime, stophour, end.station.id,
#                    end.station.name, end.station.latitude, end.station.longitude)

# saveRDS(df,file="201605-citibike-tripdata_clean.rda")


# STEP 4- HYPOTHESIS TESTING
# filter df with relevant variables (e.g. age, gender, day of week, trip start and stop information)
df = df %>% select(duration, tripduration.min, tripduration.mingroup,
                   gender, age, agegroup, monthname, dayofweek,
                   startday, starttime, starthour, startrange, startid, 
                   startname, startlat, startlon,
                   stopday, stoptime, stophour, endid,
                   endname, endlat, endlon)

# remove observations with missing values (gender and yearbirth)
df = df %>% filter(!is.na(df$gender) & (!is.na(df$age)))

saveRDS(df,file="201707-citibike-tripdata_df.rda")

df <- readRDS(file="201707-citibike-tripdata_df.rda")

# create another 'long" df'
melt_df = df %>% select(starthour,stophour,
                        startname, endname,
                        startlat, endlat, 
                        startlon, endlon, 
                        gender, age, dayofweek) %>% 
  reshape(direction='long', 
          varying=list(c('starthour', 'stophour'),
                       c('startname', 'endname'),
                       c('startlat', 'endlat'),
                       c('startlon', 'endlon')),
          times=c('start', 'stop'),
          v.names=c('hour', 'name', 'latitude', 'longitude'))

melt_df = unique(melt_df)

saveRDS(melt_df,file="201707-citibike-tripdata_melt.rda")

melt_df <- readRDS(file="201707-citibike-tripdata_melt.rda")

tile_df = df %>% select(gender, agegroup, dayofweek, startrange) %>% 
  filter(gender == 'female', agegroup =='17-24') %>%
  dplyr:: group_by(gender, agegroup, startrange, dayofweek) %>% 
  dplyr::summarise(count = n())


tile_df$dayofweek = factor(tile_df$dayofweek, levels = c("Sunday", "Saturday", "Friday", "Thursday", "Wednesday", "Tuesday", "Monday"))
tile_df = tile_df[order(tile_df$dayofweek), ]


#let filter by agegroup and gender
age_and_gender = ggplot(tile_df, aes(x=startrange, y=dayofweek, fill=count)) +
  geom_tile(color="white", size=0.1) +
  scale_x_discrete(expand = c(0, 0)) +
  labs(x=NULL, y=NULL, title="Citi Bike users per day and hour") +
  scale_fill_gradient(low = "#deebf7", high = "#3182bd", name = "# Riders") +
  theme_tufte(base_family="Helvetica") +
  theme(plot.title=element_text(hjust=0)) +
  theme(axis.ticks=element_blank()) +
  theme(axis.text=element_text(size=7)) +
  theme(legend.title=element_text(size=8)) +
  theme(legend.text=element_text(size=6)) +
  theme(legend.key.size=unit(0.2, "cm")) +
  theme(legend.key.width=unit(1, "cm")) +
  facet_wrap(gender~agegroup)

jpeg('age_and_gender.jpg')
age_and_gender
dev.off()

#ggplotly(g) %>% config(displayModeBar = F)


# Not needed anymore
# map_df = df %>% select(age, dayofweek, gender, startrange, 
#                         start.station.longitude, start.station.latitude, start.stationname) %>%
#     filter(gender == 'female' & age > 17 & age < 25 & startrange == '4am-8am' & dayofweek == 'Friday') %>%
#     group_by(gender, agegroup, startrange, dayofweek, start.station.longitude, start.station.latitude) %>% 
#     summarise(count = n()) # filter by user input (agegroup, gender) radio button for arriving or leaving

#let filter by gender, age, hour, dayofweek, arriving or departing
map_df = melt_df %>% filter(gender == 'female', age >= 17, age <= 25, hour >= 1, hour <= 5, dayofweek == 'Friday', time == 'start')

# code without bike icons
# leaflet(data = map_df) %>% 
#     addTiles() %>% 
#     addMarkers(~longitude, ~latitude, popup = ~name, label = ~name) %>% addProviderTiles('Esri.NatGeoWorldMap')

#code with bike icons:
bike_icon = makeIcon('C:/citibike-analysis/bike_icon.png', iconWidth = 25, iconHeight = 38)
bike_leaflet = leaflet(data = map_df) %>% 
  addTiles() %>% 
  addMarkers(~longitude, ~latitude, popup = ~name, label = ~name, icon = bike_icon) %>% addProviderTiles('Esri.NatGeoWorldMap')
jpeg('bike_leaflet.jpg')
bike_leaflet
dev.off()

# would have liked to add the estimated count and color icons accordingly

# 3- HOW
# estimated time by citi bike

direction_df = df %>% select(startname, endname, 
                             starthour, tripduration.min, dayofweek, 
                             startlat, startlon,
                             endlat, endlon)

direction_df$start.lat_long = apply( direction_df[ , c('startlat', 'startlon') ] , 1 , paste , collapse = ", " )
direction_df$end.lat_long = apply( direction_df[ , c('endlat', 'endlon') ] , 1 , paste , collapse = ", " )

saveRDS(direction_df,file="201605-citibike-tripdata_direction.rda")

#look at unique combination of lat & long (>100 000 rows)
unique_lat_long = unique(direction_df[,c('start.lat_long','end.lat_long')])

from = unique_lat_long$start.lat_long
to = unique_lat_long$end.lat_long
#google_df = mapdist(from, to, mode='bicycling')

# dir_df = merge(direction_df, google_df, by = c('start.lat_long', 'end.lat_long'))

## calculate the average duration according to citi bike

direction_df = direction_df %>% 
  filter (startname == '1 Ave & E 30 St' & 
          endname == 'E 17 St & Broadway' & 
          dayofweek == 'Sunday') %>%
  group_by(start.lat_long, end.lat_long) %>%
  summarise(avg_duration = mean(tripduration.min))


# calculate the average duration according to google




## using a valid Google Maps API key


## Using the first and last coordinates as the origin/destination
origin = direction_df$start.lat_long  # grab direction_df$start.lat_long (from filtered list above)
destination = direction_df$end.lat_long # grab direction_df$stop.lat_long (from filtered list above)

google_time = select(mapdist(origin,destination, mode='bicycling'), minutes)

## get the directions from Google Maps API
library(googleway)
key = "AIzaSyDqTD8Nko1u-iyl4aJLue4S5CXUcHpz05c"
res = google_directions(origin = origin,
                        destination = destination,
                        key = key)

df_polyline <- decode_pl(res$routes$overview_polyline$points)

leafy = leaflet() %>%
  addTiles() %>%
  addPolylines(data = df_polyline, lat = ~lat, lng = ~lon)
jpeg('leafy.jpg')
leafy
dev.off()

