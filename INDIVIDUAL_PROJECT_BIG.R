# Big Covid Data Project
install.packages("riem")
library("riem")
install.packages("dplyr")
library("dplyr")
install.packages("lubridate")
library(lubridate)
install.packages("zoo")
library(zoo)
install.packages("xts")
library(xts)

b <-riem_networks()
la <- riem_stations(network ="LA_ASOS") # pick station lousiana was giving problems

#Load 500 stations in the US some may not have information
meas_asos2 <- read.csv("500_stations.csv") # pick 10 stations from 50 states here are codes
colnames(meas_asos2)[1] <- "stid" # change column name

# Make value to iterate over for loop
st_asos3 <- unique(meas_asos2$stid) 

# loop for dataframe 

temp_1 <- NULL; #initalize loop variables
temp_2 <- NULL; # again

# for each unique station until the last one
for(i in 1:length(st_asos3)){
  tryCatch({   # if any errors come up we want to show but continue the loop (no station to aggregate we skip)
  temp_1 <- NULL;
  temp_12 <-NULL;
  # temp_1 this stores weather information for each station starting from beginning of this year 
  temp_1 <-riem_measures(station = as.character(st_asos3[i]), date_start = "2020-01-01",
                         date_end = as.character(Sys.Date()))
  temp_1 = data.frame(temp_1) # then we make data frame varaible out of it
  temp_1$Date <- date(temp_1$valid) # turn the valid column into a date
  temp_1$Day <- lubridate::yday(temp_1$valid) # y day gets day of year ( extract the year day of the date column )
  temp_1$Yearmonthday <- lubridate::ymd(temp_1$Date) # this will be column we use to tell what day it is
  
  # Here we aggregate all the columns we want by the month and day and get average of the variable
  temp_12 <- aggregate(temp_1[,c(5,6,7,8,9)], by =list(temp_1$Yearmonthday), mean, na.action=na.pass, na.rm=TRUE)
  
  temp_12$station <-unique(temp_1$station)
  
  # if an error comes up we continue but we show what was wrong
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  temp_2 <- rbind(temp_2, temp_12) # bind that stations information onto the previous
  
  print(i)
}
weather <- temp_2 # this is the weather data
write.csv(temp_2,file="covid_weather.csv")

# Import unique weather stations
station_id <- read.csv("station_states.csv")
colnames(station_id)[1] <- "station"

# merge weather by station id 
weather_df1 <- merge(weather,station_id,by=c("station"),all.x=TRUE)
colnames(weather_df1)[13] <- "state_id"

# create new columns to be used to order data 
weather$year <- lubridate::year(ymd(weather$date))
weather$month <- lubridate::month(ymd(weather$date))
weather$day <- lubridate::day(ymd(weather$date))
weather$week <- lubridate::week(ymd(weather$date))

# Order the data by state year month day for time series imputation
weather_df1 <- weather_df1[order(weather_df1$state,weather_df1$year,weather_df1$month,weather_df1$day),]

install.packages("imputeTS") # this package helps us impute time series data
library(imputeTS)

# create new columns from the time series imputation
weather_df1$tmpf_imp <- na_interpolation(weather_df1$tmpf)
weather_df1$dwpf_imp <- na_interpolation(weather_df1$dwpf)
weather_df1$relh_imp <- na_interpolation(weather_df1$relh)
weather_df1$drct_imp <- na_interpolation(weather_df1$drct)
weather_df1$sknt_imp <- na_interpolation(weather_df1$sknt)

# select subset of dataframe
weather_df2 <- weather_df1[c(1,2,8,9,10,11,12,13,14,15,16,17,18)]

# Lets get a dataframe of the average weather for that day in each state
weather_df_final <- aggregate(weather_df2[,c(9,10,11,12,13)], by = list(weather_df2$state,weather_df2$date), mean, na.action=na.pass, na.rm=TRUE)
colnames(weather_df_final)[1]<-"state"
colnames(weather_df_final)[2] <- "date"

weather <- weather_df_final# THIS IS FINAL DATAFRAME WE WANT NOW
colnames(weather_)[1] <- "state"
colnames(weather)[2] <- "date"

write.csv(weather_df_final,file="corona_weather.csv")

###############################################################################################################################################################
# NOW LETS GET GOOGLE TRENDS DATA FOR CORONA VIRUS
devtools::install_github("PMassicotte/gtrendsR")  # only run once

install.packages("gtrendsR")
## load library 
library(gtrendsR)
library(dplyr)

# LOOP TO GET MORE THAN ONE SEARCH TERM AND BIND THEM TOGETHER!!!!!!!!
res0 <- NULL;
res1 <- NULL;
res2 <- NULL;
res3 <- NULL;
res4 <- NULL;
res5 <- NULL;
res6 <- NULL;
res7 <- NULL;
res8 <- NULL; 
res9 <- NULL;
temp_3 <- NULL;
temp_4<- NULL;
temp_5 <- NULL;
temp_6 <- NULL;
var <- c("covid-19","corona_virus","virus","vaccine","masks")

for(i in 1:length(unique(var))){
  res0 <- NULL;
  res1 <- NULL;
  res2 <- NULL;
  res3 <- NULL;
  res4 <- NULL;
  res5 <- NULL;
  res6 <- NULL;
  res7 <- NULL;
  res8 <- NULL; 
  res9 <- NULL;
    res0 <- gtrends(c(var[i]), geo=c("US-AL","US-AK","US-AZ","US-AR","US-CA"),time = "2020-01-01 2020-04-28")
    res1 <- gtrends(c(var[i]), geo=c("US-CO","US-CT","US-DE","US-FL","US-GA"),time = "2020-01-01 2020-04-28")
    res2 <- gtrends(c(var[i]), geo=c("US-HI","US-ID","US-IL","US-IN","US-IA"),time = "2020-01-01 2020-04-28")
    res3 <- gtrends(c(var[i]), geo=c("US-KS","US-KY","US-LA","US-ME","US-MD"),time = "2020-01-01 2020-04-28")
    res4 <- gtrends(c(var[i]), geo=c("US-MA","US-MI","US-MN","US-MS","US-MO"),time = "2020-01-01 2020-04-28")
    res5 <- gtrends(c(var[i]), geo=c("US-MT","US-NE","US-NV","US-NH","US-NJ"),time = "2020-01-01 2020-04-28")
    res6 <- gtrends(c(var[i]), geo=c("US-NM","US-NY","US-NC","US-ND","US-OH"),time = "2020-01-01 2020-04-28")
    res7 <- gtrends(c(var[i]), geo=c("US-OK","US-OR","US-PA","US-RI","US-SC"),time = "2020-01-01 2020-04-28")
    res8 <- gtrends(c(var[i]), geo=c("US-SD","US-TN","US-TX","US-UT","US-VT"),time = "2020-01-01 2020-04-28")
    res9 <- gtrends(c(var[i]), geo=c("US-VA","US-WA","US-WV","US-WI","US-WY"),time = "2020-01-01 2020-04-28")
    res0 <- data.frame(res0$interest_over_time) 
    res1 <- data.frame(res1$interest_over_time) 
    res2 <- data.frame(res2$interest_over_time)
    res3 <- data.frame(res3$interest_over_time) 
    res4 <- data.frame(res4$interest_over_time) 
    res5 <- data.frame(res5$interest_over_time) 
    res6 <- data.frame(res6$interest_over_time)
    res7 <- data.frame(res7$interest_over_time) 
    res8 <- data.frame(res8$interest_over_time)
    res9 <- data.frame(res9$interest_over_time) 
    temp_3 <- rbind(temp_3,res0,res1,res2)
    temp_4 <- rbind(temp_4,res3,res4,res5,res6)
    temp_5 <- rbind(temp_5,res7,res8,res9)
    temp_6 <- rbind(temp_6,temp_5,temp_4,temp_3)
    
    print(i)
  }


# MANUALLY SEARCH FOR EACH TERM ONE BY ONE IF NEEDED.
# manually copy and paste
var <- paste("covid-19")

# searching for covid-19 Trends
res0 <- gtrends(c(var), geo=c("US-AL","US-AK","US-AZ","US-AR","US-CA"),time = "2020-01-01 2020-04-28")
res1 <- gtrends(c(var), geo=c("US-CO","US-CT","US-DE","US-FL","US-GA"),time = "2020-01-01 2020-04-28")
res2 <- gtrends(c(var), geo=c("US-HI","US-ID","US-IL","US-IN","US-IA"),time = "2020-01-01 2020-04-28")
res3 <- gtrends(c(var), geo=c("US-KS","US-KY","US-LA","US-ME","US-MD"),time = "2020-01-01 2020-04-28")
res4 <- gtrends(c(var), geo=c("US-MA","US-MI","US-MN","US-MS","US-MO"),time = "2020-01-01 2020-04-28")
res5 <- gtrends(c(var), geo=c("US-MT","US-NE","US-NV","US-NH","US-NJ"),time = "2020-01-01 2020-04-28")
res6 <- gtrends(c(var), geo=c("US-NM","US-NY","US-NC","US-ND","US-OH"),time = "2020-01-01 2020-04-28")
res7 <- gtrends(c(var), geo=c("US-OK","US-OR","US-PA","US-RI","US-SC"),time = "2020-01-01 2020-04-28")
res8 <- gtrends(c(var), geo=c("US-SD","US-TN","US-TX","US-UT","US-VT"),time = "2020-01-01 2020-04-28")
res9 <- gtrends(c(var), geo=c("US-VA","US-WA","US-WV","US-WI","US-WY"),time = "2020-01-01 2020-04-28")
plot(res0)
plot(res1)
plot(res2)
plot(res3)
plot(res4)
plot(res5)
plot(res6)
plot(res7)
plot(res8)
plot(res9)

#Extract the data frame element of list
res0 <- res0$interest_over_time
res1 <- res1$interest_over_time
res2 <- res2$interest_over_time
res3 <- res3$interest_over_time
res4 <- res4$interest_over_time
res5 <- res5$interest_over_time
res6 <- res6$interest_over_time
res7 <- res7$interest_over_time
res8 <- res8$interest_over_time
res9 <- res9$interest_over_time

# Replace <1 values with 0
res0$hits <- gsub("<1",0,res0$hits) # make na values
res1$hits <- gsub("<1",0,res1$hits) # make na values
res2$hits <- gsub("<1",0,res2$hits) # make na values
res3$hits <- gsub("<1",0,res3$hits) # make na values
res4$hits <- gsub("<1",0,res4$hits) # make na values
res5$hits <- gsub("<1",0,res5$hits) # make na values
res6$hits <- gsub("<1",0,res6$hits) # make na values
res7$hits <- gsub("<1",0,res7$hits) # make na values
res8$hits <- gsub("<1",0,res8$hits) # make na values
res9$hits <- gsub("<1",0,res9$hits) # make na values

# Convert to intiger
res0$hits <- as.integer(res0$hits)
res1$hits <- as.integer(res1$hits)
res2$hits <- as.integer(res2$hits)
res3$hits <- as.integer(res3$hits)
res4$hits <- as.integer(res4$hits)
res5$hits <- as.integer(res5$hits)
res6$hits <- as.integer(res6$hits)
res7$hits <- as.integer(res7$hits)
res8$hits <- as.integer(res8$hits)
res9$hits <- as.integer(res9$hits)

# THIS IS ALL OF THE CHLAMYDIA DATA FOR ALL STATES ONTOP OF EACHOTHER
a <- bind_rows(res0,res1,res2)
b <- bind_rows(res3,res4,res5) # fix res 4
c <- bind_rows(res6,res7,res8)
c <- bind_rows(res9,c)
covid_19 <- bind_rows(a,b,c)

# Remove irrelevlant columns 
covid <-covid_19[-c(4,6,7)]

# seperate columns to be more tidy
install.packages("tidyverse")
library(tidyverse)

# Use regular expressions to separate on multiple characters:
df <- covid %>% separate(geo, c("Country","State"), sep = "([\\-])") # here we are sperating by - the (\\ is just part of syntax)

# IMPORT LONGITUDE AND LATTITUDE
usa <- read.csv("USA.csv")
colnames(usa)[1] <- "State"

final <- merge(df,usa, by=c("State"),all.x=TRUE)
colnames(final)[1] <- "state_id" # match the map_data
colnames(final)[8] <- "state"

final$date <- as.POSIXct(final$date, format = '%m-%d-%Y')

# Seperating the date columns just in case
df2 <- final %>% separate(date, c("Year","Month","Day"), sep = "([\\-])") # here we are sperating by - the (\\ is just part of syntax)
df2$state <- toupper(df2$state) # Make everything uppercase

covid_interest <- df2 # this is the final dataframe
write.csv(covid_interest,file="covid_interest.csv")

covid_interest$date <- paste(covid_interest$Year,"-",covid_interest$Month,"-",covid_interest$Day)
searchstring <- ' '
replacementstring <-''
covid_interest$date <- gsub(searchstring,replacementstring,covid_interest$date)

# filter only needed values
covid_interest <- covid_interest[c(11,10,9,8,7,5,1)]
covid_interest$date <- as.Date(covid_interest$date)
########################################################################################################
# LETS NOT USE THIS FOR NOW BUT LETS SEE HOW IT WORKS!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# GET CENSUS DATA
install.packages("tidycensus")
library(tidycensus)
install.packages("tidyverse")
library(tidyverse)
install.packages("viridis")
library(viridis)

# get api key
https://api.census.gov/data/key_signup.html

census_api_key("e3a3dbad3edfa4d96cb59f65931694b311565c63",install =TRUE)

# pick varables
vars10 <- c("P005003", "P005004", "P005006", "P004003")

il <- get_decennial(geography = "county", variables = vars10, year = 2010,
                    summary_var = "P001001", state = "IL", geometry = TRUE)

# looking for variables pick one!!!!!!!!!!!!
# different options "sf1", "sf3", or "acs5"
v18 <- load_variables(2018, "acs5",cache = TRUE)

View(v18)

# GRABING THE MEDIAN INCOME FOR 2018 FOR VT
vt <- get_acs(geography = "county", 
              variables = c(medincome = "B19013_001"), 
              state = "VT", 
              year = 2018)

vt

# GETTING MEDIAN INCOME FROM EACH STATE 
median_income <- get_acs(geography = "state", 
              variables = c(medincome = "B19013_001"), 
              year = 2018)
median_income

# GET TOTAL POPULATION FOR EACH STATE
population <- get_acs(geography = "state", 
              variables = c(population = "B00001_001"), 
              year = 2018)
population
####################################################################################################
# LETS GET WALMART PURCHASES OF THINGS LIKE MASKS
# Developer portal is temporarily closed.......................................................
install.packages("walmartAPI")
library(walmartAPI)
install.packages("devtools")
devtools::install_github("EmilHvitfeldt/walmartAPI")
install.packages("tidyverse")
library(tidyverse)

# GET KEY FROM HERE
# https://developer.walmartlabs.com/member

walmartAPI::lookup(id = 19336123, key = key)

walmartAPI::searching("ipod", key = key)

store_locator(city = "Houston", key = key)
####################################################################################################
# LETS GET TWITTER DATA
# due to corona developer is closed for now might take long.............................
###################################################################################################
# LETS GET COVID 19 DATA!
covid <- read.csv("covid_state_count.csv")
covid$state <- toupper(covid$state)
colnames(covid)[1] <- "date"
covid$date <- as.character(as.Date(covid$date, "%m/%d/%Y"), "%Y-%m-%d")
covid$date <- as.Date(covid$date)
####################################################################################################
# COMBINING DATASETS

# merging covid and weather"
count_weather <- merge(covid,weather,by =c("date","state"),all.x = TRUE)

# REMOVE MISSING STATES AND VALUES WITH NA.OMIT 
# LOUISIANA IS MISSING WEATHER!!!!!!!!!!!!! GO BACKA AND FIX THIS!!!!!!!!!! REMERGE OR FIND NEW STATIONS
count_weather1 <- na.omit(count_weather)

# merging google trends information
final_dataframe <- merge(count_weather1,covid_interest,by=c("state","date"),all.x=TRUE)

write.csv(final_dataframe,file="final_dataframe.csv")
