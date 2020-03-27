devtools::install_github("PMassicotte/gtrendsR")  # only run once

install.packages("gtrendsR")
## load library 
library(gtrendsR)
library(dplyr)

# searching for covid-19 Trends
res0 <- gtrends(c("covid-19"), geo=c("US-AL","US-AK","US-AZ","US-AR","US-CA"),time = "2019-11-01 2020-03-24")
res1 <- gtrends(c("covid-19"), geo=c("US-CO","US-CT","US-DE","US-FL","US-GA"),time = "2019-11-01 2020-03-24")
res2 <- gtrends(c("covid-19"), geo=c("US-HI","US-ID","US-IL","US-IN","US-IA"),time = "2019-11-01 2020-03-24")
res3 <- gtrends(c("covid-19"), geo=c("US-KS","US-KY","US-LA","US-ME","US-MD"),time = "2019-11-01 2020-03-24")
res4 <- gtrends(c("covid-19"), geo=c("US-MA","US-MI","US-MN","US-MS","US-MO"),time = "2019-11-01 2020-03-24")
res5 <- gtrends(c("covid-19"), geo=c("US-MT","US-NE","US-NV","US-NH","US-NJ"),time = "2019-11-01 2020-03-24")
res6 <- gtrends(c("covid-19"), geo=c("US-NM","US-NY","US-NC","US-ND","US-OH"),time = "2019-11-01 2020-03-24")
res7 <- gtrends(c("covid-19"), geo=c("US-OK","US-OR","US-PA","US-RI","US-SC"),time = "2019-11-01 2020-03-24")
res8 <- gtrends(c("covid-19"), geo=c("US-SD","US-TN","US-TX","US-UT","US-VT"),time = "2019-11-01 2020-03-24")
res9 <- gtrends(c("covid-19"), geo=c("US-VA","US-WA","US-WV","US-WI","US-WY"),time = "2019-11-01 2020-03-24")
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
library(tidyverse)
# Use regular expressions to separate on multiple characters:
df <- covid %>% separate(geo, c("Country","State"), sep = "([\\-])") # here we are sperating by - the (\\ is just part of syntax)

# Seperating the date columns just in case
df2 <- df %>% separate(date, c("Year","Month","Day"), sep = "([\\-])") # here we are sperating by - the (\\ is just part of syntax)

# IMPORT LONGITUDE AND LATTITUDE
usa <- read.csv("USA.csv")
colnames(usa)[1] <- "State"

final <- merge(df2,usa, by=c("State"),all.x=TRUE)
colnames(final)[10] <- "region" # match the map_data

final2 <- merge(df,usa, by=c("State"),all.x=TRUE)
colnames(final2)[8] <- "region" # match the map_data

final2 <- final2[-c(4,5,6,7)]
colnames(final2)[3] <- "value"
colnames(final2)[1] <- "state"
final3 <- final2

final3$date <- as.POSIXct(final3$date, format = '%m-%d-%Y')

final3$date <- format(final3$date, "%m/%d/%y")
##############################################################
final2 # this is final dataframe!!!!!!!!!!!!!!!!!!!!!!
# this is also final dataframe 
final3
########################################################


# Plotting
install.packages(map)
library(maps)
us_states <- map_data("state")
head(us_states) # NEED TO MERGE GROUP TO FINAL 
us2 <- us_states[-c(1,2,4,6)]

# merge by region now that it is lower

final_f2 <- merge(us_states,final2,by=c("region"), all.x=TRUE)
final_f2 <- final_f2[-c(12,13)]

#keep only one day!!!!!!!!!!!!
date_test <- final_f2%>% filter(date > '2020-03-21')

df_test2 <- date_test[-c(6)]

# GET DISTINCT ROWS
library(dplyr)
df_final <- df_test2 %>% distinct()
df_final2 <- df_final


# try this  get first occurance of each stte in the column
library(dplyr)
dfz <- df_final2 %>% 
  group_by(region) %>% 
  slice(1) %>% # takes the first occurrence if there is a tie
  ungroup()


#########################################################
dfz # final dataframe!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
colnames(dfz)[3] <- "state"
dfff <- dfz[c(3,5)]
dft <- dfff 
###############################################

# plotting
install.packages(usmap)
library(usmap)
library(ggplot2)

plot_usmap(data = dfff, values = "hits", color = "red") + 
  scale_fill_continuous(name = "Hits", label = scales::comma) + 
  theme(legend.position = "right")

# add alaska and hawaii for fun
 dft[nrow(dft) + 1,] =c("AK", 10) 
 dft[nrow(dft) + 1,] =c("HI", 10)

 # THIS ONE IS NOT WORKING WHEN ADD the two states
 plot_usmap(data = dft, values = "hits", color = "red") + 
   scale_fill_continuous(name = "hits", label = scales::comma) + 
   theme(legend.position = "right")

 #
 library(tigris)
 library(leaflet)
 install.packages("jsonlite")
 library(jsonlite)
 
 states <- states(cb=T)
 
 # Let's quickly map that out
 states %>% leaflet() %>% addTiles() %>% addPolygons(popup=~NAME)
 
 bins <- c(0, 10, 20, 50, 60, 70 ,80, 100)
 pal <- colorBin("RdYlBu", domain = dft$state, bins = bins)
 
 # This works!!!!
 m %>% addPolygons(
   data = states,
      weight = 2,
   smoothFactor = 0.5,
   opacity = 1,
   color = "white",
   dashArray = "3",
   fillOpacity = 0.8,
   fillColor = pal(dfz$hits))
 m
 
 # add state names to this 
 d <- leaflet() %>%
   addProviderTiles(providers$Stamen.Toner) %>%
   addPolygons(data = states,
               weight = 2,
               smoothFactor = 0.5,
               opacity = 1,
               color = "white",
               dashArray = "3",
               fillOpacity = 0.8,
               fillColor = pal(dfz$hits))
 d
 
 # add HIGHLIGHT FEATURE!
 C <- leaflet() %>%
   addProviderTiles(providers$Stamen.Toner) %>%
   addPolygons(data = states,
               weight = 2,
               smoothFactor = 0.5,
               opacity = 1,
               color = "white",
               dashArray = "3",
               fillOpacity = 0.8,
               fillColor = pal(dfz$hits),
               highlight = highlightOptions(
                 weight = 5,
                 color = "#666666",
                 dashArray = "",
                 fillOpacity = 0.7,
                 bringToFront = TRUE))
 C
 
 # add feature to know number
 
 install.packages("htmltools")
 library(htmltools)
 library(dplyr)
 library(leaflet)
 
 labels <- paste("<p>", dfz$state, "</p>",
                 "<p>", "Hits Rate:", dfz$hits, "</p>",
                 sep = "")
 
 # add overview feature
 z <- leaflet() %>%
   addProviderTiles(providers$Stamen.Toner) %>%
   addPolygons(data = states,
               weight = 2,
               smoothFactor = 0.5,
               opacity = 1,
               color = "white",
               dashArray = "3",
               fillOpacity = 0.8,
               fillColor = pal(dfz$hits),
               highlight = highlightOptions(
                 weight = 5,
                 color = "#666666",
                 dashArray = "",
                 fillOpacity = 0.7,
                 bringToFront = TRUE
                 ),
               label = lapply(labels,HTML))
 z
 
 # add more!!!! LEGEND
 w <- leaflet() %>%
   addProviderTiles(providers$Stamen.Toner) %>%
   addPolygons(data = states,
               weight = 2,
               smoothFactor = 0.5,
               opacity = 1,
               color = "white",
               dashArray = "3",
               fillOpacity = 0.8,
               fillColor = pal(dfz$hits),
               highlight = highlightOptions(
                 weight = 5,
                 color = "#666666",
                 dashArray = "",
                 fillOpacity = 0.7,
                 bringToFront = TRUE
               ),
               label = lapply(labels,HTML)) %>%
   addLegend(pal=pal,
             values = dfz$hits,
             opacity = 0.7,
             position = "topright")
 w
 
 
 ##################################################################3
 # DO this but find usa !!! 
 # IN COVID COUNT CHANGE NAME FROM US TO SAME AS PLOTTING PACKAG
 install.packages("choroplethr")
 library(choroplethr)
 
 install.packages("choroplethrMaps")
 library(choroplethrMaps)
 
 library(dplyr)
 
 library(ggplot2)
 
 data(state.map)
 ggplot(state.map, aes(long, lat, group=group)) + geom_polygon()
 data(country.map, package = "choroplethrMaps")
 
 statemap <- data(state.map)# region
 ustate <- unique(state.map$region)
 ufin <- unique(final3$region)
 
 yesf <- final3[ufin %in% ustate,]
 
 
 
 plotdata <- yesf
 plotdata1 <-plotdata[plotdata$date == "01/22/20",]
 plotdata2 <-plotdata[plotdata$date == "01/23/20",]
 plotdata3 <-plotdata[plotdata$date == "01/24/20",]
 plotdata4 <-plotdata[plotdata$date == "1/25/2020",]
 plotdata5 <-plotdata[plotdata$date == "1/26/2020",]
 plotdata6 <-plotdata[plotdata$date == "1/27/2020",]
 plotdata7 <-plotdata[plotdata$date == "1/28/2020",]
 plotdata8 <-plotdata[plotdata$date == "1/29/2020",]
 plotdata9 <-plotdata[plotdata$date == "1/30/2020",]
 plotdata10 <-plotdata[plotdata$date == "1/31/2020",]
 plotdata11 <-plotdata[plotdata$date == "2/1/2020",]
 plotdata12 <-plotdata[plotdata$date == "2/2/2020",]
 plotdata13 <-plotdata[plotdata$date == "2/3/2020",]
 plotdata14 <-plotdata[plotdata$date == "02/04/2020",]# NO SEARCHES UNTIL HERE
 plotdata15 <-plotdata[plotdata$date == "02/05/20",]
 plotdata16 <-plotdata[plotdata$date == "02/06/20",]
 plotdata17 <-plotdata[plotdata$date == "02/07/20",]
 plotdata18 <-plotdata[plotdata$date == "02/08/20",]
 plotdata19 <-plotdata[plotdata$date == "02/09/20",]
 plotdata20 <-plotdata[plotdata$date == "02/10/20",]
 plotdata21 <-plotdata[plotdata$date == "02/11/20",]
 plotdata22 <-plotdata[plotdata$date == "02/12/20",]
 plotdata23 <-plotdata[plotdata$date == "02/13/20",]
 plotdata24 <-plotdata[plotdata$date == "02/14/20",]
 plotdata25 <-plotdata[plotdata$date == "02/15/20",]
 plotdata26 <-plotdata[plotdata$date == "02/16/20",]
 plotdata27 <-plotdata[plotdata$date == "02/17/20",]
 plotdata28 <-plotdata[plotdata$date == "02/18/20",]
 plotdata29 <-plotdata[plotdata$date == "02/19/20",]
 plotdata30 <-plotdata[plotdata$date == "02/20/20",]
 plotdata31 <-plotdata[plotdata$date == "02/21/20",]
 plotdata32 <-plotdata[plotdata$date == "02/22/20",]
 plotdata33 <-plotdata[plotdata$date == "02/23/20",]
 plotdata34 <-plotdata[plotdata$date == "02/24/20",]
 plotdata35 <-plotdata[plotdata$date == "02/25/20",]
 plotdata36 <-plotdata[plotdata$date == "02/26/20",]
 plotdata37 <-plotdata[plotdata$date == "02/27/20",]
 plotdata38 <-plotdata[plotdata$date == "02/28/20",]
 plotdata39 <-plotdata[plotdata$date == "02/29/20",]
 plotdata40 <-plotdata[plotdata$date == "03/01/20",]
 plotdata41 <-plotdata[plotdata$date == "03/02/20",]
 plotdata42 <-plotdata[plotdata$date == "03/03/20",]
 plotdata43 <-plotdata[plotdata$date == "03/04/20",]
 plotdata44 <-plotdata[plotdata$date == "03/05/20",]
 plotdata45 <-plotdata[plotdata$date == "03/06/20",]
 plotdata46 <-plotdata[plotdata$date == "03/07/20",]
 plotdata47 <-plotdata[plotdata$date == "03/08/20",]
 plotdata48 <-plotdata[plotdata$date == "03/09/20",]
 plotdata49 <-plotdata[plotdata$date == "03/10/20",]
 plotdata50 <-plotdata[plotdata$date == "03/11/20",]
 plotdata51 <-plotdata[plotdata$date == "03/12/20",]
 plotdata52 <-plotdata[plotdata$date == "03/13/20",]
 plotdata53 <-plotdata[plotdata$date == "03/14/20",]
 plotdata54 <-plotdata[plotdata$date == "03/15/20",]
 plotdata55 <-plotdata[plotdata$date == "03/16/20",]
 plotdata56 <-plotdata[plotdata$date == "03/17/20",]
 plotdata57 <-plotdata[plotdata$date == "03/18/20",]
 plotdata58 <-plotdata[plotdata$date == "03/19/20",]
 
 
 
 state_choropleth(plotdata1)
 country_choropleth(plotdata2)
 country_choropleth(plotdata3)
 country_choropleth(plotdata4)
 country_choropleth(plotdata5)
 country_choropleth(plotdata6)
 country_choropleth(plotdata7)
 country_choropleth(plotdata8)
 country_choropleth(plotdata9)
 country_choropleth(plotdata10)
 country_choropleth(plotdata11)
 country_choropleth(plotdata12)
 country_choropleth(plotdata13) # not usefull up until here
state_choropleth(plotdata14)
 state_choropleth(plotdata15)
 state_choropleth(plotdata16)
 state_choropleth(plotdata17)
 state_choropleth(plotdata18)
 state_choropleth(plotdata19)
 state_choropleth(plotdata20)
 state_choropleth(plotdata21)
 state_choropleth(plotdata22)
 state_choropleth(plotdata23)
 state_choropleth(plotdata24)
 state_choropleth(plotdata25)
 state_choropleth(plotdata26)
 state_choropleth(plotdata27)
 state_choropleth(plotdata28)
 state_choropleth(plotdata29)
 state_choropleth(plotdata30)
 state_choropleth(plotdata31)
 state_choropleth(plotdata32)
 state_choropleth(plotdata33)
 state_choropleth(plotdata34)
 state_choropleth(plotdata35)
 state_choropleth(plotdata36)
 state_choropleth(plotdata37)
 state_choropleth(plotdata38)
 state_choropleth(plotdata39)
 state_choropleth(plotdata40)
 state_choropleth(plotdata41)
 state_choropleth(plotdata42)
 state_choropleth(plotdata43)
 state_choropleth(plotdata44)
 state_choropleth(plotdata45)
 state_choropleth(plotdata46)
 state_choropleth(plotdata47)
 state_choropleth(plotdata48)
 state_choropleth(plotdata49)
 state_choropleth(plotdata50)
 state_choropleth(plotdata51)
 state_choropleth(plotdata52)
 state_choropleth(plotdata53)
 state_choropleth(plotdata54)
 state_choropleth(plotdata55)
 state_choropleth(plotdata56)
 state_choropleth(plotdata57)
 state_choropleth(plotdata58)
 