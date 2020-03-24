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

##############################################################
final2 # this is final dataframe!!!!!!!!!!!!!!!!!!!!!!
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
dfz # final dataframe
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
 