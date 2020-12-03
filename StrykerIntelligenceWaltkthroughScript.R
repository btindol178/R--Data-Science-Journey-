# LOAD REQURED PACKAGES WORKSPACE AND SET WD
rm(list=ls())
setwd("C:/Users/blake/OneDrive/Stryker Project/Country Leaflet Information Dashboard")
load("StrykerIntelligenceWaltkthroughWorkspace.rdata")
##########################################################################################################################################################################################################
##########################################################################################################################################################################################################
##########################################################################################################################################################################################################
install.packages("pacman")
pacman::p_load(dplyr, data.table, lubridate,zoo,riem,tidyr,countrycode,RCurl,stringr,tidyverse,shiny,miniUI,taskscheduleR,gtrendsR,tidycensus,sf,leaflet,mapview,viridis,tidyquant,tigris,tmap,sf,maps,tidycensus,ggsflabel,scales,tmaptools,purrr,plotly.hrbrthemes,wonderapi,DT) 
x <- c("lubridate","data.table","zoo","riem","dplyr","tidyr","countrycode","RCurl","stringr","tidyverse","shiny","miniUI","taskscheduleR","gtrendsR","tidycensus","sf","leaflet","mapview","viridis","tidyquant","tigris","tmap","sf","maps","tidycensus","ggsflabel","scales","tmaptools","purrr","plotly","hrbrthemes","DT")
lapply(x, require, character.only = TRUE)
require(lubridate);require(data.table);require(zoo);require(riem);require(dplyr);require(tidyr);require(countrycode);require(RCurl);require(stringr);require(tidyverse);require(shiny);require(miniUI);require(taskscheduleR);require(gtrendsR);require(tidycensus);require(sf);require(leaflet);require(mapview);require(viridis);require(tidyquant);require(tigris);require(tmap);require(sf);require(maps);require(scales);require(ggsflabel);require(tmaptools);require(purrr);require(plotly);require(hrbrthemes);require(DT)
##########################################################################################################################################################################################################
# Get state information to loop over
stateid <- read.csv("statesid.csv");stateid <- stateid$state_id

# census key code
census_api_key("e3a3dbad3edfa4d96cb59f65931694b311565c63",install = TRUE,overwrite = TRUE)

# Census variable list
all_vars_acs5 <- load_variables(year = 2018, dataset = "acs5") # read all the variable list
all_vars_acs5

options(tigris_use_cache = TRUE)

##########################################################################################################################################################################################################
##########################################################################################################################################################################################################
##########################################################################################################################################################################################################

# Variables for Social Vulnerability Index for period 2014 to 2018 
countyinformation <- get_acs(geography = "county", state = NULL, variables = c(totalpop2018 = "B01003_001",
                                                                               totalpoverty12mon = "B17001_001",#  (persons for whom poverty level is tracked)
                                                                               totalincomebelowpoverty = "B17001_002", # of those people people who are below powerty level income
                                                                               Malepop65and66yearsold = "B01001_020",
                                                                               Totalmale67to69yearsold ="B01001_021",
                                                                               Totalmale70to74yearsold= "B01001_022",
                                                                               Totalmale75to79yearsold ="B01001_023",
                                                                               Totalmale80to85yearsold = "B01001_024",
                                                                               Totalmale85yearsandover = "B01001_025",
                                                                               Femalepop65and66yearsold = "B01001_044",
                                                                               TotalFemale67to69yearsold = "B01001_045",
                                                                               Totalfemale70to74yearsold ="B01001_046",
                                                                               Totalfemale75to79yearsold = "B01001_047",
                                                                               Totalfemale80to85yearsoldv="B01001_048",
                                                                               Totalfemale85yearsandover ="B01001_049",
                                                                               EstimatetotalRace ="B02001_001",
                                                                               EstimatetotalnonWhite = "B02001_002",
                                                                               Totalmaleunder5 = "B01001_003",
                                                                               Totalfemaleunder5 = "B01001_027",
                                                                               Totalhealthcoverage = "B27001_001",
                                                                               TotalcoverageMale = "B27001_002",
                                                                               TotalcoverageMaleUnder6 = "B27001_003",
                                                                               TotalcoverageMale65to74 = "B27001_024",
                                                                               TotalcoverageMale75andolder = "B27001_027",
                                                                               Totalcoveragefemale = "B27001_030",
                                                                               TotalcoveragefemaleUnder6 = "B27001_031",
                                                                               Totalcoveragefemale65to74 = "B27001_052",
                                                                               Totalcoveragefemale75andolder = "B27001_055"), 
                             year = 2018, survey = "acs5", geometry = TRUE,output= "wide") # originally geometry was false
 
# get population for period 2005 to 2009 to get growth percent in each county
countypop2009 <- get_acs(geography = "county", state = NULL, variables = c(totalpop2009 ="B02001_001"), year = 2009, output = "wide", survey = "acs5", geometry = FALSE)
##############################################################################################
##############################################################################################
# Get final dataframe now have ability to find percent growth between 2005 to 2018
countyfinal <- left_join(countyinformation,countypop2009,by=c("GEOID","NAME"))

# rename for ease
countyinformation <- countyfinal
##############################################################################################
##############################################################################################
# Grab the fips code and make it a column from GEOID
countyinformation2 <- countyinformation %>%
  mutate(CountyFIPS = str_sub(GEOID, 1, 5))

##############################################################################################
##############################################################################################
# Transform dataframe to get percent of people above 65 yearold and percent of county that is below poverty
countyinformation3 <- countyinformation2 %>%
  mutate(percentbelowpoverty = totalincomebelowpovertyE/totalpoverty12monE,
         Older64Male = Malepop65and66yearsoldE + Totalmale67to69yearsoldE + Totalmale70to74yearsoldE +Totalmale75to79yearsoldE + Totalmale80to85yearsoldE + Totalmale85yearsandoverE,
         Older64Female = Femalepop65and66yearsoldE + TotalFemale67to69yearsoldE + Totalfemale70to74yearsoldE + Totalfemale75to79yearsoldE + Totalfemale80to85yearsoldvE + Totalfemale85yearsandoverE,
         totalpopulation = totalpop2018E,
         Total65orover = Older64Female + Older64Male,
         Percentunder5 = (Totalmaleunder5E +Totalfemaleunder5E)/EstimatetotalRaceE,
         PercentnonWhite = (EstimatetotalRaceE - EstimatetotalnonWhiteE)/EstimatetotalRaceE,
         PercentcoveredinsuranceInvulnerableAge = (TotalcoverageMaleUnder6E + TotalcoverageMale65to74E + TotalcoverageMale75andolderE + TotalcoveragefemaleUnder6E + Totalcoveragefemale65to74E + Totalcoveragefemale75andolderE)/TotalhealthcoverageE,
         PercentIncreaseInPopulation =(totalpop2018E - totalpop2009E)/totalpop2018E,
         TotalIncreaseInPopulation = totalpop2018E - totalpop2009E,
         PopulationToGrowthRateRatio = 1-(totalpopulation-TotalIncreaseInPopulation)/totalpopulation,
         PercentofpopOlder64 = (Older64Male + Older64Female)/totalpopulation) %>%
  select(NAME, GEOID, percentbelowpoverty, Older64Male, Older64Female, totalpopulation,Total65orover,Percentunder5,PercentnonWhite,PercentcoveredinsuranceInvulnerableAge,PercentIncreaseInPopulation,TotalIncreaseInPopulation,PopulationToGrowthRateRatio,PercentofpopOlder64,geometry)
##############################################################################################
##############################################################################################

# Z SCORE STANDARDIZTION HIGHER THE NUMBER THE MORE EXTREME THE POVERTY/AMOUNT OF OLDER 64 ETC.....
countyinformation4 <- countyinformation3 %>%
  mutate(
    z_Poverty = (percentbelowpoverty - mean(percentbelowpoverty, na.rm = TRUE))/
      sd(percentbelowpoverty, na.rm = TRUE),
    z_NonWhite = (PercentnonWhite - mean(PercentnonWhite, na.rm = TRUE))/
      sd(PercentnonWhite, na.rm = TRUE),
    z_Under5 = (Percentunder5 - mean(Percentunder5, na.rm = TRUE))/
      sd(Percentunder5, na.rm = TRUE),
    z_Over64 = (PercentofpopOlder64 - mean(PercentofpopOlder64, na.rm = TRUE))/
      sd(PercentofpopOlder64, na.rm = TRUE),
    z_CoveredVulnerableAge = (PercentcoveredinsuranceInvulnerableAge - mean(PercentcoveredinsuranceInvulnerableAge, na.rm = TRUE))/
      sd(PercentcoveredinsuranceInvulnerableAge, na.rm = TRUE))%>%
  select(NAME, GEOID, z_Poverty, z_NonWhite, z_Under5, z_Over64,z_CoveredVulnerableAge,totalpopulation,PercentIncreaseInPopulation,TotalIncreaseInPopulation,PopulationToGrowthRateRatio,PercentcoveredinsuranceInvulnerableAge,geometry)

##############################################################################################
##############################################################################################

# Z SCORE STANDARDIZTION HIGHER THE NUMBER THE MORE VULNERABLE THE COUNTY IS THE NEGATIVE NUMBERS MEAN LESS VULNERABLE
countySocialVulnerability <- countyinformation4 %>%
  mutate(VulIndex = (z_Poverty + z_NonWhite + z_Under5 + z_Over64)/4) %>%
  select(NAME, GEOID, z_Poverty, z_NonWhite, z_Under5, z_Over64,z_CoveredVulnerableAge,totalpopulation, VulIndex,PercentIncreaseInPopulation,PercentcoveredinsuranceInvulnerableAge,TotalIncreaseInPopulation,PopulationToGrowthRateRatio,geometry)

##############################################################################################
##############################################################################################
# Remove Alaska and Hawaii
vulplot <- filter(countySocialVulnerability, GEOID != "02016" & GEOID != "02130"& GEOID != "02180"& GEOID != "02282" & GEOID != "02090" & GEOID != "02158" & GEOID != "02240" & GEOID != "02050"& GEOID != "02195" & GEOID != "02100" & GEOID != "02170" & GEOID != "02110" & GEOID != "02150" & GEOID != "02220" & GEOID != "02261" & GEOID != "02020" & GEOID != "02060" & GEOID != "02068" & GEOID != "02188" & GEOID != "02230" & GEOID != "02275" & GEOID != "02013" & GEOID != "02185" & GEOID != "02122" & GEOID != "02164" & GEOID != "02198" & GEOID != "02105" & GEOID != "02290" & GEOID != "02070"  & GEOID != "15007" & GEOID != "15001" & GEOID != "15009" & GEOID != "15003" & GEOID != "15005")

 cuts <- c(-1,-0.47228210, -0.35129798, -0.25875179, -0.16936781, -0.08596625, 0.00455417,0.12415948,0.27284847,0.58412330,1.25747859,2)
# STATIC MAP FIner grade
 
 tm_shape(vulplot, projection = 2163) +
   tm_polygons("VulIndex",
               breaks = cuts,
               palette = "seq", 
               border.col = "white", 
               border.alpha = 0.5) +
   tm_legend(legend.position = c("left", "bottom")) +
   tm_layout(title = "Social Vulnerability Index (Dark=Vulnerable) ",
             title.size = 1.1,
             title.position = c("center", "top"))

 # Reverse color scheme high vulnerability is light color
 tm_shape(vulplot, projection = 2163) +
   tm_polygons("VulIndex",
               breaks = cuts,
               palette =  "-BuPu", 
               border.col = "white", 
               border.alpha = 0.5) +
   tm_legend(legend.position = c("left", "bottom")) +
   tm_layout(title = "Social Vulnerability Index (Light=Vulnerable) ",
             title.size = 1.1,
             title.position = c("center", "top"))
 ##########################################################################################################################################################################################################
 ##########################################################################################################################################################################################################
 ##########################################################################################################################################################################################################
# Filter for top vulerable counties
 top_vulnerable1 <- countySocialVulnerability[order(-countySocialVulnerability$totalpopulation) , ]; top_vulnerable1 <- top_vulnerable1[1:50,];
 
# Filter for top 50 counties
 fplot <- filter(top_vulnerable1, GEOID != "02016" & GEOID != "02130"& GEOID != "02180"& GEOID != "02282" & GEOID != "02090" & GEOID != "02158" & GEOID != "02240" & GEOID != "02050"& GEOID != "02195" & GEOID != "02100" & GEOID != "02170" & GEOID != "02110" & GEOID != "02150" & GEOID != "02220" & GEOID != "02261" & GEOID != "02020" & GEOID != "02060" & GEOID != "02068" & GEOID != "02188" & GEOID != "02230" & GEOID != "02275" & GEOID != "02013" & GEOID != "02185" & GEOID != "02122" & GEOID != "02164" & GEOID != "02198" & GEOID != "02105" & GEOID != "02290" & GEOID != "02070"  & GEOID != "15007" & GEOID != "15001" & GEOID != "15009" & GEOID != "15003" & GEOID != "15005")
 
 # Get county information
 data("county_laea", package = "tidycensus")
 county <- county_laea
 
 ############################
  #Interactive
  mapview(county)
 mapview(fplot, zcol="VulIndex",legend= TRUE)
 mapview(fplot, zcol="totalpopulation",legend= TRUE)
 mapview(fplot, zcol="PopulationToGrowthRateRatio",legend= TRUE)
 
 PopulationToGrowthIndex
 ##########################################################################################################################################################################################################
 ##########################################################################################################################################################################################################
 ##########################################################################################################################################################################################################
 # Out of top 50 vulnerable counties which ones have the highest 
 #Get population for each county from 2010 to 2018
 # Higher populaiton the higher the number
 top_PopulationToGrowthRate <- fplot[order(-fplot$PopulationToGrowthRateRatio) , ]; top_PopulationToGrowthRatio <- top_PopulationToGrowthRatio[1:30,];
 mapview(top_PopulationOutOfThose, zcol="PopulationToGrowthRateRatio",legend= TRUE)
 
# Top population out of those
top_PopulationOutOfThose <- top_PopulationToGrowthRate[order(-top_PopulationToGrowthRate$totalpopulation) , ]; top_PopulationOutOfThose <- top_PopulationOutOfThose[1:10,];
mapview(top_PopulationOutOfThose, zcol="totalpopulation",legend= TRUE)

 
 #48261  48301  
 ##########################################################################################################################################################################################################
 ##########################################################################################################################################################################################################
 ##########################################################################################################################################################################################################
 # Zoom in on the tract in side the top county
 #NAME                 GEOID  z_Poverty z_NonWhite  z_Under5   z_Over64
 # Kenedy County, Texas 48261 -1.1343675 -0.7987254  6.903314  0.9872944
 
HarrisCountyTract <- get_acs(geography = "tract"
                       , state = "MI",
                       county = "Calhoun",
                       variables = c(totalpop2018 = "B01003_001",
                                     Totalhealthcoverage = "B27001_001",
                                     TotalcoverageMale = "B27001_002",
                                     TotalcoverageMaleUnder6 = "B27001_003",
                                     TotalcoverageMale65to74 = "B27001_024",
                                     TotalcoverageMale75andolder = "B27001_027",
                                     Totalcoveragefemale = "B27001_030",
                                     TotalcoveragefemaleUnder6 = "B27001_031",
                                     Totalcoveragefemale65to74 = "B27001_052",
                                     Totalcoveragefemale75andolder = "B27001_055"),output = "wide",geometry = TRUE)

mapview(HarrisCountyTract, zcol="TotalhealthcoverageE",legend= TRUE)



 # Edit
HarrisCountyTract2 <- HarrisCountyTract %>%
   mutate(PercentcoveredinsuranceInvulnerableAge = (TotalcoverageMaleUnder6E + TotalcoverageMale65to74E + TotalcoverageMale75andolderE + TotalcoveragefemaleUnder6E + Totalcoveragefemale65to74E + Totalcoveragefemale75andolderE)/TotalhealthcoverageE) %>%
   select(NAME, GEOID,totalpop2018E,TotalhealthcoverageE, TotalcoverageMaleE,TotalcoverageMaleUnder6E,TotalcoverageMale65to74E,TotalcoverageMale75andolderE,TotalcoveragefemaleE,TotalcoveragefemaleUnder6E,Totalcoveragefemale65to74E,Totalcoveragefemale75andolderE,PercentcoveredinsuranceInvulnerableAge,TotalhealthcoverageE,geometry)

 
 mapview(HarrisCountyTract2, zcol = "PercentcoveredinsuranceInvulnerableAge", legend = TRUE)

 ##########################################################################################################################################################################################################
 ##########################################################################################################################################################################################################
 ##########################################################################################################################################################################################################
 # Go to tract level 
 # UNIQUE MAP!!!!!!!!!!!!!!!!!
 harrison_estimates <- get_estimates(geography = "county",
                              product = "characteristics",
                              breakdown = c("SEX", "AGEGROUP", "HISP","RACE"),
                              breakdown_labels = TRUE,
                              state = "TX",
                              county = "Harrison")

# TO GET ESTIMATES OF POPULATION CHARACTERISTICS
compare <- filter(harrison_estimates, str_detect(AGEGROUP, "^Age"),
                   HISP != "Both Hispanic Origins",
                   SEX != "Both sexes") %>%
   mutate(value = ifelse(SEX == "Male", -value, value)) 

####################################################
# PYRIMID PLOT POPULATION FOR EACH AGE GROUP
SEX <- compare$SEX
AGE <- compare$AGEGROUP
VALUE <- compare$value
plot_ly(compare,x = VALUE, y = AGE, color  = SEX, type = 'bar', orientation = 'h',
        hoverinfo = 'y+text+name', text = VALUE) %>%
  layout(bargap = 0.1, barmode = 'overlay',
         xaxis = list(tickmode = 'array', tickvals =  c(-2500, -2000,-1500,-1000, 0,1000,1500,2000,2500),
                      ticktext = c(-2500, -2000,-1500,-1000, 0,1000,1500,2000,2500)))

#################################################################################################
##############################################################################################
# TO GET ESTIMATES OF POPULATION CHARACTERISTICS
compare <- filter(harrison_estimates, str_detect(AGEGROUP, "^Age"),
                  HISP != "Both Hispanic Origins",
                  SEX != "Both sexes") %>%
  mutate(value = ifelse(SEX == "Male", -value, value)) 

#####################################################
# PYRIMID PLOT POPULATION FOR EACH RACE GROUP
SEX <- compare$SEX
RACE <- compare$RACE
VALUE <- compare$value
plot_ly(compare,x = VALUE, y = RACE, color  = SEX, type = 'bar', orientation = 'h',
        hoverinfo = 'y+text+name', text = VALUE) %>%
  layout(bargap = 0.1, barmode = 'overlay',
         xaxis = list(tickmode = 'array', tickvals =  c(-2500, -2000,-1500,-1000, 0,1000,1500,2000,2500),
                      ticktext = c(-2500, -2000,-1500,-1000, 0,1000,1500,2000,2500)))

#######################################################################################################################################################
#######################################################################################################################################################
#######################################################################################################################################################
# EXTRACT COVID INFORMATION 
  
  download <- getURL("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv")
  download1 <- getURL("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv")
  # download2 <- getURL("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv")
  
  # Make raw data in to csv
  confirmed <- read.csv (text = download)
  deaths <- read.csv (text = download1)
  #recovered <- read.csv (text = download2)
  
  # Reshape dataframe from wide to long
  # change the X6.30.20 to the most recent datae
  df1 <- confirmed %>% gather(date, confirmed, X1.22.20:ncol(confirmed)) # This makes sure it updates to the newest day..
  df2 <- deaths %>% gather(date, deaths, X1.22.20:ncol(deaths))
  #df3 <- recovered %>% gather(date, recovered, X1.22.20:ncol(recovered))
  
  df1 <- as.data.frame(df1)
  df2 <- as.data.frame(df2)
  #df3 <- as.data.frame(df3)
  
  #remove X with " " and . with /
  df1$date <- gsub("X","",df1$date)
  df2$date <- gsub("X","",df2$date)
  #df3$date <- gsub("X","",df3$date)
  df1$date <- gsub("\\.","/",df1$date)
  df2$date <- gsub("\\.","/",df2$date)
  #df3$date <- gsub("\\.","/",df3$date)
  
  
  # ADD 20 to the end of the date to make it 2020
  n = 2;
  df1$date = paste(df1$date,rep(2, n), sep = "")
  df1$date = paste(df1$date,rep(0, n), sep = "")
  df2$date = paste(df2$date,rep(2, n), sep = "")
  df2$date = paste(df2$date,rep(0, n), sep = "")
  #df3$date = paste(df3$date,rep(2, n), sep = "")
  #df3$date = paste(df3$date,rep(0, n), sep = "")
  
  # make date into same format
  df1$date <- as.Date(df1$date, format = "%m/%d/%Y")
  df2$date <- as.Date(df2$date, format = "%m/%d/%Y")
  
  # merge by city and date
  covidusa2<- merge(df1,df2, by = c("UID","date"), all.x = TRUE)
  covidusa2 <- covidusa2[c(6,2,8,18,10,11,13,25)]
  
  
  # change columnnames
  colnames(covidusa2)[1] <- "GEOID"
  colnames(covidusa2)[2] <- "date"
  colnames(covidusa2)[3] <- "state"
  colnames(covidusa2)[4] <- "county"
  colnames(covidusa2)[5] <- "lat"
  colnames(covidusa2)[6] <- "lon"
  colnames(covidusa2)[7] <- "confirmed"
  colnames(covidusa2)[8] <- "deaths"
  
  final_covid <- covidusa2

  write.csv(final_covid,file="final_covid.csv")
  
#######################################################################################
#######################################################################################
# Extract covid information for Harris County USA
  install.packages("hrbrthemes",lib = "C:/R/R-4.0.2/library")
  library(hrbrthemes)
final_covid_harrison <- final_covid[final_covid$state =="Texas" & final_covid$county == "Harrison",]

  # Usual area chart
  p <- final_covid_harrison %>%
    ggplot( aes(x=date, y=confirmed)) +
    geom_area(fill="#69b3a2", alpha=0.5) +
    geom_line(color="#69b3a2") +
    ylab("Covid confirmed cases") +
    ggtitle("Harrison county confirmed cases") + 
    theme(plot.title = element_text(lineheight=.8, face="bold"))
    theme_ipsum()
  
  # Turn it interactive with ggplotly
  p <- ggplotly(p)
  p

################################################################################################################################
################################################################################################################################
################################################################################################################################
  # Google Trends information for county
  today <- Sys.time()
  ftoday <- paste(today)
  ftoday <- strsplit(ftoday," ")[[1]][1] #Split the date and get it into proper format
  var3 <- paste("2020-01-22",ftoday)
  
  # Get information for Texas on these key words
  res <- gtrends(c("Covid-19","Coronavirus","Virus","Vaccine","Stryker Medical"), geo = c("US-TX"),time = var3)
  plot(res)
  
  
 # Can we sell them beds
  # Get information for Texas on these key words
  res2 <- gtrends(c("Hosptial beds","Surgial knives","Stryker Medical"), geo = c("US-TX"),time = var3)
  plot(res2)

  res3 <- data.frame(res2$interest_over_time)  
  res4 <- data.frame(res2$interest_by_dma)
  res5 <- data.frame(res2$interest_by_city)
  res6 <- data.frame(res2$related_queries)
  
  ################################################################################################################################
  ################################################################################################################################
  ################################################################################################################################  
  # United states mobility 
  downloadz <- getURL("https://raw.githubusercontent.com/ActiveConclusion/COVID19_mobility/master/google_reports/mobility_report_US.csv")
  mobility <- read.csv (text = downloadz);# get mobility data
  mobility <- mobility[mobility$county != "Total",]; # remove total
  mobility <- data.frame(mobility); # make dataframe for manipulation
  mobility[is.na(mobility)]<-0  # Na values get 0
  data(fips_codes); fips_codes <- data.frame(fips_codes); fips_codes <- fips_codes[c(3,5,1,2,4)];colnames(fips_codes)[1]<-"state"; colnames(fips_codes)[3] <- "state_abbr"
  mobility2 <- merge(fips_codes,mobility,by = c("state","county"),all.x = TRUE) # merge by county name and state name
  mobility2$GEOID <- paste(mobility2$state_code,mobility2$county_code,sep="")

  # Merge datasets 
  countySocialVulnerability2 <- countySocialVulnerability %>% separate(NAME, c('county', 'state'), sep=",")
  
  # Filter for date 
  # Filter for a specific plot
  mobilityfin <- mobility2[mobility2$date == "2020-03-23",]
  
  mobilityfin <- na.omit(mobilityfin); mobilityfin <- mobilityfin[c(2,1,13,6,7,8,9,10,11,12)]
  
  # Join dataset
  countySocialVulnerability3 <- left_join(countySocialVulnerability2,mobilityfin,by=c("GEOID"))
  countySocialVulnerability3[countySocialVulnerability3$date,] <- "2020-03-23";
  dt  <- mutate(countySocialVulnerability3 , parks = ifelse(is.na(parks), 0, parks));dt1  <- mutate(dt , retail.and.recreation = ifelse(is.na(retail.and.recreation), 0, retail.and.recreation)); dt2  <- mutate(dt1 , grocery.and.pharmacy = ifelse(is.na(grocery.and.pharmacy), 0, grocery.and.pharmacy));dt3  <- mutate(dt2 , transit.stations = ifelse(is.na(transit.stations), 0, transit.stations));dt4  <- mutate(dt3 , workplaces = ifelse(is.na(workplaces), 0, workplaces));countySocialVulnerability3  <- mutate(dt4 , residential = ifelse(is.na(residential), 0, residential))
  countySocialVulnerability4  <- mutate(countySocialVulnerability3 , date = ifelse(is.na(date), "2020-03-23", date))
  
    # Plot
  mapview(countySocialVulnerability4, zcol="parks",legend= TRUE)
  mapview(countySocialVulnerability4, zcol="workplaces",legend= TRUE) 
  ################################################################################################################################
  ################################################################################################################################
  ################################################################################################################################  
 