# Load packages
require(lubridate)
require(data.table)
require(zoo)
require(riem)
require(dplyr)
require(tidyr)
require(countrycode)
require(RCurl)
require(stringr)
require(tidyverse)
require(shiny)
require(miniUI)
install.packages("taskscheduleR")
require(taskscheduleR)
require(gtrendsR)
###################################################################################################################################
###################################################################################################################################
###################################################################################################################################
# set working directory
setwd("C:/Users/blake/OneDrive/GITHUBJUPITERPROJECT/Worldwide Covid")


final_df <- read.csv("finaldataframe.csv")
final_df <- final_df[!is.na(final_df$country),]
final_df$date <- as.Date(final_df$date)
final_df<- final_df[-c(1)]


# Get the system date and see if it is larger than the date in the dataframe up until now
today <- Sys.time()
fmax <- max(final_df$date)
# ifelse(today > max(final_df$date),"TRUE","FALSE")

if(today > fmax){
  # This is the covid_count_update
  {
  # Get data directly from the url at john hopkins
  download <- getURL("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
  download1 <- getURL("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")
  download2 <- getURL("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv")
  # Make raw data in to csv
  confirmed <- read.csv (text = download)
  deaths <- read.csv (text = download1)
  recovered <- read.csv (text = download2)
  # Reshape dataframe from wide to long
  # change the X6.30.20 to the most recent datae
  df1 <- confirmed %>% gather(date, confirmed, X1.22.20:ncol(confirmed)) # This makes sure it updates to the newest day..
  df2 <- deaths %>% gather(date, deaths, X1.22.20:ncol(deaths))
  df3 <- recovered %>% gather(date, recovered, X1.22.20:ncol(recovered))
  
  # aggregate by country
  temp1 <- aggregate(df1[,c(6)], by =list(df1$Country.Region,df1$date), sum)
  temp2 <- aggregate(df2[,c(6)], by =list(df2$Country.Region,df2$date), sum)
  temp3 <- aggregate(df3[,c(6)], by =list(df3$Country.Region,df3$date), sum)
  #remove X with " " and . with /
  temp1$Group.2 <- gsub("X"," ",temp1$Group.2)
  temp2$Group.2 <- gsub("X"," ",temp2$Group.2)
  temp3$Group.2 <- gsub("X"," ",temp3$Group.2)
  temp1$Group.2 <- gsub("\\.","/",temp1$Group.2)
  temp2$Group.2 <- gsub("\\.","/",temp2$Group.2)
  temp3$Group.2 <- gsub("\\.","/",temp3$Group.2)
  # change column names
  colnames(temp1)[1]<- "country"
  colnames(temp1)[2]<- "date"
  colnames(temp1)[3]<- "confirmed"
  colnames(temp2)[1]<- "country"
  colnames(temp2)[2]<- "date"
  colnames(temp2)[3]<- "deaths"
  colnames(temp3)[1]<- "country"
  colnames(temp3)[2]<- "date"
  colnames(temp3)[3]<- "recovered"
 # Combine dataset
  merge1 <- merge(temp1,temp2, by = c("country","date"), all.x = TRUE)
  merge2 <- merge(merge1,temp3, by = c("country","date"),all.x = TRUE)
  covid_count <- merge2
  # write.csv(covid_count,file="covid_count.csv")
  # ADD 20 to the end of the date to make it 2020
  n = 2;
  covid_count$date = paste(covid_count$date,rep(2, n), sep = "")
  covid_count$date = paste(covid_count$date,rep(0, n), sep = "")
  # Trim white space before the date
  covid_count$date <- trimws(covid_count$date)
  # Convert date to year month day
  covid_count$date <- as.Date(covid_count$date, format = "%m/%d/%Y")
  # Keep only columns that are after last remaining grab
  covid_count <- covid_count[covid_count$date > fmax,]
  # Unique countries from github file 
  ucountries <- unique(covid_count$country)
    # you input ucountries as the country name format and the outcome is three letter abriviation for the country 
  codes <- countrycode(ucountries, 'country.name', destination = "iso3c")
    # Make the ucountries and codes as dataframe 
  dat <- data.frame(ucountries, codes)
    # rename first column that was ucountries to country for mergeing purposes
  colnames(dat)[1] <- "country"
    # merge dataset by the codes vector
  covid_count_f <- merge(covid_count,dat, by = c("country"), all.x = TRUE)
  covid_count_f <- covid_count_f[c(6,2,3,4,5)]
  colnames(covid_count_f)[1] <- "country"
  write.csv(covid_count_f,file="covid_update.csv")
} # output is covid_count_f
###################################################################################################################################
  #Initiate search terms
  var = "Covid-19"; var.1 ="Coronavirus"; var.2 ="sars-cov-2";
  ftoday <- paste(today)
  ftoday <- strsplit(ftoday," ")[[1]][1] #Split the date and get it into proper format
  newfmax <- fmax - 1 # get one day greater than the max previous dataframe
  fdate <- paste(newfmax,ftoday) # get the date format needed for the gtrends stuff
  var2 <- fdate
  var2 <- "2020-01-22 2020-08-02"
  {
    ##############################################################################################################################
    ##############################################################################################################################
    con <- "AO"; con2 <- "AI"; con3 <- "AQ"; con4 <-"AG"; con5 <-"AR"; con6 <- "BO";con7 <-"BA"; con8 <- "BW"; con9 <-"BV";con10 <- "BR";con11 <- "IO"; con12 <- "BN";con13 <- "BG"; con14 <- "BF";
    con15 <-"BI"; con16 <- "CF"; con17 <- "TD"; con18 <- "CL";con19 <- "CX"; con20 <- "CC"; con21 <- "CO";con22 <- "KM"; con23 <- "CG"; con24 <-"PF";con25 <- "TF"; con26 <- "GA"; con27 <- "GE";
    con28 <- "GM"; con29 <- "GN"; con30 <- "GS"; con31 <- "GW"; con32 <- "GY"; con33 <- "HK"; con34 <- "HM"; con35 <- "HN"; con36 <- "HT";con37 <- "FM";con38 <- "MD";con39 <- "MC"; con40 <- "MN";
    con41 <- "MS"; con42 <- "NE";con43 <- "NG"; con44 <- "NU"; con45 <- "NF"; con46 <- "MP"; con47 <- "PG";con48 <- "PY"; con49 <- "PE"; con50 <-"PH";con51 <-"PN";con52 <-"GS";con53 <- "ES"; con54 <- "LK";
    con55 <- "SD"; con56 <- "SR"; con57 <- "TK"; con58 <- "TO";con59 <- "TT"; con60 <- "TN"; con61 <- "TR"; con62 <- "TM"; con63 <- "TC"; con64 <- "TV"; con65 <- "UG"; con66 <- "UA"; con67 <- "VE";
    con68 <- "VN"; con69 <- "VG";con70 <- "VI"; con71 <- "WF"; con72 <- "SS"; con73 <- "TL"; con74 <- "UM"; con75 <- "AX"; con76 <- "BL"; con77 <- "CW"; con78 <- "GG"; con79 <- "IM"; con80 <- "JE";
    con81 <-"MF";  conz <- "AN";con1z <-"AX";  con2z <- "BL"; con3z <- "BO";con4z <- "CD"; con5z <-"ES";con6z <- "HU";con7z <- "IO";con8z <- "IS"; con9z <- "KM"; con10z <- "MP"; con11z <- "MR";con12z <-"PF"; con13z <- "SN";
    con14z <- "TD"; con15z <- "TP";con16z <- "TR"; con17z <- "UM"; con18z <- "VA"; con19z <- "XZ"; con20z <- "YU";
    
    
    reserf <- NULL; reserf1 <- NULL;reserf2 <- NULL;reserf3 <-NULL;reserf4 <- NULL;reserf5 <- NULL;reserf6 <- NULL;reserf7<- NULL;reserf8<- NULL;reserf9 <- NULL; reserf10 <- NULL;
    reserf11 <- NULL;reserf12 <- NULL;reserf13 <-NULL; reserf14 <- NULL; reserf15 <- NULL;  reserf16 <- NULL; reserf17 <- NULL; reserf18 <- NULL; reserf19 <- NULL; reserf20 <- NULL;
    reserf21 <- NULL;  reserf22 <- NULL; reserf23 <-NULL;reserf24 <-NULL; reserf25 <- NULL; reserf26 <- NULL; reserf27 <- NULL; reserf28 <- NULL; reserf29 <- NULL;  reserf30 <- NULL;
    reserf31 <- NULL; reserf32 <- NULL; reserf33 <- NULL; reserf34 <- NULL; reserf35 <- NULL; reserf36 <- NULL; reserf37 <- NULL; reserf38 <- NULL; reserf39 <- NULL; reserf40 <- NULL;
    reserf41 <- NULL; reserf42 <- NULL; reserf43 <- NULL;reserf44 <- NULL;reserf45 <- NULL;reserf46 <- NULL;reserf47 <- NULL;reserf48 <- NULL;reserf49 <- NULL;reserf50 <- NULL;
    res_final <- NULL;res_final2 <- NULL;res_final3 <-NULL; res_final4 <-NULL; res_final5 <- NULL;  res_final6 <- NULL;res_final7 <- NULL; res_final8 <- NULL;  res_final9 <- NULL; res_final10 <- NULL; res_final11 <- NULL;
    res_final12 <- NULL; res_final13 <- NULL; res_final14 <- NULL;  res_final15 <- NULL;  res_final16 <- NULL; final_trends <- NULL;
    
    reserf0 <- NULL;reserfz <- NULL; reserf1z <- NULL;reserf2z <- NULL;reserf3z <-NULL;reserf4z <- NULL;reserf5z <- NULL;reserf6z <- NULL;reserf7z<- NULL;reserf8z<- NULL;reserf9z <- NULL; reserf10z <- NULL;
    reserf11z <- NULL;res_finalz <- NULL;res_final2z <- NULL;res_final3z <-NULL; final_trends_2 <- NULL;
    
    
    temp_1 <- NULL;temp_2<- NULL;temp_3 <- NULL;temp_4<- NULL;temp_5 <- NULL;temp_6 <- NULL;temp_7 <- NULL;temp_8<- NULL;temp_9 <- NULL;
    temp_10<- NULL;temp_11 <- NULL;temp_12 <- NULL;temp_13 <- NULL;temp_14 <- NULL;temp_15 <- NULL;temp_16 <- NULL;temp_17 <- NULL;temp_18 <- NULL;
    temp_19 <- NULL;temp_20<- NULL;temp_21 <- NULL;temp_22<- NULL;temp_23 <- NULL;temp_24 <- NULL;temp_25 <- NULL;temp_26<- NULL;temp_27 <- NULL;
    temp_28<- NULL;temp_29 <- NULL;temp_30 <- NULL;temp_31 <- NULL;temp_32 <- NULL;temp_33 <- NULL;temp_34 <- NULL;temp_35 <- NULL;
    temp_36 <- NULL;temp_37<- NULL;temp_38 <- NULL;temp_39 <- NULL;temp_40 <- NULL;
    temp_a <- NULL;temp_b<- NULL;temp_c <- NULL;temp_d<- NULL;temp_e <- NULL;temp_f <- NULL;temp_g <- NULL;temp_h<- NULL;temp_i <- NULL;
    temp_j<- NULL;temp_k <- NULL;temp_l <- NULL;
    
    
    reserz <- NULL;  reser0z <- NULL;  reser1z <- NULL;
    reser2z <- NULL;  reser3z <- NULL;  reser4z <- NULL;
    reser5z <- NULL;  reser6z <- NULL;  reser7z <- NULL;
    reser8z <- NULL;  reser9z <- NULL;  reser10z <- NULL;
    reser11z <- NULL;  reser12z <- NULL;  reser13z <- NULL;
    reser14z <- NULL;  reser15z <- NULL;  reser16z <- NULL;
    reser17z <- NULL;  reser18z <- NULL;  reser19z <- NULL;
    reser20z <- NULL;  reser21z <- NULL;  reser22z <- NULL;
    reser23z <- NULL;  reser24z <- NULL;  reser25z <- NULL;
    reser26z <- NULL;  reser27z <- NULL;  reser28z <- NULL;
    reser29z <- NULL;  reser30z <- NULL;  reser31z <- NULL;
    reser32z <- NULL;  reser33z <- NULL;  reser34z <- NULL;
    reser35z <- NULL;  reser36z <- NULL;  reser37z <- NULL;
    reser38z <- NULL;  reser39z <- NULL;  reser40z <- NULL;
    reser41z <- NULL;  reser42z <- NULL;  reser43z <- NULL;
    reser44z <- NULL;  reser45z <- NULL;  reser46z <- NULL;
    reser47z <- NULL;  reser48z <- NULL;  reser49z <- NULL;
    reser50z <- NULL;  reser51z <- NULL;  reser52z <- NULL;
    reser53z <- NULL;  reser54z <- NULL;  reser55z <- NULL;
    reser56z <- NULL;  reser57z <- NULL;  reser58z <- NULL;
    reser <- NULL;  reser0 <- NULL;  reser1 <- NULL;
    reser2 <- NULL;  reser3 <- NULL;  reser4 <- NULL;
    reser5 <- NULL;  reser6 <- NULL;  reser7 <- NULL;
    reser8 <- NULL;  reser9 <- NULL;  reser10 <- NULL;
    reser11 <- NULL;  reser12 <- NULL;  reser13 <- NULL;
    reser14 <- NULL;  reser15 <- NULL;  reser16 <- NULL;
    reser17 <- NULL;  reser18 <- NULL;  reser19 <- NULL;
    reser20 <- NULL;  reser21 <- NULL;  reser22 <- NULL;
    reser23 <- NULL;  reser24 <- NULL;  reser25 <- NULL;
    reser26 <- NULL;  reser27 <- NULL;  reser28 <- NULL;
    reser29 <- NULL;  reser30 <- NULL;  reser31 <- NULL;
    reser32 <- NULL;  reser33 <- NULL;  reser34 <- NULL;
    reser35 <- NULL;  reser36 <- NULL;  reser37 <- NULL;
    reser38 <- NULL;  reser39 <- NULL;  reser40 <- NULL;
    reser41 <- NULL;  reser42 <- NULL;  reser43 <- NULL;
    reser44 <- NULL;  reser45 <- NULL;  reser46 <- NULL;
    reser47 <- NULL;  reser48 <- NULL;  reser49 <- NULL;
    reser50 <- NULL;  reser51 <- NULL;  reser52 <- NULL;
    reser53 <- NULL;  reser54 <- NULL;  reser55 <- NULL;
    reser56 <- NULL;  reser57 <- NULL;  reser58 <- NULL;
    reser59 <- NULL;  reser60 <- NULL;  reser61 <- NULL;
    reser62 <- NULL;  reser63 <- NULL;  reser64 <- NULL;
    reser65 <- NULL;  reser66 <- NULL;  reser67 <- NULL;
    reser68 <- NULL;  reser69 <- NULL;  reser70 <- NULL;
    reser71 <- NULL;  reser72 <- NULL;  reser73 <- NULL;
    reser74 <- NULL;  reser75 <- NULL;  reser76 <- NULL;
    reser77 <- NULL;  reser78 <- NULL;  reser79 <- NULL;
    reser80 <- NULL;  reser81 <- NULL;  reser82 <- NULL;
    reser83 <- NULL;  reser84 <- NULL;  reser85 <- NULL;
    reser86 <- NULL;  reser87 <- NULL;  reser88 <- NULL;
    reser89 <- NULL;  reser90 <- NULL;  reser91 <- NULL;
    reser92 <- NULL;  reser93 <- NULL;  reser94 <- NULL;
    reser95 <- NULL;  reser96 <- NULL;  reser97 <- NULL;
    reser98 <- NULL;  reser99 <- NULL;  reser100 <- NULL;
    reser101 <- NULL;  reser102 <- NULL;  reser103 <- NULL;
    reser104 <- NULL;  reser105 <- NULL;  reser106 <- NULL;
    reser107 <- NULL;  reser108 <- NULL;  reser109 <- NULL;
    reser110 <- NULL;  reser111 <- NULL;  reser112 <- NULL;
    reser113 <- NULL;  reser114 <- NULL;  reser115 <- NULL;
    reser116 <- NULL;  reser117 <- NULL;  reser118 <- NULL;
    reser119 <- NULL;  reser120 <- NULL;  reser121 <- NULL;
    reser122 <- NULL;  reser123 <- NULL;  reser124 <- NULL;
    reser125 <- NULL;  reser126 <- NULL;  reser127 <- NULL;
    reser128 <- NULL;  reser129 <- NULL;  reser130 <- NULL;
    reser131 <- NULL;  reser132 <- NULL;  reser133 <- NULL;
    reser134 <- NULL;  reser135 <- NULL;  reser136 <- NULL;
    reser137 <- NULL;  reser138 <- NULL;  reser139 <- NULL;
    reser140 <- NULL;  reser141 <- NULL;  reser142 <- NULL;
    reser143 <- NULL;  reser144 <- NULL;  reser145 <- NULL;
    reser146 <- NULL;  reser147 <- NULL;  reser148 <- NULL;
    reser149 <- NULL;  reser150 <- NULL;  reser151 <- NULL;
    reser152 <- NULL;  reser153 <- NULL;  reser154 <- NULL;
    reser155 <- NULL;  reser156 <- NULL;  reser157 <- NULL;
    reser158 <- NULL;  reser159 <- NULL;  reser160 <- NULL;
    reser161 <- NULL;  reser162 <- NULL;  reser163 <- NULL;
    reser164 <- NULL;  reser165 <- NULL;  reser166 <- NULL;
    reser167 <- NULL;  reser168 <- NULL;  reser169 <- NULL;
    reser170 <- NULL;  reser171 <- NULL;  reser172 <- NULL;
    reser173 <- NULL;  reser174 <- NULL;  reser175 <- NULL;
    reser176 <- NULL;  reser177 <- NULL;  reser178 <- NULL;
    reser179 <- NULL;  reser180 <- NULL;  reser181 <- NULL;
    reser182 <- NULL;  reser183 <- NULL;  reser184 <- NULL;
    reser185 <- NULL;  reser186 <- NULL;  reser187 <- NULL;
    reser188 <- NULL;  reser189 <- NULL;  reser190 <- NULL;
    reser191 <- NULL;  reser192 <- NULL;  reser193 <- NULL;
    reser194 <- NULL;  reser195 <- NULL;  reser196 <- NULL;
    reser197 <- NULL;  reser198 <- NULL;  reser199 <- NULL;
    reser200 <- NULL;  reser201 <- NULL;  reser202 <- NULL;
    reser203 <- NULL;  reser204 <- NULL;  reser205 <- NULL;
    reser206 <- NULL;  reser207 <- NULL;  reser208 <- NULL;
    reser209 <- NULL;  reser210 <- NULL;  reser211 <- NULL;
    reser212 <- NULL;  reser213 <- NULL;  reser214 <- NULL;
    reser215 <- NULL;  reser216 <- NULL;  reser217 <- NULL;
    reser218 <- NULL;  reser219 <- NULL;  reser220 <- NULL;
    reser221 <- NULL;  reser222 <- NULL;  reser223 <- NULL;
    reser224 <- NULL;  reser225 <- NULL;  reser226 <- NULL;
    reser227 <- NULL;  reser228 <- NULL;  reser229 <- NULL;
    reser230 <- NULL;  reser231 <- NULL;  reser232 <- NULL;
    reser233 <- NULL;  reser234 <- NULL;  reser235 <- NULL;
    reser236 <- NULL;  reser237 <- NULL;  reser238 <- NULL;
    reser239 <- NULL;  reser240 <- NULL;  reser241 <- NULL;
    res0.0 <- NULL;res0.1 <- NULL;res0.2 <- NULL;
    res1.0 <- NULL;res1.1 <- NULL;res1.2 <- NULL;
    res2.0 <- NULL;res2.1 <- NULL;res2.2 <- NULL;
    res3.0 <- NULL;res3.1 <- NULL;res3.2 <- NULL;
    res4.0 <- NULL;res4.1 <- NULL;res4.2 <- NULL;
    res5.0 <- NULL;res5.1 <- NULL;res5.2 <- NULL;
    res6.0 <- NULL;res6.1<- NULL;res6.2 <- NULL;
    res7.0 <- NULL;res7.1 <- NULL;res7.2 <- NULL;
    res8.0 <- NULL;res8.1 <- NULL;res8.2 <- NULL;
    res9.0 <- NULL;res9.1 <- NULL;res9.2 <- NULL;
    res10.0 <- NULL;res10.1 <- NULL;res10.2 <- NULL;
    res11.0 <- NULL;res11.1 <- NULL;res11.2 <- NULL;
    res12.0 <- NULL;res12.1 <- NULL;res12.2 <- NULL;
    res13.0 <- NULL;res13.1 <- NULL;res13.2 <- NULL;
    res14.0 <- NULL;res14.1 <- NULL;res14.2 <- NULL;
    res15.0 <- NULL;res15.1 <- NULL;res15.2 <- NULL;
    res16.0 <- NULL;res16.1 <- NULL;res16.2 <- NULL;
    res17.0 <- NULL;res17.1 <- NULL;res17.2 <- NULL;
    res18.0 <- NULL;res18.1 <- NULL;res18.2 <- NULL;
    res19.0 <- NULL;res19.1 <- NULL;res19.2 <- NULL;
    res20.0 <- NULL;res20.1 <- NULL;res20.2 <- NULL;
    res21.0 <- NULL;res21.1 <- NULL;res21.2 <- NULL;
    res22.0 <- NULL;res22.1 <- NULL;res22.2 <- NULL;
    res23.0 <- NULL;res23.1 <- NULL;res23.2 <- NULL;
    res24.0 <- NULL;res24.1 <- NULL;res24.2 <- NULL;
    res25.0 <- NULL;res25.1 <- NULL;res25.2 <- NULL;
    res26.0 <- NULL;res26.1 <- NULL;res26.2 <- NULL;
    res27.0 <- NULL;res27.1 <- NULL;res27.2 <- NULL;
    res28.0 <- NULL;res28.1 <- NULL;res28.2 <- NULL;
    res29.0<- NULL;res29.1 <- NULL;res29.2 <- NULL;
    res30.0 <- NULL;res30.1 <- NULL;res30.2 <- NULL;
    res31.0 <- NULL;res31.1 <- NULL;res31.2 <- NULL;
    res32.0 <- NULL;res32.1 <- NULL;res32.2 <- NULL;
    res33.0 <- NULL;res33.1 <- NULL;res33.2 <- NULL;
    res34.0 <- NULL;res34.1 <- NULL;res34.2 <- NULL;
    res35.0 <- NULL;res35.1 <- NULL;res35.2 <- NULL;
    res36.0 <- NULL;res36.1 <- NULL;res36.2 <- NULL;
    res37.0 <- NULL;res37.1 <- NULL;res37.2 <- NULL;
    res38.0 <- NULL;res38.1 <- NULL;res38.2 <- NULL;
    res39.0 <- NULL;res39.1 <- NULL;res39.2 <- NULL;
    res40.0 <- NULL;res40.1 <- NULL;res40.2 <- NULL;
    res41.0 <- NULL;res41.1 <- NULL;res41.2 <- NULL;
    res42.0 <- NULL;res42.1 <- NULL;res42.2 <- NULL;
    res43.0 <- NULL;res43.1 <- NULL;res43.2 <- NULL;
    res44.0 <- NULL;res44.1 <- NULL;res44.2 <- NULL;
    res45.0 <- NULL;res45.1 <- NULL;res45.2 <- NULL;
    res46.0 <- NULL;res46.1 <- NULL;res46.2 <- NULL;
    res47.0 <- NULL;res47.1 <- NULL;res47.2 <- NULL;
    res48.0 <- NULL;res48.1 <- NULL;res48.2 <- NULL; 
    res49.0 <- NULL;res49.1 <- NULL;res49.2 <- NULL;
    res50 <-NULL; res50.1 <- NULL; res50.2 <- NULL;
    
    res0.0 <- gtrends(c(var), geo=c("AF","AL","DZ","AS","AD"),time = var2);res0.1 <- gtrends(c(var.1), geo=c("AF","AL","DZ","AS","AD"),time = var2);res0.2 <- gtrends(c(var.2), geo=c("AF","AL","DZ","AS","AD"),time = var2);
    res1.0 <- gtrends(c(var), geo=c("AO","AI","AQ","AG","AR"),time = var2);res1.1 <- gtrends(c(var.1), geo=c("AO","AI","AQ","AG","AR"),time = var2);res1.2 <- gtrends(c(var.2), geo=c("AO","AI","AQ","AG","AR"),time = var2);
    res2.0 <- gtrends(c(var), geo=c("AM","AW","AU","AT","AZ"),time = var2);res2.1 <- gtrends(c(var.1), geo=c("AM","AW","AU","AT","AZ"),time = var2);res2.2 <- gtrends(c(var.2), geo=c("AM","AW","AU","AT","AZ"),time = var2);
    res3.0 <- gtrends(c(var), geo=c("BS","BH","BD","BB","BY"),time = var2);res3.1 <- gtrends(c(var.1), geo=c("BS","BH","BD","BB","BY"),time = var2);res3.2 <- gtrends(c(var.2), geo=c("BS","BH","BD","BB","BY"),time = var2);
    res4.0 <- gtrends(c(var), geo=c("BE","BZ","BJ","BM","BT"),time = var2);res4.1 <- gtrends(c(var.1), geo=c("BE","BZ","BJ","BM","BT"),time = var2);res4.2 <- gtrends(c(var.2), geo=c("BE","BZ","BJ","BM","BT"),time = var2);
    res5.0 <- gtrends(c(var), geo=c("BO","BA","BW","BV","BR"),time = var2);res5.1 <- gtrends(c(var.1), geo=c("BO","BA","BW","BV","BR"),time = var2);res5.2 <- gtrends(c(var.2), geo=c("BO","BA","BW","BV","BR"),time = var2);
    res6.0 <- gtrends(c(var), geo=c("IO","BN","BG","BF","BI"),time = var2);res6.1 <- gtrends(c(var.1), geo=c("IO","BN","BG","BF","BI"),time = var2);res6.2 <- gtrends(c(var.2), geo=c("IO","BN","BG","BF","BI"),time = var2);
    res7.0 <- gtrends(c(var), geo=c("KH","CM","CA","CV","KY"),time = var2);res7.1 <- gtrends(c(var.1), geo=c("KH","CM","CA","CV","KY"),time = var2);res7.2 <- gtrends(c(var.2), geo=c("KH","CM","CA","CV","KY"),time = var2);
    res8.0 <- gtrends(c(var), geo=c("CF","TD","CL","CX"),time = var2);res8.1 <- gtrends(c(var.1), geo=c("CF","TD","CL","CX"),time = var2);res8.2 <- gtrends(c(var.2), geo=c("CF","TD","CL","CX"),time = var2);
    res9.0 <- gtrends(c(var), geo=c("CC","CO","KM","CG","CD"),time = var2);res9.1 <- gtrends(c(var.1), geo=c("CC","CO","KM","CG","CD"),time = var2);res9.2 <- gtrends(c(var.2), geo=c("CC","CO","KM","CG","CD"),time = var2);
    res10.0 <- gtrends(c(var), geo=c("CK","CR","CI","HR","CU"),time = var2);res10.1 <- gtrends(c(var.1), geo=c("CK","CR","CI","HR","CU"),time = var2);res10.2 <- gtrends(c(var.2), geo=c("CK","CR","CI","HR","CU"),time = var2);
    res11.0 <- gtrends(c(var), geo=c("CY","CZ","DK","DJ","DM"),time = var2);res11.1 <- gtrends(c(var.1), geo=c("CY","CZ","DK","DJ","DM"),time = var2);res11.2 <- gtrends(c(var.2), geo=c("CY","CZ","DK","DJ","DM"),time = var2);
    res12.0 <- gtrends(c(var), geo=c("DO","EC","EG","SV"),time = var2);res12.1 <- gtrends(c(var.1), geo=c("DO","EC","EG","SV"),time = var2);res12.2 <- gtrends(c(var.2), geo=c("DO","EC","EG","SV"),time = var2);
    res13.0 <- gtrends(c(var), geo=c("GQ","ER","EE","ET","FK"),time = var2);res13.1 <- gtrends(c(var.1), geo=c("GQ","ER","EE","ET","FK"),time = var2);res13.2 <- gtrends(c(var.2), geo=c("GQ","ER","EE","ET","FK"),time = var2);
    res14.0 <- gtrends(c(var), geo=c("FO","FJ","FI","FR","GF"),time = var2);res14.1 <- gtrends(c(var.1), geo=c("FO","FJ","FI","FR","GF"),time = var2);res14.2 <- gtrends(c(var.2), geo=c("FO","FJ","FI","FR","GF"),time = var2);
    res15.0 <- gtrends(c(var), geo=c("PF","TF","GA","GM","GE"),time = var2);res15.1 <- gtrends(c(var.1), geo=c("PF","TF","GA","GM","GE"),time = var2);res15.2 <- gtrends(c(var.2), geo=c("PF","TF","GA","GM","GE"),time = var2);
    res16.0 <- gtrends(c(var), geo=c("DE","GH","GI","GB","GR"),time = var2);res16.1 <- gtrends(c(var.1), geo=c("DE","GH","GI","GB","GR"),time = var2);res16.2 <- gtrends(c(var.2), geo=c("DE","GH","GI","GB","GR"),time = var2);
    res17.0 <- gtrends(c(var), geo=c("GL","GD","GP","GU","GT"),time = var2);res17.1 <- gtrends(c(var.1), geo=c("GL","GD","GP","GU","GT"),time = var2);res17.2 <- gtrends(c(var.2), geo=c("GL","GD","GP","GU","GT"),time = var2);
    res18.0 <- gtrends(c(var), geo=c("GN","GW","GY","HT","HM"),time = var2);res18.1 <- gtrends(c(var.1), geo=c("GN","GW","GY","HT","HM"),time = var2);res18.2 <- gtrends(c(var.2), geo=c("GN","GW","GY","HT","HM"),time = var2);
    res19.0 <- gtrends(c(var), geo=c("VA","HN","HK","HU","IS"),time = var2);res19.1 <- gtrends(c(var.1), geo=c("VA","HN","HK","HU","IS"),time = var2);res19.2 <- gtrends(c(var.2), geo=c("VA","HN","HK","HU","IS"),time = var2);
    res20.0 <- gtrends(c(var), geo=c("IN","ID","IR","IQ","IE"),time = var2);res20.1 <- gtrends(c(var.1), geo=c("IN","ID","IR","IQ","IE"),time = var2);res20.2 <- gtrends(c(var.2), geo=c("IN","ID","IR","IQ","IE"),time = var2);
    res21.0 <- gtrends(c(var), geo=c("IL","IT","JM","JP","JO"),time = var2);res21.1 <- gtrends(c(var.1), geo=c("IL","IT","JM","JP","JO"),time = var2);res21.2 <- gtrends(c(var.2), geo=c("IL","IT","JM","JP","JO"),time = var2);
    res22.0 <- gtrends(c(var), geo=c("KZ","KE","KI","KP","KR"),time = var2);res22.1 <- gtrends(c(var.1), geo=c("KZ","KE","KI","KP","KR"),time = var2);res22.2 <- gtrends(c(var.2), geo=c("KZ","KE","KI","KP","KR"),time = var2);
    res23.0 <- gtrends(c(var), geo=c("KW","KG","LA","LV","LB"),time = var2);res23.1 <- gtrends(c(var.1), geo=c("KW","KG","LA","LV","LB"),time = var2);res23.2 <- gtrends(c(var.2), geo=c("KW","KG","LA","LV","LB"),time = var2);
    res24.0 <- gtrends(c(var), geo=c("LS","LR","LY","LI","LT"),time = var2);res24.1 <- gtrends(c(var.1), geo=c("LS","LR","LY","LI","LT"),time = var2);res24.2 <- gtrends(c(var.2), geo=c("LS","LR","LY","LI","LT"),time = var2);
    res25.0 <- gtrends(c(var), geo=c("LU","MO","MK","MG","MW"),time = var2);res25.1 <- gtrends(c(var.1), geo=c("LU","MO","MK","MG","MW"),time = var2);res25.2 <- gtrends(c(var.2), geo=c("LU","MO","MK","MG","MW"),time = var2);
    res26.0 <- gtrends(c(var), geo=c("MY","MV","ML","MT","MH"),time = var2);res26.1 <- gtrends(c(var.1), geo=c("MY","MV","ML","MT","MH"),time = var2);res26.2 <- gtrends(c(var.2), geo=c("MY","MV","ML","MT","MH"),time = var2);
    res27.0 <- gtrends(c(var), geo=c("MQ","MR","MU","YT","MX"),time = var2);res27.1 <- gtrends(c(var.1), geo=c("MQ","MR","MU","YT","MX"),time = var2);res27.2 <- gtrends(c(var.2), geo=c("MQ","MR","MU","YT","MX"),time = var2);
    res28.0 <- gtrends(c(var), geo=c("FM","MD","MC","MN","MS"),time = var2);res28.1 <- gtrends(c(var.1), geo=c("FM","MD","MC","MN","MS"),time = var2);res28.2 <- gtrends(c(var.2), geo=c("FM","MD","MC","MN","MS"),time = var2);
    res29.0 <- gtrends(c(var), geo=c("MA","MZ","MM","NR","NP"),time = var2);res29.1 <- gtrends(c(var.1), geo=c("MA","MZ","MM","NR","NP"),time = var2);res29.2 <- gtrends(c(var.2), geo=c("MA","MZ","MM","NR","NP"),time = var2);
    res30.0 <- gtrends(c(var), geo=c("NL","NC","NZ","NI"),time = var2);res30.1 <- gtrends(c(var.1), geo=c("NL","NC","NZ","NI"),time = var2);res30.2 <- gtrends(c(var.2), geo=c("NL","NC","NZ","NI"),time = var2);
    res31.0 <- gtrends(c(var), geo=c("NE","NG","NU","NF","MP"),time = var2);res31.1 <- gtrends(c(var.1), geo=c("NE","NG","NU","NF","MP"),time = var2);res31.2 <- gtrends(c(var.2), geo=c("NE","NG","NU","NF","MP"),time = var2);
    res32.0 <- gtrends(c(var), geo=c("NO","OM","PK","PW","PA"),time = var2);res32.1 <- gtrends(c(var.1), geo=c("NO","OM","PK","PW","PA"),time = var2);res32.2 <- gtrends(c(var.2), geo=c("NO","OM","PK","PW","PA"),time = var2);
    res33.0 <- gtrends(c(var), geo=c("PG","PY","PE","PH","PN"),time = var2);res33.1 <- gtrends(c(var.1), geo=c("PG","PY","PE","PH","PN"),time = var2);res33.2 <- gtrends(c(var.2), geo=c("PG","PY","PE","PH","PN"),time = var2);
    res34.0 <- gtrends(c(var), geo=c("PL","PT","PR","QA","RE"),time = var2);res34.1 <- gtrends(c(var.1), geo=c("PL","PT","PR","QA","RE"),time = var2);res34.2 <- gtrends(c(var.2), geo=c("PL","PT","PR","QA","RE"),time = var2);
    res35.0 <- gtrends(c(var), geo=c("RO","RU","RW","SH","KN"),time = var2);res35.1 <- gtrends(c(var.1), geo=c("RO","RU","RW","SH","KN"),time = var2);res35.2 <- gtrends(c(var.2), geo=c("RO","RU","RW","SH","KN"),time = var2);
    res36.0 <- gtrends(c(var), geo=c("LC","PM","VC","WS","SM"),time = var2);res36.1 <- gtrends(c(var.1), geo=c("LC","PM","VC","WS","SM"),time = var2);res36.2 <- gtrends(c(var.2), geo=c("LC","PM","VC","WS","SM"),time = var2);
    res37.0 <- gtrends(c(var), geo=c("ST","SA","SC","SL","SG"),time = var2);res37.1 <- gtrends(c(var.1), geo=c("ST","SA","SC","SL","SG"),time = var2);res37.2 <- gtrends(c(var.2), geo=c("ST","SA","SC","SL","SG"),time = var2);
    res38.0 <- gtrends(c(var), geo=c("SK","SI","SB","SO","ZA"),time = var2);res38.1 <- gtrends(c(var.1), geo=c("SK","SI","SB","SO","ZA"),time = var2);res38.2 <- gtrends(c(var.2), geo=c("SK","SI","SB","SO","ZA"),time = var2);
    res39.0 <- gtrends(c(var), geo=c("GS","ES","LK","SD","SR"),time = var2);res39.1 <- gtrends(c(var.1), geo=c("GS","ES","LK","SD","SR"),time = var2);res39.2 <- gtrends(c(var.2), geo=c("GS","ES","LK","SD","SR"),time = var2);
    res40.0 <- gtrends(c(var), geo=c("SJ","SZ","SE","CH","SY"),time = var2);res40.1 <- gtrends(c(var.1), geo=c("SJ","SZ","SE","CH","SY"),time = var2);res40.2 <- gtrends(c(var.2), geo=c("SJ","SZ","SE","CH","SY"),time = var2);
    res41.0 <- gtrends(c(var), geo=c("TW","TJ","TZ","TH","TG"),time = var2);res41.1 <- gtrends(c(var.1), geo=c("TW","TJ","TZ","TH","TG"),time = var2);res41.2 <- gtrends(c(var.2), geo=c("TW","TJ","TZ","TH","TG"),time = var2);
    res42.0 <- gtrends(c(var), geo=c("TK","TO","TT","TN","TR"),time = var2);res42.1 <- gtrends(c(var.1), geo=c("TK","TO","TT","TN","TR"),time = var2);res42.2 <- gtrends(c(var.2), geo=c("TK","TO","TT","TN","TR"),time = var2);
    res43.0 <- gtrends(c(var), geo=c("TM","TC","TV","UG","UA"),time = var2);res43.1 <- gtrends(c(var.1), geo=c("TM","TC","TV","UG","UA"),time = var2);res43.2 <- gtrends(c(var.2), geo=c("TM","TC","TV","UG","UA"),time = var2);
    res44.0 <- gtrends(c(var), geo=c("AE","US","UY","UZ","VU"),time = var2);res44.1 <- gtrends(c(var.1), geo=c("AE","US","UY","UZ","VU"),time = var2);res44.2 <- gtrends(c(var.2), geo=c("AE","US","UY","UZ","VU"),time = var2);
    res45.0 <- gtrends(c(var), geo=c("VE","VN","VG","VI","WF"),time = var2);res45.1 <- gtrends(c(var.1), geo=c("VE","VN","VG","VI","WF"),time = var2);res45.2 <- gtrends(c(var.2), geo=c("VE","VN","VG","VI","WF"),time = var2);
    res46.0 <- gtrends(c(var), geo=c("EH","YE","ZM","ZW","SX"),time = var2);res46.1 <- gtrends(c(var.1), geo=c("EH","YE","ZM","ZW","SX"),time = var2);res46.2 <- gtrends(c(var.2), geo=c("EH","YE","ZM","ZW","SX"),time = var2);
    res47.0 <- gtrends(c(var), geo=c("BQ","ME","PS","RS","SN"),time = var2);res47.1 <- gtrends(c(var.1), geo=c("BQ","ME","PS","RS","SN"),time = var2);res47.2 <- gtrends(c(var.2), geo=c("BQ","ME","PS","RS","SN"),time = var2); 
    res48.0 <- gtrends(c(var), geo=c("SS","TL","UM","AX","BL"),time = var2);res48.1 <- gtrends(c(var.1), geo=c("SS","TL","UM","AX","BL"),time = var2);res48.2 <- gtrends(c(var.2), geo=c("SS","TL","UM","AX","BL"),time = var2); 
    res49.0 <- gtrends(c(var), geo=c("CW","GG","IM","JE","MF"),time = var2);res49.1 <- gtrends(c(var.1), geo=c("CW","GG","IM","JE","MF"),time = var2);res49.2 <- gtrends(c(var.2), geo=c("CW","GG","IM","JE","MF"),time = var2);
    res50 <-gtrends(c(var), geo=c("CN"),time = var2);
    res50.1 <- gtrends(c(var.1), geo=c("CN"),time = var2);
    res50.2 <- gtrends(c(var.2), geo=c("CN"),time = var2);
    reser <- gtrends(c("sars-cov-2"), geo= con,time = var2);  reser <- data.frame(reser$interest_over_time);  reser0 <- gtrends(c("Covid-19") , geo= con,time = var2);  reser0 <- data.frame(reser0$interest_over_time);  reser1 <- gtrends(c("Coronavirus"), geo= con,time = var2);  reser1 <- data.frame(reser1$interest_over_time)
    reser2 <- gtrends(c("sars-cov-2"), geo= con2,time = var2);  reser2 <- data.frame(reser2$interest_over_time);  reser3 <- gtrends(c("Coivd-19") , geo= con2,time = var2);  reser3 <- data.frame(reser3$interest_over_time);  reser4 <- gtrends(c("Coronavirus"), geo= con2,time = var2);  reser4 <- data.frame(reser4$interest_over_time)
    reser5 <- gtrends(c("sars-cov-2"), geo= con3,time = var2);   reser5 <- data.frame(reser5$interest_over_time);  reser6 <- gtrends(c("Covid-19") , geo= con3,time = var2);    reser6 <- data.frame(reser6$interest_over_time);  reser7 <- gtrends(c("Coronavirus"), geo= con3,time = var2); reser7 <- data.frame(reser7$interest_over_time)
    reser8 <- gtrends(c("sars cov 2"), geo= con4,time = var2);   reser8 <- data.frame(reser8$interest_over_time);reser9 <- gtrends(c("Covid-19") , geo= con4,time = var2);    reser9 <- data.frame(reser9$interest_over_time);  reser10 <- gtrends(c("Coronavirus"), geo= con4,time = var2);reser10 <- data.frame(reser10$interest_over_time)
    reser11 <- gtrends(c("sars-cov-2"), geo= con5,time = var2);  reser11 <- data.frame(reser11$interest_over_time);  reser12 <- gtrends(c("Covid-19") , geo= con5,time = var2);   reser12 <- data.frame(reser12$interest_over_time);reser13 <- gtrends(c("Coronavirus"), geo= con5,time = var2);reser13 <- data.frame(reser13$interest_over_time)
    reser14 <- gtrends(c("sars-cov-2"), geo= con6,time = var2);  reser14 <- data.frame(reser14$interest_over_time);  reser15 <- gtrends(c("Covid-19") , geo= con6,time = var2);   reser15 <- data.frame(reser15$interest_over_time);reser16 <- gtrends(c("Coronavirus"), geo= con6,time = var2);reser16 <- data.frame(reser16$interest_over_time)
    reser17 <- gtrends(c("sars-cov-2"), geo= con7,time = var2);  reser17 <- data.frame(reser17$interest_over_time);  reser18 <- gtrends(c("Covid-19") , geo= con7,time = var2);   reser18 <- data.frame(reser18$interest_over_time);reser19 <- gtrends(c("Coronavirus"), geo= con7,time = var2);reser19 <- data.frame(reser19$interest_over_time)
    reser20 <- gtrends(c("sars-cov-2"), geo= con8,time = var2);  reser20 <- data.frame(reser20$interest_over_time);  reser21 <- gtrends(c("Covid-19") , geo= con8,time = var2);   reser21 <- data.frame(reser21$interest_over_time);reser22 <- gtrends(c("Coronavirus"), geo= con8,time = var2);reser22 <- data.frame(reser22$interest_over_time)
    reser23 <- gtrends(c("sars-cov-2"), geo= con9,time = var2);  reser23 <- data.frame(reser23$interest_over_time);  reser24 <- gtrends(c("Covid-19") , geo= con9,time = var2);   reser24 <- data.frame(reser24$interest_over_time);reser25 <- gtrends(c("Coronavirus"), geo= con9,time = var2);reser25 <- data.frame(reser25$interest_over_time)
    reser26 <- gtrends(c("sars-cov-2"), geo= con10,time = var2);  reser26 <- data.frame(reser26$interest_over_time);  reser27 <- gtrends(c("Covid-19") , geo= con10,time = var2);   reser27 <- data.frame(reser27$interest_over_time);reser28 <- gtrends(c("Coronavirus"), geo= con10,time = var2);reser28 <- data.frame(reser28$interest_over_time)
    reser29 <- gtrends(c("sars-cov-2"), geo= con11,time = var2);  reser29 <- data.frame(reser29$interest_over_time);  reser30 <- gtrends(c("Covid-19") , geo= con11,time = var2);   reser30 <- data.frame(reser30$interest_over_time);reser31 <- gtrends(c("Coronavirus"), geo= con11,time = var2);reser31 <- data.frame(reser31$interest_over_time)
    reser32 <- gtrends(c("sars-cov-2"), geo= con12,time = var2);  reser32 <- data.frame(reser32$interest_over_time);  reser33 <- gtrends(c("Covid-19") , geo= con12,time = var2);   reser33 <- data.frame(reser33$interest_over_time);reser34 <- gtrends(c("Coronavirus"), geo= con12,time = var2);reser34 <- data.frame(reser34$interest_over_time)
    reser35 <- gtrends(c("sars-cov-2"), geo= con13,time = var2);  reser35 <- data.frame(reser35$interest_over_time);  reser36 <- gtrends(c("Covid-19") , geo= con13,time = var2);   reser36 <- data.frame(reser36$interest_over_time);reser37 <- gtrends(c("Coronavirus"), geo= con13,time = var2);reser37 <- data.frame(reser37$interest_over_time)
    reser38 <- gtrends(c("sars-cov-2"), geo= con14,time = var2);  reser38 <- data.frame(reser38$interest_over_time);  reser39 <- gtrends(c("Covid-19") , geo= con14,time = var2);   reser39 <- data.frame(reser39$interest_over_time);reser40 <- gtrends(c("Coronavirus"), geo= con14,time = var2);reser40 <- data.frame(reser40$interest_over_time)
    reser41 <- gtrends(c("sars-cov-2"), geo= con15,time = var2);  reser41 <- data.frame(reser41$interest_over_time);  reser42 <- gtrends(c("Covid-19") , geo= con15,time = var2);   reser42<- data.frame(reser42$interest_over_time);reser43 <- gtrends(c("Coronavirus"), geo= con15,time = var2);reser43 <- data.frame(reser43$interest_over_time)
    reser44 <- gtrends(c("sars-cov-2"), geo= con16,time = var2);  reser44 <- data.frame(reser44$interest_over_time);  reser45 <- gtrends(c("Covid-19") , geo= con16,time = var2);   reser45<- data.frame(reser45$interest_over_time);reser46 <- gtrends(c("Coronavirus"), geo= con16,time = var2);reser46 <- data.frame(reser46$interest_over_time)
    reser47 <- gtrends(c("sars-cov-2"), geo= con17,time = var2);  reser47 <- data.frame(reser47$interest_over_time);  reser48 <- gtrends(c("Covid-19") , geo= con17,time = var2);   reser48<- data.frame(reser48$interest_over_time);reser49 <- gtrends(c("Coronavirus"), geo= con17,time = var2);reser49 <- data.frame(reser49$interest_over_time)
    reser50 <- gtrends(c("sars-cov-2"), geo= con18,time = var2);  reser50 <- data.frame(reser50$interest_over_time);  reser51 <- gtrends(c("Covid-19") , geo= con18,time = var2);   reser51<- data.frame(reser51$interest_over_time);reser52 <- gtrends(c("Coronavirus"), geo= con18,time = var2);reser52 <- data.frame(reser52$interest_over_time)
    reser53 <- gtrends(c("sars-cov-2"), geo= con19,time = var2);  reser53 <- data.frame(reser53$interest_over_time);  reser54 <- gtrends(c("Covid-19") , geo= con19,time = var2);   reser54<- data.frame(reser54$interest_over_time);reser55 <- gtrends(c("Coronavirus"), geo= con19,time = var2);reser55 <- data.frame(reser55$interest_over_time)
    reser56 <- gtrends(c("sars-cov-2"), geo= con20,time = var2);  reser56 <- data.frame(reser56$interest_over_time);  reser57 <- gtrends(c("Covid-19") , geo= con20,time = var2);   reser57<- data.frame(reser57$interest_over_time);reser58 <- gtrends(c("Coronavirus"), geo= con20,time = var2);reser58 <- data.frame(reser58$interest_over_time)
    reser59 <- gtrends(c("sars-cov-2"), geo= con21,time = var2);  reser59 <- data.frame(reser59$interest_over_time);  reser60 <- gtrends(c("Covid-19") , geo= con21,time = var2);   reser60<- data.frame(reser60$interest_over_time);reser61 <- gtrends(c("Coronavirus"), geo= con21,time = var2);reser61 <- data.frame(reser61$interest_over_time)
    reser62 <- gtrends(c("sars-cov-2"), geo= con22,time = var2);  reser62 <- data.frame(reser62$interest_over_time);  reser63 <- gtrends(c("Covid-19") , geo= con22,time = var2);   reser63<- data.frame(reser63$interest_over_time);reser64 <- gtrends(c("Coronavirus"), geo= con22,time = var2);reser64 <- data.frame(reser64$interest_over_time)
    reser65 <- gtrends(c("sars-cov-2"), geo= con23,time = var2);  reser65 <- data.frame(reser65$interest_over_time);  reser66 <- gtrends(c("Covid-19") , geo= con23,time = var2);   reser66<- data.frame(reser66$interest_over_time);reser67 <- gtrends(c("Coronavirus"), geo= con23,time = var2);reser67 <- data.frame(reser67$interest_over_time)
    reser68 <- gtrends(c("sars-cov-2"), geo= con24,time = var2);  reser68 <- data.frame(reser68$interest_over_time);  reser69 <- gtrends(c("Covid-19") , geo= con24,time = var2);   reser69<- data.frame(reser69$interest_over_time);reser70 <- gtrends(c("Coronavirus"), geo= con24,time = var2);reser70 <- data.frame(reser70$interest_over_time)
    reser71 <- gtrends(c("sars-cov-2"), geo= con25,time = var2);  reser71 <- data.frame(reser71$interest_over_time);  reser72 <- gtrends(c("Covid-19") , geo= con25,time = var2);   reser72<- data.frame(reser72$interest_over_time);reser73 <- gtrends(c("Coronavirus"), geo= con25,time = var2);reser73 <- data.frame(reser73$interest_over_time)
    reser74 <- gtrends(c("sars-cov-2"), geo= con26,time = var2);  reser74 <- data.frame(reser74$interest_over_time);  reser75 <- gtrends(c("Covid-19") , geo= con26,time = var2);   reser75<- data.frame(reser75$interest_over_time);reser76 <- gtrends(c("Coronavirus"), geo= con26,time = var2);reser76 <- data.frame(reser76$interest_over_time)
    reser77 <- gtrends(c("sars-cov-2"), geo= con27,time = var2);  reser77 <- data.frame(reser77$interest_over_time);  reser78 <- gtrends(c("Covid-19") , geo= con27,time = var2);   reser78<- data.frame(reser78$interest_over_time);reser79 <- gtrends(c("Coronavirus"), geo= con27,time = var2);reser79 <- data.frame(reser79$interest_over_time)
    reser80 <- gtrends(c("sars-cov-2"), geo= con28,time = var2);  reser80 <- data.frame(reser80$interest_over_time);  reser81 <- gtrends(c("Covid-19") , geo= con28,time = var2);   reser81<- data.frame(reser81$interest_over_time);reser82 <- gtrends(c("Coronavirus"), geo= con28,time = var2);reser82 <- data.frame(reser82$interest_over_time)
    reser83 <- gtrends(c("sars-cov-2"), geo= con29,time = var2);  reser83 <- data.frame(reser83$interest_over_time);  reser84 <- gtrends(c("Covid-19") , geo= con29,time = var2);   reser84<- data.frame(reser84$interest_over_time);reser85 <- gtrends(c("Coronavirus"), geo= con29,time = var2);reser85 <- data.frame(reser85$interest_over_time)
    reser86 <- gtrends(c("sars-cov-2"), geo= con30,time = var2);  reser86 <- data.frame(reser86$interest_over_time);  reser87 <- gtrends(c("Covid-19") , geo= con30,time = var2);   reser87<- data.frame(reser87$interest_over_time);reser88 <- gtrends(c("Coronavirus"), geo= con30,time = var2);reser88 <- data.frame(reser88$interest_over_time)
    reser89 <- gtrends(c("sars-cov-2"), geo= con31,time = var2);  reser89 <- data.frame(reser89$interest_over_time);  reser90 <- gtrends(c("Covid-19") , geo= con31,time = var2);   reser90<- data.frame(reser90$interest_over_time);reser91 <- gtrends(c("Coronavirus"), geo= con31,time = var2);reser91 <- data.frame(reser91$interest_over_time)
    reser92 <- gtrends(c("sars-cov-2"), geo= con32,time = var2);  reser92 <- data.frame(reser92$interest_over_time);  reser93 <- gtrends(c("Covid-19") , geo= con32,time = var2);   reser93<- data.frame(reser93$interest_over_time);reser94 <- gtrends(c("Coronavirus"), geo= con32,time = var2);reser94 <- data.frame(reser94$interest_over_time)
    reser95 <- gtrends(c("sars-cov-2"), geo= con33,time = var2);  reser95 <- data.frame(reser95$interest_over_time);  reser96 <- gtrends(c("Covid-19") , geo= con33,time = var2);   reser96<- data.frame(reser96$interest_over_time);reser97 <- gtrends(c("Coronavirus"), geo= con33,time = var2);reser97 <- data.frame(reser97$interest_over_time)
    reser98 <- gtrends(c("sars-cov-2"), geo= con34,time = var2);  reser98 <- data.frame(reser98$interest_over_time);  reser99 <- gtrends(c("Covid-19") , geo= con34,time = var2);   reser99<- data.frame(reser99$interest_over_time);reser100 <- gtrends(c("Coronavirus"), geo= con34,time = var2);reser100 <- data.frame(reser100$interest_over_time)
    reser101 <- gtrends(c("sars-cov-2"), geo= con35,time = var2);  reser101 <- data.frame(reser101$interest_over_time);  reser102 <- gtrends(c("Covid-19") , geo= con35,time = var2);   reser102 <- data.frame(reser102$interest_over_time); reser103 <- gtrends(c("Coronavirus"), geo= con35,time = var2);reser103 <- data.frame(reser103$interest_over_time)
    reser104 <- gtrends(c("sars-cov-2"), geo= con36,time = var2);  reser104 <- data.frame(reser104$interest_over_time);  reser105 <- gtrends(c("Covid-19") , geo= con36,time = var2);   reser105 <- data.frame(reser105$interest_over_time); reser106 <- gtrends(c("Coronavirus"), geo= con36,time = var2);reser106 <- data.frame(reser106$interest_over_time)
    reser107 <- gtrends(c("sars-cov-2"), geo= con37,time = var2);  reser107 <- data.frame(reser107$interest_over_time);  reser108 <- gtrends(c("Covid-19") , geo= con37,time = var2);   reser108 <- data.frame(reser108$interest_over_time); reser109 <- gtrends(c("Coronavirus"), geo= con37,time = var2);reser109 <- data.frame(reser109$interest_over_time)
    reser110 <- gtrends(c("sars-cov-2"), geo= con38,time = var2);  reser110 <- data.frame(reser110$interest_over_time);  reser111 <- gtrends(c("Covid-19") , geo= con38,time = var2);   reser111 <- data.frame(reser111$interest_over_time); reser112 <- gtrends(c("Coronavirus"), geo= con38,time = var2);reser112 <- data.frame(reser112$interest_over_time)
    reser113 <- gtrends(c("sars-cov-2"), geo= con39,time = var2);  reser113 <- data.frame(reser113$interest_over_time);  reser114 <- gtrends(c("Covid-19") , geo= con39,time = var2);   reser114 <- data.frame(reser114$interest_over_time); reser115 <- gtrends(c("Coronavirus"), geo= con39,time = var2);reser115 <- data.frame(reser115$interest_over_time)
    reser116 <- gtrends(c("sars-cov-2"), geo= con40,time = var2);  reser116 <- data.frame(reser116$interest_over_time);  reser117 <- gtrends(c("Covid-19") , geo= con40,time = var2);   reser117 <- data.frame(reser117$interest_over_time); reser118 <- gtrends(c("Coronavirus"), geo= con40,time = var2);reser118 <- data.frame(reser118$interest_over_time)
    reser119 <- gtrends(c("sars-cov-2"), geo= con41,time = var2);  reser119 <- data.frame(reser119$interest_over_time);  reser120 <- gtrends(c("Covid-19") , geo= con41,time = var2);   reser120 <- data.frame(reser120$interest_over_time); reser121 <- gtrends(c("Coronavirus"), geo= con41,time = var2);reser121 <- data.frame(reser121$interest_over_time)
    reser122 <- gtrends(c("sars-cov-2"), geo= con42,time = var2);  reser122 <- data.frame(reser122$interest_over_time);  reser123 <- gtrends(c("Covid-19") , geo= con42,time = var2);   reser123 <- data.frame(reser123$interest_over_time); reser124 <- gtrends(c("Coronavirus"), geo= con42,time = var2);reser124 <- data.frame(reser124$interest_over_time)
    reser125 <- gtrends(c("sars-cov-2"), geo= con43,time = var2);  reser125 <- data.frame(reser125$interest_over_time);  reser126 <- gtrends(c("Covid-19") , geo= con43,time = var2);   reser126 <- data.frame(reser126$interest_over_time); reser127 <- gtrends(c("Coronavirus"), geo= con43,time = var2);reser127 <- data.frame(reser127$interest_over_time)
    reser128 <- gtrends(c("sars-cov-2"), geo= con44,time = var2);  reser128 <- data.frame(reser128$interest_over_time);  reser129 <- gtrends(c("Covid-19") , geo= con44,time = var2);   reser129 <- data.frame(reser129$interest_over_time); reser130 <- gtrends(c("Coronavirus"), geo= con44,time = var2);reser130 <- data.frame(reser130$interest_over_time)
    reser131 <- gtrends(c("sars-cov-2"), geo= con45,time = var2);  reser131 <- data.frame(reser131$interest_over_time);  reser132 <- gtrends(c("Covid-19") , geo= con45,time = var2);   reser132 <- data.frame(reser132$interest_over_time); reser133 <- gtrends(c("Coronavirus"), geo= con45,time = var2);reser133 <- data.frame(reser133$interest_over_time)
    reser134 <- gtrends(c("sars-cov-2"), geo= con46,time = var2);  reser134 <- data.frame(reser134$interest_over_time);  reser135 <- gtrends(c("Covid-19") , geo= con46,time = var2);   reser135 <- data.frame(reser135$interest_over_time); reser136 <- gtrends(c("Coronavirus"), geo= con46,time = var2);reser136 <- data.frame(reser136$interest_over_time)
    reser137 <- gtrends(c("sars-cov-2"), geo= con47,time = var2);  reser137 <- data.frame(reser137$interest_over_time);  reser138 <- gtrends(c("Covid-19") , geo= con47,time = var2);   reser138 <- data.frame(reser138$interest_over_time); reser139 <- gtrends(c("Coronavirus"), geo= con47,time = var2);reser139 <- data.frame(reser139$interest_over_time)
    reser140 <- gtrends(c("sars-cov-2"), geo= con48,time = var2);  reser140 <- data.frame(reser140$interest_over_time);  reser141 <- gtrends(c("Covid-19") , geo= con48,time = var2);   reser141 <- data.frame(reser141$interest_over_time); reser142 <- gtrends(c("Coronavirus"), geo= con48,time = var2);reser142 <- data.frame(reser142$interest_over_time)
    reser143 <- gtrends(c("sars-cov-2"), geo= con49,time = var2);  reser143 <- data.frame(reser143$interest_over_time);  reser144 <- gtrends(c("covid 19") , geo= con49,time = var2);   reser144 <- data.frame(reser144$interest_over_time); reser145 <- gtrends(c("Coronavirus"), geo= con49,time = var2);reser145 <- data.frame(reser145$interest_over_time)
    reser146 <- gtrends(c("sars-cov-2"), geo= con50,time = var2);  reser146 <- data.frame(reser146$interest_over_time);  reser147 <- gtrends(c("Covid-19") , geo= con50,time = var2);   reser147 <- data.frame(reser147$interest_over_time); reser148 <- gtrends(c("Coronavirus"), geo= con50,time = var2);reser148 <- data.frame(reser148$interest_over_time)
    reser149 <- gtrends(c("sars-cov-2"), geo= con51,time = var2);  reser149 <- data.frame(reser149$interest_over_time);  reser150 <- gtrends(c("Covid-19") , geo= con51,time = var2);   reser150 <- data.frame(reser150$interest_over_time); reser151 <- gtrends(c("Coronavirus"), geo= con51,time = var2);reser151 <- data.frame(reser151$interest_over_time)
    reser152 <- gtrends(c("sars-cov-2"), geo= con52,time = var2);  reser152 <- data.frame(reser152$interest_over_time);  reser153 <- gtrends(c("Covid-19") , geo= con52,time = var2);   reser153 <- data.frame(reser153$interest_over_time); reser154 <- gtrends(c("Coronavirus"), geo= con52,time = var2);reser154 <- data.frame(reser154$interest_over_time)
    reser155 <- gtrends(c("sars-cov-2"), geo= con53,time = var2);  reser155 <- data.frame(reser155$interest_over_time);  reser156 <- gtrends(c("Covid-19") , geo= con53,time = var2);   reser156 <- data.frame(reser156$interest_over_time); reser157 <- gtrends(c("Coronavirus"), geo= con53,time = var2);reser157 <- data.frame(reser157$interest_over_time)
    reser158 <- gtrends(c("sars-cov-2"), geo= con54,time = var2);  reser158 <- data.frame(reser158$interest_over_time);  reser159 <- gtrends(c("Covid-19") , geo= con54,time = var2);   reser159 <- data.frame(reser159$interest_over_time); reser160 <- gtrends(c("Coronavirus"), geo= con54,time = var2);reser160 <- data.frame(reser160$interest_over_time)
    reser161 <- gtrends(c("sars-cov-2"), geo= con55,time = var2);  reser161 <- data.frame(reser161$interest_over_time);  reser162 <- gtrends(c("Covid-19") , geo= con55,time = var2);   reser162 <- data.frame(reser162$interest_over_time); reser163 <- gtrends(c("Coronavirus"), geo= con55,time = var2);reser163 <- data.frame(reser163$interest_over_time)
    reser164 <- gtrends(c("sars-cov-2"), geo= con56,time = var2);  reser164 <- data.frame(reser164$interest_over_time);  reser165 <- gtrends(c("Covid-19") , geo= con56,time = var2);   reser165 <- data.frame(reser165$interest_over_time); reser166 <- gtrends(c("Coronavirus"), geo= con56,time = var2);reser166 <- data.frame(reser166$interest_over_time)
    reser167 <- gtrends(c("sars-cov-2"), geo= con57,time = var2);  reser167 <- data.frame(reser167$interest_over_time);  reser168 <- gtrends(c("Covid-19") , geo= con57,time = var2);   reser168 <- data.frame(reser168$interest_over_time); reser169 <- gtrends(c("Coronavirus"), geo= con57,time = var2);reser169 <- data.frame(reser169$interest_over_time)
    reser170 <- gtrends(c("sars-cov-2"), geo= con58,time = var2);  reser170 <- data.frame(reser170$interest_over_time);  reser171 <- gtrends(c("Covid-19") , geo= con58,time = var2);   reser171 <- data.frame(reser171$interest_over_time); reser172 <- gtrends(c("Coronavirus"), geo= con58,time = var2);reser172 <- data.frame(reser172$interest_over_time)
    reser173 <- gtrends(c("sars-cov-2"), geo= con59,time = var2);  reser173 <- data.frame(reser173$interest_over_time);  reser174 <- gtrends(c("Covid-19") , geo= con59,time = var2);   reser174 <- data.frame(reser174$interest_over_time); reser175 <- gtrends(c("Coronavirus"), geo= con59,time = var2);reser175 <- data.frame(reser175$interest_over_time)
    reser176 <- gtrends(c("sars-cov-2"), geo= con60,time = var2);  reser176 <- data.frame(reser176$interest_over_time);  reser177 <- gtrends(c("Covid-19") , geo= con60,time = var2);   reser177 <- data.frame(reser177$interest_over_time); reser178 <- gtrends(c("Coronavirus"), geo= con60,time = var2);reser178 <- data.frame(reser178$interest_over_time)
    reser179 <- gtrends(c("sars-cov-2"), geo= con61,time = var2);  reser179 <- data.frame(reser179$interest_over_time);  reser180 <- gtrends(c("Covid-19") , geo= con61,time = var2);   reser180 <- data.frame(reser180$interest_over_time); reser181 <- gtrends(c("Coronavirus"), geo= con61,time = var2);reser181 <- data.frame(reser181$interest_over_time)
    reser182 <- gtrends(c("sars-cov-2"), geo= con62,time = var2);  reser182 <- data.frame(reser182$interest_over_time);  reser183 <- gtrends(c("Covid-19") , geo= con62,time = var2);   reser183 <- data.frame(reser183$interest_over_time); reser184 <- gtrends(c("Coronavirus"), geo= con62,time = var2);reser184 <- data.frame(reser184$interest_over_time)
    reser185 <- gtrends(c("sars-cov-2"), geo= con63,time = var2);  reser185 <- data.frame(reser185$interest_over_time);  reser186 <- gtrends(c("Covid-19") , geo= con63,time = var2);   reser186 <- data.frame(reser186$interest_over_time); reser187 <- gtrends(c("Coronavirus"), geo= con63,time = var2);reser187 <- data.frame(reser187$interest_over_time)
    reser188 <- gtrends(c("sars-cov-2"), geo= con64,time = var2);  reser188 <- data.frame(reser188$interest_over_time);  reser189 <- gtrends(c("Covid-19") , geo= con64,time = var2);   reser189 <- data.frame(reser189$interest_over_time); reser190 <- gtrends(c("Coronavirus"), geo= con64,time = var2);reser190 <- data.frame(reser190$interest_over_time)
    reser191 <- gtrends(c("sars-cov-2"), geo= con65,time = var2);  reser191 <- data.frame(reser191$interest_over_time);  reser192 <- gtrends(c("Covid-19") , geo= con65,time = var2);   reser192 <- data.frame(reser192$interest_over_time); reser193 <- gtrends(c("Coronavirus"), geo= con65,time = var2);reser193 <- data.frame(reser193$interest_over_time)
    reser194 <- gtrends(c("sars-cov-2"), geo= con66,time = var2);  reser194 <- data.frame(reser194$interest_over_time);  reser195 <- gtrends(c("Covid-19") , geo= con66,time = var2);   reser195 <- data.frame(reser195$interest_over_time); reser196 <- gtrends(c("Coronavirus"), geo= con66,time = var2);reser196 <- data.frame(reser196$interest_over_time)
    reser197 <- gtrends(c("sars-cov-2"), geo= con67,time = var2);  reser197 <- data.frame(reser197$interest_over_time);  reser198 <- gtrends(c("Covid-19") , geo= con67,time = var2);   reser198 <- data.frame(reser198$interest_over_time); reser199 <- gtrends(c("Coronavirus"), geo= con67,time = var2);reser199 <- data.frame(reser199$interest_over_time)
    reser200 <- gtrends(c("sars-cov-2"), geo= con68,time = var2);  reser200 <- data.frame(reser200$interest_over_time);  reser201 <- gtrends(c("Covid-19") , geo= con68,time = var2);   reser201 <- data.frame(reser201$interest_over_time); reser202 <- gtrends(c("Coronavirus"), geo= con68,time = var2);reser202 <- data.frame(reser202$interest_over_time)
    reser203 <- gtrends(c("sars-cov-2"), geo= con69,time = var2);  reser203 <- data.frame(reser203$interest_over_time);  reser204 <- gtrends(c("Covid-19") , geo= con69,time = var2);   reser204 <- data.frame(reser204$interest_over_time); reser205 <- gtrends(c("Coronavirus"), geo= con69,time = var2);reser205 <- data.frame(reser205$interest_over_time)
    reser206 <- gtrends(c("sars-cov-2"), geo= con70,time = var2);  reser206 <- data.frame(reser206$interest_over_time);  reser207 <- gtrends(c("Covid-19") , geo= con70,time = var2);   reser207 <- data.frame(reser207$interest_over_time); reser208 <- gtrends(c("Coronavirus"), geo= con70,time = var2);reser208 <- data.frame(reser208$interest_over_time)
    reser209 <- gtrends(c("sars-cov-2"), geo= con71,time = var2);  reser209 <- data.frame(reser209$interest_over_time);  reser210 <- gtrends(c("Covid-19") , geo= con71,time = var2);   reser210 <- data.frame(reser210$interest_over_time); reser211 <- gtrends(c("Coronavirus"), geo= con71,time = var2);reser211 <- data.frame(reser211$interest_over_time)
    reser212 <- gtrends(c("sars-cov-2"), geo= con72,time = var2);  reser212 <- data.frame(reser212$interest_over_time);  reser213 <- gtrends(c("Covid-19") , geo= con72,time = var2);   reser213 <- data.frame(reser213$interest_over_time); reser214 <- gtrends(c("Coronavirus"), geo= con72,time = var2);reser214 <- data.frame(reser214$interest_over_time)
    reser215 <- gtrends(c("sars-cov-2"), geo= con73,time = var2);  reser215 <- data.frame(reser215$interest_over_time);  reser216 <- gtrends(c("Covid-19") , geo= con73,time = var2);   reser216 <- data.frame(reser216$interest_over_time); reser217 <- gtrends(c("Coronavirus"), geo= con73,time = var2);reser217 <- data.frame(reser217$interest_over_time)
    reser218 <- gtrends(c("sars-cov-2"), geo= con74,time = var2);  reser218 <- data.frame(reser218$interest_over_time);  reser219 <- gtrends(c("Covid-19") , geo= con74,time = var2);   reser219 <- data.frame(reser219$interest_over_time); reser220 <- gtrends(c("Coronavirus"), geo= con74,time = var2);reser220 <- data.frame(reser220$interest_over_time)
    reser221 <- gtrends(c("sars-cov-2"), geo= con75,time = var2);  reser221 <- data.frame(reser221$interest_over_time);  reser222 <- gtrends(c("Covid-19") , geo= con75,time = var2);   reser222 <- data.frame(reser222$interest_over_time); reser223 <- gtrends(c("Coronavirus"), geo= con75,time = var2);reser223 <- data.frame(reser223$interest_over_time)
    reser224 <- gtrends(c("sars-cov-2"), geo= con76,time = var2);  reser224 <- data.frame(reser224$interest_over_time);  reser225 <- gtrends(c("Covid-19") , geo= con76,time = var2);   reser225 <- data.frame(reser225$interest_over_time); reser226 <- gtrends(c("Coronavirus"), geo= con76,time = var2);reser226 <- data.frame(reser226$interest_over_time)
    reser227 <- gtrends(c("sars-cov-2"), geo= con77,time = var2);  reser227 <- data.frame(reser227$interest_over_time);  reser228 <- gtrends(c("Covid-19") , geo= con77,time = var2);   reser228 <- data.frame(reser228$interest_over_time); reser229 <- gtrends(c("Coronavirus"), geo= con77,time = var2);reser229 <- data.frame(reser229$interest_over_time)
    reser230 <- gtrends(c("sars-cov-2"), geo= con78,time = var2);  reser230 <- data.frame(reser230$interest_over_time);  reser231 <- gtrends(c("Covid-19") , geo= con78,time = var2);   reser231 <- data.frame(reser231$interest_over_time); reser232 <- gtrends(c("Coronavirus"), geo= con78,time = var2);reser232 <- data.frame(reser232$interest_over_time)
    reser233 <- gtrends(c("sars-cov-2"), geo= con79,time = var2);  reser233 <- data.frame(reser233$interest_over_time);  reser234 <- gtrends(c("covid 19") , geo= con79,time = var2);   reser234 <- data.frame(reser234$interest_over_time); reser235 <- gtrends(c("Coronavirus"), geo= con79,time = var2);reser235 <- data.frame(reser235$interest_over_time)
    reser236 <- gtrends(c("sars-cov-2"), geo= con80,time = var2);  reser236 <- data.frame(reser236$interest_over_time);  reser237 <- gtrends(c("Covid-19") , geo= con80,time = var2);   reser237 <- data.frame(reser237$interest_over_time); reser238 <- gtrends(c("Coronavirus"), geo= con80,time = var2);reser238 <- data.frame(reser238$interest_over_time)
    reser239 <- gtrends(c("sars-cov-2"), geo= con81,time = var2);  reser239 <- data.frame(reser239$interest_over_time);  reser240 <- gtrends(c("Covid-19") , geo= con81,time = var2);   reser240 <- data.frame(reser240$interest_over_time); reser241 <- gtrends(c("Coronavirus"), geo= con81,time = var2);reser241 <- data.frame(reser241$interest_over_time)
    reserz <- gtrends(c("sars-cov-2"), geo= conz,time = var2);  reserz <- data.frame(reserz$interest_over_time);  reser0z <- gtrends(c("Covid-19") , geo= conz,time = var2);  reser0z <- data.frame(reser0z$interest_over_time);  reser1z <- gtrends(c("Coronavirus"), geo= conz,time = var2);  reser1z <- data.frame(reser1z$interest_over_time)
    reser2z <- gtrends(c("sars-cov-2"), geo= con2z,time = var2);  reser2z <- data.frame(reser2z$interest_over_time);  reser3z <- gtrends(c("Covid-19") , geo= con2z,time = var2);  reser3z <- data.frame(reser3z$interest_over_time);  reser4z <- gtrends(c("Coronavirus"), geo= con2z,time = var2);  reser4z <- data.frame(reser4z$interest_over_time)
    reser5z <- gtrends(c("sars-cov-2"), geo= con3z,time = var2);   reser5z <- data.frame(reser5z$interest_over_time);  reser6z <- gtrends(c("Covid-19") , geo= con3z,time = var2);    reser6z <- data.frame(reser6z$interest_over_time);  reser7z <- gtrends(c("Coronavirus"), geo= con3z,time = var2); reser7z <- data.frame(reser7z$interest_over_time)
    reser8z <- gtrends(c("sars-cov-2"), geo= con4z,time = var2);   reser8z <- data.frame(reser8z$interest_over_time);reser9z <- gtrends(c("Covid-19") , geo= con4z,time = var2);    reser9z <- data.frame(reser9z$interest_over_time);  reser10z <- gtrends(c("Coronavirus"), geo= con4z,time = var2);reser10z <- data.frame(reser10z$interest_over_time)
    reser11z <- gtrends(c("sars-cov-2"), geo= con5z,time = var2);  reser11z <- data.frame(reser11z$interest_over_time);  reser12z <- gtrends(c("Covid-19") , geo= con5z,time = var2);   reser12z <- data.frame(reser12z$interest_over_time);reser13z <- gtrends(c("Coronavirus"), geo= con5z,time = var2);reser13z <- data.frame(reser13z$interest_over_time)
    reser14z <- gtrends(c("sars-cov-2"), geo= con6z,time = var2);  reser14z <- data.frame(reser14z$interest_over_time);  reser15z <- gtrends(c("Covid-19") , geo= con6z,time = var2);   reser15z <- data.frame(reser15z$interest_over_time);reser16z <- gtrends(c("Coronavirus"), geo= con6z,time = var2);reser16z <- data.frame(reser16z$interest_over_time)
    reser17z <- gtrends(c("sars-cov-2"), geo= con7z,time = var2);  reser17z <- data.frame(reser17z$interest_over_time);  reser18z <- gtrends(c("Covid-19") , geo= con7z,time = var2);   reser18z <- data.frame(reser18z$interest_over_time);reser19z <- gtrends(c("Coronavirus"), geo= con7z,time = var2);reser19z <- data.frame(reser19z$interest_over_time)
    reser20z <- gtrends(c("sars-cov-2"), geo= con8z,time = var2);  reser20z <- data.frame(reser20z$interest_over_time);  reser21z <- gtrends(c("Covid-19") , geo= con8z,time = var2);   reser21z <- data.frame(reser21z$interest_over_time);reser22z <- gtrends(c("Coronavirus"), geo= con8z,time = var2);reser22z <- data.frame(reser22z$interest_over_time)
    reser23z <- gtrends(c("sars-cov-2"), geo= con9z,time = var2);  reser23z <- data.frame(reser23z$interest_over_time);  reser24z <- gtrends(c("Covid-19") , geo= con9z,time = var2);   reser24z <- data.frame(reser24z$interest_over_time);reser25z <- gtrends(c("Coronavirus"), geo= con9z,time = var2);reser25z <- data.frame(reser25z$interest_over_time)
    reser26z <- gtrends(c("sars-cov-2"), geo= con10z,time = var2);  reser26z <- data.frame(reser26z$interest_over_time);  reser27z <- gtrends(c("Covid-19") , geo= con10z,time = var2);   reser27z <- data.frame(reser27z$interest_over_time);reser28z <- gtrends(c("Coronavirus"), geo= con10z,time = var2);reser28z <- data.frame(reser28z$interest_over_time)
    reser29z <- gtrends(c("sars-cov-2"), geo= con11z,time = var2);  reser29z <- data.frame(reser29z$interest_over_time);  reser30z <- gtrends(c("Covid-19") , geo= con11z,time = var2);   reser30z <- data.frame(reser30z$interest_over_time);reser31z <- gtrends(c("Coronavirus"), geo= con11z,time = var2);reser31z <- data.frame(reser31z$interest_over_time)
    reser32z <- gtrends(c("sars-cov-2"), geo= con12z,time = var2);  reser32z <- data.frame(reser32z$interest_over_time);  reser33z <- gtrends(c("Covid-19") , geo= con12z,time = var2);   reser33z <- data.frame(reser33z$interest_over_time);reser34z <- gtrends(c("Coronavirus"), geo= con12z,time = var2);reser34z <- data.frame(reser34z$interest_over_time)
    reser35z <- gtrends(c("sars-cov-2"), geo= con13z,time = var2);  reser35z <- data.frame(reser35z$interest_over_time);  reser36z <- gtrends(c("Covid-19") , geo= con13z,time = var2);   reser36z <- data.frame(reser36z$interest_over_time);reser37z <- gtrends(c("Coronavirus"), geo= con13z,time = var2);reser37z <- data.frame(reser37z$interest_over_time)
    reser38z <- gtrends(c("sars-cov-2"), geo= con14z,time = var2);  reser38z <- data.frame(reser38z$interest_over_time);  reser39z <- gtrends(c("Covid-19") , geo= con14z,time = var2);   reser39z <- data.frame(reser39z$interest_over_time);reser40z <- gtrends(c("Coronavirus"), geo= con14z,time = var2);reser40z <- data.frame(reser40z$interest_over_time)
    reser41z <- gtrends(c("sars-cov-2"), geo= con15z,time = var2);  reser41z <- data.frame(reser41z$interest_over_time);  reser42z <- gtrends(c("Covid-19") , geo= con15z,time = var2);   reser42z<- data.frame(reser42z$interest_over_time);reser43z <- gtrends(c("Coronavirus"), geo= con15z,time = var2);reser43z <- data.frame(reser43z$interest_over_time)
    reser44z <- gtrends(c("sars-cov-2"), geo= con16z,time = var2);  reser44z <- data.frame(reser44z$interest_over_time);  reser45z <- gtrends(c("Covid-19") , geo= con16z,time = var2);   reser45z<- data.frame(reser45z$interest_over_time);reser46z <- gtrends(c("Coronavirus"), geo= con16z,time = var2);reser46z <- data.frame(reser46z$interest_over_time)
    reser47z <- gtrends(c("sars-cov-2"), geo= con17z,time = var2);  reser47z <- data.frame(reser47z$interest_over_time);  reser48z <- gtrends(c("Covid-19") , geo= con17z,time = var2);   reser48z<- data.frame(reser48z$interest_over_time);reser49z <- gtrends(c("Coronavirus"), geo= con17z,time = var2);reser49z <- data.frame(reser49z$interest_over_time)
    reser50z <- gtrends(c("sars-cov-2"), geo= con18z,time = var2);  reser50z <- data.frame(reser50z$interest_over_time);  reser51z <- gtrends(c("Covid-19") , geo= con18z,time = var2);   reser51z<- data.frame(reser51z$interest_over_time);reser52z <- gtrends(c("Coronavirus"), geo= con18z,time = var2);reser52z <- data.frame(reser52z$interest_over_time)
    reser53z <- gtrends(c("sars-cov-2"), geo= con19z,time = var2);  reser53z <- data.frame(reser53z$interest_over_time);  reser54z <- gtrends(c("Covid-19") , geo= con19z,time = var2);   reser54z<- data.frame(reser54z$interest_over_time);reser55z <- gtrends(c("Coronavirus"), geo= con19z,time = var2);reser55z <- data.frame(reser55z$interest_over_time)
    reser56z <- gtrends(c("sars-cov-2"), geo= con20z,time = var2);  reser56z <- data.frame(reser56z$interest_over_time);  reser57z <- gtrends(c("Covid-19") , geo= con20z,time = var2);   reser57z<- data.frame(reser57z$interest_over_time);reser58z <- gtrends(c("Coronavirus"), geo= con20z,time = var2);reser58z <- data.frame(reser58z$interest_over_time)
    
    
    res0.0 <- data.frame(res0.0$interest_over_time);res0.1 <- data.frame(res0.1$interest_over_time);res0.2 <- data.frame(res0.2$interest_over_time);
    res1.0 <- data.frame(res1.0$interest_over_time);res1.1 <- data.frame(res1.1$interest_over_time);res1.2 <- data.frame(res1.2$interest_over_time);
    res2.0 <- data.frame(res2.0$interest_over_time);res2.1 <- data.frame(res2.1$interest_over_time);res2.2 <- data.frame(res2.2$interest_over_time);
    res3.0 <- data.frame(res3.0$interest_over_time);res3.1 <- data.frame(res3.1$interest_over_time);res3.2 <- data.frame(res3.2$interest_over_time); 
    res4.0 <- data.frame(res4.0$interest_over_time);res4.1 <- data.frame(res4.1$interest_over_time);res4.2 <- data.frame(res4.2$interest_over_time); 
    res5.0<- data.frame(res5.0$interest_over_time);res5.1 <- data.frame(res5.1$interest_over_time);res5.2 <- data.frame(res5.2$interest_over_time); 
    res6.0 <- data.frame(res6.0$interest_over_time);res6.1 <- data.frame(res6.1$interest_over_time);res6.2 <- data.frame(res6.2$interest_over_time);
    res7.0 <- data.frame(res7.0$interest_over_time);res7.1 <- data.frame(res7.1$interest_over_time);res7.2 <- data.frame(res7.2$interest_over_time);
    res8.0 <- data.frame(res8.0$interest_over_time);res8.1 <- data.frame(res8.1$interest_over_time);res8.2 <- data.frame(res8.2$interest_over_time);
    res9.0 <- data.frame(res9.0$interest_over_time);res9.1 <- data.frame(res9.1$interest_over_time);res9.2 <- data.frame(res9.2$interest_over_time); 
    res10.0 <- data.frame(res10.0$interest_over_time);res10.1 <- data.frame(res10.1$interest_over_time);res10.2 <- data.frame(res10.2$interest_over_time); 
    res11.0 <- data.frame(res11.0$interest_over_time);res11.1 <- data.frame(res11.1$interest_over_time);res11.2 <- data.frame(res11.2$interest_over_time); 
    res12.0 <- data.frame(res12.0$interest_over_time);res12.1 <- data.frame(res12.1$interest_over_time);res12.2 <- data.frame(res12.2$interest_over_time);
    res13.0 <- data.frame(res13.0$interest_over_time);res13.1 <- data.frame(res13.1$interest_over_time);res13.2 <- data.frame(res13.2$interest_over_time); 
    res14.0 <- data.frame(res14.0$interest_over_time);res14.1 <- data.frame(res14.1$interest_over_time);res14.2 <- data.frame(res14.2$interest_over_time); 
    res15.0 <- data.frame(res15.0$interest_over_time);res15.1 <- data.frame(res15.1$interest_over_time);res15.2 <- data.frame(res15.2$interest_over_time); 
    res16.0 <- data.frame(res16.0$interest_over_time);res16.1 <- data.frame(res16.1$interest_over_time);res16.2 <- data.frame(res16.2$interest_over_time);
    res17.0 <- data.frame(res17.0$interest_over_time);res17.1 <- data.frame(res17.1$interest_over_time);res17.2 <- data.frame(res17.2$interest_over_time); 
    res18.0 <- data.frame(res18.0$interest_over_time);res18.1 <- data.frame(res18.1$interest_over_time);res18.2 <- data.frame(res18.2$interest_over_time);
    res19.0 <- data.frame(res19.0$interest_over_time);res19.1 <- data.frame(res19.1$interest_over_time);res19.2 <- data.frame(res19.2$interest_over_time); 
    res20.0 <- data.frame(res20.0$interest_over_time);res20.1 <- data.frame(res20.1$interest_over_time);res20.2 <- data.frame(res20.2$interest_over_time); 
    res21.0 <- data.frame(res21.0$interest_over_time);res21.1 <- data.frame(res21.1$interest_over_time);res21.2 <- data.frame(res21.2$interest_over_time); 
    res22.0 <- data.frame(res22.0$interest_over_time);res22.1 <- data.frame(res22.1$interest_over_time);res22.2 <- data.frame(res22.2$interest_over_time);
    res23.0 <- data.frame(res23.0$interest_over_time);res23.1 <- data.frame(res23.1$interest_over_time);res23.2 <- data.frame(res23.2$interest_over_time); 
    res24.0 <- data.frame(res24.0$interest_over_time);res24.1 <- data.frame(res24.1$interest_over_time);res24.2 <- data.frame(res24.2$interest_over_time); 
    res25.0 <- data.frame(res25.0$interest_over_time);res25.1 <- data.frame(res25.1$interest_over_time);res25.2 <- data.frame(res25.2$interest_over_time); 
    res26.0 <- data.frame(res26.0$interest_over_time);res26.1 <- data.frame(res26.1$interest_over_time);res26.2 <- data.frame(res26.2$interest_over_time);
    res27.0 <- data.frame(res27.0$interest_over_time);res27.1 <- data.frame(res27.1$interest_over_time);res27.2 <- data.frame(res27.2$interest_over_time); 
    res28.0 <- data.frame(res28.0$interest_over_time);res28.1 <- data.frame(res28.1$interest_over_time);res28.2 <- data.frame(res28.2$interest_over_time);
    res29.0 <- data.frame(res29.0$interest_over_time);res29.1 <- data.frame(res29.1$interest_over_time);res29.2 <- data.frame(res29.2$interest_over_time); 
    res30.0 <- data.frame(res30.0$interest_over_time);res30.1 <- data.frame(res30.1$interest_over_time);res30.2 <- data.frame(res30.2$interest_over_time); 
    res31.0 <- data.frame(res31.0$interest_over_time);res31.1 <- data.frame(res31.1$interest_over_time);res31.2 <- data.frame(res31.2$interest_over_time); 
    res32.0 <- data.frame(res32.0$interest_over_time);res32.1 <- data.frame(res32.1$interest_over_time);res32.2 <- data.frame(res32.2$interest_over_time);
    res33.0 <- data.frame(res33.0$interest_over_time);res33.1 <- data.frame(res33.1$interest_over_time);res33.2 <- data.frame(res33.2$interest_over_time); 
    res34.0 <- data.frame(res34.0$interest_over_time);res34.1 <- data.frame(res34.1$interest_over_time);res34.2 <- data.frame(res34.2$interest_over_time); 
    res35.0 <- data.frame(res35.0$interest_over_time);res35.1 <- data.frame(res35.1$interest_over_time);res35.2 <- data.frame(res35.2$interest_over_time); 
    res36.0 <- data.frame(res36.0$interest_over_time);res36.1 <- data.frame(res36.1$interest_over_time);res36.2 <- data.frame(res36.2$interest_over_time);
    res37.0 <- data.frame(res37.0$interest_over_time);res37.1 <- data.frame(res37.1$interest_over_time);res37.2 <- data.frame(res37.2$interest_over_time); 
    res38.0 <- data.frame(res38.0$interest_over_time);res38.1 <- data.frame(res38.1$interest_over_time);res38.2 <- data.frame(res38.2$interest_over_time);
    res39.0 <- data.frame(res39.0$interest_over_time);res39.1 <- data.frame(res39.1$interest_over_time);res39.2 <- data.frame(res39.2$interest_over_time); 
    res40.0 <- data.frame(res40.0$interest_over_time);res40.1 <- data.frame(res40.1$interest_over_time);res40.2 <- data.frame(res40.2$interest_over_time); 
    res41.0 <- data.frame(res41.0$interest_over_time);res41.1 <- data.frame(res41.1$interest_over_time);res41.2 <- data.frame(res41.2$interest_over_time); 
    res42.0 <- data.frame(res42.0$interest_over_time);res42.1 <- data.frame(res42.1$interest_over_time);res42.2 <- data.frame(res42.2$interest_over_time);
    res43.0 <- data.frame(res43.0$interest_over_time);res43.1 <- data.frame(res43.1$interest_over_time);res43.2 <- data.frame(res43.2$interest_over_time); 
    res44.0 <- data.frame(res44.0$interest_over_time);res44.1 <- data.frame(res44.1$interest_over_time);res44.2 <- data.frame(res44.2$interest_over_time); 
    res45.0 <- data.frame(res45.0$interest_over_time);res45.1 <- data.frame(res45.1$interest_over_time);res45.2 <- data.frame(res45.2$interest_over_time); 
    res46.0 <- data.frame(res46.0$interest_over_time);res46.1 <- data.frame(res46.1$interest_over_time);res46.2 <- data.frame(res46.2$interest_over_time);
    res47.0 <- data.frame(res47.0$interest_over_time);res47.1 <- data.frame(res47.1$interest_over_time);res47.2 <- data.frame(res47.2$interest_over_time); 
    res48.0 <- data.frame(res48.0$interest_over_time);res48.1 <- data.frame(res48.1$interest_over_time);res48.2 <- data.frame(res48.2$interest_over_time);
    res49.0 <- data.frame(res49.0$interest_over_time);res49.1 <- data.frame(res49.1$interest_over_time);res49.2 <- data.frame(res49.2$interest_over_time);
    res50 <- data.frame(res50$interest_over_time);res50.1 <- data.frame(res50.1$interest_over_time);res50.2 <- data.frame(res50.2$interest_over_time);
    
    
    temp_1 <- rbind(temp_1,res0.0,res1.0,res2.0,res3.0)
    temp_2 <- rbind(temp_2,res4.0,res5.0,res6.0,res7.0)
    temp_3 <- rbind(temp_3,res8.0,res9.0,res10.0,res11.0)
    temp_4 <- rbind(temp_4,res12.0,res13.0,res14.0,res15.0)
    temp_5 <- rbind(temp_5,res16.0,res17.0,res18.0,res19.0)
    temp_6 <- rbind(temp_6,res20.0,res21.0,res22.0,res23.0)
    temp_7 <- rbind(temp_7,res24.0,res25.0,res26.0,res27.0)
    temp_8 <- rbind(temp_8,res28.0,res29.0,res30.0,res31.0)
    temp_9 <- rbind(temp_9,res32.0,res33.0,res34.0,res35.0)
    temp_10 <- rbind(temp_10,res36.0,res37.0,res38.0,res39.0)
    temp_11 <- rbind(temp_11,res40.0,res41.0,res42.0,res43.0)
    temp_12 <- rbind(temp_12,res44.0,res45.0,res46.0,res47.0)
    temp_13 <- rbind(temp_13,res48.0,res49.0)
    temp_14 <- rbind(temp_14,res0.1,res1.1,res2.1,res3.1)
    temp_15 <- rbind(temp_15,res4.1,res5.1,res6.1,res7.1)
    temp_16<- rbind(temp_16,res8.1,res9.1,res10.1,res11.1)
    temp_17<- rbind(temp_17,res12.1,res13.1,res14.1,res15.1)
    temp_18<- rbind(temp_18,res16.1,res17.1,res18.1,res19.1)
    temp_19<- rbind(temp_19,res20.1,res21.1,res22.1,res23.1)
    temp_20 <- rbind(temp_20,res24.1,res25.1,res26.1,res27.1)
    temp_21<- rbind(temp_21,res28.1,res29.1,res30.1,res31.1)
    temp_22 <- rbind(temp_22,res32.1,res33.1,res34.1,res35.1)
    temp_23 <- rbind(temp_23,res36.1,res37.1,res38.1,res39.1)
    temp_24 <- rbind(temp_24,res40.1,res41.1,res42.1,res43.1)
    temp_25 <- rbind(temp_25,res44.1,res45.1,res46.1,res47.1)
    temp_26 <- rbind(temp_26,res48.1,res49.1)
    temp_27 <- rbind(temp_27,res0.2,res1.2,res2.2,res3.2)
    temp_28 <- rbind(temp_28,res4.2,res5.2,res6.2,res7.2)
    temp_29<- rbind(temp_29,res8.2,res9.2,res10.2,res11.2)
    temp_30<- rbind(temp_30,res12.2,res13.2,res14.2,res15.2)
    temp_31 <- rbind(temp_31,res16.2,res17.2,res18.2,res19.2)
    temp_32 <- rbind(temp_32,res20.2,res21.2,res22.2,res23.2)
    temp_33 <- rbind(temp_33,res24.2,res25.2,res26.2,res27.2)
    temp_34 <- rbind(temp_34,res28.2,res29.2,res30.2,res31.2)
    temp_35 <- rbind(temp_35,res32.2,res33.2,res34.2,res35.2)
    temp_36 <- rbind(temp_36,res36.2,res37.2,res38.2,res39.2)
    temp_37 <- rbind(temp_37,res40.2,res41.2,res42.2,res43.2)
    temp_38 <- rbind(temp_38,res44.2,res45.2,res46.2,res47.2)
    temp_39 <- rbind(temp_39,res48.2,res49.2)
    temp_40 <- rbind(res50,res50.1,res50.2)
    
    temp_a <- rbind(temp_1,temp_2,temp_3,temp_4)
    temp_b <- rbind(temp_5,temp_6,temp_7,temp_8)
    temp_c <- rbind(temp_9,temp_10,temp_11,temp_12,temp_13)   
    temp_d <- rbind(temp_14,temp_15,temp_16,temp_17)
    temp_e <- rbind(temp_18,temp_19,temp_20,temp_21)
    temp_f <- rbind(temp_21,temp_22,temp_23,temp_24)
    temp_g <- rbind(temp_25,temp_26,temp_27,temp_28)
    temp_h <- rbind(temp_29,temp_30,temp_31,temp_32)
    temp_i <- rbind(temp_33,temp_34,temp_35,temp_36)
    temp_j <- rbind(temp_37,temp_38,temp_39,temp_40)
    
    temp_k <- rbind(temp_a,temp_b,temp_c,temp_d,temp_j)
    temp_l <- rbind(temp_e,temp_f,temp_g,temp_h,temp_i)
    
    final_trends_z <- rbind(temp_k,temp_l)
    ########################################################################reserf <- rbind(reser,reser0,reser1,reser2,reser3)
    reserf0 <-rbind(reser,reser0,reser1,reser2,reser3)
    reserf1 <- rbind(reser4,reser5,reser6,reser7,reser8)
    reserf2 <- rbind(reser9,reser10,reser11,reser12,reser13)
    reserf3 <- rbind(reser14,reser15,reser16,reser17,reser18)
    reserf4 <- rbind(reser19,reser20,reser21,reser22,reser23)
    reserf5 <- rbind(reser24,reser25,reser26,reser27,reser28)
    reserf6 <- rbind(reser29,reser30,reser31,reser32,reser33)
    reserf7 <- rbind(reser34,reser35,reser36,reser37,reser38)
    reserf8 <- rbind(reser34,reser35,reser36,reser37,reser38)
    reserf9 <- rbind(reser39,reser40,reser41,reser42,reser43)
    reserf10 <- rbind(reser44,reser45,reser46,reser47,reser48)
    reserf11 <- rbind(reser49,reser50,reser51,reser52,reser53)
    reserf12 <- rbind(reser54,reser55,reser56,reser57,reser58)
    reserf13 <- rbind(reser59,reser60,reser61,reser62,reser63)
    reserf14 <- rbind(reser64,reser65,reser66,reser67,reser68)
    reserf15 <- rbind(reser69,reser70,reser71,reser72,reser73)
    reserf16 <- rbind(reser74,reser75,reser76,reser77,reser78)
    reserf17 <- rbind(reser79,reser80,reser81,reser82,reser83)
    reserf18 <- rbind(reser84,reser85,reser86,reser87,reser88)
    reserf19 <- rbind(reser89,reser90,reser91,reser92,reser93)
    reserf20 <- rbind(reser94,reser95,reser96,reser97,reser98)
    reserf21 <- rbind(reser99,reser100,reser101,reser102,reser103)
    reserf22 <- rbind(reser104,reser105,reser106,reser107,reser108)
    reserf23 <- rbind(reser109,reser110,reser111,reser112,reser113)
    reserf24 <- rbind(reser114,reser115,reser116,reser117,reser118)
    reserf25 <- rbind(reser119,reser120,reser121,reser122,reser123)
    reserf26 <- rbind(reser124,reser125,reser126,reser127,reser128)
    reserf27 <- rbind(reser129,reser130,reser131,reser132,reser133)
    reserf28 <- rbind(reser134,reser135,reser136,reser137,reser138)
    reserf29 <- rbind(reser139,reser140,reser141,reser142,reser143)
    reserf30 <- rbind(reser144,reser145,reser146,reser147,reser148)
    reserf31 <- rbind(reser149,reser150,reser151,reser152,reser153)
    reserf32 <- rbind(reser154,reser155,reser156,reser157,reser158)
    reserf33 <- rbind(reser159,reser160,reser161,reser162,reser163)
    reserf34 <- rbind(reser164,reser165,reser166,reser167,reser168)
    reserf35 <- rbind(reser169,reser170,reser171,reser172,reser173)
    reserf36 <- rbind(reser174,reser175,reser176,reser177,reser178)
    reserf37 <- rbind(reser179,reser180,reser181,reser182,reser183)
    reserf38 <- rbind(reser184,reser185,reser186,reser187,reser188)
    reserf39 <- rbind(reser189,reser190,reser191,reser192,reser193)
    reserf40 <- rbind(reser194,reser195,reser196,reser197,reser198)
    reserf41 <- rbind(reser199,reser200,reser201,reser202,reser203)
    reserf42 <- rbind(reser204,reser205,reser206,reser207,reser208)
    reserf43 <- rbind(reser209,reser210,reser211,reser212,reser213)
    reserf44 <- rbind(reser214,reser215,reser216,reser217,reser218)
    reserf45 <- rbind(reser219,reser220,reser221,reser222,reser223)
    reserf46 <- rbind(reser224,reser225,reser226,reser227,reser228)
    reserf47 <- rbind(reser229,reser230,reser231,reser232,reser233)
    reserf48 <- rbind(reser234,reser235,reser236,reser237,reser238)
    reserf49 <- rbind(reser239,reser240,reser241)
    
    res_final <- rbind(res_final,reserf,reserf1,reserf2,reserf3)
    res_final2 <- rbind(res_final2,reserf4,reserf5,reserf6,reserf7)
    res_final3 <- rbind(res_final3,reserf8,reserf9,reserf10,reserf11)
    res_final4 <- rbind(res_final4,reserf12,reserf13,reserf14,reserf15)
    res_final5 <- rbind(res_final5,reserf16,reserf17,reserf18,reserf19)
    res_final6 <- rbind(res_final6,reserf20,reserf21,reserf22,reserf23)
    res_final7 <- rbind(res_final7,reserf24,reserf25,reserf26,reserf27)
    res_final8 <- rbind(res_final8,reserf28,reserf29,reserf30,reserf31)
    res_final9 <- rbind(res_final9,reserf32,reserf33,reserf34,reserf35)
    res_final10 <- rbind(res_final10,reserf36,reserf37,reserf38,reserf39)
    res_final11 <- rbind(res_final11,reserf40,reserf41,reserf42,reserf43)
    res_final12 <- rbind(res_final12,reserf44,reserf45,reserf46,reserf47)
    res_final13 <- rbind(res_final13,reserf48,reserf49)
    res_final14 <- rbind(res_final14,res_final,res_final2,res_final3,res_final4)
    res_final15 <- rbind(res_final15,res_final5,res_final6,res_final7,res_final8)
    res_final16 <- rbind(res_final16,res_final9,res_final10,res_final11,res_final12)
    
    final_trends <- rbind(final_trends,res_final13,res_final14,res_final15,res_final16)
    
    ##########################################################################################################################
    reserfz <- rbind(reserz,reser0z,reser1z,reser2z,reser3z)
    reserf1z <- rbind(reser4z,reser5z,reser6z,reser7z,reser8z)
    reserf2z <- rbind(reser9z,reser10z,reser11z,reser12z,reser13z)
    reserf3z <- rbind(reser14z,reser15z,reser16z,reser17z,reser18z)
    reserf4z <- rbind(reser19z,reser20z,reser21z,reser22z,reser23z)
    reserf5z <- rbind(reser24z,reser25z,reser26z,reser27z,reser28z)
    reserf6z <- rbind(reser29z,reser30z,reser31z,reser32z,reser33z)
    reserf7z <- rbind(reser34z,reser35z,reser36z,reser37z,reser38z)
    reserf8z <- rbind(reser39z,reser40z,reser41z,reser42z,reser43z)
    reserf9z <- rbind(reser44z,reser45z,reser46z,reser47z,reser48z)
    reserf10z <- rbind(reser49z,reser50z,reser51z,reser52z,reser53z)
    reserf11z <- rbind(reser54z,reser55z,reser56z,reser57z,reser58z)
    
    res_finalz <- rbind(res_finalz,reserfz,reserf1z,reserf2z,reserf3z)
    res_final2z <- rbind(res_final2z,reserf4z,reserf5z,reserf6z,reserf7z)
    res_final3z <- rbind(res_final3z,reserf8z,reserf9z,reserf10z,reserf11z)
    
    final_trends_2 <- rbind(final_trends_2,res_finalz,res_final2z,res_final3z)
    
    final_trends_final<- rbind(final_trends_2,final_trends,final_trends_z)
    
    write.csv(final_trends_final,file="trends_update.csv") # RUN THIS WHEN GET BACK
  final_trends_final <- read.csv("trends_update.csv")
    
###################################################################################################################################
    ###################################################################################################################################
    ###################################################################################################################################
  # get unique geo for final covid trend
  cc2 <- unique(final_trends_final$geo)
  
  # get vector changing 2 letter vector into 3
  codes2 <- countrycode(cc2, origin = 'iso2c', destination = 'iso3c') 
  
  # turn cc2 final_covid_trend geo and codes2 into dataframe
  dat2 <- data.frame(cc2, codes2)
  
  # name column geo to merge
  colnames(dat2)[1] <- "geo"
  
  # merge three letter code at end of dataframe
  final_trends_final <- merge(final_trends_final,dat2, by = c("geo"),all.x = TRUE)
  
  final_trends_final <- final_trends_final[c(8,2,3,4)] # keep only valid columns
  ######################################################################################
  # FIX fix numeric and column structure problems for google trends
  #####################################################################################
  
  # fix the covid 19 into covid-19 only three terms
  word <- "covid 19"
  final_trends_final$keyword <- gsub(paste0("\\b(",paste(word),")\\b"), "Covid-19", final_trends_final$keyword) # this brings up problems with china doesnt have trends to begin with
  
  ######################################################################################
  # FIX HITS REMOVE "<1" REPLACE WITH 0 
  #####################################################################################
  final_trends_final$hits <- gsub("<1",0,final_trends_final$hits)
  final_trends_final$hits <- as.numeric(final_trends_final$hits)
  
  #####################################################################################
  # RESTRUCTURE DATAFRAME
  #####################################################################################
  # SPREAD FUNCTIONS NEEDS INDEX COLUMN
  final_trends_final$row_num <- seq.int(nrow(final_trends_final))
  
  final_trends_final <- final_trends_final[,c(5,1,2,3,4)]
  
  #####################################################################################
  # Narrow to wide 
  #####################################################################################
  # Spread hits column in dataframe into 3 columns
  
  # SPLIT INTO 3 COLUMNS
  final_trends_final <- spread(final_trends_final, keyword, hits)
  
  # REMOVE INDEX COLUMN
  final_trends_final <- final_trends_final[-c(1)]
  final_trends_final[is.na(final_trends_final)] <- 0 # make na values 0
  
  
  final_trends_final <- aggregate(final_trends_final[,c(3,4,5)], by=list(final_trends_final$codes2, final_trends_final$date),FUN = mean, na.rm = TRUE)
  colnames(final_trends_final)[1] <- "country"
  colnames(final_trends_final)[2] <- "date"
  colnames(final_trends_final)[3] <- "Coronavirus"
  colnames(final_trends_final)[4] <- "Covid.19"
  colnames(final_trends_final)[5] <- "sars.cov.2"
  
  
  final_trends_final$date <- as.Date(final_trends_final$date, format = "%Y-%m-%d")
  
  final_trends_final$Coronavirus <- round(final_trends_final$Coronavirus, digits = 0)
  final_trends_final$Covid.19 <- round(final_trends_final$Covid.19, digits = 0)
  final_trends_final$sars.cov.2 <- round(final_trends_final$sars.cov.2, digits = 0)
  
  write.csv(final_trends_final,file="trend_update.csv")
  
  } # Output is final_trends_final
###################################################################################################################################
  # Weather data 
   {
    b <-riem_networks()
    country <- unique(b$code)
    country1 <- data.frame(country);
   # Code to grab 
    temp0 <-NULL;
    temp1 <-NULL;
    temp2 <- NULL;
    for(i in 272:length(country)){
      tryCatch({ 
        temp0 <- country1$country[i]
        temp1 <- riem_stations(network = country[i])
       temp1 <- cbind(temp1,temp0)
        temp2 <- rbind(temp2,temp1)
       # if an error comes up we continue but we show what was wrong
      }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
      
      print(i)
    }
    worldstations <- temp2
    write.csv(worldstations,file="worldstations.csv")

    st_asos3 <- unique(worldstations$id)
  } # get all search terms for weather
  ######################################################################################
  ######################################################################################
  # Get all weather data
  
  
  # Load the stations for weather
worldstations <- read.csv("worldstations.csv")
  st_asos3 <- unique(worldstations$id)
  # Weather Continued
  # Change var to change the date to start grabing weather data from until today..
  var <- newfmax;
  var <-"2020-01-22"
  # The loop will run and save all information in a csv called (covid_weather)
  # the dataframe is called weather 
  {
    # loop for dataframe 
    temp_1 <- NULL; #initalize loop variables
    temp_2 <- NULL; # again
    temp_12 <- NULL;
    # for each unique station until the last one
    for(i in 1:length(st_asos3)){
      tryCatch({   # if any errors come up we want to show but continue the loop (no station to aggregate we skip)
        temp_1 <- NULL;
        temp_12 <-NULL;
        # temp_1 this stores weather information for each station starting from beginning of this year 
        temp_1 <-riem_measures(station = as.character(st_asos3[i]), date_start = var,
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
      
      
      # MAKE A NEW COLUMN FOR EACH STATE OR COUNTRY 
      
      
      print(i)
    }
    
  } # Loop to get weather data
  ######################################################################################
  # Manipulate the rest of the data to be in proper format
  {
  weather <- temp_2 # this is the weather data
  weather_2f <- weather
  write.csv(weather,file="weather_update.csv")
  colnames(weather_2f)[1] <- "date"
  colnames(weather_2f)[7] <- "id"
  
  weather_2f <- weather_2f[c(7,1,2,3,4,5,6)]
  
  weather_3f <- merge(weather_2f, worldstations, by = c("id"),all.x = TRUE)
  b <- data.frame(b)
  colnames(b)[1]<- "names"
  colnames(b)[2]<- "region"
  colnames(weather_3f)[11]<- "names"
  weather_4f <- merge(weather_3f,b, by = c("names") ,all.x = TRUE)
  weather2_f <- weather_4f[c(12,3,4,5,6,7,8)]
  colnames(weather2_f)[1] <- "country"

  write.csv(weather2_f,file="weather_update2.csv") # left off line 817 with weather2_f
  
  weatherff <- weather2_f
######################################################################################
######################################################################################
# RENAME ASOS AND AWOS
# Remove ASOS from country column (REMOVE ASOS AND AWOS)
# Rename countries
######################################################################################
  
  word <- "ASOS"; word1 <- "AWOS" # also remove AWOS
  weather2_f$country <- gsub(paste0("\\b(",paste(word),")\\b"), "", weather2_f$country)
  weather2_f$country <- gsub(paste0("\\b(",paste(word1),")\\b"), "", weather2_f$country)
  

  ######################################################################################
  ######################################################################################
  ######################################################################################
  ######################################################################################
  # RENAME EACH STATE WITH USA
  # UPLOAD USA STATE NAMES TO REPLACE WITH USA
  # import list of states in usa
  ######################################################################################
  
  usa <- read.csv("USA.csv")
  colnames(usa)[1]<-"state"
  
  # Turn each of the 50 states into UNITED STATES...
  x <- usa$name
  # Repalce all off the states with the value US... # reduced down to 211
  weather2_f$country <- sub(paste0("\\b", x, "\\b", collapse="|"), "United States of America ", weather2_f$country, ignore.case = T)
  
  # make all canada territories in canada (CANADA) 
  # REDUCED TO AROUND 199 COUNTRIES
  x1 <- c("Quebec CA","Alberta CA","Yukon Canada","Manitoba CA","British Columbia CA","New Brunswick CA","Newfoundland CA","Northwest Territories CA","Nova Scotia CA","Nunavut Canada","Ontario CA","Prince Edward Island Canada","Saskatchewan CA")
  weather2_f$country <- sub(paste0("\\b", x1, "\\b", collapse="|"), "Canada", weather2_f$country, ignore.case = T)
  
######################################################################################
######################################################################################
# TAKE THE UNIQUE COUNTRY NAMES AND GET THE 3 LETTER ABRIVIATION FROM THEM
# USE COUNTRY CODE TO GET THREE LETTER CODE FROM THE COUNTRY NAME
######################################################################################
  uweather <- unique(weather2_f$country)
  codes3 <- countrycode(uweather, origin = 'country.name', destination = 'iso3c') 
  dat3 <- data.frame(uweather, codes3)
  colnames(dat3)[1] <- "country"
  weather2_f2 <- merge(weather2_f, dat3, by = c("country"),all.x = TRUE)
  weather2_f2 <- weather2_f2[c(8,1,2,3,4,5,6,7)]
  
write.csv(weather2_f2,file="weather_update.csv")
weather3_f3 <- weather2_f2
#weather2_f2 <- weather3_f3
######################################################################################
######################################################################################
######################################################################################
# AGGREGATE WEATHER DATA
######################################################################################
  weather2_f2 <- aggregate(weather2_f2[,c(4,5,6,7,8)], by=list(weather2_f2$codes3, weather2_f2$date), FUN=mean, na.rm = TRUE) # 187 unique codes
  colnames(weather2_f2)[1] <- "country"
  colnames(weather2_f2)[2] <- "date"
  weather2_f2 <- na.omit(weather2_f2) # ATA IS ONLY COUNTRY WITH PROBLEMS
   write.csv(weather2_f2,file="weather_update.csv")
   
   
   
   # impute time series
   weather2_f2 <-weather2_f2
   # THIS CODE SPLITS UP INTO MANY DATAFRAMES FOR EACH COUNTRY CODE AND THEN IMPUTES AND REBINDS THEM TOGETHER
   weather2_f2 <- weather2_f2 %>% split(final_df2$country) %>% 
     lapply(function(x) {na.locf(na.locf(x), fromLast=T)}) %>% 
     do.call(rbind, .)
   
   
   weather2_f2 <- weather2_f2[!duplicated(weather2_f2), ]
   
   write.csv(weather2_f2,file="weather_update.csv")
   
  # weather2_f2 <- read.csv("weather_update.csv")
   ##########################################
   
  }
  ###################################################################################################################################
  #population data, median age and income data links
  # https://worldpopulationreview.com/countries/?fbclid=IwAR00GQH4sXsqgh6VKaI0kbpbUHRWYuLKjBrfwtQ4plSJdZXZNYyObC8-k5o
  {
    population1 <- read.csv("population1.csv")
    population1 <- population1[-c(1)]
    colnames(population1)[1]<- "country"
  } # 
  ###################################################################################################################################
##############################################################################################################################################################
############################################################################################################################################################################################################################################################################################################################
##############################################################################################################################################################
# Merge all the dataframes together
{
  maxcovid <-max(covid_count_f$date)
  maxtrend <-max(final_trends_final$date)# this is the lowest date.
  maxweather <-max(weather2_f2$date)

  ucovid <- unique(covid_count_f$country)
  utrends <- unique(final_trends_final$country)
  uweather <- unique(weather2_f2$country)
  
  # merge country and trend
  merge1 <- merge(covid_count_f,final_trends_final, by =c("country","date"),all.x = TRUE)
  
  merge1Z <- merge1[merge1$country != "AGO" & merge1$country != "ATG" & merge1$country != "BDI" & merge1$country != "BFA" & merge1$country != "BIH" & merge1$country != "BOL" & merge1$country != "CAF" & merge1$country != "COG" & merge1$country != "GAB" & merge1$country != "GIN" & merge1$country != "GMB" & merge1$country != "GNB" & merge1$country != "GUY" & merge1$country != "HND" & merge1$country != "HTI" & merge1$country != "MCO" & merge1$country != "MDA" & merge1$country != "MNG" & merge1$country != "NAM" & merge1$country != "NER" & merge1$country != "NGA" & merge1$country != "TCD" & merge1$country != "TLS" & merge1$country != "TTO" & merge1$country != "UGA" & merge1$country != "VAT" & merge1$country != "ARG" & merge1$country != "BWA" & merge1$country != "CHL" & merge1$country != "COD" & merge1$country != "COL" & merge1$country != "GEO" & merge1$country != "HUN" & merge1$country != "ISL" & merge1$country != "LKA" & merge1$country != "PHL" & merge1$country != "PRY" & merge1$country != "SSD" & merge1$country != "SUR" & merge1$country != "TUN" & merge1$country != "VNM",] 
  merge1 <- merge1Z[!is.na(merge1Z$country),]
  
  merge2 <- merge(merge1,weather2_f2, by =c("country","date"),all.x = TRUE)
  merge2Z <- merge2[merge2$country != "AND" & merge2$country != "BRN" & merge2$country != "ERI" & merge2$country != "ESH" & merge2$country != "KGZ" & merge2$country != "LBY" & merge2$country != "LIE" & merge2$country != "LSO" & merge2$country != "MDG" & merge2$country != "MLT" & merge2$country != "MNE" & merge2$country != "PAN" & merge2$country != "PSE" & merge2$country != "SMR" & merge2$country != "SRB" & merge2$country != "YEM",] 
  
  # impute leftover countries
  # THIS CODE SPLITS UP INTO MANY DATAFRAMES FOR EACH COUNTRY CODE AND THEN IMPUTES AND REBINDS THEM TOGETHER
  merge2Z <- merge2Z %>% split(merge2Z$country) %>% 
    lapply(function(x) {na.locf(na.locf(x), fromLast=T)}) %>% 
    do.call(rbind, .)
  
  # make merge2Z merge2
  merge2 <- merge2Z
  
  merge3 <- merge(merge2,population1, by =c("country"),all.x = TRUE)
  final_data <- merge3
  write.csv(final_data,file = "final_update.csv")
  #final_data <- read.csv("final_datazz.csv")
  #final_data <- final_data[-c(1)]
  
  
  final_datafz <- rbind(final_data,final_df) # update the new dataframe
  
  write.csv(final_datafz,file = "finaldataframe.csv")
  

}
}
