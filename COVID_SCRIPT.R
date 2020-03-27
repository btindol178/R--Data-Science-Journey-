devtools::install_github("PMassicotte/gtrendsR")
install.packages("gtrendsR")
library(gtrendsR)
install.packages("ggplot2")
library(ggplot2)

# Country DF
countries1 <- read.csv("countrycodes1.csv")
colnames(countries1)[1] <- "geo"

# DATAFRAME OF COUNTRIES CODE AND NAME
countries <- read.csv("country.csv")

# REMOVE THE NAME COLUMN JUST CODE
country <- countries[-2]

# TERMS TO LOOK UP LATER
terms <- c("corona","corona virus","virus")

# LOOK UP ALL VALUES OF CORONA VIRUS FOR THESE COUNTRIES
res <- gtrends(c("corona virus"), geo = c("AD","AE","AF"),time = "2019-11-01 2020-03-19")
plot(res)
df <- as.data.frame(res$interest_over_time)

res1 <- gtrends(c("corona virus"), geo = c("AG","AI","AL"),time = "2019-11-01 2020-03-19")
plot(res1)
df1 <- as.data.frame(res1$interest_over_time)

res2 <- gtrends(c("corona virus"), geo = c("AM","AO","AR"),time = "2019-11-01 2020-03-19")
plot(res2) 
df2 <- as.data.frame(res2$interest_over_time)

res3 <- gtrends(c("corona virus"), geo = c("AS","AT","AU"),time = "2019-11-01 2020-03-19")
plot(res3) 
df3 <- as.data.frame(res3$interest_over_time)

res4 <- gtrends(c("corona virus"), geo = c("AW","AX","AZ"),time = "2019-11-01 2020-03-19")
plot(res4) 
df4 <- as.data.frame(res4$interest_over_time)

res5 <- gtrends(c("corona virus"), geo = c("BA","BB","BD"),time = "2019-11-01 2020-03-19")
plot(res5) 
df5 <- as.data.frame(res5$interest_over_time)

res6 <- gtrends(c("corona virus"), geo = c("BE","BF","BG"),time = "2019-11-01 2020-03-19")
plot(res6) 
df6 <- as.data.frame(res6$interest_over_time)

# RBIND CURRENT DATAFRAMES
temp <- rbind(df,df1,df2,df3,df4,df5,df6)

res7 <- gtrends(c("corona virus"), geo = c("BH","BI","BJ"),time = "2019-11-01 2020-03-19")
plot(res7) 
df7 <- as.data.frame(res7$interest_over_time)

res8 <- gtrends(c("corona virus"), geo = c("BL","BM","BN"),time = "2019-11-01 2020-03-19")
plot(res8) 
df8 <- as.data.frame(res8$interest_over_time)

res9 <- gtrends(c("corona virus"), geo = c("BO","BQ","BR"),time = "2019-11-01 2020-03-19")
plot(res9) 
df9 <- as.data.frame(res9$interest_over_time)

res10 <- gtrends(c("corona virus"), geo = c("BS","BT","BW"),time = "2019-11-01 2020-03-19")
plot(res10) 
df10 <- as.data.frame(res10$interest_over_time)

res11 <- gtrends(c("corona virus"), geo = c("BY","BZ","CA"),time = "2019-11-01 2020-03-19")
plot(res11) 
df11 <- as.data.frame(res11$interest_over_time)

# RBIND SECOND CURRENT SET OF DATAFRAMES
temp1 <- rbind(df7,df8,df9,df10,df11)

res12 <- gtrends(c("corona virus"), geo = c("CG","CD","CF"),time = "2019-11-01 2020-03-19")
plot(res12) 
df12 <- as.data.frame(res12$interest_over_time)

res13 <- gtrends(c("corona virus"), geo = c("CH","CI","CK"),time = "2019-11-01 2020-03-19")
plot(res13) 
df13 <- as.data.frame(res13$interest_over_time)

res14 <- gtrends(c("corona virus"), geo = c("CL","CM","CN"),time = "2019-11-01 2020-03-19")
plot(res14) 
df14 <- as.data.frame(res14$interest_over_time)

res15 <- gtrends(c("corona virus"), geo = c("CO","CR","CU"),time = "2019-11-01 2020-03-19")
plot(res15) 
df15 <- as.data.frame(res15$interest_over_time)

res16 <- gtrends(c("corona virus"), geo = c("CV","CW","CY"),time = "2019-11-01 2020-03-19")
plot(res16) 
df16 <- as.data.frame(res16$interest_over_time)

# RBIND THIRD CURRENT SET OF DATAFRAMES
temp2 <- rbind(df12,df13,df14,df15,df16)

res17 <- gtrends(c("corona virus"), geo = c("CZ","DE","DJ"),time = "2019-11-01 2020-03-19")
plot(res17) 
df17 <- as.data.frame(res17$interest_over_time)

res18 <- gtrends(c("corona virus"), geo = c("DK","DM","DO"),time = "2019-11-01 2020-03-19")
plot(res18) 
df18 <- as.data.frame(res18$interest_over_time)

res19 <- gtrends(c("corona virus"), geo = c("DZ","EC","EE"),time = "2019-11-01 2020-03-19")
plot(res19) 
df19 <- as.data.frame(res19$interest_over_time)

res20 <- gtrends(c("corona virus"), geo = c("EG","EH","ER"),time = "2019-11-01 2020-03-19")
plot(res20) 
df20 <- as.data.frame(res20$interest_over_time)

res21 <- gtrends(c("corona virus"), geo = c("ES","ET","FI"),time = "2019-11-01 2020-03-19")
plot(res21) 
df21 <- as.data.frame(res21$interest_over_time)

res22 <- gtrends(c("corona virus"), geo = c("FJ","FK","FM"),time = "2019-11-01 2020-03-19")
plot(res22) 
df22 <- as.data.frame(res22$interest_over_time)

# RBIND FOURTH CURRENT SET OF DATAFRAMES
temp3 <- rbind(df17,df18,df19,df20,df21,df22)

res23 <- gtrends(c("corona virus"), geo = c("FO","FR","GA"),time = "2019-11-01 2020-03-19")
plot(res23) 
df23 <- as.data.frame(res23$interest_over_time)

res24 <- gtrends(c("corona virus"), geo = c("GB","GD","GE"),time = "2019-11-01 2020-03-19")
plot(res24) 
df24 <- as.data.frame(res24$interest_over_time)

res25 <- gtrends(c("corona virus"), geo = c("GF","GG","GH"),time = "2019-11-01 2020-03-19")
plot(res25) 
df25 <- as.data.frame(res25$interest_over_time)

res26 <- gtrends(c("corona virus"), geo = c("GI","GL","GM"),time = "2019-11-01 2020-03-19")
plot(res26) 
df26 <- as.data.frame(res26$interest_over_time)

res27 <- gtrends(c("corona virus"), geo = c("GN","GP","GQ"),time = "2019-11-01 2020-03-19")
plot(res27) 
df27 <- as.data.frame(res27$interest_over_time)

# RBIND FITH CURRENT SET OF DATAFRAMES
temp4 <- rbind(df23,df24,df25,df26,df27)

res28 <- gtrends(c("corona virus"), geo = c("GR","GT","GU"),time = "2019-11-01 2020-03-19")
plot(res28) 
df28 <- as.data.frame(res28$interest_over_time)

res29 <- gtrends(c("corona virus"), geo = c("GW","GY","HK"),time = "2019-11-01 2020-03-19")
plot(res29) 
df29 <- as.data.frame(res29$interest_over_time)

res30 <- gtrends(c("corona virus"), geo = c("HN","HR","HT"),time = "2019-11-01 2020-03-19")
plot(res30) 
df30 <- as.data.frame(res30$interest_over_time)

res31 <- gtrends(c("corona virus"), geo = c("HU","ID","IE"),time = "2019-11-01 2020-03-19")
plot(res31) 
df31 <- as.data.frame(res31$interest_over_time)

res32 <- gtrends(c("corona virus"), geo = c("IL","IM","IN"),time = "2019-11-01 2020-03-19")
plot(res32) 
df32 <- as.data.frame(res32$interest_over_time)

res33 <- gtrends(c("corona virus"), geo = c("IQ","IR","IS"),time = "2019-11-01 2020-03-19")
plot(res33) 
df33 <- as.data.frame(res33$interest_over_time)

# RBIND SIX CURRENT SET OF DATAFRAMES
temp5 <- rbind(df28,df29,df30,df31,df32,df33)

res34 <- gtrends(c("corona virus"), geo = c("IT","JE","JM"),time = "2019-11-01 2020-03-19")
plot(res34) 
df34 <- as.data.frame(res34$interest_over_time)

res35 <- gtrends(c("corona virus"), geo = c("JO","JP","KE"),time = "2019-11-01 2020-03-19")
plot(res35) 
df35 <- as.data.frame(res35$interest_over_time)

res36 <- gtrends(c("corona virus"), geo = c("KG","KH","KM"),time = "2019-11-01 2020-03-19")
plot(res36) 
df36 <- as.data.frame(res36$interest_over_time)

res37 <- gtrends(c("corona virus"), geo = c("KN","KW","KR"),time = "2019-11-01 2020-03-19")
plot(res37) 
df37 <- as.data.frame(res37$interest_over_time)

res38 <- gtrends(c("corona virus"), geo = c("KY","KZ","LA"),time = "2019-11-01 2020-03-19")
plot(res38) 
df38 <- as.data.frame(res38$interest_over_time)

# RBIND SEVEN CURRENT SET OF DATAFRAMES
temp6 <- rbind(df34,df35,df36,df37,df38)

res39 <- gtrends(c("corona virus"), geo = c("LB","LC","LI"),time = "2019-11-01 2020-03-19")
plot(res39) 
df39 <- as.data.frame(res39$interest_over_time)

res40 <- gtrends(c("corona virus"), geo = c("LK","LR","LS"),time = "2019-11-01 2020-03-19")
plot(res40) 
df40 <- as.data.frame(res40$interest_over_time)

res41 <- gtrends(c("corona virus"), geo = c("LT","LU","LV"),time = "2019-11-01 2020-03-19")
plot(res41) 
df41 <- as.data.frame(res41$interest_over_time)

res42 <- gtrends(c("corona virus"), geo = c("LY","MA","MD"),time = "2019-11-01 2020-03-19")
plot(res42) 
df42 <- as.data.frame(res42$interest_over_time)

res43 <- gtrends(c("corona virus"), geo = c("ME","MF","MG"),time = "2019-11-01 2020-03-19")
plot(res43) 
df43 <- as.data.frame(res43$interest_over_time)

# RBIND EIGHT CURRENT SET OF DATAFRAMES
temp7 <- rbind(df39,df40,df41,df42,df43)

res44 <- gtrends(c("corona virus"), geo = c("MH","MK","ML"),time = "2019-11-01 2020-03-19")
plot(res44) 
df44 <- as.data.frame(res44$interest_over_time)

res45 <- gtrends(c("corona virus"), geo = c("MM","MN","MO"),time = "2019-11-01 2020-03-19")
plot(res45) 
df45 <- as.data.frame(res45$interest_over_time)

res46 <- gtrends(c("corona virus"), geo = c("MP","MQ","MR"),time = "2019-11-01 2020-03-19")
plot(res46) 
df46 <- as.data.frame(res46$interest_over_time)

res47 <- gtrends(c("corona virus"), geo = c("MS","MT","MU"),time = "2019-11-01 2020-03-19")
plot(res47) 
df47 <- as.data.frame(res47$interest_over_time)

res48 <- gtrends(c("corona virus"), geo = c("MV","MW","MX"),time = "2019-11-01 2020-03-19")
plot(res48) 
df48 <- as.data.frame(res48$interest_over_time)

# RBIND NINE CURRENT SET OF DATAFRAMES
temp8 <- rbind(df44,df45,df46,df47,df48)

res49 <- gtrends(c("corona virus"), geo = c("MY","MZ","NC"),time = "2019-11-01 2020-03-19")
plot(res49) 
df49 <- as.data.frame(res49$interest_over_time)

res50 <- gtrends(c("corona virus"), geo = c("NE","NG","NI"),time = "2019-11-01 2020-03-19")
plot(res50) 
df50 <- as.data.frame(res50$interest_over_time)

res51 <- gtrends(c("corona virus"), geo = c("NL","NO","NP"),time = "2019-11-01 2020-03-19")
plot(res51) 
df51 <- as.data.frame(res51$interest_over_time)

res52 <- gtrends(c("corona virus"), geo = c("NZ","OM","PA"),time = "2019-11-01 2020-03-19")
plot(res52) 
df52 <- as.data.frame(res52$interest_over_time)

res53 <- gtrends(c("corona virus"), geo = c("PE","PF","PG"),time = "2019-11-01 2020-03-19")
plot(res53) 
df53 <- as.data.frame(res53$interest_over_time)

# RBIND TEN CURRENT SET OF DATAFRAMES
temp9 <- rbind(df49,df50,df51,df52,df53)

res54 <- gtrends(c("corona virus"), geo = c("PH","PK","PL"),time = "2019-11-01 2020-03-19")
plot(res54) 
df54 <- as.data.frame(res54$interest_over_time)

res55 <- gtrends(c("corona virus"), geo = c("PR","PS","PT"),time = "2019-11-01 2020-03-19")
plot(res55) 
df55 <- as.data.frame(res55$interest_over_time)

res56 <- gtrends(c("corona virus"), geo = c("PW","PY","QA"),time = "2019-11-01 2020-03-19")
plot(res56) 
df56 <- as.data.frame(res56$interest_over_time)

res57 <- gtrends(c("corona virus"), geo = c("RO","RS","RU"),time = "2019-11-01 2020-03-19")
plot(res57) 
df57 <- as.data.frame(res57$interest_over_time)

res58 <- gtrends(c("corona virus"), geo = c("RW","SA","SB"),time = "2019-11-01 2020-03-19")
plot(res58) 
df58 <- as.data.frame(res58$interest_over_time)

# RBIND ELEVEN CURRENT SET OF DATAFRAMES
temp10 <- rbind(df44,df55,df56,df57,df58)

res59 <- gtrends(c("corona virus"), geo = c("SC","SD","SE"),time = "2019-11-01 2020-03-19")
plot(res59) 
df59 <- as.data.frame(res59$interest_over_time)

res60 <- gtrends(c("corona virus"), geo = c("SG","SH","SI"),time = "2019-11-01 2020-03-19")
plot(res60) 
df60 <- as.data.frame(res60$interest_over_time)

res61 <- gtrends(c("corona virus"), geo = c("SJ","SK","SL"),time = "2019-11-01 2020-03-19")
plot(res61) 
df61 <- as.data.frame(res61$interest_over_time)

res62 <- gtrends(c("corona virus"), geo = c("SM","SN","SO"),time = "2019-11-01 2020-03-19")
plot(res62) 
df62 <- as.data.frame(res62$interest_over_time)

res63 <- gtrends(c("corona virus"), geo = c("SR","SS","ST"),time = "2019-11-01 2020-03-19")
plot(res63) 
df63 <- as.data.frame(res63$interest_over_time)

res64 <- gtrends(c("corona virus"), geo = c("SV","SX","SY"),time = "2019-11-01 2020-03-19")
plot(res64) 
df64 <- as.data.frame(res64$interest_over_time)

res65 <- gtrends(c("corona virus"), geo = c("SZ","TC","TD"),time = "2019-11-01 2020-03-19")
plot(res65) 
df65 <- as.data.frame(res65$interest_over_time)

# RBIND TWELVE CURRENT SET OF DATAFRAMES
temp11 <- rbind(df59,df60,df61,df62,df63,df64,df65)

res66 <- gtrends(c("corona virus"), geo = c("TG","TH","TJ"),time = "2019-11-01 2020-03-19")
plot(res66) 
df66 <- as.data.frame(res66$interest_over_time)

res67 <- gtrends(c("corona virus"), geo = c("TM","TN","TO"),time = "2019-11-01 2020-03-19")
plot(res67) 
df67 <- as.data.frame(res67$interest_over_time)

res68 <- gtrends(c("corona virus"), geo = c("TR","TT","TW"),time = "2019-11-01 2020-03-19")
plot(res68) 
df68 <- as.data.frame(res68$interest_over_time)

res69 <- gtrends(c("corona virus"), geo = c("TZ","UA","UG"),time = "2019-11-01 2020-03-19")
plot(res69) 
df69 <- as.data.frame(res69$interest_over_time)

res70 <- gtrends(c("corona virus"), geo = c("US","UY","UZ"),time = "2019-11-01 2020-03-19")
plot(res70) 
df70 <- as.data.frame(res70$interest_over_time)

# RBIND THIRTEEN CURRENT SET OF DATAFRAMES
temp12 <- rbind(df66,df67,df68,df69,df70)

res71 <- gtrends(c("corona virus"), geo = c("VC","VE","VG"),time = "2019-11-01 2020-03-19")
plot(res71) 
df71 <- as.data.frame(res71$interest_over_time)

res72 <- gtrends(c("corona virus"), geo = c("VI","VN","VU"),time = "2019-11-01 2020-03-19")
plot(res72) 
df72 <- as.data.frame(res72$interest_over_time)

res73 <- gtrends(c("corona virus"), geo = c("WS","YE","YT"),time = "2019-11-01 2020-03-19")
plot(res73) 
df73 <- as.data.frame(res73$interest_over_time)

res74 <- gtrends(c("corona virus"), geo = c("ZA","ZM","ZW"),time = "2019-11-01 2020-03-19")
plot(res74) 
df74 <- as.data.frame(res74$interest_over_time)

# RBIND FOURTEEN CURRENT SET OF DATAFRAMES
temp13 <- rbind(df71,df72,df73,df74)

# MAKING FINAL DATAFRAME
fin_df <- rbind(temp,temp1,temp2,temp3,temp4,temp5,temp6)
fin_df1 <- rbind(temp7,temp8,temp9,temp10,temp11,temp12,temp13)
final_df <-rbind(fin_df,fin_df1)
##########################################################
write.csv(final_df,"COVID19.csv")
#THIS IS FINAL DATAFRAME!
final_df  # THIS IS THE TREND DATAFRAME!!!!!!!!!!!!!!!!!!!
#########################################################
# New Merged final dataframe
df_fin <- merge(final_df,countries1,by=c("geo"),all.x=TRUE)
covidtrend <- df_fin # this will be the final dataframe

#importing all of the count information until this day
covidcount <- read.csv("covid-19 actual final data.csv")

# find unique countries and match them 
countcountry <- unique(covidcount$Country.Region)
trendcountry <- unique(covidtrend$country.name)

# picking the countries from count that are in trend!
yes <- covidtrend[trendcountry %in% countcountry,]

# writing and importing trend that has all counts countries
write.csv(yes,file="count&trend.csv")
trend_full <- read.csv("count&trend.csv")
colnames(trend_full)[9] <- "country_name"
colnames(covidcount)[2] <- "country_name"
colnames(covidcount)[1] <- "province_state" 

#FINAL DATAFRAME THAT MERGES BOTH
COVID_19 <- merge(trend_full,covidcount, by=c("country_name"),all.X = TRUE)
write.csv(COVID_19,file="COVID_19.csv")













# VISUALIZATION!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

install.packages("leaflet")
library(leaflet)

install.packages("rgdal")
library(rgdal)

install.packages("choroplethr")
library(choroplethr)

install.packages("choroplethrMaps")
library(choroplethrMaps)

library(dplyr)
# MAKING SURE THE COUNTRY NAME FORMAT MATCHES THE ONES FROM THIS PACKAGE
data(country.map, package = "choroplethrMaps")
# GETTING UNIQUE COUNTRY NAMES FROM PACKAGE
umap <- unique(country.map$region)

# Just plotting for one day for simplicity
mar18 <- COVID_19[COVID_19$date == "2020-03-18",]

# convert column name to lower
mar18$country_name <- tolower(mar18[,1])

#getting unique mar 18 names 
umar <- unique(mar18$country_name)

# see if the variable names are in the package choroplethrMaps
yes1 <- mar18[umar %in% umap,]

# aggregate by country 
aggdata <- yes1 %>%
  group_by(country_name) %>%
  summarise(value = mean(Confirmed))

#rename column
colnames(aggdata)[1] <- "region"

plotdata <- aggdata

country_choropleth(plotdata)
