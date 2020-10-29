rm(list=ls())
setwd("C:/Users/blake/OneDrive/Stryker Project/Interview Project/Deliverables")

# load librarys
library(tidyverse)
library(readxl)
#install.packages("writexl")
library(writexl)
library(lubridate)
library(dplyr)
library(purrr)

#get excel worksheet path
wb_source <- "C:/Users/blake/OneDrive/Stryker Project/Interview Project/BusinessAnalyticsCaseStudy.xlsx"

#read it 
wb_sheets <- readxl::excel_sheets(wb_source) 

# print sheets
print(wb_sheets)

# Load everything into the Global Environment splitting workbook sheets into individual dataframes
wb_sheets %>%
  purrr::map(function(sheet){ # iterate through each sheet name
    assign(x = sheet,
           value = readxl::read_xlsx(path = wb_source, sheet = sheet),
           envir = .GlobalEnv)
  })
######################################################################################################################################
######################################################################################################################################
######################################################################################################################################
#1a Calculate average Discount %
mean(SalesOrderDetail$UnitPriceDiscount)
# The average discount percent is %0.002826067

# Because there was duplicates per salesorderID needed to groupby
meandiscountpercent <- SalesOrderDetail%>%
  group_by(SalesOrderID)%>%
  summarise(averagediscount = mean(UnitPriceDiscount))
mean(meandiscountpercent$averagediscount) #0.0006131339 including sales orders with no discounts

meandiscountpercent2 <- meandiscountpercent[meandiscountpercent$averagediscount > 0,]
mean(meandiscountpercent2$averagediscount) #0.02106142
######################################################################################################################################
######################################################################################################################################
######################################################################################################################################

#1b Average Selling Price this is with discount 
mean(SalesOrderDetail$LineTotal) 
# 905.4492

mean(SalesOrderDetail$Revenue) 
# 909.7974 without discount

DiscountPriceVsNot <- sum(SalesOrderDetail$Revenue) - sum(SalesOrderDetail$LineTotal)
DiscountPriceVsNot
# 527507.9 lost profit due to discount

avgsellprice <- merge(SalesOrderHeader,SalesOrderDetail,by=c("SalesOrderID"),all.x=TRUE)

mean(avgsellprice$UnitPrice) # Average unit price


######################################################################################################################################
######################################################################################################################################
######################################################################################################################################

#1c Growth Vs. Prior year by product Category and Ship year.
Product <- Product[c(1:12,14,13)]
merge1 <- merge(Product,ProductSubCategory, by =c("ProductSubcategoryID"),all.x=TRUE)
names(merge1)[3] <- "Product Name"; names(merge1)[16] <- "ProductSubCategory Name"
merge1 <- merge1[c(1:14,16,15)]
merge2 <- merge(merge1,ProductCategory, by =c("ProductCategoryID"),all.x=TRUE)
names(merge2)[17] <- "ProductCategory Name"
merge2 <- merge2[c(1,2,4:17,3)]
merge3 <- merge(merge2,SalesOrderDetail, by =c("ProductID"),all.x=TRUE)
merge3 <- merge3[c(1:17,19:25,18)]
merge4 <- merge(merge3,SalesOrderHeader, by =c("SalesOrderID"),all.x=TRUE)
yoy_df <- merge4
yoy_df$revenue <- yoy_df$OrderQty * yoy_df$UnitPrice
yoy_df$year <- year(yoy_df$ShipDate)
yoy_df$month <- month(yoy_df$ShipDate)
yoy_df$day <- day(yoy_df$ShipDate)

options(scipen = 999)
df2 <- yoy_df %>% group_by(ShipDate,ProductCategoryID) %>%
  summarize(amount=sum(revenue))
df2$amount <- round(df2$amount,digits = 2)
df2 <- df2[order(df2$ShipDate,df2$ProductCategoryID,-df2$amount),]
df2$year <- year(df2$ShipDate)
df2$month <- month(df2$ShipDate)
df2$day <- day(df2$ShipDate)
df3 <- merge(df2,ProductCategory,by=c("ProductCategoryID"),all.x=TRUE)

ShipDate2011 <- df2[df2$year == "2011",];colnames(ShipDate2011)[3] <- "amount_2011"
ShipDate2012 <- df2[df2$year == "2012",];colnames(ShipDate2012)[3] <- "amount_2012"
ShipDate2013 <- df2[df2$year == "2013",];colnames(ShipDate2013)[3] <- "amount_2013"
ShipDate2014 <- df2[df2$year == "2014",];colnames(ShipDate2014)[3] <- "amount_2014"

dff <- merge(ShipDate2011,ShipDate2012, by = c("month","day","ProductCategoryID"),all.x = TRUE)
dff <- dff[c(1,2,3,5,8)]
dff2 <- merge(dff,ShipDate2013, by =c("month","day","ProductCategoryID"),all.x = TRUE)
dff2 <- dff2[c(1:5,7)]
dff3 <- merge(dff2,ShipDate2014, by =c("month","day","ProductCategoryID"),all.x = TRUE)
dff3 <- dff3[c(1:6,8)]
dff3[is.na(dff3)]<- 0
dfvar <-  ((dff3$amount_2012 / dff3$amount_2011)-1)*100
dfvar1 <-  ((dff3$amount_2013 / dff3$amount_2012)-1)*100
dfvar2 <-  ((dff3$amount_2014 / dff3$amount_2013)-1)*100
dff4 <- dff3
dff4$diff_2012_2011 <- round(dfvar,digits = 2)
dff4$diff_2013_2012 <- round(dfvar1,digits = 2)
dff4$diff_2014_2013 <- round(dfvar2,digits = 2)
dff4 <- dff4[-c(227:482),]
dff4 <- dff4[c(1,2,4:10,3)]
growthvsprior <- merge(dff4,ProductCategory,by=c("ProductCategoryID"),all.x=TRUE);colnames(growthvsprior)[11] <- "Product Cateogry Name"

growthvsprior # Visualize this 
#1d Select appropriate visual to present the result.
# in power bi
??????????????????????????????????????????????????????????????? #Not Done

######################################################################################################################################
######################################################################################################################################
######################################################################################################################################

# 2.What product sub-categories are driving the sales growth according to you?
# (Display Top 5 for ship year 2014)
SubCat_Growth <-  yoy_df %>% group_by(ProductSubcategoryID,year) %>%
  summarize(Revenue=sum(revenue))
SubCat_Growth2 <- merge(SubCat_Growth,ProductSubCategory, by =c("ProductSubcategoryID"),all.x = TRUE)
SubCat_Growth2 <- SubCat_Growth2[-c(4)];colnames(SubCat_Growth2)[4] <- "Product Sub Category Name"
SubCat_Growth3 <- na.omit(SubCat_Growth2)
SubCat_Growth4 <- SubCat_Growth3 %>%
  arrange(ProductSubcategoryID,year)

# taking the revenue for 2014 - 2011 to get largest growing amount.....
SubCat_Growth5 <- SubCat_Growth4  %>% group_by(ProductSubcategoryID) %>% 
  mutate(
    first=first(Revenue), 
    last=last(Revenue), 
  )
SubCat_Growth5$Growth <- round(((SubCat_Growth5$last/SubCat_Growth5$first)-1)*100,digits = 2)

SubCat_Growth6 <- SubCat_Growth5[c(1,4,7)]
SubCat_Growth7 <- distinct(SubCat_Growth6)
SubCat_Growth8 <- SubCat_Growth7 %>% # The top growing categories!!!!!!!! from first to last year difference.
  arrange(desc(Growth))

head(SubCat_Growth8,n=5)
#Answer Helmets almost 700%,Jerseys,Caps,Shorts,Socks

#####################################################################################################################################
# Top 5 for shipyear 2014

# filter for 2014
SubCat_Growth2014 <- SubCat_Growth5[SubCat_Growth5$year == 2014,]
SubCat_Growth2014_top5 <- SubCat_Growth2014 %>%
  arrange(desc(Revenue))
SubCat_Growth2014_top5$Revenue <- round(SubCat_Growth2014_top5$Revenue,digits = 2)
  


#filter for either 2014 or 2013 to get growth from that year
SubCat_Growth2014_2013diff <- SubCat_Growth5[SubCat_Growth5$year == 2014 | SubCat_Growth5$year == 2013,]

# 2014/2013
SubCat_Growth2014_2013diff_Growth <- SubCat_Growth2014_2013diff  %>% group_by(ProductSubcategoryID) %>% 
  mutate(
    first=first(Revenue), 
    last=last(Revenue), 
  )

#2014/2013
SubCat_Growth2014_2013diff_Growth$Growth <- round(((SubCat_Growth2014_2013diff_Growth$last/SubCat_Growth2014_2013diff_Growth$first)-1)*100,digits = 2)
SubCat_Growth2014_2013diff_Growth <- SubCat_Growth2014_2013diff_Growth[c(1,4,7)] # get rid of year
SubCat_Growth2014_2013diff_Growth <- distinct(SubCat_Growth2014_2013diff_Growth)# get rid of one of the years because there the same
SubCat_Growth2014_2013diff_Growth <- SubCat_Growth2014_2013diff_Growth %>% # The top growing categories!!!!!!!! from first to last year difference.
  arrange(desc(Growth))
SubCat_Growth2014_2013diff_Growth <- SubCat_Growth2014_2013diff_Growth[-c(4:8),]
SubCat_Growth2014_2013diff_Growth <- head(SubCat_Growth2014_2013diff_Growth,n=5)


# These are the top 5 categories when you take the difference from 2014-2013

# 3.VP of sales wants to see a report of sales amount by territories and year. 
# She's also asked for the top 3 territories by growth for the ship year 2014.
yoy_df2 <- yoy_df;
yoy_df2 <- yoy_df2[c(1:36,38:50,37)]
yoy_territory <- merge(SalesTerritory,yoy_df2,by = c("TerritoryID"),all.x=TRUE)
colnames(yoy_territory)[2] <- "CountryRegion";colnames(yoy_territory)[4] <- "Continent"

# This is the territory table they want to see.
yoy_territory2 <- yoy_territory %>%
  group_by(TerritoryID,year) %>%
  summarize(Revenue=sum(revenue))%>%
  arrange(TerritoryID,year)

yoy_territory2.5 <- merge(yoy_territory2,SalesTerritory, by = c("TerritoryID"),all.x= TRUE)


# Top 3 territories by growth
yoy_territory2_2014_2013diff <- yoy_territory2[yoy_territory2$year == 2014 | yoy_territory2$year == 2013,]
yoy_territory2_2014_2013diff_Growth <- yoy_territory2_2014_2013diff  %>% group_by(TerritoryID) %>% 
  mutate(
    first=first(Revenue), 
    last=last(Revenue), 
  )
yoy_territory2_2014_2013diff_Growth$Growth <- round(((yoy_territory2_2014_2013diff_Growth$last/yoy_territory2_2014_2013diff_Growth$first)-1)*100,digits = 2)

yoy_territory2_2014_2013diff_Growth2 <- yoy_territory2_2014_2013diff_Growth[yoy_territory2_2014_2013diff_Growth$year == 2014,]

yoy_territory2_2014_2013diff_Growth2 <- yoy_territory2_2014_2013diff_Growth2 %>%
  arrange(desc(Growth))

yoy_territory2_2014_2013diff_Growth4 <- merge(yoy_territory2_2014_2013diff_Growth2,SalesTerritory, by = c("TerritoryID"),all.x= TRUE)

yoy_territory2_2014_2013diff_Growth4 <-yoy_territory2_2014_2013diff_Growth4 %>%
  arrange(desc(Growth))

# Answer
head(yoy_territory2_2014_2013diff_Growth4,n=3) # when you consider 2014-2013 growth......

# Consider 2014 when you get growth from all 4 years
yoy_territory2_2014_2011diff <- yoy_territory2[yoy_territory2$year == 2014 | yoy_territory2$year == 2011,]
yoy_territory2_2014_2011diff_Growth <- yoy_territory2_2014_2011diff  %>% group_by(TerritoryID) %>% 
  mutate(
    first=first(Revenue), 
    last=last(Revenue), 
  )
yoy_territory2_2014_2011diff_Growth$Growth <- round((yoy_territory2_2014_2011diff_Growth$last/yoy_territory2_2014_2011diff_Growth$first),digits = 2)

yoy_territory2_2014_2011diff_Growth2 <- yoy_territory2_2014_2011diff_Growth[yoy_territory2_2014_2011diff_Growth$year == 2014,]

yoy_territory2_2014_2011diff_Growth2 <- yoy_territory2_2014_2011diff_Growth2 %>%
  arrange(desc(Growth))

yoy_territory2_2014_2011diff_Growth4 <- merge(yoy_territory2_2014_2011diff_Growth2,SalesTerritory, by = c("TerritoryID"),all.x= TRUE)

yoy_territory2_2014_2011diff_Growth4 <-yoy_territory2_2014_2011diff_Growth4 %>%
  arrange(desc(Growth))

head(yoy_territory2_2014_2011diff_Growth4,n=3) # 2014 growth when you consider 2014-2011

#4 4.	Which product sub category is the most profitable for the company in 2014 (order year)?
# Adventure works is evaluating a potential change where it may stop charging freight to customers and take a hit instead, calculate the impact on profitability.
yoy_2014_Max <- yoy_df[yoy_df$year == 2014,]
yoy_2014_Max2 <- yoy_2014_Max %>%
  group_by(ProductSubcategoryID)%>%
  summarize(Revenue=sum(revenue))%>%
  arrange(desc(Revenue))

yoy_2014_Max3 <- yoy_2014_Max[c(4,17)]
yoy_2014_Max4 <- merge(yoy_2014_Max3,yoy_2014_Max2, by =c("ProductSubcategoryID"),all.x = TRUE)
yoy_2014_Max5 <- distinct(yoy_2014_Max4)
yoy_2014_Max5 <- yoy_2014_Max5 %>%
  arrange(desc(Revenue))

# Answer Mountain Bikes are most profitable

# Part 2 Adventure works is evaluating a potential change where it may stop charging freight to customers and take a hit instead, calculate the impact on profitability.
totalrevenue <- sum(SalesOrderHeader$TotalDue);totalrevenue
Revenuefreight <- sum(SalesOrderHeader$Freight);Revenuefreight
RevenuewithoutFreight <- totalrevenue-Revenuefreight;RevenuewithoutFreight
RevenuewithoutFreightpercent <- (RevenuewithoutFreight/totalrevenue)-1;RevenuewithoutFreightpercent
# we would take a 2% hit with a loss of $3183430 in Revenue
freightanalysisdf <- data.frame(totalrevenue,Revenuefreight,RevenuewithoutFreight,RevenuewithoutFreightpercent)

#5.	Adventure works is analyzing a contract with a distributor to sell Mountain Frames via the distributor.
#The distributor guarantees an additional volume growth of 500 units annually but wants to charge a 5% distributor fees on total order amount. Analyze if Adventure works should accept this offer considering 2014 (order year) sales data.

Mountain_Frame_Projection <- yoy_territory[yoy_territory$ProductSubcategoryID == 12 & yoy_territory$ProductCategoryID == 2,]

Mountain_Frame_Projection2 <- Mountain_Frame_Projection %>%
  group_by(year)%>%
  summarise(Revenue = sum(revenue),Quantity = sum(OrderQty))

Mountain_Frame_Projection2$MeanCostPerUnit <- Mountain_Frame_Projection2$Revenue/Mountain_Frame_Projection2$Quantity
Mountain_Frame_Projection2$LagMeanCostUnit <- lag(Mountain_Frame_Projection2$MeanCostPerUnit)
Mountain_Frame_Projection2$GrowthTrend <- (Mountain_Frame_Projection2$MeanCostPerUnit/Mountain_Frame_Projection2$LagMeanCostUnit)-1
# Took average of last 3 years cant go less than zero price reduction??
Mountain_Frame_Projection2$AveragequantitySoldAcrossAllYears <- mean(Mountain_Frame_Projection2[c(2:4),]$MeanCostPerUnit)
Mountain_Frame_Projection2$promisedfivehundredunits <- 500
Mountain_Frame_Projection2$averagequantitycosttimes500 <- Mountain_Frame_Projection2$AveragequantitySoldAcrossAllYears * 500
Mountain_Frame_Projection2$meanofallyearsrevenue_projected2015 <- mean(Mountain_Frame_Projection2$Revenue)
Mountain_Frame_Projection2$meanrevenueplusprojected2015 <- Mountain_Frame_Projection2$averagequantitycosttimes500 + Mountain_Frame_Projection2$meanofallyearsrevenue_projected2015
Mountain_Frame_Projection2$fivepercentfeelosstotal <-Mountain_Frame_Projection2$meanrevenueplusprojected2015 * .05
Mountain_Frame_Projection2$revenuepostfee <-Mountain_Frame_Projection2$meanrevenueplusprojected2015- (Mountain_Frame_Projection2$meanrevenueplusprojected2015 * .05)
Mountain_Frame_Projection2$benifit <- (Mountain_Frame_Projection2$revenuepostfee/ Mountain_Frame_Projection2$meanofallyearsrevenue_projected2015)-1 # 10 percent benifit
Mountain_Frame_Projection2$fivehundredtimesprevious2014 <- 500*Mountain_Frame_Projection2$MeanCostPerUnit[4]
Mountain_Frame_Projection2$fivehundredplus2014revenue <-Mountain_Frame_Projection2$Revenue[4] + Mountain_Frame_Projection2$fivehundredtimesprevious2014[4]
Mountain_Frame_Projection2$fivehundredplus2014revenueloss <-Mountain_Frame_Projection2$fivehundredplus2014revenue*.05
Mountain_Frame_Projection2$revenuepostfee2014 <-Mountain_Frame_Projection2$fivehundredplus2014revenue- (Mountain_Frame_Projection2$fivehundredplus2014revenue * .05)
Mountain_Frame_Projection2$benifit2014 <- (Mountain_Frame_Projection2$revenuepostfee2014/ Mountain_Frame_Projection2$Revenue[4])-1
Mountain_Frame_Projection2$postfivehundred_divpre2014 <- (Mountain_Frame_Projection2$fivehundredplus2014revenue/Mountain_Frame_Projection2$Revenue[4])-1 # 19% INCREASE EVEN 2ITH 

# 10% increase do it if revenue is on par with average

#6.	Calculate the average number of days between the order date and ship date at product sub category level. 
#The industry standard is an average of 4 days.
#Select an appropriate visual to display the trend at year-month level.
  
SalesOrderHeader$DiffDate <- SalesOrderHeader$ShipDate - SalesOrderHeader$OrderDate
mean(SalesOrderHeader$DiffDate)
# Time difference of 7.000286 days

yoy_df$Daystoship <- abs(yoy_df$ShipDate - yoy_df$OrderDate)
yoy_dfz <-yoy_df %>%
  group_by(ProductSubcategoryID,ShipDate) %>%
  summarise(ShipDiff = mean(Daystoship))

yoy_dfz$year <- year(yoy_dfz$ShipDate)
yoy_dfz$month <- month(yoy_dfz$ShipDate)
yoy_dfz$day <- day(yoy_dfz$ShipDate)


#7.	The VP of sales loves to see CAGR.
#Given the data in Sales Header table, how many years of CAGR can be calculated? 
#What's the over-all CAGR for Orders.
library(tidyverse)
SalesOrderHeader$year <- year(SalesOrderHeader$OrderDate)
Cagr <-aggregate(SalesOrderHeader$TotalDue, by=list(SalesOrderHeader$year), FUN=sum)
colnames(Cagr) <- c("year","revenue")

 # Cagr2<- Cagr %>%
 #  arrange(year) %>%  #in case the years are not in order (here they are)
 #  mutate(lagY = lag(year), #get the lag year
 #         lagD = lag(revenue), #get lad Data
 #         t = year - lagY, #calculate time
 #         CAGR = (revenue / lagD)^(1/t) - 1)# %>% #calculate CAGR
 #  select(-lagY, -lagD, -t) #remove unwanted variables

 # You can calculate it like this as well.
  Cagr <-  Cagr %>%
    arrange(year) %>%
    mutate(CAGR = (revenue/lag(revenue))^(1/(year-lag(year))) - 1)

# 3 years can be calculated.
  
# Cumulative CAGR 
  CumulativeCAGR <- (Cagr[4,2] / Cagr[1,2])^(1/3)-1
  CumulativeCAGR
  #0.1656411