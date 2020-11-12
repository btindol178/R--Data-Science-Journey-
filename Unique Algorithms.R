# linear regression for each group in this case product category 
u_prod <- unique(merge35f$product_category_name)  # getting unique category names
temp3F <- NA
temp1F <- NA
for(i in 1:length(u_prod))# for each 
{
  temp1F$adj_rsq <- summary(lm(review_score ~  total_payment 
                               ,data = merge35f[merge35f$product_category_name == u_prod[i],],na.action=na.exclude))$adj.r.squared
  temp1F$product_name = paste(u_prod[i])
  temp3F <- rbind(temp3F,temp1F)
  print(i);
}
temp3F
temp3F <- data.frame(temp3F) # make model a data frame 
temp3F <- temp3F[-c(1),-c(1)] # remove un needed information & this is in list format
mat1 <- data.frame(lapply(temp3F,unlist)) # get the information out of list format into dataframe 
mat2 <-mat1[order(mat1$adj_rsq),] # order the information go to bottom for top r square categories 



--------------------------------------------------------------------------------------
#marginal effect loop figure it out
mat1 <- final_f1;
var1 <- "hits"
var2 <- "Actual.Per.Capita.Costs"
var3 <- paste(var1,var2,sep="");
mat1[,var3] <- mat1[,var1]*mat1[,var2];

marg1 = lmer(count ~  hits + Actual.Per.Capita.Costs + hitsActual.Per.Capita.Costs + meanhitsneighbor + meancountneighbor +  week + tmpf + relh + 
               population_count + median_income + arrival_rate + median_age + as.factor(year)+ as.factor(state) + as.factor(disease) + 
               (0+hits|state),data = mat1)
summary(marg1); AIC(marg1); 

mat1 <- final_f1;       
mat1m1 <- mat1;
mod <- c(-0.1,0.1,0);

for(i in 1:3)
{
  
  mat1m1 <- mat1;
  ## First fix the value of the moderator then calculate the marginal effect of the explanatory variable
  
  mat1m1[,var2] <- mat1m1[,var2] + mod[i]*mat1m1[,var2];
  mat1m1[,var3] <- mat1m1[,var1]*mat1m1[,var2];
  
  ## predictions for new value of moderator 
  mat1m1[,paste("pred",i,"0", sep = "")] <- predict(marg1,mat1m1);
  
  ## now increase the explanatory variable
  
  mat1m1[,var1] <- mat1m1[,var1] + 0.01*mat1m1[,var1];
  mat1m1[,var3] <- mat1m1[,var1]*mat1m1[,var2];
  
  mat1m1[,paste("pred",i,"1", sep = "")] <- predict(marg1,mat1m1);
  mat1m1[,paste("eff",i, sep = "")] <- (mat1m1[,paste("pred",i,"1", sep = "")] - mat1m1[,paste("pred",i,"0", sep = "")]) 
  mat1[,paste("eff",i, sep = "")] <- mat1m1[,paste("eff",i, sep = "")];
  
}
summary(mat1[,c("eff1","eff2","eff3")])

## Hardcoded marginal effect 1
## moderator icnreased or decreased by 10%
## Explanatory varible modified by 1 

mod <- c(-0.1,0.1,0);

for(i in 1:3)
{
  ## First fix the value of the moderator then calculate the marginal effect of the explanatory variable
  mat1m1 <- mat1;
  mat1m1[,var2] <- mat1m1[,var2] + mod[i]*mat1m1[,var2];
  mat1m1[,var3] <- mat1m1[,var1]*mat1m1[,var2];
  
  ## predictions for new value of moderator 
  mat1m1[,paste("pred",i,"0", sep = "")] <- predict(marg1,mat1m1);
  
  ## now increase the explanatory variable
  
  mat1m1[,var1] <- mat1m1[,var1] + 1;
  mat1m1[,var3] <- mat1m1[,var1]*mat1m1[,var2];
  
  mat1m1[,paste("pred",i,"1", sep = "")] <- predict(marg1,mat1m1);
  mat1m1[,paste("eff",i, sep = "")] <- (mat1m1[,paste("pred",i,"1", sep = "")] - mat1m1[,paste("pred",i,"0", sep = "")]) 
  mat1[,paste("eff",i, sep = "")] <- mat1m1[,paste("eff",i, sep = "")];
}
summary(mat1[,c("eff1","eff2","eff3")])

## agrgegate eff1,eff2 and eff3 across the states 

aggregate(mat1[,c("eff1","eff2","eff3")], by=list(mat1$state),
          FUN=mean, na.rm=TRUE)
------------------------------------------------------------------------------------------

#  "mabenificiariespercap"    "ffsbenificiariespercap" 
# LAGGING WEEKLY6 DATA!!!!!!!!!!!!!!!!!
u_disease <- unique(finalx$disease);
u_state <- unique(finalx$state);
# finalx is a copy of final_f4
# do the lag 1 and 2  of hits, count, nhits, ncount, remove irrelevant columns. 
finalx <- final_f1
final_f1 <- finalx
var = "mabenificiariespercap";
lag = 2;
temp1 <- NULL;
for(i in 1:length(u_disease))
{
  
  for(j in 1:length(u_state))
  {
    temp <- finalx[finalx$disease==u_disease[i] & finalx$state==u_state[j] ,];
    temp <- temp[order(temp$year)&order(temp$week),];
    temp[,paste("l",lag,var,sep = "")] <- NA;
    temp[(1+lag):nrow(temp),paste("l",lag,var,sep = "")] <- temp[1:(nrow(temp)-lag),var]; 
    temp1 <- rbind(temp1,temp)
    print(paste(i,"------",j));
  }
}
finalx = temp1


-------------------------------------------------------------------------------------------------




# SUBSETTING 
new  <- subset(mdata, year == 2016 & state == "MICHIGAN" & mdata$disease == "campylobacter")


#PLOTING X AND Y AXES 
library(latticeExtra)
xyplot(campy$count+ campy$hits ~ campy$week, campy, type = "l")

obj1 <- xyplot(campy$count ~ campy$week, campy, type = "l", lwd =2)
obj2 <- xyplot(campy$hits ~ campy$week, campy, type = "l",lwd=2)

doubleYScale(obj1, obj2, add.ylab2=TRUE)

al <- doubleYScale(obj1,obj2, text = c("COUNT","HITS"), add.ylab2 = TRUE)

-----------------------------------------------------------------------------------------------

# PLOTTING LOOP TO PDF FILE
num_state <- length(unique(mdata$state)) # Lenght of unique states column 
state_list <- unique(mdata$state) # list of unique staes
pdf("syphilis_Trend.pdf")
for(i in 1:num_state) # for 1 through unique length of state
{
  
  {
    par(new=FALSE) # start with fresh plot not added on 
    # plotting for year 2015 and each unique state and when disease is syphilis column 8 (week for x axis), and for year 2015 and state i and disease syphilis plot y axis count of disease
    plot(mdata[mdata$year==2015 & mdata$state==state_list[i] & mdata$disease == "syphilis",8],mdata[mdata$year==2015 & mdata$state==state[i]& mdata$disease == "syphilis",11],type = "l",lty = 1,cex = 1.5,xlim = c(1,52),ylim = c(0,100), col = "RED", xlab= "Weeks",ylab= " Number of Cases",main = paste(state[i]))
    par(new=TRUE) # layer plot (layering the year )
    plot(mdata[mdata$year==2016 & mdata$state==state_list[i] & mdata$disease == "syphilis",8],mdata[mdata$year==2016 & mdata$state==state[i]& mdata$disease == "syphilis",11],type = "l",lty = 1,cex = 1.5,xlim = c(1,52),ylim = c(0,100), col = "BLUE", xlab= "Weeks",ylab= " Number of Cases",main = paste(state[i]))
    par(new=TRUE)
    plot(mdata[mdata$year==2017 & mdata$state==state_list[i] & mdata$disease == "syphilis",8],mdata[mdata$year==2017 & mdata$state==state[i]& mdata$disease == "syphilis",11],type = "l",lty = 1,cex = 1.5,xlim = c(1,52),ylim = c(0,100), col = "GREEN", xlab= "Weeks",ylab= " Number of Cases",main = paste(state[i]))
    legend("topleft", legend=c("2015", "2016","2017"),col=c("RED", "BLUE","GREEN"), lty=1:2, cex=0.8)
  }
  print(i)
}
dev.off();

--------------------------------------------------------------------------------------------------------

# IMPUTATION USING LINEAR REGRESSION
u_state <- unique(data2$state) # Unique states 

var <- "percenteligibleforma" # labeling any particular column (var)

for(i in 1:length(unique(data2$state))) # for the length of the column and each unique state
{
  
  temp1 <- data2[data2$state==u_state[i],] # for each state i in the data2 state make df temp1
  temp1 <- temp1[order(temp1$year,temp1$week),]  # reorder temp1 by year and week
  treg1 <- lm(as.formula(paste(var,"~","year+week")),temp1) # run linear regression pasting the var using year week as predictors 
  pred1 <- predict(treg1,temp1[is.na(temp1[,var]),]) # predict the values for treg1(ordered lm model) where the values are missing in the var column 
  data2[data2$state==u_state[i] & is.na(data2[,var]), var] <- pred1 # in the data2 dataframe (original dataframe for each statei and the value is na repalce with prediction imputation)
  print(i);
}


------------------------------------------------------------------------------------------------------
# This loop makes two new dataframes and takes the unique weather stations in st_asos3 and pulls the information from that 
# then makes an hour column from (time/valid column date week year year week and aggregated the weather variables by year 
# taking the mean 
temp_1 <- NULL;
temp_2 <- NULL;

for(i in 494:length(st_asos3)){
  
  temp_1 <- NULL;
  temp_12 <-NULL;
  
  temp_1 <-riem_measures(station = as.character(st_asos3[i]), date_start = "2014-01-01",
                         date_end = as.character(Sys.Date()))
  temp_1 = data.frame(temp_1)
  temp_1$Hour <- hour(temp_1$valid)
  temp_1$Date <- date(temp_1$valid)
  temp_1$Week <- lubridate::week(ymd(temp_1$Date))
  temp_1$Year <- year(temp_1$valid)
  temp_1$yearweek <- paste(temp_1$Year,temp_1$Week)
  
  temp_12 <- aggregate(temp_1[,c(5,6,7,8,9)], by =list(temp_1$yearweek), mean, na.action=na.pass, na.rm=TRUE)
  temp_12$station <-unique(temp_1$station)
  
  
  temp_2 <- rbind(temp_2, temp_12)
  
  print(i)
}


---------------------------------------------------------------------------------------------------





# finding the number of unique diseases per year 
unique(data[,c("disease","year")])
# getting a summary of lyme disease and count of diseases for that unique disease
summary(data[data$disease == "lyme disease", "count"])


# Aggregating  the columns 4:8 (weather variables by state and year week taking the mean)
wether_new <- aggregate(final_weather2[,c(4:8)], by =list(final_weather2$state,final_weather2$yearweek), mean)


# Merging google trends and disease data by state year month week and yearweek
DISEASE_GTA5 <-merge(GT_ALL,DISEASE_FINAL, by = c("state_id","year","month","week","yearweek","state"), all.x = TRUE)

# Keep only the matching values in a vector
# Step 3 - make temp2 is were temp reporting area matches the values in state 2 for state
# temp is disease datafram NNDSS and state2 is list of states minus some in temp trying to match 
temp2 <- temp[temp$Reporting.Area %in% state2,] # check where temp reporting area is in state 2

# This is creating a dataframe that takes the column of states in state 2's unique states and matches it up in 
# a second column making every pair of states possible and row binding the first dataframe to the second one 

t2 <- NULL;
for(i in 1:length(state2))
{
  t1 <- NULL;
  for(j in 1:(length(state2)-i))
  {
    t1$state1[j] <- as.character(state2[i]) 
    t1$state2[j] <- as.character(state2[i+j])
  }
  t1 <- data.frame(t1)
  t2 <- rbind(t2,t1)
}
t2 <- t2[-nrow(t2),];
state_pair <- t2
------------------------------------------------------------------------------------
# This takes temp2 (disease dataframe with states and week and makes na values 0 and then for each year and state 
# takes the correlaiton 
temp2[is.na(temp2$Hepatitis..viral..acute...type.C..Current.week),4] <- 0

for(i in 1:nrow(state_pair))
{
  t1 <- temp2[temp2$Reporting.Area==state_paird$state1[i] & temp2$MMWR.Year == 2014,]
  t2 <- temp2[temp2$Reporting.Area==state_paird$state2[i] & temp2$MMWR.Year == 2014,]
  
  state_paird$cor2014[i] <- cor(t1[,4],t2[,4], use = "everything",method = "pearson")
  
  t1 <- temp2[temp2$Reporting.Area==state_paird$state1[i] & temp2$MMWR.Year == 2015,]
  t2 <- temp2[temp2$Reporting.Area==state_paird$state2[i] & temp2$MMWR.Year == 2015,]
  
  state_paird$cor2015[i] <- cor(t1[,4],t2[,4], use = "everything",method = "pearson")
  
  t1 <- temp2[temp2$Reporting.Area==state_paird$state1[i] & temp2$MMWR.Year == 2016,]
  t2 <- temp2[temp2$Reporting.Area==state_paird$state2[i] & temp2$MMWR.Year == 2016,]
  
  state_paird$cor2016[i] <- cor(t1[,4],t2[,4], use = "everything",method = "pearson")
  
  
  t1 <- temp2[temp2$Reporting.Area==state_paird$state1[i] & temp2$MMWR.Year == 2017,]
  t2 <- temp2[temp2$Reporting.Area==state_paird$state2[i] & temp2$MMWR.Year == 2017,]
  
  state_paird$cor2017[i] <- cor(t1[,4],t2[,4], use = "everything",method = "pearson")
  
  
  t1 <- temp2[temp2$Reporting.Area==state_paird$state1[i] & temp2$MMWR.Year == 2018,]
  t2 <- temp2[temp2$Reporting.Area==state_paird$state2[i] & temp2$MMWR.Year == 2018,]
  
  state_paird$cor2018[i] <- cor(t1[,4],t2[,4], use = "everything",method = "pearson")
  
  
  state_paird$s1_lon[i] <- loc[loc$State == state_pair$state1[i],3]
  state_paird$s1_lat[i] <- loc[loc$State == state_pair$state1[i],2]
  
  state_paird$s2_lon[i] <- loc[loc$State == state_pair$state2[i],3]
  state_paird$s2_lat[i] <- loc[loc$State == state_pair$state2[i],2]
  
  state_paird$distance <- earth.dist(state_paird$s1_lon,state_paird$s1_lat,state_paird$s2_lon,state_paird$s2_lat)
  cor(state_paird[!is.na(state_paird$cor2015),"cor2015"],state_paird[!is.na(state_paird$cor2015),"distance"])
  
  
  print(i);
}
# MODELS 

#REGRESSION
mdl1 = lm(count ~  lag1hits +  week + tmpf + relh + pop_density + median_income + arrival_rate + median_age + actualpercapcost + as.factor(year) + as.factor(state) + as.factor(disease),data = final1)
summary(mdl1); AIC(mdl1);

# RANDOM INTERCEPT 
mdl2 = lmer(count ~  lag1hits +  week + tmpf + relh + pop_density + median_income + arrival_rate + median_age + actualpercapcost + as.factor(year) + as.factor(disease) + (1|state),data = final1)
summary(mdl2); AIC(mdl2);

# RANDOM COEFFICEINT FIXED INTERCEPT 
mdl3 = lmer(count ~  lag1hits +  week + tmpf + relh + pop_density + median_income + arrival_rate + median_age + actualpercapcost +as.factor(year)+ as.factor(state) + as.factor(disease) + (0+lag1hits|state),data = final1)
summary(mdl3); AIC(mdl3); 

# INTERACTION PERCAP COST RANDOM COEFICENT FIXED INTERCEPT 
mdl4 = lmer(count ~  lag1hits +  week + tmpf + relh + pop_density + median_income + arrival_rate + median_age + actualpercapcost + lag1hits*actualpercapcost+  as.factor(year)+ as.factor(state) + as.factor(disease) + (0+lag1hits|state),data = final1)
summary(mdl4); AIC(mdl4);

# INTERACTION OUTPATIENT PER CAP COST COEFICENT FIEXED INTERCEPT 
mdl5 = lmer(count ~  lag1hits +  week + tmpf + relh + pop_density + median_income + arrival_rate + median_age + outpatientpercap + lag1hits*outpatientpercap+  as.factor(year)+ as.factor(state) + as.factor(disease) + (0+lag1hits|state),data = final1)
summary(mdl5); AIC(mdl5);

# INTERACTION INPATIENT PERCAP COST COEFICENT FIXED INTERCEPT 
mdl6 = lmer(count ~  lag1hits +  week + tmpf + relh + pop_density + median_income + arrival_rate + median_age + inpatientpercapcost + lag1hits*inpatientpercapcost+  as.factor(year)+ as.factor(state) + as.factor(disease) + (0+lag1hits|state),data = final1)
summary(mdl6); AIC(mdl6);

# EXTRACTING COEFICENTS
coefmdl3 <- coef(mdl3)
df1 <- NULL
df1$state <-  row.names((coefmdl3$state))
df1$lag1hitscoef <- coefmdl3$state$lag1hits
df1 <- data.frame(df1);
finaldfm8 <- merge(df1,finaldf1, by = "state",all.x = TRUE)


# Make HTML situation for all of the models
library(texreg)
linear <-htmlreg(list(mdl1,mdl2,mdl3), file = "LMER_MODELS1.doc", inline.css = FALSE,
                 doctype = TRUE, html.tag = TRUE, head.tag = TRUE, body.tag = TRUE)

linear1 <-htmlreg(list(mdl4,mdl5,mdl6), file = "LMER_INTERACTIONS1.doc", inline.css = FALSE,
                  doctype = TRUE, html.tag = TRUE, head.tag = TRUE, body.tag = TRUE)
# PLOTTING COEF FOR MODEL 
library(ggplot2)
dx = finaldfm8[order(finaldfm8$lag1hitscoef),]
qplot(dx$lag1hitscoef, state, colour = state, main = "Internet Coef Per State",
      xlab = "Internet Search Coef", ylab = "States",data = finaldfm8)

# correlaition plot 
# correlation matrix
corr2 <- cor(final1[,c("lag1search","count","temperature","relative_humidity","pop_density","median_income","arrival_rate","median_age","actualpercapcost","inpatientpercapcost","outpatientpercap")])
library("ggcorrplot")
ggcorrplot(corr2, hc.order = TRUE, title = "Correlation of state level variables", type = "lower",lab = TRUE)



# testing linearity 
scatter.smooth(x=final1$count, y=final1$lag1hits, main="count ~ lag1hits")  # scatterplot
scatter.smooth(x=finaldfm8$lag1hitscoef, y=finaldfm8$mpercapcost, main="lag1hitscoef ~ mpercapcost")  # scatterplot

# plotting for heteroscdacity skewed 
plot(density(finaldfm8$lag1hitscoef), main="Density Plot: hitscoef", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(finaldfm8$lag1hitscoef), 2))) 
plot(density(final1$lag1hits), main="Density Plot: hitscoef", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(final1$lag1hits), 35))) 


# TESTING THE NORMALITY IN DISTRIBUTION (NOT WORKING)
m1s = rstandard(m1)
qqnorm(m1s, ylab="Standardized Residuals",xlab="Normal Scores", main="Linear Regression Model") 
qqline(m1s)

# plotting coef for each state
dx = finaldfm8[order(finaldfm8$lag1hitscoef),]
dotchart(dx$lag1hitscoef,labels=row.names(coefm12$state),cex=.6,
         main="M12 Model", 
         xlab="Hits Coef ")