library(lme4)
library(MuMIn)
library(lmerTest)
library(mlogit)
# final dataframe 
final_f12 <- read.csv("final_f12.csv") # RTHIS IS THE FINAL DF
 write.csv(final_f12, file="final_f12.csv")
# laged with values not removed
tct <- read.csv("tct.csv")
# lagged with values removed 
tct1 <- read.csv("tct1.csv")

# THIS IS THE FINAL DATAFRAME !!!!!!!!!!!!!!!!!!!
finalmodeldf1 # this is cleanmodeldf1 but ordered
cleanmodeldf1 <- merge(cleanmodeldf,tct1,by=c("state_id","year"),all.x=TRUE)
write.csv(cleanmodeldf1, file = "cleanmodeldf1.csv")

#original no lag
totalcosttest <- read.csv("lag_valuesf.csv")
write.csv(totalcosttest,file="totalcosttest.csv")

# lagged values
write.csv(tct1,file="tct1.csv")
tct <- read.csv("tct.csv")

cleanmodeldf <- read.csv("cleanmodeldf.csv")
final_f1 <- read.csv("final_f1.csv")
finalx <- read.csv("finalx.csv")

write.csv(cleanmodeldf,file="cleanmodeldf.csv")
write.csv(final_f1,file="final_f1.csv")
write.csv(finalx,file="finalx.csv")

# dataframes
#final_f1 new best dataframe 
final_f1 # NEW DATAFRAME TO BE USED 
final1 # this is the final df!! USE THIS ONE FOR LMER
final1 <- read.csv("final1.csv")# unmeaned raw dataset before meaned
write.csv(final1,file="final1.csv")

finaldfm8 # final dataframe!!!! from the model USE THIS ONE FOR LM 
finaldfm8 <- read.csv("finaldfm8")
write.csv(finaldfm8,file="finaldfm8.csv")

finaldf1 #meaned dataframe 
finaldf1 <- read.csv("finaldf1.csv") # meaned dataframe
write.csv(finaldf1,"finaldf1.csv")


# Getting Rsquared adjusted 
r.squaredGLMM(mdl2)


#REGRESSION
mdl1 = lm(count ~  lag1hits +  week + tmpf + relh + pop_density + median_income + arrival_rate + median_age + actualpercapcost + as.factor(year) + as.factor(state) + as.factor(disease),data = final_f1)
summary(mdl1); AIC(mdl1);

# RANDOM INTERCEPT 
mdl2 = lmer(count ~  lag1hits +  week + tmpf + relh + pop_density + median_income + arrival_rate + median_age + actualpercapcost + as.factor(year) + as.factor(disease) + (1|state),data = final_f1)
summary(mdl2); AIC(mdl2);

# RANDOM COEFFICEINT FIXED INTERCEPT 
mdl3 = lmer(count ~  lag1hits +  week + tmpf + relh + pop_density + median_income + arrival_rate + median_age + actualpercapcost +as.factor(year)+ as.factor(state) + as.factor(disease) + (0+lag1hits|state),data = final_f1)
summary(mdl3); AIC(mdl3); 

# INTERACTION PERCAP COST RANDOM COEFICENT FIXED INTERCEPT 
mdl4 = lmer(count ~  lag1hits +  week + tmpf + relh + pop_density + median_income + arrival_rate + median_age + actualpercapcost + lag1hits*actualpercapcost+  as.factor(year)+ as.factor(state) + as.factor(disease) + (0+lag1hits|state),data = final_f1)
summary(mdl4); AIC(mdl4);

# INTERACTION OUTPATIENT PER CAP COST COEFICENT FIEXED INTERCEPT 
mdl5 = lmer(count ~  lag1hits +  week + tmpf + relh + pop_density + median_income + arrival_rate + median_age + outpatientpercap + lag1hits*outpatientpercap+  as.factor(year)+ as.factor(state) + as.factor(disease) + (0+lag1hits|state),data = final_f1)
summary(mdl5); AIC(mdl5);

# INTERACTION INPATIENT PERCAP COST COEFICENT FIXED INTERCEPT 
mdl6 = lmer(count ~  lag1hits +  week + tmpf + relh + pop_density + median_income + arrival_rate + median_age + inpatientpercapcost + lag1hits*inpatientpercapcost+  as.factor(year)+ as.factor(state) + as.factor(disease) + (0+lag1hits|state),data = final_f1)
summary(mdl6); AIC(mdl6);
-------------------------------------------------------------------------------------------------
#ADDING LAG VARIABLES INTO MODELS
  
# INTERACTION PERCAP COST RANDOM COEFICENT FIXED INTERCEPT 
mdl7 = lmer(count ~  lag2hits +  week + tmpf + relh + pop_density_lag + median_income_lag + arrival_rate + median_age_lag + actualpercapcost_lag + lag2hits*actualpercapcost_lag +  as.factor(year)+ as.factor(state) + as.factor(disease) + (0+lag2hits|state),data = final_f1)
summary(mdl7); AIC(mdl7);

# INPATIENT PER CAP COST RANDOM COEF FIXED INTERCEPT INTERACTION (DOES NOT HAVE LAG)
mdl8 = lmer(count ~  lag2hits +  week + tmpf + relh + pop_density_lag + median_income_lag + arrival_rate + median_age_lag + inpatientpercapcost_lag + lag2hits*inpatientpercapcost_lag+  as.factor(year)+ as.factor(state) + as.factor(disease) + (0+lag2hits|state),data = final_f1)
summary(mdl8); AIC(mdl8);

# INTERACTION OUTPATIENT PER CAP COST RANDOM COEFICENT FIEXED INTERCEPT  # LOWEST AIC 
mdl9 = lmer(count ~  lag2hits +  week + tmpf + relh + pop_density_lag + median_income_lag + arrival_rate + median_age + outpatientpercapcost_lag + lag2hits*outpatientpercapcost_lag +  as.factor(year)+ as.factor(state) + as.factor(disease) + (0+lag2hits|state),data = final_f1)
summary(mdl9); AIC(mdl9);
-------------------------------------------------------------------------------------------------
 # CHECKING THE IMPORTANCE OF NEIGHBORING HITS MODELS 
  
# mean of 1 week lag of neighboring hits
mdln1 = lmer(count ~  lag2hits + l1meanhitsneighbor +  week + tmpf + relh + pop_density + median_income + arrival_rate + median_age + actualpercapcost + lag2hits*outpatientpercap+  as.factor(year)+ as.factor(state) + as.factor(disease) + (0+lag2hits|state),data = final_f1)
summary(mdln1); AIC(mdln1);
  
# Mean of 2 week lag of neighboring hits
mdl10 = lmer(count ~  lag2hits + l2meanhitsneighbor +  week + tmpf + relh + pop_density_lag + median_income_lag + arrival_rate + median_age_lag + actualpercapcost_lag + lag2hits*actualpercapcost_lag +  as.factor(year)+ as.factor(state) + as.factor(disease) + (0+lag2hits|state),data = final_f1)
summary(mdl10); AIC(mdl10);

mdl11 = lmer(count ~  lag2hits + l2meanhitsneighbor +  week + tmpf + relh + pop_density_lag + median_income_lag + arrival_rate + median_age_lag + inpatientpercapcost_lag + lag2hits*inpatientpercapcost_lag +  as.factor(year)+ as.factor(state) + as.factor(disease) + (0+lag2hits|state),data = final_f1)
summary(mdl11); AIC(mdl11);

mdl12 = lmer(count ~  lag2hits + l2meanhitsneighbor +  week + tmpf + relh + pop_density_lag + median_income_lag + arrival_rate + median_age_lag + outpatientpercapcost_lag + lag2hits*outpatientpercapcost_lag +  as.factor(year)+ as.factor(state) + as.factor(disease) + (0+lag2hits|state),data = final_f1)
summary(mdl12); AIC(mdl12);


# mean of 1 week lag of neighboring count
mdl15 = lmer(count ~  lag2hits +l1meancountneighbor + week + tmpf + relh + pop_density_lag + median_income_lag + arrival_rate + median_age + actualpercapcost + lag2hits*outpatientpercap+  as.factor(year)+ as.factor(state) + as.factor(disease) + (0+lag2hits|state),data = final_f1)
summary(mdl15); AIC(mdl15);

# total test cost per captia
mdl13 = lmer(count ~  lag2hits +l2meancountneighbor + week + tmpf + relh + pop_density_lag + median_income_lag + arrival_rate + median_age_lag + testpercapcost_lag + lag2hits*testpercapcost_lag +  as.factor(year)+ as.factor(state) + as.factor(disease) + (0+lag2hits|state),data = final_f12)
summary(mdl13); AIC(mdl13);

# percent eligible for ma 
mdl14 = lmer(count ~  lag2hits +l2meancountneighbor + week + tmpf + relh + pop_density_lag + median_income_lag + arrival_rate + median_age_lag + Percent_Eligibleforma_lag + lag2hits*Percent_Eligibleforma_lag +  as.factor(year)+ as.factor(state) + as.factor(disease) + (0+lag2hits|state),data = final_f12)
summary(mdl14); AIC(mdl14);

# PREDICTION SCRIPT!!!!!!!!!!!!!!!
train1 <- cleanmodeldf1[cleanmodeldf1$year %in% c(2015,2016), ] # making train df that only contians 2015 and 2016 years
temp1 <- cleanmodeldf1[cleanmodeldf1$year==2017,] # this is the year only 2017 of dataframe 
train2 <- temp1[!(temp1$month %in% c(10,11,12)), ] # this is only year 2017 minus last quarter

train3 <- rbind(train1,train2) # this combines 2015, 2016 and 2017 minus last quarter 

test <- temp1[(temp1$month %in% c(10,11,12)), ] # this is the testing df only 2017 last quarter

#REGRESSION
mdl1 <- lm(count ~  lag1hits +  week + tmpf + relh + pop_density + median_income + arrival_rate + median_age + actualpercapcost + as.factor(year) + as.factor(state) + as.factor(disease),data = train3)
summary(mdl1); AIC(mdl1);
pred <- predict(mdl1,test);
act <- test$count;
mae1 <- mean(abs(pred-act))

# RANDOM INTERCEPT
mdl2 = lmer(count ~  lag1hits +  week + tmpf + relh + pop_density + median_income + arrival_rate + median_age + actualpercapcost + as.factor(year) + as.factor(disease) + (1|state),data = train3)
summary(mdl2); AIC(mdl2);
pred <- predict(mdl2,test);
act <- test$count;
mae2 <- mean(abs(pred-act))

# RANDOM COEFFICEINT FIXED INTERCEPT 
mdl3 = lmer(count ~  lag1hits +  week + tmpf + relh + pop_density + median_income + arrival_rate + median_age + actualpercapcost +as.factor(year)+ as.factor(state) + as.factor(disease) + (0+lag1hits|state),data = train3)
summary(mdl3); AIC(mdl3);
pred <- predict(mdl3,test);
act <- test$count;
mae3 <- mean(abs(pred-act))

# RANDOM COEFFICEINT FIXED INTERCEPT lag1count added here
mdl31 = lmer(count ~  lag1hits + lag1count +  week + tmpf + relh + pop_density + median_income + arrival_rate + median_age + actualpercapcost +as.factor(year)+ as.factor(state) + as.factor(disease) + (0+lag1hits|state),data = train3)
summary(mdl31); AIC(mdl31);
pred <- predict(mdl31,test);
act <- test$count;
mae31 <- mean(abs(pred-act))

# INTERACTION PERCAP COST RANDOM COEFICENT FIXED INTERCEPT 
mdl4 = lmer(count ~  lag1hits +  week + tmpf + relh + pop_density + median_income + arrival_rate + median_age + actualpercapcost + lag1hits*actualpercapcost+  as.factor(year)+ as.factor(state) + as.factor(disease) + (0+lag1hits|state),data = train3)
summary(mdl4); AIC(mdl4);
pred <- predict(mdl4,test);
act <- test$count;
mae4 <- mean(abs(pred-act))

# INTERACTION PERCAP COST RANDOM COEFICENT FIXED INTERCEPT ADD LAG 1 COUNT
mdl41 = lmer(count ~  lag1hits + lag1count + week + tmpf + relh + pop_density + median_income + arrival_rate + median_age + actualpercapcost + lag1hits*actualpercapcost+  as.factor(year)+ as.factor(state) + as.factor(disease) + (0+lag1hits|state),data = train3)
summary(mdl41); AIC(mdl41);
pred <- predict(mdl41,test);
act <- test$count;
mae41 <- mean(abs(pred-act))

# INTERACTION OUTPATIENT PER CAP COST COEFICENT FIEXED INTERCEPT
mdl5 = lmer(count ~  lag1hits +  week + tmpf + relh + pop_density + median_income + arrival_rate + median_age + outpatientpercap + lag1hits*outpatientpercap+  as.factor(year)+ as.factor(state) + as.factor(disease) + (0+lag1hits|state),data = train3)
summary(mdl5); AIC(mdl5);
pred <- predict(mdl5,test);
act <- test$count;
mae5 <- mean(abs(pred-act))

# THIS IS THE LOWEST MAE !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# INTERACTION OUTPATIENT PER CAP COST COEFICENT FIEXED INTERCEPT add lag1count
mdl51 = lmer(count ~  lag1hits + lag1count +   week + tmpf + relh + pop_density + median_income + arrival_rate + median_age + outpatientpercap + lag1hits*outpatientpercap+  as.factor(year)+ as.factor(state) + as.factor(disease) + (0+lag1hits|state),data = train3)
summary(mdl51); AIC(mdl51);
pred <- predict(mdl51,test);
act <- test$count;
mae51 <- mean(abs(pred-act))

# INTERACTION INPATIENT PERCAP COST COEFICENT FIXED INTERCEPT 
mdl6 = lmer(count ~  lag1hits +  week + tmpf + relh + pop_density + median_income + arrival_rate + median_age + inpatientpercapcost + lag1hits*inpatientpercapcost+  as.factor(year)+ as.factor(state) + as.factor(disease) + (0+lag1hits|state),data = train3)
summary(mdl6); AIC(mdl6);
pred <- predict(mdl6,test);
act <- test$count;
mae6 <- mean(abs(pred-act))

# INTERACTION INPATIENT PERCAP COST COEFICENT FIXED INTERCEPT add lag 1 hits
mdl61 = lmer(count ~  lag1hits + lag1count + week + tmpf + relh + pop_density + median_income + arrival_rate + median_age + inpatientpercapcost + lag1hits*inpatientpercapcost+  as.factor(year)+ as.factor(state) + as.factor(disease) + (0+lag1hits|state),data = train3)
summary(mdl6); AIC(mdl6);
pred <- predict(mdl61,test);
act <- test$count;
mae61 <- mean(abs(pred-act))

# INTERACTION PERCAP COST RANDOM COEFICENT FIXED INTERCEPT lagged independent yearly variables 
mdl7 = lmer(count ~  lag2hits +  week + tmpf + relh + pop_density_lag + median_income_lag + arrival_rate + median_age_lag + actualpercapcost_lag + lag2hits*actualpercapcost_lag +  as.factor(year)+ as.factor(state) + as.factor(disease) + (0+lag2hits|state),data = train3)
summary(mdl7); AIC(mdl7);
pred <- predict(mdl7,test);
act <- test$count;
mae7 <- mean(abs(pred-act))

# INTERACTION PERCAP COST RANDOM COEFICENT FIXED INTERCEPT lagged independent yearly variables and lag1count as independent 
mdl71 = lmer(count ~  lag2hits + lag1count + week + tmpf + relh + pop_density_lag + median_income_lag + arrival_rate + median_age_lag + actualpercapcost_lag + lag2hits*actualpercapcost_lag +  as.factor(year)+ as.factor(state) + as.factor(disease) + (0+lag2hits|state),data = train3)
summary(mdl71); AIC(mdl71);
pred <- predict(mdl71,test);
act <- test$count;
mae71 <- mean(abs(pred-act))

# INPATIENT PER CAP COST RANDOM COEF FIXED INTERCEPT INTERACTION lagged independent yearly variables 
mdl8 = lmer(count ~  lag2hits +  week + tmpf + relh + pop_density_lag + median_income_lag + arrival_rate + median_age_lag + inpatientpercapcost_lag + lag2hits*inpatientpercapcost_lag+  as.factor(year)+ as.factor(state) + as.factor(disease) + (0+lag2hits|state),data = train3)
summary(mdl8); AIC(mdl8);
pred <- predict(mdl8,test);
act <- test$count;
mae8 <- mean(abs(pred-act))

# INPATIENT PER CAP COST RANDOM COEF FIXED INTERCEPT INTERACTION lagged independent yearly variables and add lag1 count as 
mdl81 = lmer(count ~  lag2hits + lag1count+  week + tmpf + relh + pop_density_lag + median_income_lag + arrival_rate + median_age_lag + inpatientpercapcost_lag + lag2hits*inpatientpercapcost_lag+  as.factor(year)+ as.factor(state) + as.factor(disease) + (0+lag2hits|state),data = train3)
summary(mdl81); AIC(mdl81);
pred <- predict(mdl81,test);
act <- test$count;
mae81 <- mean(abs(pred-act))

# INTERACTION OUTPATIENT PER CAP COST RANDOM COEFICENT FIEXED INTERCEPT 
mdl9 = lmer(count ~  lag2hits +  week + tmpf + relh + pop_density_lag + median_income_lag + arrival_rate + median_age + outpatientpercapcost_lag + lag2hits*outpatientpercapcost_lag +  as.factor(year)+ as.factor(state) + as.factor(disease) + (0+lag2hits|state),data = train3)
summary(mdl9); AIC(mdl9);
pred <- predict(mdl9,test);
act <- test$count;
mae9 <- mean(abs(pred-act))

# INTERACTION OUTPATIENT PER CAP COST RANDOM COEFICENT FIEXED INTERCEPT and lag1count
mdl91 = lmer(count ~  lag2hits + lag1count + week + tmpf + relh + pop_density_lag + median_income_lag + arrival_rate + median_age + outpatientpercapcost_lag + lag2hits*outpatientpercapcost_lag +  as.factor(year)+ as.factor(state) + as.factor(disease) + (0+lag2hits|state),data = train3)
summary(mdl91); AIC(mdl91);
pred <- predict(mdl91,test);
act <- test$count;
mae91 <- mean(abs(pred-act))

# mean of 1 week lag of neighboring hits
mdln1 = lmer(count ~  lag2hits + l1meanhitsneighbor +  week + tmpf + relh + pop_density + median_income + arrival_rate + median_age + actualpercapcost + lag2hits*outpatientpercap+  as.factor(year)+ as.factor(state) + as.factor(disease) + (0+lag2hits|state),data = train3)
summary(mdln1); AIC(mdln1);
pred <- predict(mdln1,test);
act <- test$count;
maen1 <- mean(abs(pred-act))

# mean of 1 week lag of neighboring hits plust lag1count
mdln11 = lmer(count ~  lag2hits + lag1count + l1meanhitsneighbor +  week + tmpf + relh + pop_density + median_income + arrival_rate + median_age + actualpercapcost + lag2hits*outpatientpercap+  as.factor(year)+ as.factor(state) + as.factor(disease) + (0+lag2hits|state),data = train3)
summary(mdln11); AIC(mdln11);
pred <- predict(mdln11,test);
act <- test$count;
maen11 <- mean(abs(pred-act))

# Mean of 2 week lag of neighboring hits actual per cap cost
mdl10 = lmer(count ~  lag2hits + l2meanhitsneighbor +  week + tmpf + relh + pop_density_lag + median_income_lag + arrival_rate + median_age_lag + actualpercapcost_lag + lag2hits*actualpercapcost_lag +  as.factor(year)+ as.factor(state) + as.factor(disease) + (0+lag2hits|state),data = train3)
summary(mdl10); AIC(mdl10);
pred <- predict(mdl10,test);
act <- test$count;
mae10 <- mean(abs(pred-act))

# Mean of 2 week lag of neighboring hits and lag1count actual per cap cost
mdl101 = lmer(count ~  lag2hits + lag1count +l2meanhitsneighbor +  week + tmpf + relh + pop_density_lag + median_income_lag + arrival_rate + median_age_lag + actualpercapcost_lag + lag2hits*actualpercapcost_lag +  as.factor(year)+ as.factor(state) + as.factor(disease) + (0+lag2hits|state),data = train3)
summary(mdl101); AIC(mdl101);
pred <- predict(mdl101,test);
act <- test$count;
mae101 <- mean(abs(pred-act))

# Mean of 2 week lag of neighboring hits inpatient  per cap cost
mdl11 = lmer(count ~  lag2hits + l2meanhitsneighbor +  week + tmpf + relh + pop_density_lag + median_income_lag + arrival_rate + median_age_lag + inpatientpercapcost_lag + lag2hits*inpatientpercapcost_lag +  as.factor(year)+ as.factor(state) + as.factor(disease) + (0+lag2hits|state),data = train3)
summary(mdl11); AIC(mdl11);
pred <- predict(mdl11,test);
act <- test$count;
mae11 <- mean(abs(pred-act))

# Mean of 2 week lag of neighboring hits inpatient  per cap cost and lag1count 
mdl111 = lmer(count ~  lag2hits + lag1count + l2meanhitsneighbor +  week + tmpf + relh + pop_density_lag + median_income_lag + arrival_rate + median_age_lag + inpatientpercapcost_lag + lag2hits*inpatientpercapcost_lag +  as.factor(year)+ as.factor(state) + as.factor(disease) + (0+lag2hits|state),data = train3)
summary(mdl111); AIC(mdl111);
pred <- predict(mdl111,test);
act <- test$count;
mae111 <- mean(abs(pred-act))

# Mean of 2 week lag of neighboring hits outpatient per cap cost
mdl12 = lmer(count ~  lag2hits + l2meanhitsneighbor +  week + tmpf + relh + pop_density_lag + median_income_lag + arrival_rate + median_age_lag + outpatientpercapcost_lag + lag2hits*outpatientpercapcost_lag +  as.factor(year)+ as.factor(state) + as.factor(disease) + (0+lag2hits|state),data = train3)
summary(mdl12); AIC(mdl12);
pred <- predict(mdl12,test);
act <- test$count;
mae12 <- mean(abs(pred-act))

# Mean of 2 week lag of neighboring hits outpatient per cap cost and lag1count
mdl121 = lmer(count ~  lag2hits + lag1count + l2meanhitsneighbor +  week + tmpf + relh + pop_density_lag + median_income_lag + arrival_rate + median_age_lag + outpatientpercapcost_lag + lag2hits*outpatientpercapcost_lag +  as.factor(year)+ as.factor(state) + as.factor(disease) + (0+lag2hits|state),data = train3)
summary(mdl121); AIC(mdl121);
pred <- predict(mdl121,test);
act <- test$count;
mae121 <- mean(abs(pred-act))

# total test cost per captia
mdl13 = lmer(count ~  lag2hits +l2meancountneighbor + week + tmpf + relh + pop_density_lag + median_income_lag + arrival_rate + median_age_lag + testpercapcost_lag + lag2hits*testpercapcost_lag +  as.factor(year)+ as.factor(state) + as.factor(disease) + (0+lag2hits|state),data = train3)
summary(mdl13); AIC(mdl13);
pred <- predict(mdl13,test);
act <- test$count;
mae13 <- mean(abs(pred-act))

# total test cost per captia with lag1count
mdl131 = lmer(count ~  lag2hits + lag1count + l2meancountneighbor + week + tmpf + relh + pop_density_lag + median_income_lag + arrival_rate + median_age_lag + testpercapcost_lag + lag2hits*testpercapcost_lag +  as.factor(year)+ as.factor(state) + as.factor(disease) + (0+lag2hits|state),data = train3)
summary(mdl131); AIC(mdl131);
pred <- predict(mdl131,test);
act <- test$count;
mae131 <- mean(abs(pred-act))

# percent eligible for ma 
mdl14 = lmer(count ~  lag2hits +l2meancountneighbor + week + tmpf + relh + pop_density_lag + median_income_lag + arrival_rate + median_age_lag + Percent_Eligibleforma_lag + lag2hits*Percent_Eligibleforma_lag +  as.factor(year)+ as.factor(state) + as.factor(disease) + (0+lag2hits|state),data = train3)
summary(mdl14); AIC(mdl14);
pred <- predict(mdl14,test);
act <- test$count;
mae14 <- mean(abs(pred-act))

# percent eligible for ma with lag1count
mdl141 = lmer(count ~  lag2hits + lag1count + l2meancountneighbor + week + tmpf + relh + pop_density_lag + median_income_lag + arrival_rate + median_age_lag + Percent_Eligibleforma_lag + lag2hits*Percent_Eligibleforma_lag +  as.factor(year)+ as.factor(state) + as.factor(disease) + (0+lag2hits|state),data = train3)
summary(mdl141); AIC(mdl141);
pred <- predict(mdl141,test);
act <- test$count;
mae141 <- mean(abs(pred-act))

# TRYING TO PLOT ACTUAL VS PREDICTED AFTER FORECASTING DIFFERENT ALGORITHMS 
predvsactual <- cbind(pred,act)  # putting predicted hits next to actual hits 
par(new=TRUE) 
plot(pred,type = "l",col = "green") 
par(new=TRUE)
plot(act,type = "l",col = "red")



# EXTRACTING COEFICENTS
coefmdl51 <- coef(mdl51)
df51 <- NULL
df51$state <-  row.names((coefmdl51$state))
df51$lag1hitscoef <- coefmdl51$state$lag1hits
df51 <- data.frame(df51);
finaldfm81 <- merge(df1,cleanmodeldf, by = "state",all.x = TRUE)

# PLOTTING COEF FOR MODEL 
library(ggplot2)
dx <- df51[order(df51$lag1hitscoef),] # order the dataframe for coef plotting 

# this works kinda
grps <- as.factor(dx$state)
dotchart(dx$lag1hitscoef,col="red",pch=1,labels=dx$state,
         main="model coef",
         xlab="lag1hitscoef")

# Scatter plot 
# THIS IS THE WAY TO MAKE THE COEF PLOT 
dx1 <- dx[order(dx$lag1hitscoef),]
sp3 <- ggplot(dx1, aes(x=lag1hitscoef, y=state, color=dx1$state)) + geom_point()
sp4 <- sp3 + theme(legend.position = "none")
sp5 <- sp4 + labs(title = "State Level Online Interest Coefficient",
            subtitle = "Hierarchical Model (mdl51)"


# Make HTML situation for all of the models
library(texreg)
linear <-htmlreg(list(mdl13,mdl14), file = "LMER_INTERACTION_PERCENT_MA&TEST.doc", inline.css = FALSE,
                  doctype = TRUE, html.tag = TRUE, head.tag = TRUE, body.tag = TRUE,stars = c(0.001, 0.01, 0.05, 0.1),digits = 5)

linear1 <-htmlreg(list(mdl4,mdl5,mdl6), file = "LMER_INTERACTIONS_F.doc", inline.css = FALSE,
                 doctype = TRUE, html.tag = TRUE, head.tag = TRUE, body.tag = TRUE,stars = c(0.001, 0.01, 0.05, 0.1),digits = 5)


 # correlaition plot 
# correlation matrix
corr2 <- cor(final1[,c("online interest","count","temperature","humidity","population density","median income","arrival rate","median age","total per-capita cost","inpatient cost","outpatient cost")])
library("ggcorrplot")
ggcorrplot(corr2, hc.order = TRUE, title = "Correlation of state level variables", type = "lower",lab = TRUE)

# summary statistics
sumstats <- final1[,c(9,11,13,17,26,33,38,41,47,48,49)]
x <- sumstats
summary <- lapply(x , function(x) rbind( mean = mean(x) ,
                                         sd = sd(x) ,
                                         median = median(x) ,
                                         minimum = min(x) ,
                                         maximum = max(x)) )
summary <- data.frame(summary)
summary_f <- round(summary,3)

# Exporting dataframe to pdf
library(gridExtra)
pdf("summary_stats.pdf", height=11, width=16)
grid.table(summary_f)
dev.off()


# Grabbing a subset of the data for Time series prediciton TIME SERIES!!!
# subsetting syphilis in michigan for google trends
library(xts)
library(zoo)
library(lubridate)
cleanmodeldf2 <- cleanmodeldf1[cleanmodeldf1$state_id == "MI" & cleanmodeldf1$disease == "syphilis" & cleanmodeldf1$year == 2016,c(8,10)]
# make into time series object
cleanmodeldf2 <- ts(cleanmodeldf2)
# Plot object
plot(cleanmodeldf2)
# find the column means 
colMeans(cleanmodeldf2)
# acf plot 
acf(cleanmodeldf2)

    