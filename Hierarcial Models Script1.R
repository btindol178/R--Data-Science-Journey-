library(lme4)
library(MuMIn)
library(lmerTest)
# copyfinal_f1 is the dataframe. 

final_f1 <- read.csv("final_f1.csv");

med_var <- read.csv("medicalwithoutpercent.csv");
med_var2 <- aggregate(med_var[,c("Beneficiaries.with.Part.A.and.Part.B","MA.Beneficiaries","FFS.Beneficiaries","Total.Actual.Costs","IP.Actual.Costs","Tests.Actual.Costs",
                                 "Part.B.Drugs.Actual.Costs")], by=list(med_var$State,med_var$ï..year),
                      FUN=sum, na.rm=TRUE)
colnames(med_var2)[1:2] <- c("state_id","year")

med_var3 <- aggregate(med_var[,c("Actual.Per.Capita.Costs","IP.Per.Capita.Actual.Costs","OP.Per.Capita.Actual.Costs",
                                 "Tests.Per.Capita.Actual.Costs","Part.B.Drugs.Per.Capita.Actual.Costs")], by=list(med_var$State,med_var$ï..year),
                      FUN=mean, na.rm=TRUE)
colnames(med_var3)[1:2] <- c("state_id","year")
lmed_var2 <- NULL; lmed_var3 <- NULL;
lmed_var2 <- med_var2;lmed_var2$year = lmed_var2$year + 1;
lmed_var3 <- med_var3;lmed_var3$year = lmed_var3$year + 1;

colnames(lmed_var2)[3:9] <- paste("l",colnames(lmed_var2)[3:9],sep = "")
colnames(lmed_var3)[3:7] <- paste("l",colnames(lmed_var3)[3:7],sep = "")



## merge the med_var2 with final_f1
final_f1 <- copyfinal_f1;
final_f1 <- merge(final_f1,med_var2,by = c("state_id","year"),all.x = TRUE)
final_f1 <- merge(final_f1,med_var3,by = c("state_id","year"),all.x = TRUE)

final_f1 <- merge(final_f1,lmed_var2,by = c("state_id","year"),all.x = TRUE)
final_f1 <- merge(final_f1,lmed_var3,by = c("state_id","year"),all.x = TRUE)

summary(final_f1)
final_f1 <- final_f1[!is.na(final_f1$Actual.Per.Capita.Costs),];

## merge the lagged values to final_f1;
final_f1$pabpercap <- final_f1$Beneficiaries.with.Part.A.and.Part.B / final_f1$population_count;
final_f1$mapercap <- final_f1$MA.Beneficiaries / final_f1$population_count;
final_f1$ffspercap <- final_f1$ffsbenificiaries / final_f1$population_count;
final_f1$tcpercap <- final_f1$Total.Actual.Costs / final_f1$population_count;

final_f1$lmapercap <- final_f1$lMA.Beneficiaries / final_f1$population_count_lag;
final_f1$lffspercap <- final_f1$lFFS.Beneficiaries / final_f1$population_count_lag;
final_f1$lpabpercap <- final_f1$lBeneficiaries.with.Part.A.and.Part.B / final_f1$population_count_lag;
final_f1$lpabben <- final_f1$lBeneficiaries.with.Part.A.and.Part.B / final_f1$population_count_lag;


# Getting Rsquared adjusted
library(MuMIn)
r.squaredGLMM(mdl8)

cor(final_f1[,c("count","hits", "meanhitsneighbor","meancountneighbor","tmpf","relh","mapercap","population_count_lag","median_income_lag","median_age_lag","arrival_rate")],use = "complete.obs")


## Models 
library(sjstats)
(std_beta(mdl2, type = "std2"))
library(lme4)
library(sjstats)
library(lm.beta)
library(lmerTest)
## Models 

mdl0 = lm(count ~  hits+   week + tmpf + relh + lmapercap +  population_count_lag + median_income_lag + arrival_rate + median_age_lag + as.factor(year) + as.factor(disease),data = final_f1)
summary(mdl0); AIC(mdl0);
stdmdl0 <- lm.beta(mdl0);
summary(stdmdl0)

mdl1 = lm(count ~  hits+   week + tmpf + relh + lmapercap +  population_count_lag + median_income_lag + arrival_rate + median_age_lag + as.factor(year) + as.factor(state) + as.factor(disease),data = final_f1)
summary(mdl1); AIC(mdl1);
stdmdl1 <- lm.beta(mdl1);
summary(stdmdl1)

mdl2 = lmer(count ~  hits +   week + tmpf + relh +lmapercap + population_count_lag + median_income_lag + arrival_rate + median_age_lag+ as.factor(year) + as.factor(disease) + (1|state),data = final_f1)
summary(mdl2); AIC(mdl2);
(std_beta(mdl2, type = "std2"))

mdl3 = lmer(count ~  hits+   week + tmpf + relh + lmapercap + population_count_lag + median_income_lag + arrival_rate + median_age_lag  +as.factor(year)+ as.factor(state) + as.factor(disease) + (0+hits|state),data = final_f1)
summary(mdl3); AIC(mdl3); 
(std_beta(mdl3, type = "std2"))

mdl4 = lmer(count ~  hits + meanhitsneighbor + meancountneighbor +  week + tmpf + relh +lmapercap + population_count_lag + median_income_lag + arrival_rate + median_age_lag +as.factor(year)+ as.factor(state) + as.factor(disease) + (0+hits|state),data = final_f1)
summary(mdl4); AIC(mdl4); 
(std_beta(mdl4, type = "std2"))

mdl5 = lmer(count ~  hits + hits*meanhitsneighbor + meanhitsneighbor + meancountneighbor +  week + tmpf + relh + lmapercap + lmapercap + population_count_lag + median_income_lag + arrival_rate + median_age_lag +as.factor(year)+ as.factor(state) + as.factor(disease) + (0+hits|state),data = final_f1)
summary(mdl5); AIC(mdl5); 
(std_beta(mdl5, type = "std2"))

mdl6 = lmer(count ~  hits + hits*meancountneighbor +meanhitsneighbor + meancountneighbor + week + tmpf + relh + lmapercap +  population_count_lag + median_income_lag + arrival_rate + median_age_lag +as.factor(year)+ as.factor(state) + as.factor(disease) + (0+hits|state),data = final_f1)
summary(mdl6); AIC(mdl6); 
(std_beta(mdl6, type = "std2"))

mdl7 = lmer(count ~  hits + hits*lmapercap + meanhitsneighbor + meancountneighbor +  week + tmpf + relh + lmapercap + population_count_lag + median_income_lag + arrival_rate + median_age_lag +as.factor(year)+ as.factor(state) + as.factor(disease) + (0+hits|state),data = final_f1)
summary(mdl7); AIC(mdl7); 
(std_beta(mdl7, type = "std2"))

mdl8 = lmer(count ~  hits + hits*median_income_lag +  lmapercap + meanhitsneighbor + meancountneighbor +  week + tmpf + relh + lmapercap + population_count_lag + median_income_lag + arrival_rate + median_age_lag +as.factor(year)+ as.factor(state) + as.factor(disease) + (0+hits|state),data = final_f1)
summary(mdl8); AIC(mdl8); 
(std_beta(mdl8, type = "std2"))

library(texreg)
htmlreg(list(mdl1,mdl2,mdl3,mdl4), file = "model1.doc", inline.css = FALSE,
        doctype = TRUE, html.tag = TRUE, head.tag = TRUE, body.tag = TRUE,stars = c(0.001, 0.01, 0.05, 0.1),digits = 5)


## the models work well for the lag values

cor(final_f1[,c("lmapercap", "meanhitsneighbor","meancountneighbor","population_count_lag","median_income_lag","median_age_lag","arrival_rate")],use = "complete.obs")


## Now the script for the marginal effect 
#Average Marginal Effects fix moderator min and max and increase hits by 1 works wel 

mat1 <- final_f1;
var1 <- "hits"
var2 <- "median_income_lag"
var3 <- paste(var1,var2,sep="");
mat1[,var3] <- mat1[,var1]*mat1[,var2];

# Change equation after changing var2
marg1 = lmer(count ~  hits + lmapercap + hitsmedian_income_lag + meanhitsneighbor + meancountneighbor +  week + tmpf + relh + 
               population_count_lag + median_income_lag + arrival_rate + median_age_lag + as.factor(year)+ as.factor(state) + as.factor(disease) + 
               (0+hits|state),data = mat1)
summary(marg1); AIC(marg1); 

mat1 <- final_f1;       
mat1m1 <- mat1;
#mod <- c(-0.25,0.25,0);
mod <- c(summary(mat1[,var2])[1],summary(mat1[,var2])[6],0)

for(i in 1:3)
{
  mat1m1 <- mat1;
  ## First fix the value of the moderator then calculate the marginal effect of the explanatory variable
  
  #mat1m1[,var2] <- mat1m1[,var2] + mod[i]*mat1m1[,var2];
  
  mat1m1[,var2] <- mod[i]; # if second mod for fixed values are used 
  
  if(i==3){mat1m1[,var2]=mat1[,var2]}
  mat1m1[,var3] <- mat1m1[,var1]*mat1m1[,var2];
  
  ## predictions for new value of moderator 
  mat1m1[,paste("pred",i,"0", sep = "")] <- predict(marg1,mat1m1);
  
  ## now increase the explanatory variable
  
  mat1m1[,var1] <- mat1m1[,var1] + 0.1*mat1m1[,var1];
  #mat1m1[,var1] <- mat1m1[,var1] + 1;
  mat1m1[,var3] <- mat1m1[,var1]*mat1m1[,var2];
  
  mat1m1[,paste("pred",i,"1", sep = "")] <- predict(marg1,mat1m1);
  mat1m1[,paste("eff",i, sep = "")] <- (mat1m1[,paste("pred",i,"1", sep = "")] - mat1m1[,paste("pred",i,"0", sep = "")]) 
  
  #mat1m1[,paste("eff",i, sep = "")] <- (mat1m1[,paste("pred",i,"1", sep = "")] - mat1m1[,paste("pred",i,"0", sep = "")])/ mat1m1[,paste("pred",i,"0", sep = "")]
  
  #mat1m1[,paste("eff",i, sep = "")] <- (mat1m1[,paste("pred",i,"1", sep = "")] - mat1m1[,"count"])
  

  mat1[,paste("eff",i, sep = "")] <- mat1m1[,paste("eff",i, sep = "")];
  
}
summary(mat1[,c("eff1","eff2","eff3")])

------##Now the script for predicting the marginal  effect 
  
  mat1 <- final_f1;
var1 <- "hits"
var2 <- "meancountneighbor"
var3 <- paste(var1,var2,sep="");
mat1[,var3] <- mat1[,var1]*mat1[,var2];

# Change equation after changing var2
marg1 = lmer(count ~  hits + lmapercap + hitsmeancountneighbor + meanhitsneighbor + meancountneighbor +  week + tmpf + relh + 
               population_count_lag + median_income_lag + arrival_rate + median_age_lag + as.factor(year)+ as.factor(state) + as.factor(disease) + 
               (0+hits|state),data = mat1)
summary(marg1); AIC(marg1); 

mat1 <- final_f1;       
mat1m1 <- mat1;
#mod <- c(-0.25,0.25,0);
mod <- c(summary(mat1[,var2])[2],summary(mat1[,var2])[5],0)

for(i in 1:3)
{
  mat1m1 <- mat1;
  ## First fix the value of the moderator then calculate the marginal effect of the explanatory variable
  
  #mat1m1[,var2] <- mat1m1[,var2] + mod[i]*mat1m1[,var2];
  mat1m1[,var2] <- mod[i]; # if second mod for fixed values are used 
  if(i==3){mat1m1[,var2]=mat1[,var2]}
  mat1m1[,var3] <- mat1m1[,var1]*mat1m1[,var2];
  
  ## predictions for new value of moderator 
  mat1m1[,paste("pred",i,"0", sep = "")] <- predict(marg1,mat1m1);
  
  ## now increase the explanatory variable
  
  mat1m1[,var1] <- mat1m1[,var1] + 0.01*mat1m1[,var1];
  #mat1m1[,var1] <- mat1m1[,var1] + 1;
  mat1m1[,var3] <- mat1m1[,var1]*mat1m1[,var2];
  
  mat1m1[,paste("pred",i,"1", sep = "")] <- predict(marg1,mat1m1);
  #mat1m1[,paste("eff",i, sep = "")] <- (mat1m1[,paste("pred",i,"1", sep = "")] - mat1m1[,paste("pred",i,"0", sep = "")]) 
  
  mat1m1[,paste("eff",i, sep = "")] <- (mat1m1[,paste("pred",i,"1", sep = "")] - mat1m1[,paste("pred",i,"0", sep = "")])/ mat1m1[,paste("pred",i,"0", sep = "")]
  
  mat1[,paste("eff",i, sep = "")] <- mat1m1[,paste("eff",i, sep = "")];
  
}
summary(mat1[,c("eff1","eff2","eff3")])
------------------
  ## Get aggregate mat1 statewise 
  
  temp1 <- aggregate(mat1[,c("eff1","eff2","eff3")],by = list(mat1$state),FUN = "mean")
temp1
write.csv(temp1,"margineff.csv")
----------------------
  
  # Marginal Effect for non interaction variables 
  # Try model without interaction,here increase var by 1% and check changes in count 
  
  # Change equation after changing var2
  # marg1 = lmer(count ~  hits + lmapercap +  meanhitsneighbor + meancountneighbor +  week + tmpf + relh + 
  #                population_count_lag + median_income_lag + arrival_rate + median_age_lag + as.factor(year)+ as.factor(state) + as.factor(disease) + 
  #                (0+hits|state),data = mat1)
  # summary(marg1); AIC(marg1); 
  
marg1 = lmer(count ~  hits + lmapercap + hits*lmapercap + meanhitsneighbor + meancountneighbor +  week + tmpf + relh +
                 population_count_lag + median_income_lag + arrival_rate + median_age + as.factor(year)+ as.factor(state) + as.factor(disease) +
                 (0+hits|state),data = mat1)
summary(marg1); AIC(marg1);

mat1 <- final_f1;       
mat1m1 <- mat1;
var1 <- "tmpf"
for(i in 1:1)
{
  mat1m1 <- mat1;
  
  ## predictions for new value of moderator 
  mat1m1[,paste("pred",i,"0", sep = "")] <- predict(marg1,mat1m1);
  
  mat1m1[,var1] <- mat1m1[,var1] + 0.1*mat1m1[,var1];
  #mat1m1[,var1] <- mat1m1[,var1] + 1;
  
  mat1m1[,paste("pred",i,"1", sep = "")] <- predict(marg1,mat1m1);
  #mat1m1[,paste("eff",i, sep = "")] <- (mat1m1[,paste("pred",i,"1", sep = "")] - mat1m1[,paste("pred",i,"0", sep = "")]) 
  #mat1m1[,paste("eff",i, sep = "")] <- (mat1m1[,paste("pred",i,"1", sep = "")] - mat1m1[,paste("pred",i,"0", sep = "")]) /mat1m1[,paste("pred",i,"0", sep = "")] 
  
  mat1m1[,paste("eff",i, sep = "")] <- (mat1m1[,paste("pred",i,"1", sep = "")] - mat1m1[,"count"]) 
  
  
  mat1[,paste("eff",i, sep = "")] <- mat1m1[,paste("eff",i, sep = "")] 
  
}
summary(mat1[,c("eff1")])

------------------
  # Marginal effect for the variable with interaction
  
mat1 <- final_f1;
var1 <- "hits"
var2 <- "lmapercap"
var3 <- paste(var1,var2,sep="");
mat1[,var3] <- mat1[,var1]*mat1[,var2];

# Change equation after changing var2
marg1 = lmer(count ~  hits + mapercap + hitslmapercap + meanhitsneighbor + meancountneighbor +  week + tmpf + relh + 
               population_count_lag + median_income_lag + arrival_rate + median_age + as.factor(year)+ as.factor(state) + as.factor(disease) + 
               (0+hits|state),data = mat1)
summary(marg1); AIC(marg1); 

mat1 <- final_f1;       
mat1m1 <- mat1;
mod <- c(-0.25,0.25,0);

for(i in 1:3)
{
  mat1m1 <- mat1;
  ## First fix the value of the moderator then calculate the marginal effect of the explanatory variable
  
  mat1m1[,var2] <- mat1m1[,var2] + mod[i]*mat1m1[,var2];
  mat1m1[,var3] <- mat1m1[,var1]*mat1m1[,var2];
  
  ## predictions for new value of moderator 
  mat1m1[,paste("pred",i,"0", sep = "")] <- predict(marg1,mat1m1);
  
  ## now increase the explanatory variable
  
  mat1m1[,var2] <- mat1m1[,var2] + 0.1*mat1m1[,var2];
  #mat1m1[,var1] <- mat1m1[,var1] + 1;
  mat1m1[,var3] <- mat1m1[,var1]*mat1m1[,var2];
  
  mat1m1[,paste("pred",i,"1", sep = "")] <- predict(marg1,mat1m1);
  #mat1m1[,paste("eff",i, sep = "")] <- (mat1m1[,paste("pred",i,"1", sep = "")] - mat1m1[,paste("pred",i,"0", sep = "")]) 
  mat1m1[,paste("eff",i, sep = "")] <- (mat1m1[,paste("pred",i,"1", sep = "")] - mat1m1[,"count"]) 
  mat1[,paste("eff",i, sep = "")] <- mat1m1[,paste("eff",i, sep = "")];
}
summary(mat1[,c("eff1","eff2","eff3")])

## Marginal effects for model without interaction terms 

# Marginal effect for the variable with interaction

mat1 <- final_f1;
var2 <- "hits"

# Change equation after changing var2
marg1 = lmer(count ~  hits + mapercap +  meanhitsneighbor + meancountneighbor +  week + tmpf + relh + 
               population_count_lag + median_income_lag + arrival_rate + median_age_lag + as.factor(year)+ as.factor(state) + as.factor(disease) + 
               (0+hits|state),data = mat1)
summary(marg1); AIC(marg1); 

mat1 <- final_f1;       
mat1m1 <- mat1;

for(i in 1:3)
{
  mat1m1 <- mat1;
  ## First fix the value of the moderator then calculate the marginal effect of the explanatory variable
  
  #mat1m1[,var2] <- mat1m1[,var2] + mod[i]*mat1m1[,var2];
  #mat1m1[,var3] <- mat1m1[,var1]*mat1m1[,var2];
  
  ## predictions for new value of moderator 
  mat1m1[,paste("pred",i,"0", sep = "")] <- predict(marg1,mat1m1);
  
  ## now increase the explanatory variable
  
  mat1m1[,var1] <- mat1m1[,var2] + 0.1*mat1m1[,var1];
  #mat1m1[,var1] <- mat1m1[,var1] + 1;
  #mat1m1[,var3] <- mat1m1[,var1]*mat1m1[,var2];
  
  mat1m1[,paste("pred",i,"1", sep = "")] <- predict(marg1,mat1m1);
  #mat1m1[,paste("eff",i, sep = "")] <- (mat1m1[,paste("pred",i,"1", sep = "")] - mat1m1[,paste("pred",i,"0", sep = "")]) 
  mat1m1[,paste("eff",i, sep = "")] <- (mat1m1[,paste("pred",i,"1", sep = "")] - mat1m1[,"count"]) 
  mat1[,paste("eff",i, sep = "")] <- mat1m1[,paste("eff",i, sep = "")];
}
summary(mat1[,c("eff1","eff2","eff3")])


------------------
  ## Plots 
  
  library(usmap)
library(ggplot2)

temp1 <-mat1[,c("state_id","eff1","eff2","eff3")]

temp1 <- aggregate(mat1[,c("eff1","eff2","eff3")], by=list(mat1$state_id),
                   FUN=mean, na.rm=TRUE)
colnames(temp1)[1] <- "abbr";

temp2 <- merge(statepop,temp1, by = "abbr", all.y = TRUE)

plot_usmap(data = temp2, values = "eff3", color = "black") + 
  scale_fill_continuous(
    low = "white", high = "blue", name = "Average Marginal Effect\n(10% increase in online search)", label = scales::comma
  ) + theme(legend.position = "right")

---------------------
  
  ## Predictions
  ## First create lag variables 
  ## script for lagging weekly variables 
  lfinal <- final_f1;
#copylfinal <- lfinal
lag = 3;
## weekly variables for lag hits, meanhitsneighbor, meancountneighbor, tempf, relh
#var = c("tmpf","relh","hits","meanhitsneighbor","meancountneighbor");
# var = c("tmpf","relh");
var = c("arrival_rate");
u_disease <- unique(lfinal$disease);
u_state <- unique(lfinal$state)

for(k in 1:length(var))
{
  # finalx is a copy of final_f4
  # do the lag 1 and 2  of hits, count, nhits, ncount, remove irrelevant columns. 
  lfinal[,paste("l",lag,var[k],sep = "")] <- NA; 
  temp1 <- NULL; 
  for(i in 1:length(u_disease))
  {
    
    for(j in 1:length(u_state))
    {
      temp <- lfinal[lfinal$disease==u_disease[i] & lfinal$state==u_state[j] ,];
      temp <- temp[order(temp$year, temp$week),];
      #temp[,paste("l",lag,var,sep = "")] <- NA;
      
      temp[(1+lag):nrow(temp),paste("l",lag,var[k],sep = "")] <- temp[1:(nrow(temp)-lag),var[k]]; 
      temp1 <- rbind(temp1,temp);
      print(paste(i,"------",j));
    }
  }
  lfinal = temp1;
  #lfinal <- temp1;
  #lfinal[,paste("l",lag,var[k],sep = "")] <- temp1[,paste("l",lag,var[k],sep = "")];
}

## 
# i=1;
# train <- lfinal[lfinal$disease==u_disease[i] & !(lfinal$year==2017 & lfinal$week > 39), ] 
# test <- lfinal[lfinal$disease==u_disease[i] & (lfinal$year==2017 & lfinal$week > 39), ] 

## test and train without selecting specific disease 

train <- lfinal[!(lfinal$year==2017 & lfinal$week > 39), ] 
test <- lfinal[(lfinal$year==2017 & lfinal$week > 39), ]

## Train models on test data and 

md0 <- lm(count ~  l3hits+   l3tmpf + l3relh + l3arrival_rate + week + population_count_lag + median_income_lag +  median_age_lag +
            as.factor(year) + as.factor(state) + as.factor(disease),data = train)

md1 <- lmer(count ~  l3hits+   l3tmpf + l3relh + l3arrival_rate + week +  population_count_lag + median_income_lag + median_age_lag  +
              as.factor(year)+ as.factor(state) + as.factor(disease) + (0+l3hits|state),data = train)

md2 <- lmer(count ~  l3hits+   l3tmpf + l3relh + l3arrival_rate + week +  l3meanhitsneighbor + l3meancountneighbor + population_count_lag + median_income_lag +  median_age_lag  +
              as.factor(year)+ as.factor(state) + as.factor(disease) + (0+l3hits|state),data = train)

md3 <- lmer(count ~  l3hits + l3hits*lmapercap +  lmapercap + l3tmpf + l3relh + l3arrival_rate + l3meanhitsneighbor + l3meancountneighbor + week +population_count_lag + median_income_lag +  median_age_lag
            +as.factor(year)+ as.factor(state) + as.factor(disease) + (0+l3hits|state),data = train)

## Predictions for linear models 

mdl_pred <- NULL
mdl_pred$act <- test$count;
mdl_pred$p1 <- predict(md0,test);
mdl_pred$p2 <- predict(md1,test);
mdl_pred$p3 <- predict(md2,test);
mdl_pred$p4 <- predict(md3,test);
mdl_pred$disease <- test$disease;

mdl_pred <- data.frame(mdl_pred);
results <- NULL;results <- matrix(NA,nrow(mdl_pred),(ncol(mdl_pred)-2))
for(i in 2:5)
{
  results[,i-1] <- (abs(mdl_pred[,1]-mdl_pred[,i]));
}
results <- data.frame(results)
results$disease = mdl_pred$disease

#results <- NULL;results <- matrix(NA,nrow(mdl_pred),(ncol(mdl_pred)-2))
for(i in 1:4)
{
  print(aggregate(results[,i],by = list(results$disease), FUN = mean))
}

#--------------------------------------------

#predictions seperately for each disease 

train <- lfinal[!(lfinal$year==2017 & lfinal$week > 26), ] 
test <- lfinal[(lfinal$year==2017 & lfinal$week > 26), ]

train[,c("year")] <- as.factor(train[,c("year")]);
test[,c("year")] <- as.factor(test[,c("year")]);

## Train models on test data and 

md0 <- lm(count ~  l3hits+   l3tmpf + l3relh + l3arrival_rate + week + population_count_lag + median_income_lag +  median_age_lag +
            (year) + (state) + (disease),data = train)

md1 <- lmer(count ~  l3hits+   l3tmpf + l3relh + l3arrival_rate + week +  population_count_lag + median_income_lag + median_age_lag  +
              (year)+ (state) + (disease) + (0+l3hits|state),data = train)

md2 <- lmer(count ~  l3hits+   l3tmpf + l3relh + l3arrival_rate + week +  l3meanhitsneighbor + l3meancountneighbor + population_count_lag + median_income_lag +  median_age_lag  +
              (year)+ (state) + (disease) + (0+l3hits|state),data = train)

md3 <- lmer(count ~  l3hits + l3hits*lmapercap +  lmapercap + l3tmpf + l3relh + l3arrival_rate + l3meanhitsneighbor + l3meancountneighbor + week +population_count_lag + median_income_lag +  median_age_lag
            +(year)+ (state) + (disease) + (0+l3hits|state),data = train)

## Predictions for linear models 

mdl_pred <- NULL
mdl_pred$act <- test$count;
mdl_pred$p1 <- predict(md0,test);
mdl_pred$p2 <- predict(md1,test);
mdl_pred$p3 <- predict(md2,test);
mdl_pred$p4 <- predict(md3,test);
mdl_pred$disease <- test$disease;

mdl_pred <- data.frame(mdl_pred);
results <- NULL;results <- matrix(NA,nrow(mdl_pred),(ncol(mdl_pred)-2))
for(i in 2:5)
{
  results[,i-1] <- (abs(mdl_pred[,1]-mdl_pred[,i]));
}
results <- data.frame(results)
results$disease = mdl_pred$disease
summary(results)
#results <- NULL;results <- matrix(NA,nrow(mdl_pred),(ncol(mdl_pred)-2))
for(i in 1:4)
{
  print(aggregate(results[,i],by = list(results$disease), FUN = mean))
}

#results <- NULL;results <- matrix(NA,nrow(mdl_pred),(ncol(mdl_pred)-2))
for(i in 1:4)
{
  print(aggregate(results[,i],by = list(results$disease), FUN = mean))
}

#-------------------------------------------


## Predictions for non linear models 
library(caret); library(e1071); library(ranger); library(dplyr); library(rpart);library(brnn);library(RSNNS);library(nnet);library(party);
library(adabag); library(neuralnet)


train1 <- train[complete.cases(train),c("count","week","l1tmpf","l1relh","population_count_lag","median_income_lag","arrival_rate","median_age_lag","year","state","disease")]

train2 <- train[complete.cases(train),c("count", "lag1hits","week","l1tmpf","l1relh","population_count_lag","median_income_lag","arrival_rate","median_age_lag","year","state","disease")]

train3 <- train[complete.cases(train),c("count", "lag1hits","l1meanhitsneighbor","l1meancountneighbor","lmapercap", "week","l1tmpf","l1relh","population_count_lag","median_income_lag","arrival_rate","median_age_lag","year","state","disease")]

test1 <- test[,c("count","week","l1tmpf","l1relh","population_count_lag","median_income_lag","arrival_rate",
                 "median_age_lag","year","state","disease")]
test2 <- test[,c("count", "lag1hits","week","l1tmpf","l1relh","population_count_lag","median_income_lag",
                 "arrival_rate","median_age_lag","year","state","disease")]
test3 <- test[,c("count", "lag1hits","l1meanhitsneighbor","l1meancountneighbor","lmapercap", "week","l1tmpf","l1relh","population_count_lag","median_income_lag",
                 "arrival_rate","median_age_lag","year","state","disease")]

train1$disease <- as.numeric(train1$disease); train1$state <- as.numeric(train1$state); 
test1$disease <- as.numeric(test1$disease); test1$state <- as.numeric(test1$state); 
train2$disease <- as.numeric(train2$disease); train2$state <- as.numeric(train2$state); 
test2$disease <- as.numeric(test2$disease); test2$state <- as.numeric(test2$state); 
train3$disease <- as.numeric(train3$disease); train3$state <- as.numeric(train3$state); 
test3$disease <- as.numeric(test3$disease); test3$state <- as.numeric(test3$state); 

library(neuralnet);
nn1 <- neuralnet(count ~    week + l1tmpf + l1relh + population_count_lag + median_income_lag + arrival_rate + median_age_lag +
                   (year) + (state) + (disease),data = train1[complete.cases(train1),], hidden=c(10,5),linear.output=T)

nn2 <- neuralnet(count ~  lag1hits+   week + l1tmpf + l1relh + population_count_lag + median_income_lag + arrival_rate + median_age_lag +
                   (year) + (state) + (disease),data = train2[complete.cases(train2),], hidden = 3)

nn3 <- neuralnet(count ~  lag1hits+ l1meanhitsneighbor + l1meancountneighbor + lmapercap + week + l1tmpf + l1relh + population_count_lag + median_income_lag + arrival_rate + median_age_lag +
                   (year) + (state) + (disease),data = train3[complete.cases(train3),], hidden = 3)

nn1 <- neuralnet(count ~    week + l1tmpf + l1relh + population_count_lag + median_income_lag + arrival_rate + median_age_lag ,data = train1[complete.cases(train1),], hidden=c(10,5),linear.output=T)

## SVM is working fine here. 
library(e1071);
svm3 <- svm(count ~ lag1hits+ l1meanhitsneighbor + l1meancountneighbor + lmapercap + week + l1tmpf + l1relh + population_count_lag + median_income_lag + arrival_rate + median_age_lag +
              (year) + (state) + (disease),data = train3[complete.cases(train3),], kernal = "polynomial",degree = 3)


nn_pred <- NULL
nn_pred$act <- test1$count;
nn_pred$p1 <- compute(nn1,test1);
nn_pred$p2 <- predict(nn2,test2);
nn_pred$p3 <- predict(nn3,test3);

nn_pred <- data.frame(nn_pred);
results <- NULL;
for(i in 2:ncol(nn_pred))
{
  results[i-1] <- mean(abs(nn_pred[,1]-nn_pred[,i]),na.rm = TRUE);
}
results;



# Machine learning model
library("party");

library(caret); library(e1071); library(ranger); library(dplyr); library(rpart);library(brnn);library(RSNNS);library(nnet);library(party);
library(adabag); library(neuralnet); library(elasticnet); library(RWeka);library(randomForest); library(kernlab)
library(qrnn);library(extraTrees);library(RRF)

model1 <- caret::train(x = train1[,-1], y = train1[,1],method= "lasso")
model1 <- caret::train(x = train1[,-1], y = train1[,1],method= "M5")
model1 <- caret::train(x = train1[,-1], y = train1[,1],method= "glmnet")
model1 <- caret::train(x = train1[,-1], y = train1[,1],method= "knn")
model1 <- caret::train(x = train1[,-1], y = train1[,1],method= "adaboost", tree_depth = 2,
                       n_rounds = 200, verbose = TRUE) 
model1 <- caret::train(x = train1[,-1], y = train1[,1],method= "enet") 
model1 <- caret::train(x = train1[,-1], y = train1[,1],method= "rbf")
library(frbs); library(plyr)
model1 <- caret::train(x = train1[,-1], y = train1[,1],method= "M5Rules")

model1 <- caret::train(x = train1[,-1], y = train1[,1],method= "RRF") 



mdl_pred <- NULL
mdl_pred$act <- test3$count;
mdl_pred$p1 <- predict(md0,test);
mdl_pred$p2 <- predict(md1,test);
mdl_pred$p3 <- predict(md2,test);
mdl_pred$p4 <- predict(md3,test);
mdl_pred$p5 <-  (predict(svm3,test3[,-1]))

mdl_pred <- data.frame(mdl_pred);
results <- NULL;
for(i in 2:ncol(mdl_pred))
{
  results[i-1] <- mean(abs(mdl_pred[,1]-mdl_pred[,i]),na.rm = TRUE);
}
results;


model1 <- train(x = dtemp11[-i,-1], y = as.factor(dtemp11[-i,1]),method= algo[j])
out1 <- as.character(predict(model1,  dtemp11[i,-1], type="raw"))

#predicth1 <- predict(marg1,mat1h1);


test_model <- lmer(count ~  hits + hits*mapercap +  mapercap + meanhitsneighbor + meancountneighbor +  week + tmpf + relh + population_count + median_income + arrival_rate + median_age +as.factor(year)+ as.factor(state) + as.factor(disease) + (0+hits|state),data = final_f1)
summary(mdl8); AIC(mdl8);

----------------
  
  
  mdl8 = lmer(count ~  hits + hits*lmapercap +  lmapercap + meanhitsneighbor + meancountneighbor +  week + tmpf + relh + population_count_lag + median_income_lag + arrival_rate + median_age_lag +as.factor(year)+ as.factor(state) + as.factor(disease) + (0+hits|state),data = final_f1)
summary(mdl8); AIC(mdl8); 

mat1$countpc <- mat1$count / mat1$population_count

temp = lmer(countpc ~  hits + hits*lmapercap +  lmapercap + meanhitsneighbor + meancountneighbor +  week + tmpf + relh + population_count + median_income_lag + arrival_rate + 
              median_age_lag +as.factor(year)+ as.factor(state) + as.factor(disease) + (0+hits|state),data = mat1)
summary(temp);

## ggplot model 

# Use temp1 matrix

library(usmap)
library(ggplot2)

temp1 <-mat1[,c("state_id","eff1","eff2","eff3")]

temp1 <- aggregate(mat1[,c("eff1","eff2","eff3")], by=list(mat1$state_id),
                   FUN=mean, na.rm=TRUE)
colnames(temp1)[1] <- "abbr";

temp2 <- merge(statepop,temp1, by = "abbr", all.y = TRUE)

plot_usmap(data = temp2, values = "eff3", color = "black") + 
  scale_fill_continuous(
    low = "white", high = "blue", name = "AME \n (Online Activity)", label = scales::comma
  ) + theme(legend.position = "right")



predicth0 <- predict(marg1,mat1);
predicth1 <- predict(marg1,mat1h1);

mat10 <- mat1;
## change the moderator
mat10[,var2] <- mat10[,var2] + 0.01*mat10[,var2];

mat1$predict0 <- predict(marg1,mat1);

mat101

mat10[,var1] <- mat10[,var1] + 0.01*mat10[,var1];
mat1h1cl[,var2] <- math1[,var1] + 0.01*math1[,var1];

mat1h1[,var3] <- math1[,var1]*math1[,var2];

mat1h1 <- mat1;
mat1h1[,var1] <- math1[,var1] + 0.01*math1[,var1];
mat1h1[,var3] <- math1[,var1]*math1[,var2];


mat1$predict0 <- predict(marg1,mat1);
mat1$predict1 <- predict(marg1,mat1) 



---------------------
  
  
  
  
  mdl3 = lmer(count ~  hits + meanhitsneighbor + week + tmpf + relh + pop_density + median_income + arrival_rate + median_age + actualpercapcost +as.factor(year)+ as.factor(state) + as.factor(disease) + (0+hits|state),data = final_f1)
summary(mdl3); AIC(mdl3); 
(std_beta(mdl3, type = "std2"))

##Now the script for predicting the marginal  effect 
# let model be mdl4; 

mdl4 = lmer(count ~  hits + ffsbenificiariespercap+ hits*ffsbenificiariespercap + meanhitsneighbor + meancountneighbor +  week + tmpf + relh + pop_density + median_income + arrival_rate + median_age  +as.factor(year)+ as.factor(state) + as.factor(disease) + (0+hits|state),data = final_f1)
summary(mdl4); AIC(mdl4); 

mat1 <- final_f1;
var1 <- "hits"
var2 <- "actualpercapcost"
var3 <- paste(var1,var2,sep="");
mat1[,var3] <- mat1[,var1]*mat1[,var2];

marg1 = lmer(count ~  hits + actualpercapcost+ hitsactualpercapcost + meanhitsneighbor + meancountneighbor +  week + tmpf + relh + pop_density + median_income + arrival_rate + median_age  +
               as.factor(year)+ as.factor(state) + as.factor(disease) + (0+hits|state),data = mat1)
summary(marg1); AIC(marg1);
mat1h1 <- mat1;
mat1h1[,var1] <- math1[,var1] + 0.01*math1[,var1];
mat1h1[,var3] <- math1[,var1]*math1[,var2];

predicth0 <- predict(marg1,mat1);
predicth1 <- predict(marg1,mat1h1);

mat1hcl <- mat1;
mat1h1cl[,var2] <- mat1h1cl[,var1] + 0.01*mat1h1cl[,var1];

mat1h1cl[,var1] <- math1[,var1] + 0.01*math1[,var1];
mat1h1cl[,var2] <- math1[,var1] + 0.01*math1[,var1];

mat1h1[,var3] <- math1[,var1]*math1[,var2];

mat1h1 <- mat1;
mat1h1[,var1] <- math1[,var1] + 0.01*math1[,var1];
mat1h1[,var3] <- math1[,var1]*math1[,var2];


mat1$predict0 <- predict(marg1,mat1);
mat1$predict1 <- predict(marg1,mat1) 



cor(final_f1[,c("hits","tmpf","relh","pop_density","median_income","arrival_rate","median_age","actualpercapcost","meanhitsneighbor")])
#REGRESSION
mdl1 = lm(count ~  lag1hits +  week + tmpf + relh + pop_density + median_income + arrival_rate + median_age + actualpercapcost + as.factor(year) + as.factor(state) + as.factor(disease),data = final_f1)
summary(mdl1); AIC(mdl1);

# RANDOM INTERCEPT 
mdl2 = lmer(count ~  lag1hits +  week + tmpf + relh + pop_density + median_income + arrival_rate + median_age + actualpercapcost + as.factor(year) + as.factor(disease) + (1|state),data = final_f1)
summary(mdl2); AIC(mdl2);
(std_beta(mdl2, type = "std2"))

# RANDOM COEFFICEINT FIXED INTERCEPT 
mdl3 = lmer(count ~  lag1hits +  week + tmpf + relh + pop_density + median_income + arrival_rate + median_age + actualpercapcost +as.factor(year)+ as.factor(state) + as.factor(disease) + (0+lag1hits|state),data = final_f1)
summary(mdl3); AIC(mdl3); 



# INTERACTION PERCAP COST RANDOM COEFICENT FIXED INTERCEPT 
mdl4 = lmer(count ~  lag1hits +  week + tmpf + relh + pop_density + median_income + arrival_rate + median_age + actualpercapcost + lag1hits*actualpercapcost+  as.factor(year)+ as.factor(state) + as.factor(disease) + (0+lag1hits|state),data = final_f1)
summary(mdl4); AIC(mdl4);
temp <- (std_beta(mdl4, type = "std2"))
temp$z <- temp$std.estimate / temp$std.error;
temp$pvalue = exp(-0.717 * temp$z - 0.416 * (temp$z * temp$z))

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
mdl8 = lmer(count ~  lag2hits +  week + tmpf + relh + pop_density + median_income + arrival_rate + median_age + inpatientpercapcost + lag2hits*inpatientpercapcost+  as.factor(year)+ as.factor(state) + as.factor(disease) + (0+lag2hits|state),data = final_f1)
summary(mdl8); AIC(mdl8);

# INTERACTION OUTPATIENT PER CAP COST RANDOM COEFICENT FIEXED INTERCEPT 
mdl9 = lmer(count ~  lag2hits +  week + tmpf + relh + pop_density_lag + median_income_lag + arrival_rate + median_age + outpatientpercapcost_lag + lag2hits*outpatientpercapcost_lag +  as.factor(year)+ as.factor(state) + as.factor(disease) + (0+lag2hits|state),data = final_f1)
summary(mdl9); AIC(mdl9);
-------------------------------------------------------------------------------------------------
  # CHECKING THE IMPORTANCE OF NEIGHBORING HITS MODELS 
  
  # mean of 1 week lag of neighboring hits
  mdln1 = lmer(count ~  lag2hits + l1meanhitsneighbor +  week + tmpf + relh + pop_density + median_income + arrival_rate + median_age + actualpercapcost + lag2hits*outpatientpercap+  as.factor(year)+ as.factor(state) + as.factor(disease) + (0+lag2hits|state),data = final_f1)
summary(mdln1); AIC(mdln1);

# Mean of 2 week lag of neighboring hits
mdln2 = lmer(count ~  lag2hits + l2meanhitsneighbor +  week + tmpf + relh + pop_density_lag + median_income_lag + arrival_rate + median_age_lag + actualpercapcost_lag + lag2hits*actualpercapcost_lag +  as.factor(year)+ as.factor(state) + as.factor(disease) + (0+lag2hits|state),data = final_f1)
summary(mdln2); AIC(mdln2);

mdln3 = lmer(count ~  lag2hits + l2meanhitsneighbor +  week + tmpf + relh + pop_density_lag + median_income_lag + arrival_rate + median_age_lag + inpatientpercapcost_lag + lag2hits*inpatientpercapcost_lag +  as.factor(year)+ as.factor(state) + as.factor(disease) + (0+lag2hits|state),data = final_f1)
summary(mdln3); AIC(mdln3);

mdln4 = lmer(count ~  lag2hits + l2meanhitsneighbor +  week + tmpf + relh + pop_density_lag + median_income_lag + arrival_rate + median_age_lag + outpatientpercapcost_lag + lag2hits*outpatientpercapcost_lag +  as.factor(year)+ as.factor(state) + as.factor(disease) + (0+lag2hits|state),data = final_f1)
summary(mdln4); AIC(mdln4);

mdln5 = lmer(count ~  lag2hits + l2meanhitsneighbor +  week + tmpf + relh + pop_density_lag + median_income_lag + arrival_rate + median_age_lag + actualpercapcost + lag2hits*actualpercapcost +  as.factor(year)+ as.factor(state) + as.factor(disease) + (0+lag2hits|state),data = final_f1)
summary(mdln5); AIC(mdln5);

mdln6 = lmer(count ~  lag2hits + l2meanhitsneighbor +  week + tmpf + relh + pop_density_lag + median_income_lag + arrival_rate + median_age_lag + actualpercapcost + lag2hits*actualpercapcost +  as.factor(year)+ as.factor(state) + as.factor(disease) + (0+lag2hits|state),data = final_f1)
summary(mdln6); AIC(mdln6);

# mean of 1 week lag of neighboring count
mdln3 = lmer(count ~  lag2hits +l1meancountneighbor + week + tmpf + relh + pop_density + median_income + arrival_rate + median_age + actualpercapcost + lag2hits*outpatientpercap+  as.factor(year)+ as.factor(state) + as.factor(disease) + (0+lag2hits|state),data = final_f1)
summary(mdln3); AIC(mdln3);

# total test cost per captia
mdln4 = lmer(count ~  lag2hits +l2meancountneighbor + week + tmpf + relh + pop_density + median_income + arrival_rate + median_age + testpercapcost_lag + lag2hits*testpercapcost_lag +  as.factor(year)+ as.factor(state) + as.factor(disease) + (0+lag2hits|state),data = final_f12)
summary(mdln4); AIC(mdln4);

# percent eligible for ma
mdln5 = lmer(count ~  lag2hits +l2meancountneighbor + week + tmpf + relh + pop_density + median_income + arrival_rate + median_age + Percent_Eligibleforma_lag + lag2hits*Percent_Eligibleforma_lag +  as.factor(year)+ as.factor(state) + as.factor(disease) + (0+lag2hits|state),data = final_f12)
summary(mdln5); AIC(mdln5);


# EXTRACTING COEFICENTS
coefmdl3 <- coef(mdl3)
df1 <- NULL
df1$state <-  row.names((coefmdl3$state))
df1$lag2hitscoef <- coefmdl3$state$lag2hits
df1 <- data.frame(df1);
finaldfm81 <- merge(df1,cleanmodeldf, by = "state",all.x = TRUE)

# PLOTTING COEF FOR MODEL 
library(ggplot2)
dx = df1[order(df1$lag2hitscoef),]
ggplot(dx, aes(dx$lag2hitscoef, dx$state));
p1 <-p + geom_point(aes(color = factor(state))
                    
                    # Make HTML situation for all of the models
                    library(texreg)
                    linear <-htmlreg(list(mdl1,mdl2,mdl3), file = "LMER_MODELS_F.doc", inline.css = FALSE,
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
                    
                    ## Creating lag variables in the data 
                    
                    lfinal <- final_f1;
                    ## script for lagging weekly variables 
                    
                    lag = 1;
                    var = "tmpf"
                    lfinal[,paste("lag",tlag,var,sep = "")] <- NA;
                    u_disease <- unique(lfinal$disease);
                    u_state <- unique(lfinal$state);
                    # finalx is a copy of final_f4
                    # do the lag 1 and 2  of hits, count, nhits, ncount, remove irrelevant columns. 
                    temp1 <- NULL
                    for(i in 1:length(u_disease))
                    {
                      
                      for(j in 1:length(u_state))
                      {
                        temp <- lfinal[lfinal$disease==u_disease[i] & lfinal$state==u_state[j] ,];
                        temp <- temp[order(temp$year)&order(temp$week),];
                        #temp[,paste("l",lag,var,sep = "")] <- NA;
                        
                        temp[(1+lag):nrow(temp),paste("lag",tlag,var,sep = "")] <- temp[1:(nrow(temp)-lag),var]; 
                        temp1 <- rbind(temp1,temp)
                        print(paste(i,"------",j));
                      }
                    }
                    lfinal <- temp1;
                    