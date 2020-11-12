# Mediation script
require(foreign)
require(ggplot2)
require(MASS)
require(Hmisc)
require(reshape2)
require(lme4)
require(ordinal)
require(MCMCglmm)
library(pscl)
install.packages("pscl")
install.packages("tidyverse")
library("tidyverse")
install.packages("dplyr")
library(dplyr)
require(dplyr)
####################################################################################
# MERGING PAYMENT INFORMATION INTO THE DATASET
orderpayments <- read.csv("orders.csv")

# different types of data before being merged 
credit_card <- orderpayments[orderpayments$payment_type == "credit_card",]
voucher <-  orderpayments[orderpayments$payment_type == "voucher",]
debit_card  <- orderpayments[orderpayments$payment_type == "debit_card ",]
not_defined <- orderpayments[orderpayments$payment_type == "not_defined",]

# this is only creditcard payments
merge_payment <- merge35f[merge35f$payment_type == "credit_card",]

temp <-data.frame(table(merge_payment[,c("order_id","payment_type")]))
temp.1 <- temp[temp$Freq == 1,];
temp.1.1 <- temp[temp$Freq == 2,];# this is the duplicate order_id's 
temp.2 <- merge_payment[merge_payment$order_id %in% temp.1$order_id,] # THIS GETS RID OF EXTRA ORDER_ITEM_ID DUPLICATE COLUMNS
# i will use temp.3 not sure whhat to do from here unless remerge ever dataframe!!!!!

# this is the duplicate order_id's (and concequently review_comment)
temp.1.1 <- temp[temp$Freq == 2,];# this is the duplicate order_id's 
temp.2.2 <- merge_payment[merge_payment$order_id %in% temp.1.1$order_id,] # dataframe of duplicates

# THIS IS THE FINAL DATAFRAME !!!!!!!!!!!!!!!!!!!!!!!!!!!
dataf1 <- merge_payment[merge_payment$order_item_id == 1,] # THIS IS CORRECT KEEPS UNIQUE ORDER_ID BUT GETS RID OF EXTRA ORDER_ITEM_ID INFORMATION 
################################################################################################################
# FIXING UP A FEW COLUMNS BEFORE MOVING ON TO ANALYSIS -----
---------------------------------------------------------------------------------------------------------------------
temper <- summary(lm(num_review_score ~  total_payment + order_lead_time + delay + price + package_volume + as.factor(seller_id) + freight_value + as.factor(customer_city) + time
                     ,data = merge35f[merge35f$product_category_name == "cama_mesa_banho",]))

temper <- summary(lm(num_review_score ~  total_payment + order_lead_time + delay + price + package_volume + as.factor(seller_id) + freight_value + as.factor(customer_city) + as.factor(time)
                     ,data = merge35f[merge35f$product_category_name == "cama_mesa_banho",]))

merge35f$order_purchase_timestamp <- as.POSIXct(merge35f$order_purchase_timestamp, format = "%Y-%m-%d %H:%M:%S")


merge35f$time <- strftime(merge35f$order_purchase_timestamp,format="%W") 
-----------------------------------------------------------------------------------------------------------------
# TO DO !!!!
# filter for only orders that have one reveiw!!!!!!!!!!!!!! MIGHT HAVE DONE THIS (look at brazilliandatascript)
# filter for only one type of payments !!!!!!!!!!!!!!!!!!!!!!!!!!!! STILL HAVE TO MERGE PAYMENTS 

-----------------------------------------------------------------------------------------------
# Datasets
merge32f <- read.csv("merger32F.csv") # this is removed na values of review score and only columns with coments 
merge33f <- read.csv("merge33f.csv") # this is only non na values of review score with status as delivered!!
#This are the two final dataframes now
merge34f <- merge33f #added a few more columns 
merge35f <- merge34f # this is the final dummy dataframe !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
merge35f <- read.csv("merge35f.csv")
write.csv(merge35f,file="merge35f.csv")
---------------------------------------------------------------------------------------------
u_prod <- unique(merge35f$product_category_name)  # getting unique category names
temp3F <- NA
temp1F <- NA
for(i in 1:length(u_prod))# for each 
{
  temp1F$adj_rsq <- summary(lm(review_score ~  total_payment +
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
# compare top rsquared with categories that have the most information
sort(table(merge35f$product_category_name))
-----------------------------------------------------------------------------------
  # TOP 10 CATEGORIES WITH MOST NUMBER OF TRANSACTIONS!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
temp <- c("automotivo","ferramentas_jardim",
            "telefonia","relogios_presentes","utilidades_domesticas","informatica_acessorios",
            "moveis_decoracao","esporte_lazer","beleza_saude","cama_mesa_banho")

# MAKING A SUBSET OF THE DATA THAT IS THE TOP PRODUCT CATEGORIES
merge36f <- merge35f[merge35f$product_category_name %in% temp,]

# TOP 9 categories when run linear model have the best R squared values 
temp1 <- c("pc_gamer", "moveis_sala", "telefonia_fixa", "fashion_esporte","artigos_de_natal", "agro_industria_e_comercio","flores","fraldas_higiene", "fashion_roupa_infanto_juvenil")
#subset of top R squared 
merge37f <-merge35f[merge35f$product_category_name %in% temp1,] # not alot of observations
-----------------------------------------------------------------------------------------------------  
#numeric review column
merge35f$num_review_score <- as.numeric(as.character(merge35f$review_score))
merge35f$num_comment <- as.numeric(merge35f$comment)
###################################################################################################
merge38f <- merge35f[!(is.na(merge35f$delay),]
merge38f <- merge38f[!(is.na(merge38f$package_volume)),]
merge38f <- merge38f[!(is.na(merge38f$order_lead_time)),]

# Starting mediation analysis !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# making response a factor
  install.packages("MASS")
library(MASS)
install.packages("DescTools")
library(DescTools)
install.packages("mediation")
library("mediation")
install.packages("rsq")
library("rsq")

merge35f # THIS IS THE DATAFRAME WITH ALL OF THE INFORMATION STILL

# This is the final dataframe(did not re-mergege everything just getting creditcard and non repeating order_id)
#This should get rid of repeating columns of comment (maybe some loss of information) because
#dataf1 only has credit card information and order_item_id of 1 meaning duplicate orders dont exist and retaining 
#information about ourders that have multiple items just keeping first iteration (use total payment for model not price)
dataf1 <- merge_payment[merge_payment$order_item_id != 2,] # this is the FINAL DATAFRAME (ONLY CREDITCARD & NO DUPLICATES) 

# include payment information into model
temp1 <- dataf1[,c("total_payment","delay","order_lead_time","package_volume","num_review_score","comment")]
temp1 <- temp1[complete.cases(temp1),];


##########################################################################
# use total payment and not price in model
mdl1 <- polr(review_score ~  total_payment , data = merge35f)
PseudoR2(mdl1)
mdl2 <- polr(review_score ~  total_payment + delay , data = merge35f)
PseudoR2(mdl2)
mdl3 <- polr(review_score ~  total_payment + delay + order_lead_time , data = merge35f)
PseudoR2(mdl3)
######################################################################################
# This is the medator model we will use for AIC and most relevent independent variables 
mdl4 <- polr(review_score ~  total_payment + delay + order_lead_time + package_volume , data = merge35f)  
PseudoR2(mdl4)  

# This is the outcome model that we will use 
mdl1r <- polr(comment ~  total_payment + delay + order_lead_time + package_volume , data = merge35f)
PseudoR2(mdl1r)
############################################################################################
#CAUSAL MEDIATION ANALYSIS 
# THIS IS THE FIAL MODEL THAT WE ARE USING!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
install.packages("arm")
library(arm)
# TRYING MODELS WITH POLR (not quite working )
#MODEL M = MEDATION MODEL
# MODEL Y = OUTCOME MODEL 
model.m1 <- polr(review_score ~  total_payment + delay + order_lead_time + package_volume , data = merge35f)  
PseudoR2(model.m1)  
m1.coef <- model.m1$coefficients
m1.1 <- standardize(model.m1)

model.y1 <- polr(comment ~  total_payment + delay + order_lead_time + package_volume + review_score , data = merge35f)
PseudoR2(model.y1)
y1.coef <- model.y1$coefficients
y1.1 <- standardize(model.y1)

#First model allows for non parametric bootstrap
out.1 <- mediate(model.m1, model.y1, sims = 1000, treat = "delay",
                    mediator = "review_score")
summary(out.1)
# This modelinference proceeds via the quasi-Bayesian Monte Carlo approximation
out.2 <- mediate(model.m1, model.y1, sims = 1000, treat = "delay",
                    mediator = "review_score")
summary(out.2)

##############################################################################################
# CAUSAL MEDIATION ANALYSIS WITH LINEAR REGRESSION INSTEAD
model.lm1 <- lm(num_review_score ~  total_payment + delay + order_lead_time + package_volume, merge35f[merge35f$product_category_name == "cama_mesa_banho",])  
summary(model.lm1)  
model.lmy1 <- lm(num_comment ~  total_payment + delay + order_lead_time + package_volume + num_review_score , merge35f[merge35f$product_category_name == "cama_mesa_banho",])
summary(model.lmy1)
#First model allows for non parametric bootstrap
out.lm1 <- mediate(model.lm1, model.lmy1, sims = 1000, boot = TRUE, treat = "delay",
                 mediator = "num_review_score")
summary(out.lm1)
# This modelinference proceeds via the quasi-Bayesian Monte Carlo approximation
out.lm2 <- mediate(model.lm1, model.lmy1, sims = 1000, treat = "delay",
                 mediator = "num_review_score")
summary(out.lm2)
###################################################################################

#CAUSAL MEDIATION ANALYSIS ORDERED AND GLM (BOTH)
temp1 <- merge35f[,c("total_payment","delay","order_lead_time","package_volume","num_review_score","comment")]
temp1 <- temp1[complete.cases(temp1),];
model.b1 <- glm(num_review_score ~  total_payment + delay + order_lead_time + package_volume , data = temp1,family = poisson)  
PseudoR2(model.b1)  
model.b2  <- glm(comment ~  total_payment + delay + order_lead_time +num_review_score, data = temp1, family = binomial)
rsq(model.b2)
# MEDIATION ANALYSIS RESULTS
out.bm1 <- mediate(model.b1, model.b2, sims = 1000, treat = "delay",
                   mediator = "num_review_score")
summary(out.b1)
# This modelinference proceeds via the quasi-Bayesian Monte Carlo approximation
out.bm2 <- mediate(model.b1, model.b2, sims = 1000, treat = "delay",
                   mediator = "review_score")
summary(out.b2)

#####################################################################################



