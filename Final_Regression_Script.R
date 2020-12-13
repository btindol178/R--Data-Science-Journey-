# Final Exam 
###############################################################################################################################################################
setwd("C:/Users/blake/OneDrive/WMU FALL 2020/Regression/Final Exam")
healthy = read.table("healthybreakfast.txt", header = TRUE)
library(ggplot2)
install.packages("GGally")
library(GGally)
library(gridExtra)
library(dplyr)

###############################################################################################################################################################
# second shelf has highest sugar content
boxplot(healthy$sugars~healthy$shelf)

###############################################################################################################################################################
# Requirements for a healthy breakfast 
# FAT: no more than 30% of their calories in the form of fat,
# PROTIEN: no more than 50 grams (women) or 63 grams (men)
# One gram of fat contains 9 calories and carbohydrates and proteins contain 4 calories per gram
# A "good" diet should also contain 20-35 grams of dietary fiber.

###############################################################################################################################################################

# Visualization with plots
# Histograms of the different variables help find which cereals are the best and worst in a particular
#category. Scatterplots provide insight into relationships between, say, sugars and calories, fat and
#calories, etc.

# Surprisingly, many healthier cereals ended up having lower ratings. Cereals on the middle shelf in supermarkets tended to have the lowest ratings

# 1) Exploritory data analysis

healthy2 = healthy; healthy2$shelf <- as.factor(healthy2$shelf)
# Create function to add regression line to scatter plot matrix
sm_regression <- function(data, mapping, ...){
  p <- ggplot(data = data, mapping = mapping) + 
    geom_point(alpha = 0.4) + 
    geom_smooth(method=lm, fill="grey10", color="grey10", ...)
  p
}

healthy2 %>% 
  select(calories,protein,sodium, carbo, fat, sugars,fiber,potass,shelf,rating) %>% 
  ggpairs(columns = 1:10, lower = list(continuous = sm_regression)) +
  theme_bw()

healthy %>% 
  select(calories,protein,sodium, carbo, fat, sugars,fiber,potass,shelf,rating) %>% 
  ggcorr(palette = "RdBu", label = TRUE, label_round =  2)

# rank healthy sugars
healthyrank2 <- healthy[order(sugars),]; healthyrank2 <- head(healthyrank2,n=25)
# find most accouring number of grams of sugars by shelf
sugarrank <- data.frame(table(healthyrank2$sugars,healthyrank2$shelf));colnames(sugarrank) <- c(1,2,3)

fat = healthy$fat;calories = healthy$calories;protein= healthy$protein; sugars= healthy$sugars;vitamins = healthy$vitamins;
carbo= healthy$carbo;potass = healthy$potass; fiber= healthy$fiber; rating = healthy$rating

# Other plots 
par(mfrow=c(2,2))
boxplot(healthy$rating~healthy$mfr,main= 'Manufacturer vs rating',xlab = "manufacturer",ylab = "rating",col = rainbow(6))
boxplot(healthy$rating~healthy$shelf,main= 'shelf vs rating',xlab = "shelf #",ylab = "rating", col = c("red","blue","green"))
plot(healthy$weight,healthy$rating,main = "rating by weight",xlab= "weight",ylab = "rating")
abline(lm(healthy$rating ~ healthy$weight, data = healthy), col = "blue")
plot(healthy$cups,healthy$rating,main = "rating by cupps",xlab= "cups",ylab = "rating")
abline(lm(healthy$rating ~ healthy$cups, data = healthy), col = "blue")

# mean of that column conditional for shelf = 1 or 2 or 3
mean(healthy[healthy$shelf==1, "potass"], na.rm = TRUE)
mean(healthy[healthy$shelf ==2, "potass"], na.rm = TRUE)
mean(healthy[healthy$shelf ==3, "potass"], na.rm = TRUE)


## Find cereals which has highest and lowest content of calories, protein, fat, sugar, carbohydrate, vitamins, and potassium through the below codes
fatmax = healthy[which.max(fat),1]
fatmin = healthy[which.min(fat),1]
calmax = healthy[which.max(calories),1]
calmin = healthy[which.min(calories),1]
promax = healthy[which.max(protein),1]
promin = healthy[which.min(protein),1]
vitamax = healthy[which.max(vitamins),1]
vitamin =healthy[which.min(vitamins),1]
sugarmax=healthy[which.max(sugars) ,1]
sugarmin=healthy[which.min(sugars),1]
carbomax=healthy[which.min(carbo),1]
carbomin=healthy[which.max(carbo),1]
potassmax=healthy[which.min(potass),1]
potassmin=healthy[which.max(potass),1]
fibermax=healthy[which.min(fiber),1]
fibermin=healthy[which.max(fiber),1]
ratingmax=healthy[which.max(rating),1]
ratingmin=healthy[which.min(rating),1]

brandquantile <- brandquantile()

list2 <- c("max","min")
healthysummary = data.frame(minmax = list2 ,fat = c(fatmax,fatmin),calories =c(calmax,calmin),protein = c(promax,promin),
                            vitamins = c(vitamax,vitamin),sugars=c(sugarmax,sugarmin),carbo=c(carbomax,carbomin),potass=c(potassmax,potassmin),fiber=c(fibermax,fibermin),
                            rating=c(ratingmax,ratingmin))



carbquant1 = quantile(healthy$carbo);carbquant2 =unname(carbquant1); 
calquant1 = quantile(healthy$calories);calquant2 =unname(calquant1); 
sugarquant1 = quantile(healthy$sugars);sugarquant2 =unname(sugarquant1); 
protienquant1 = quantile(healthy$protein);protienquant2 =unname(protienquant1); 
fatquant1 = quantile(healthy$fat);fatquant2 =unname(fatquant1); 
potassquant1 = quantile(healthy$potass);potassquant2 =unname(potassquant1); 
vitaminquant1 = quantile(healthy$vitamins);vitaminquant2 =unname(vitaminquant1); 
fiberquant1 = quantile(healthy$fiber);fiberquant2 = unname(fiberquant1); 
shelfquant1 = quantile(healthy$shelf);shelfquant2 = unname(shelfquant1); 
sodiumquant1 = quantile(healthy$sodium);sodiumquant2 =unname(sodiumquant1); 
ratingquant1 = quantile(healthy$rating);ratingquant2 =unname(ratingquant1); 
sodiumquant1 = quantile(healthy$sodium);sodiumquant2 =unname(sodiumquant1); 




quantile = c("0%","25%","50%","75%","100%")
healthmatrixnumbers = data.frame(percentile = quantile2, carbohydrates = c(carbquant2[1],carbquant2[2],carbquant2[3],carbquant2[4],carbquant2[5]),
                          calories = c(calquant2[1],calquant2[2],calquant2[3],calquant2[4],calquant2[5]),
                          sugars = c(sugarquant2[1],sugarquant2[2],sugarquant2[3],sugarquant2[4],sugarquant2[5]),
                          protein = c(protienquant2[1],protienquant2[2],protienquant2[3],protienquant2[4],protienquant2[5]),
                          fat = c(fatquant2[1],fatquant2[2],fatquant2[3],fatquant2[4],fatquant2[5]),
                          potassium = c(potassquant2[1],potassquant2[2],potassquant2[3],potassquant2[4],potassquant2[5]),
                          vitamins = c(vitaminquant2[1],vitaminquant2[2],vitaminquant2[3],vitaminquant2[4],vitaminquant2[5]),
                          fiber = c(fiberquant2[1],fiberquant2[2],fiberquant2[3],fiberquant2[4],fiberquant2[5]),
                          sodium = c(sodiumquant2[1],sodiumquant2[2],sodiumquant2[3],sodiumquant2[4],sodiumquant2[5]),
                          shelf = c(shelfquant2[1],shelfquant2[2],shelfquant2[3],shelfquant2[4],shelfquant2[5]),
                          rating = c(ratingquant2[1],ratingquant2[2],ratingquant2[3],ratingquant2[4],ratingquant2[5]))

healthmatrixnumbers$carb_percent = round((healthmatrixnumbers$carbohydrates * 4)/healthmatrixnumbers$calories,digits = 3)
healthmatrixnumbers$fat_percent = round((healthmatrixnumbers$fat *9)/healthmatrixnumbers$calories,digits = 3)
healthmatrixnumbers$protein_percent = round((healthmatrixnumbers$protein *4)/healthmatrixnumbers$calories,digits=3)
healthmatrixnumbers$sugar_percent = round((healthmatrixnumbers$sugars *4)/healthmatrixnumbers$calories,digits=3)
healthmatrixnumbers$sodium = healthmatrixnumbers$sodium
healthmatrixnumbers$potassium =healthmatrixnumbers$potassium
healthmatrixnumbers$fiber = healthmatrixnumbers$fiber
healthmatrixnumbers$vitamins = healthmatrixnumbers$vitamins

healthmatrixpercent <- healthmatrixnumbers[,c(1,13:16,7:10)]

# The health matrix code for names (ALSO VISUALIZE DECENDING ORDER FOR A FEW COLUMNS RATING DECENTING BY CALORIES)




carb_0=healthy[which(healthy$carbo ==carbquant2[1]),1];
carb_25=healthy[which(healthy$carbo ==carbquant2[2]),1];
carb_50=healthy[which(healthy$carbo ==carbquant2[3]),1];
carb_75=healthy[which(healthy$carbo ==carbquant2[4]),1];
carb_100=healthy[which(healthy$carbo ==carbquant2[5]),1];

cal_0=healthy[which(healthy$calories ==calquant2[1]),1];
cal_25=healthy[which(healthy$calories ==calquant2[2]),1];
cal_50=healthy[which(healthy$calories ==calquant2[3]),1];
cal_75=healthy[which(healthy$calories ==calquant2[4]),1];
cal_100=healthy[which(healthy$calories ==calquant2[5]),1]; 

sugar_0=healthy[which(healthy$sugars ==sugarquant2[1]),1];
sugar_25=healthy[which(healthy$sugars ==sugarquant2[2]),1];
sugar_50=healthy[which(healthy$sugars ==sugarquant2[3]),1];
sugar_75=healthy[which(healthy$sugars ==sugarquant2[4]),1];
sugar_100=healthy[which(healthy$sugars ==sugarquant2[5]),1];

protein_0=healthy[which(healthy$protein ==protienquant2[1]),1];
protein_25=healthy[which(healthy$protein ==protienquant2[2]),1];
protein_50=healthy[which(healthy$protein ==protienquant2[3]),1];
protein_75=healthy[which(healthy$protein ==protienquant2[4]),1];
protein_100=healthy[which(healthy$protein ==protienquant2[5]),1];

fat_0=healthy[which(healthy$fat ==fatquant2[1]),1];
fat_25=healthy[which(healthy$fat ==fatquant2[2]),1];
fat_50=healthy[which(healthy$fat ==fatquant2[3]),1];
fat_75=healthy[which(healthy$fat ==fatquant2[4]),1];
fat_100=healthy[which(healthy$fat ==fatquant2[5]),1];

potassium_0=healthy[which(healthy$potass ==potassquant2[1]),1];
potassium_25=healthy[which(healthy$potass ==potassquant2[2]),1];
potassium_50=healthy[which(healthy$potass ==potassquant2[3]),1];
potassium_75=healthy[which(healthy$potass ==potassquant2[4]),1];
potassium_100=healthy[which(healthy$potass ==potassquant2[5]),1];

vitamins_0=healthy[which(healthy$vitamins ==vitaminquant2[1]),1];
vitamins_25=healthy[which(healthy$vitamins ==vitaminquant2[2]),1];
vitamins_50=healthy[which(healthy$vitamins ==vitaminquant2[3]),1];
vitamins_75=healthy[which(healthy$vitamins ==vitaminquant2[4]),1];
vitamins_100=healthy[which(healthy$vitamins ==vitaminquant2[5]),1];
  
fiber_0=healthy[which(healthy$fiber ==fiberquant2[1]),1];
fiber_25=healthy[which(healthy$fiber ==fiberquant2[2]),1];
fiber_50=healthy[which(healthy$fiber ==fiberquant2[3]),1];
fiber_75=healthy[which(healthy$fiber ==fiberquant2[4]),1];
fiber_100=healthy[which(healthy$fiber ==fiberquant2[5]),1];

rating_0=healthy[which(healthy$rating ==ratingquant2[1]),1];
rating_25=healthy[which(healthy$rating ==ratingquant2[2]),1];
rating_50=healthy[which(healthy$rating ==ratingquant2[3]),1];
rating_75=healthy[which(healthy$rating ==ratingquant2[4]),1];
rating_100=healthy[which(healthy$rating ==ratingquant2[5]),1];
  
  
shelf_0=healthy[which(healthy$shelf ==shelfquant2[1]),1];
shelf_25=healthy[which(healthy$shelf ==shelfquant2[2]),1];
shelf_50=healthy[which(healthy$shelf ==shelfquant2[3]),1];
shelf_75=healthy[which(healthy$shelf ==shelfquant2[4]),1];
shelf_100=healthy[which(healthy$shelf ==shelfquant2[5]),1];

sodium_0=healthy[which(healthy$sodium ==sodiumquant2[1]),1];
sodium_25=healthy[which(healthy$sodium ==sodiumquant2[2]),1];
sodium_50=healthy[which(healthy$sodium ==sodiumquant2[3]),1];
sodium_75=healthy[which(healthy$sodium ==sodiumquant2[4]),1];
sodium_100=healthy[which(healthy$sodium ==sodiumquant2[5]),1];


require(data.table)

brand_percentile <- function(y){
  dt = rbindlist(
    lapply(y, function(x) data.table(t(x))),
    fill = TRUE
  );
  
  dt = t(dt)
  dt = data.frame(dt);
  colnames(dt) <- c("0%","25%","50%","75%","100%","Varible")
  return(dt)
}


list1 = list(carb_0,carb_25,carb_50,carb_75,carb_100,rep("Carbs",length(list1)+1))  ;carb_percentiles <- brand_percentile(list1)
list2 =list(cal_0,cal_25,cal_50,cal_75,cal_100,rep("Calories",length(cal_75))) ;cal_percentiles <- brand_percentile(list2)
list3 =list(sugar_0,sugar_25,sugar_50,sugar_75,sugar_100,rep("Sugar",length(sugar_25)));sugar_percentiles <- brand_percentile(list3)
list4 =list(protein_0,protein_25,protein_50,protein_75,protein_100,rep("Protein",length(protein_50)));protein_percentiles <- brand_percentile(list4)
list5 =list(fat_0,fat_25,fat_50,fat_75,fat_100,rep("Fat",length(fat_50)));fat_percentiles <- brand_percentile(list5)
list6 =list(potassium_0,potassium_25,potassium_50,potassium_75,potassium_100,rep("potassium",length(potassium_50)));potassium_percentiles <- brand_percentile(list6)
list7 =list(vitamins_0,vitamins_25,vitamins_50,vitamins_75,vitamins_100,rep("Vitamins",length(vitamins_50)));vitamins_percentiles <- brand_percentile(list7)
list8 =list(fiber_0,fiber_25,fiber_50,fiber_75,fiber_100,rep("Fiber",length(fiber_0)));fiber_percentiles <- brand_percentile(list8)
list9 =list(sodium_0,sodium_25,sodium_50,sodium_75,sodium_100,rep("Sodium",length(sodium_0)));sodium_percentiles <- brand_percentile(list9)
list10 =list(shelf_0,shelf_25,shelf_50,shelf_75,shelf_100,rep("Shelf",length(sodium_75))); shelf_percentiles <- brand_percentile(list10)
list11 =list(rating_0,rating_25,rating_50,rating_75,rating_100,rep("Rating",length(rating_75))); rating_percentiles <- brand_percentile(list11)

list_fin = rbind(carb_percentiles,cal_percentiles,sugar_percentiles,protein_percentiles,fat_percentiles,potassium_percentiles,vitamins_percentiles,fiber_percentiles,sodium_percentiles,shelf_percentiles,rating_percentiles)

percentile_0_rank0 <-   table(list_fin[,c(1)]);percentile_0_rank <- sort(percentile_0_rank0, decreasing = TRUE); percentile_0_rank <- data.frame(percentile_0_rank);colnames(percentile_0_rank)<- c("Brand","Zero_Percentile")
percentile_25_rank0 <-   table(list_fin[,c(2)]);percentile_25_rank <- sort(percentile_25_rank0, decreasing = TRUE) ; percentile_25_rank <- data.frame(percentile_25_rank);colnames(percentile_25_rank)<- c("Brand","25th_Percentile")
percentile_50_rank0 <-   table(list_fin[,c(3)]);percentile_50_rank <- sort(percentile_50_rank0, decreasing = TRUE) ; percentile_50_rank <- data.frame(percentile_50_rank);colnames(percentile_50_rank)<- c("Brand","50th_Percentile")
percentile_75_rank0 <-   table(list_fin[,c(4)]);percentile_75_rank <- sort(percentile_75_rank0, decreasing = TRUE) ; percentile_75_rank<- data.frame(percentile_75_rank);colnames(percentile_75_rank)<- c("Brand","75th_Percentile")
percentile_100_rank0 <-   table(list_fin[,c(5)]);percentile_100_rank <- sort(percentile_100_rank0, decreasing = TRUE) ; percentile_100_rank <- data.frame(percentile_100_rank);colnames(percentile_100_rank)<- c("Brand","100th_Percentile")

merge1 <- merge(percentile_0_rank,percentile_25_rank,by=c("Brand"),all.x=TRUE)
merge2 <- merge(merge1,percentile_50_rank,by=c("Brand"),all.x=TRUE)
merge3 <- merge(merge2,percentile_75_rank,by=c("Brand"),all.x=TRUE)
merge4 <- merge(merge3,percentile_100_rank,by=c("Brand"),all.x=TRUE)
Percentile_Rank = merge4

#Script 6
boxplot(healthy$rating~healthy$mfr, data = healthy, xlab = "Manufacturer", ylab = "Rating", main = "Rating vs Manufacturer",  col = c("red","blue","green"))

#Script 7 box plot
boxplot(healthy$rating ~ healthy$shelf, data = healthy, xlab = "Shelf #", ylab = "Rating", main = "Rating vs Shelf", col = c("red","blue","green"))


# Linear models different combinations
options(digits=5)
options(scipen = 999)
#Carbs
mdl_carbs <- lm(rating ~ carbo, data = healthy);mdl_carbs
summary(mdl_carbs)$coefficients;
summary(mdl_carbs)$r.squared;
cor(healthy$rating,healthy$carbo)

# Calories
options(scipen = 0)
mdl_calories <- lm(rating ~ calories, data = healthy);mdl_calories
summary(mdl_calories)$coefficients;
summary(mdl_calories)$r.squared;
cor(healthy$rating,healthy$calories)

#Sodium
options(digits=5)
options(scipen = 999)
mdl_sodium<- lm(rating ~ sodium, data = healthy);mdl_sodium
summary(mdl_sodium)$coefficients;
summary(mdl_sodium)$r.squared;
cor(healthy$rating,healthy$sodium)

# Fat
options(digits=5)
options(scipen = 999)
mdl_fat <- lm(rating ~ fat, data = healthy);mdl_fat
summary(mdl_fat)$coefficients;
summary(mdl_fat)$r.squared;
cor(healthy$rating,healthy$fat)

#Potassium
mdl_potassium <- lm(rating ~ potass, data = healthy);mdl_potassium
summary(mdl_potassium)$coefficients;
summary(mdl_potassium)$r.squared;
cor(healthy$rating,healthy$potass)

#Sugar
options(scipen = 0)
mdl_sugar <- lm(rating ~ sugars, data = healthy);mdl_sugar
summary(mdl_sugar)$coefficients;
summary(mdl_sugar)$r.squared;
cor(healthy$rating,healthy$sugars)

#Protein
mdl_protein <- lm(rating ~ protein, data = healthy);mdl_protein
summary(mdl_protein)$coefficients;
summary(mdl_protein)$r.squared;
cor(healthy$rating,healthy$protein)

#Fiber
mdl_fiber <- lm(rating ~ fiber, data = healthy);mdl_fiber
summary(mdl_fiber)$coefficients;
summary(mdl_fiber)$r.squared;
cor(healthy$rating,healthy$fiber)

#Vitamins
options(digits=5)
options(scipen = 999)
mdl_vitamins <- lm(rating ~ vitamins, data = healthy);mdl_vitamins
summary(mdl_vitamins)$coefficients;
summary(mdl_vitamins)$r.squared;
cor(healthy$rating,healthy$vitamins)


install.packages("texreg")
library(texreg)
linear <-htmlreg(list(mdl_carbs,mdl_calories,mdl_sodium,mdl_fat), file = "Breakfast_Cereal.doc", inline.css = FALSE,
                 doctype = TRUE, html.tag = TRUE, head.tag = TRUE, body.tag = TRUE,stars = c(0.001, 0.01, 0.05, 0.1),digits = 5)

# Full Model 
full_mdl <- lm(rating ~ calories + carbo + protein + fat +sugars +  fiber + sodium + vitamins + potass, data = healthy);mdl_vitamins
summary(full_mdl)

#Visualize the model diognostics
plot(full_mdl)

install.packages("olsrr")
library(olsrr)
ols_plot_cooksd_bar(full_mdl)




# Aggregate shelfs by sum of rating what shelf has highest rating
shelf1 <- healthy[healthy$shelf == 1,c("rating","shelf")]
shelf2 <- healthy[healthy$shelf == 2,c("rating","shelf")]
shelf3 <- healthy[healthy$shelf == 3,c("rating","shelf")]

rating_shelf1 <- mean(healthy[healthy$shelf == 1,"rating"]);rating_shelf1
rating_shelf2 <- mean(healthy[healthy$shelf == 2,"rating"]);rating_shelf2
rating_shelf3 <- mean(healthy[healthy$shelf == 3,"rating"]);rating_shelf3

rating_by_shelf <- data.frame(shelf= c("shelf_1","shelf_2","shelf_3"),mean_rating =c(rating_shelf1,rating_shelf2,rating_shelf3))
rating_by_shelf


#get healthy dataframe and non healthy
healthycereals<- healthy[order(-rating),]; healthycereals <- head(healthycereals,n=25);healthycereals2 <- healthycereals[,c(1,4:12,16)];healthycereals2
nothealthycereals<- healthy[order(rating),]; nothealthycereals <- head(nothealthycereals,n=25);nothealthycereals2 <- nothealthycereals[,c(1,4:12,16)];nothealthycereals2
healthycereals <- data.frame(healthycereals2)
nothealthycereals <- data.frame(nothealthycereals2)

anova(healthycereals2)
