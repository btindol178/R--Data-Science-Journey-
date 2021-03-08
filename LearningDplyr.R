rm(list=ls())
setwd("C:/Users/btindol/OneDrive - Stryker/Linked In Learn/Dplyr-Tidyverse")
########################################################################################################################################
install.packages("tidyverse")
library(tidyverse)
library(dplyr)

##########################################
#load other data
library(datasets)
data(iris)
names(iris)
###########################
# Left join df....
merge <- data.frame(species = c("versicolor","virginica"),yesno = c("yes","no"))


###############################
# Replace period in column names 
df <- iris %>%  
    setNames(tolower(gsub("\\.","_",names(.))))%>% # replace . in names with _
    mutate(petalarea = petal_length * petal_width, sepalarea = sepal_length * sepal_width)%>% # make 2 new columns
    arrange(desc(petalarea)) %>% # arrange in desc order
    select(sepal_length,sepal_width,petal_length,petal_width,species,petalarea,sepalarea) %>% # select all the columns 
    rename(sepalareas = sepalarea) %>% # simple rename to make it plural
    filter(sepal_width < 4.1) %>% # filter for this column less than 4.1
    filter(species %in% c("virginica","versicolor")) %>% # filter for this or that 
    filter(species == "virginica" | species == "versicolor") %>% # same thing this or that 
    filter( !(petal_length > 6.1 | sepal_length > 7.2)) %>%# filter not in this ()
    mutate(petal_length=replace(petal_length, petal_length==5.5, NA)) %>%# put na values in table(is.na(df$petal_length)) check with this
    mutate(petal_length = replace_na(petal_length,0)) %>% # replace na values that you just created with zero.. 
    mutate(sepal_length = as.numeric(sepal_length),   # change data types
           sepal_width = as.numeric(sepal_width),
           petal_length = as.numeric(petal_length),
           petal_width = as.numeric(petal_width),
           species = as.character(species),
           petalarea = as.numeric(petalarea), 
           sepalareas = as.numeric(sepalareas)) %>%  
    mutate(predictorz = ifelse(species == "versicolor", 0, 1)) %>% # ifelse for a column 
    left_join(merge, by = "species")%>% # left join dont need to call out the iris dataframe
    group_by(species) %>% 
    summarise(
    count = n(),
    meansepal = mean(sepal_length, na.rm = TRUE),
    meanpetal = mean(petal_length, na.rm = TRUE))%>%
    ungroup() # to  perform more calculations after otherwise summing might be off..
    
   
  
  df <- iris %>%  
  setNames(tolower(gsub("\\.","_",names(.))))%>% # replace . in names with _
  mutate(petalarea = petal_length * petal_width, sepalarea = sepal_length * sepal_width)%>% # make 2 new columns
  arrange(desc(petalarea)) %>% # arrange in desc order
  select(sepal_length,sepal_width,petal_length,petal_width,species,petalarea,sepalarea) %>% # select all the columns 
  rename(sepalareas = sepalarea) %>% # simple rename to make it plural
  filter(sepal_width < 4.1) %>% # filter for this column less than 4.1
  filter(species %in% c("virginica","versicolor")) %>% # filter for this or that 
  filter(species == "virginica" | species == "versicolor") %>% # same thing this or that 
  filter( !(petal_length > 6.1 | sepal_length > 7.2)) %>%# filter not in this ()
  mutate(petal_length=replace(petal_length, petal_length==5.5, NA)) %>%# put na values in table(is.na(df$petal_length)) check with this
  mutate(petal_length = replace_na(petal_length,0)) %>% # replace na values that you just created with zero.. 
  mutate(sepal_length = as.numeric(sepal_length),   # change data types
         sepal_width = as.numeric(sepal_width),
         petal_length = as.numeric(petal_length),
         petal_width = as.numeric(petal_width),
         species = as.character(species),
         petalarea = as.numeric(petalarea), 
         sepalareas = as.numeric(sepalareas)) %>%  
  mutate(predictorz = ifelse(species == "versicolor", 0, 1)) %>% # ifelse for a column 
  left_join(merge, by = "species")%>% 
  mutate(newcolumn = "fast_cat")%>%
  separate(newcolumn,c("fast","cat"),sep = "\\_")%>% # separate the generated column by the underscore.
  
# to do wide to long.. long to wide
# learn how to ungroup 
# cumsum .... 
#count subgroups

data(mtcars)
mtcars

# Get every combination of cyl and gear
mtcars %>% 
  group_by(cyl, gear) %>% 
  summarise(n = n())

# sunspots for cumsum 
data("AirPassengers")

air <- data.frame(airquality)

# Cumulative sum
air2 <- air %>% 
  mutate(var2 = cumsum(Wind))

# Pivot Wider!!!
air2 %>%
  pivot_wider(names_from = Month, values_from = Wind)
