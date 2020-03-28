########################################################################
library(rvest)
library(tidyverse)
library(data.table)
library(DT)
library(magrittr)
library(digest)
library(RPostgreSQL)
library(tidytext)
library(config)
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(RCurl)
library(XML)
library(stringr)
library(zoo)
library(ggplot2)
library(knitr)


listings7 <- data.frame(title=character(),
                       company=character(), 
                       location=character(), 
                       summary=character(), 
                       link=character(), 
                       description = character(),
                       stringsAsFactors=FALSE) 

# when searching for a job url it will not have & start it will end at all you ust add the &start= or it wont work!!!!!!!!!!!!!!!!!!
for (i in seq(0, 990, 10)){
  url_ds <- paste0('https://www.indeed.com/jobs?q=database+administrator&l=all&start=',i)
  var <- read_html(url_ds)
  
  #job title
  title <-  var %>% 
    html_nodes('#resultsCol .jobtitle') %>%
    html_text() %>%
    str_extract("(\\w+.+)+") 
  
  #company
  company <- var %>% 
    html_nodes('#resultsCol .company') %>%
    html_text() %>%
    str_extract("(\\w+).+") 
  
  #location
  location <- var %>%
    html_nodes('#resultsCol .location') %>%
    html_text() %>%
    str_extract("(\\w+.)+,.[A-Z]{2}")   
  #summary
  summary <- var %>%
    html_nodes('#resultsCol .summary') %>%
    html_text() %>%
    str_extract(".+")
  
  #link
  link <- var %>%
    html_nodes('#resultsCol .jobtitle .turnstileLink, #resultsCol a.jobtitle') %>%
    html_attr('href') 
  link <- paste0("https://www.indeed.com",link)
  
  listings7 <- rbind(listings7, as.data.frame(cbind(title,
                                                  company,
                                                  location,
                                                  summary,
                                                  link)))
}

# rbind dataframes
indeed_df <- rbind(listings,listings2,listings3,listings4)
indeed_df2 <- rbind(listings5,listings6,listings7)

#this is the final dataframe
final_indeed <- rbind(indeed_df,indeed_df2)

write.csv(final_indeed,file="final_indeed.csv")

