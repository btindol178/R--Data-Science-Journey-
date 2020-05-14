# BIG DATA VISUALIZATION
https://campus.datacamp.com/courses/visualizing-big-data-with-trelliscope-in-r/ggplot2-trelliscopejs?ex=2
df <- read.csv("final_dataframe_withoil.csv")
df <- df[-c(1)]

df1 <- read.csv("online_retail.csv")
colnames(df1)[1] <- "invoice"

library(dplyr)
library(ggplot2)


# Summarize the number of purchases per country
total_count <- df1 %>%
  group_by(Country,StockCode) %>%
  summarise(purchases = n())

# top 10 purchases in decending order
total_count_top10 <- df1 %>%
  group_by(StockCode) %>%
  summarise(purchases2 = n()) %>%
  top_n(n=10) %>%
  arrange(desc(purchases2))

uniquetop <- unique(total_count_top10$StockCode)

# top dataframe of top ten codes!!!!!!!!!!!!!!!!!
df3 <- df1[df1$StockCode %in% uniquetop, ]

#https://campus.datacamp.com/courses/visualizing-big-data-with-trelliscope-in-r/general-strategies-for-visualizing-big-data?ex=2

# Create a histogram most frequent bought codes
p <- ggplot(df3, aes(StockCode)) +
  geom_bar() 
p
########################################################
# top 10 purchases in decending order
buyer_count_top10 <- df1 %>%
  group_by(invoice) %>%
  summarise(buyer = n()) %>%
  top_n(n=10) %>%
  arrange(desc(buyer))

uniquetop <- unique(buyer_count_top10$invoice)

# top dataframe of top ten codes!!!!!!!!!!!!!!!!!
df4 <- df1[df1$invoice %in% uniquetop, ]

# Create a histogram of who bought the most (what invoice)
p <- ggplot(df4, aes(invoice)) +
  geom_bar() 
p

# Lets Get Revenue!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
df1$Revenue <- df1$Quantity * df1$Price

# top 15 revenues per country
  top_revenues <- df1 %>%
  group_by(Country, StockCode) %>%
  summarise(Per_Product_Rev = sum(Revenue)) %>%
  top_n(Per_Product_Rev,n=15)%>%
  arrange(desc(Per_Product_Rev))
  

  # top 15 revenues per country
  top_revenues_USA <- df1 %>%
    filter(df1$Country == "USA")%>%
    group_by(StockCode) %>%
    summarise(Per_Product_Rev = sum(Revenue)) %>%
    top_n(Per_Product_Rev,n=15)%>%
    arrange(desc(Per_Product_Rev))
  
  # Which codes have top revenue
  top_revenues_codes<- df1 %>%
    group_by(StockCode) %>%
    summarise(Per_Product_Rev = sum(Revenue)) %>%
    top_n(Per_Product_Rev,n=3)%>%
    arrange(desc(Per_Product_Rev))

# top codes by revenue
topuniquecode <- unique(top_revenues_codes$StockCode)

# dataframe of only top 15 codes...................
top_codes_df<- df1 %>%
  filter(df1$StockCode %in% topuniquecode)
  group_by(Country,StockCode) %>%
  summarise(Per_Product_Rev = sum(Revenue))
  
  
#########################################################
# Which country purchases the most???
top_purch_country <- df1 %>%
  group_by(Country)%>%
  summarize(total_buys = n())%>%
  top_n(total_buys,n=3)%>%
  arrange(desc(total_buys))

# list of top unique countries
top_unique_purch <- unique(top_purch_country$Country)

# dataframe of only those top countries
top_countries <- df1[df1$Country %in% top_unique_purch,]

# This gives top countrys by number of purchases it works but slow
p <- ggplot(top_countries, aes(x=Country, y=invoice))+
    geom_bar(stat='identity', fill="forest green")+
    ylab("purchase by country")
p

################################################################
# Which codes have top revenue

#top_revenues_codes<- df1 %>%
 # group_by(StockCode) %>%
 # summarise(Per_Product_Rev = sum(Revenue)) %>%
 # top_n(Per_Product_Rev,n=3)%>%
 # arrange(desc(Per_Product_Rev))

# top codes by revenue
#topuniquecode <- unique(top_revenues_codes$StockCode)

# now only keep top 3 Stock Codes by revenue and top 3 countries by frequency of purchases
topcountrycode <- top_countries[top_countries$StockCode %in% topuniquecode,]


# Plot the data by country geom bar
# country on x axis invoice on y and wraped by stock code
# show country on x and number of invoices for each country on y fasced by each individual of the top 3 stock codes. 
 z <- ggplot(topcountrycode, aes(Country, invoice)) +
    geom_bar(stat = 'identity') +
    facet_wrap(~StockCode) 
z
##############
# show total revenue by country for each stock code fasceted by country 
# obviously UK is the most revenue brining country 
 p <- ggplot(topcountrycode, aes(StockCode, Revenue)) +
      geom_bar(stat = 'identity') + facet_wrap(~Country)
p
####################################################################
# TRELLISCOPE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
install.packages("trelliscopejs")
library(trelliscopejs)
  
  # Create the plot
  ggplot(df1, aes(InvoiceDate, Revenue)) +
    geom_line() +
    # Facet on country and continent
    facet_trelliscope(~ Country)
  