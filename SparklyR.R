# install dpylr
install.packages("dplyr")
library(dplyr)

# Load sparklyr
install.packages("sparklyr")
library(sparklyr)

spark_install() # not sure i need to do this every time

Sys.setenv(JAVA_HOME = "C:/Program Files/Java/jdk1.8.0_231")


# Connect to your Spark cluster
spark_conn <- spark_connect(master ="local")

# Print the version of Spark
spark_version(spark_conn)

# Disconnect from Spark
spark_disconnect(spark_conn) # dont do this yet

# Download a dataset or zip file
df <- spark_read_csv(spark_conn, name = "Advertising1",path = "C:/Users/blake/OneDrive/GITHUBJUPITERPROJECT/Advertising1.csv",header = TRUE, delimiter = ",",infer_schema = TRUE)

# structure of df
str(df)

# Copy df to Spark
df_tbl <- copy_to(spark_conn, df)

# List the data frames available in Spark
src_tbls(spark_conn)

# Link to the df table in Spark
df_tbl_1 <- tbl(spark_conn, "df")

# See how big the dataset is
dim(df_tbl_1)

# See how small the tibble is
object_size(df_tbl_1)

# Print 5 rows, all columns
print(df_tbl, n = 5, width = Inf)

# Examine structure of tibble
str(df_tbl)

# Examine structure of data
glimpse(df_tbl)

# Manipulating data!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# df_tbl has been pre-defined
df_tbl

# Manipulate the track metadata
df2 <- df_tbl %>%
  # Select columns
  select(TV, Radio, Newspaper,Sales)

# Try to select columns using [ ]
tryCatch({
  # Selection code here
  df_tbl[, c("TV", "Radio", "Newspaper", "Sales","HighLow")]
},
error = print
)

# FILTERING THE DATA
df3 <- df_tbl %>%
  # Select columns
  select(TV, Radio, Newspaper,Sales) %>%
  # Filter rows
  filter(TV > 15)

# Arranging rows!
# Manipulate the track metadata
df4 <- df_tbl %>%
  select(TV, Radio, Newspaper,Sales) %>%
  filter(TV > 15, Newspaper > 5) %>%
  arrange(desc(Sales), TV, Radio)

# MUTATING COLUMNS
# Manipulate the track metadata
df5 <- df_tbl %>%
  # Select columns
  select(TV, Radio, Newspaper,Sales) %>%
  # Mutate columns
  mutate(TV_AMPLIFIED = TV*2)

# SUMMARIZE COLUMNS
df6 <- df_tbl %>%
  # Select columns
  select(TV, Radio, Newspaper,Sales) %>%
  # Mutate columns
  mutate(tv_campaign = TV*2) %>%
  # Summarize columns
  summarize(tv_campaign_mean = mean(tv_campaign))

# MORE ADVANCED CONCEPTS ( MATCHING COLUMN NAMES)
df7 <- df_tbl %>%
  # Select columns starting with S
  select(starts_with("S"))

df8 <- df_tbl %>%
  # Select columns ending with id
  select(ends_with("paper"))

df9 <- df_tbl %>%
  # Select columns containing a
  select(contains("a"))

library(stringr)
# a: A letter means "match that letter".
# .: A dot means "match any character, including letters, numbers, punctuation, etc.".
# ?: A question mark means "the previous character is optional".
df10 <-df_tbl %>%
  # Select columns matching ti.?t
  select(matches("ti.?t"))

df11 <- df_tbl %>%
  # Only return rows with distinct Sales
  distinct(Sales) # this is like unique in R

# Count and Top n values
df12 <- df_tbl %>%
  # Count the artist_name values
  count(Sales, sort = TRUE) %>% # better for data with factors 
  # Restrict to top 20
  top_n(20)

# COLLECTING DATA BACK FROM SPARK!!!!!!!!!!!!!!!!!!!
results <- df_tbl %>%
  # Filter where sales is greater than 5
  filter(Sales > 5)

# Examine the class of the results
class(results)

# Collect your results
collected <- results %>%
  collect()

# Examine the class of the collected results
class(collected)

# STORING INTERMEDIATE RESULTS
computed <- df_tbl %>%
  # Filter where sales is greater than 7
  filter(Sales > 7) %>%
  # Compute the results
  compute("HighSales")

# See the available datasets
src_tbls(spark_conn)

# Examine the class of the computed results
class(computed)

# GROUPS !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
group_df <- df_tbl %>%
  # Group by artist
  group_by(Sales) %>%
  # Calc mean duration
  summarize(mean_TV = mean(TV)) # better with factor data

df13 <- group_df %>%
  # Sort by ascending mean duration
  arrange(mean_TV)

df14 <- group_df %>%
  # Sort by descending mean duration
  arrange(desc(mean_TV))

df15 <- df_tbl %>%
  # Group by sales
  group_by(Sales) %>%
  # Calc
  mutate(tv_news_diff = TV - min(Newspaper)) %>%
  # Arrange by descending 
  arrange(desc(tv_news_diff))

# USING SQL!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
library(DBI)
# Write SQL query
query <- "SELECT * FROM df WHERE Sales > 7 AND TV >1"

# Run the query
(results <- dbGetQuery(spark_conn, query))

# JOINS 
# didnt want to look for datasets so just imporvised doesnt work but its the idea
df1_tbl
df2_tbl

# Left join 
joined <- left_join(df1_tbl, df2_tbl, by = "id")

# How many rows and columns are in the joined table?
dim(joined)

# Anti Join
anti_joined <- anti_join(df1_tbl, df2_tbl, by = "id")

#semi join
semi_joined <- semi_join(df1_tbl, df2_tbl, by = "id")

# Two other interfaces!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#Feature transformation!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
df2 <- spark_read_csv(spark_conn, name = "churn",path = "C:/Users/blake/OneDrive/GITHUBJUPITERPROJECT/churn.csv",header = TRUE, delimiter = ",",infer_schema = TRUE)

# Copy df to Spark
df2_tbl <- copy_to(spark_conn, df2)

# List the data frames available in Spark
src_tbls(spark_conn)

# Link to the df table in Spark
df_tbl_2 <- tbl(spark_conn, "df2")

# BINARIZE ALLOWS YOU TO MAKE COLUMN 0 OR 1
helpyes <- df2_tbl %>%
  # Select customer_service_calls
  select(customer_service_calls) %>%
  # Binarize to customer_service_calls
  ft_binarizer("customer_service_calls", "help_yes", threshold = 0.5) %>%
  # Collect the result
  collect() %>%
  # Convert "help_yes to logical
  mutate(help_yes = as.logical("help_yes"))

# Draw a barplot of helpyes
ggplot(helpyes, aes(help_yes)) +
  geom_bar()

# CONTINUOUSE TO CATEGORICAL
hotttnesss_over_time <- track_metadata_tbl %>%
  # Select artist_hotttnesss and year
  select(artist_hotttnesss, year) %>%
  # Convert year to numeric
  mutate(year = as.numeric(year)) %>%
  # Bucketize year to decade using decades vector
  ft_bucketizer("year", "decade", splits = decades) %>%
  # Collect the result
  collect() %>%
  # Convert decade to factor using decade_labels
  mutate(decade = factor(decade, labels = decade_labels))

# Draw a boxplot of artist_hotttnesss by decade
ggplot(hotttnesss_over_time, aes(decade, artist_hotttnesss)) +
  geom_boxplot()

familiarity_by_duration <- track_metadata_tbl %>%
  # Select duration and artist_familiarity
  select(duration, artist_familiarity) %>%
  # Bucketize duration
  ft_quantile_discretizer("duration", "duration_bin", n.buckets = 5) %>%
  # Collect the result
  collect() %>%
  # Convert duration bin to factor
  mutate(duration_bin = factor(duration_bin, labels = duration_labels))

# Draw a boxplot of artist_familiarity by duration_bin
ggplot(familiarity_by_duration, aes(duration_bin, artist_familiarity)) +
  geom_boxplot()

# WORD TOKENIZATION!!!!!!!!!!!!!!!!!!!!!!!
title_text <- track_metadata_tbl %>%
  # Select artist_name, title
  select(artist_name, title) %>%
  # Tokenize title to words
  ft_tokenizer("title", "word") %>%
  # Collect the result
  collect() %>%
  # Flatten the word column 
  mutate(word = lapply(word, as.character)) %>% 
  # Unnest the list column
  unnest(word)

sentimental_artists <- title_text_tbl %>%
  # Inner join with sentiments on word field
  inner_join(afinn_sentiments_tbl, by = "word") %>%
  # Group by artist
  group_by(artist_name) %>%
  # Summarize to get positivity
  summarize(positivity = sum(score))

sentimental_artists %>%
  # Arrange by ascending positivity
  arrange(positivity) %>%
  # Get top 5
  top_n(5)

sentimental_artists %>%
  # Arrange by descending positivity
  arrange(desc(positivity)) %>%
  # Get top 5
  top_n(5)

track_metadata_tbl %>%
  # Select artist_mbid column
  select(artist_mbid) %>%
  # Split it by hyphens
  ft_regex_tokenizer("artist_mbid", "artist_mbid_chunks", pattern = "-")


# SORT VS ARRANGING
# Compare timings of arrange() and sdf_sort()
microbenchmark(
  arranged = track_metadata_tbl %>%
    # Arrange by year, then artist_name, then release, then title
    arrange(year, artist_name, release, title) %>%
    # Collect the result
    collect(),
  sorted = track_metadata_tbl %>%
    # Sort by year, then artist_name, then release, then title
    sdf_sort(c("year", "artist_name", "release", "title")) %>%
    # Collect the result
    collect(),
  times = 5
)

# EXPLORING SPARK DATA TYPES!!!!!!!!!!!!!!!!!!!!
# Get the schema
(schema <- sdf_schema(df2_tbl))

# Transform the schema
schema %>%
  lapply(data.frame, stringsAsFactors = FALSE) %>%
  bind_rows()


# SHRINK THE DATA BY SAMPLING!!!!!!!!!!!!!!!!!!!!!!!
df2_tbl %>%
  # Sample the data without replacement
  sdf_sample(0.01, replacement = FALSE, seed = 20000229) %>%
  # Compute the result
  compute("sample_df2")


# TESTING AND PARTITIONING

partition <- df2_tbl %>%
  select(churn,customer_service_calls,total_intl_charge,total_intl_calls,account_length)

partitioned <- partition %>%
  # Partition into training and testing sets
  sdf_random_split(training = 0.7, testing = 0.3)

# Get the dimensions of the training set
dim(partitioned$training)

# Get the dimensions of the testing set
dim(partitioned$testing)

# MACHINE LEARNING ON SPARK
train <- partitioned$training
test <- partitioned$testing

feature_colnames <- df2_tbl %>%
  # Get the column names
  colnames() %>%
  # Limit to the churn columns
  str_subset(fixed("churn"))

gradient_boosted_trees_model <- df2_tbl %>%
  # Run the gradient boosted trees model
  ml_gradient_boosted_trees("churn", feature_colnames)

responses <- track_data_to_predict_tbl %>%
  # Select the response column
  select(year) %>%
  # Collect the results
  collect() %>%
  # Add in the predictions
  mutate(
    predicted_year = predict(
      gradient_boosted_trees_model,
      track_data_to_predict_tbl
    )
  )

# Draw a scatterplot of predicted vs. actual
ggplot(responses, aes(actual, predicted)) +
  # Add the points
  geom_point(alpha = 0.1) +
  # Add a line at actual = predicted
  geom_abline(intercept = 0, slope = 1)

residuals <- responses %>%
  # Transmute response data to residuals
  transmute(residual = predicted - actual)

# Draw a density plot of residuals
ggplot(residuals, aes(residual)) +
  # Add a density curve
  geom_density() +
  # Add a vertical line through zero
  geom_vline(xintercept = 0)

# track_data_to_model_tbl has been pre-defined
track_data_to_model_tbl

feature_colnames <- track_data_to_model_tbl %>%
  # Get the column names
  colnames() %>%
  # Limit to the timbre columns
  str_subset(fixed("timbre"))

random_forest_model <- track_data_to_model_tbl %>%
  # Run the gradient boosted trees model
  ml_random_forest("year", feature_colnames)

# Create a response vs. actual dataset
responses <- track_data_to_predict_tbl %>%
  # Select the response column
  select(year) %>%
  # Collect the results
  collect() %>%
  # Add in the predictions
  mutate(
    predicted_year = predict(
      random_forest_model,
      track_data_to_predict_tbl
    )
  )

# Draw a scatterplot of predicted vs. actual
ggplot(both_responses, aes(actual, predicted, color = model)) +
  # Add a smoothed line
  geom_smooth() +
  # Add a line at actual = predicted
  geom_abline(intercept = 0, slope = 1)

# Create a tibble of residuals
residuals <- both_responses %>%
  mutate(residual = predicted - actual)

# Draw a density plot of residuals
ggplot(residuals, aes(residual, color = model)) +
  # Add a density curve
  geom_density() +
  # Add a vertical line through zero
  geom_vline(xintercept = 0)

both_responses %>%
  # Add a residual column
  mutate(
    residual = predicted - actual
  ) %>%
  # Group by model
  group_by(model) %>%
  # Calculate the root mean square error
  summarize(
    rmse = sqrt(mean(residual ^ 2))
  )