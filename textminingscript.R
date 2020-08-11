# Text mining!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!1
# text mining package
library(tm)

# Import text data from CSV, no factors
tweets <- read.csv("text_data1.csv", stringsAsFactors = FALSE)

# View the structure of tweets
str(tweets)

# Isolate text from tweets
coffee_tweets <- tweets$text

# Make a vector source from coffee_tweets list of 1000 values
coffee_source <- VectorSource(coffee_tweets)

# make a nested list VCorpus!
# Make a volatile corpus from coffee_corpus
coffee_corpus <- VCorpus(coffee_source)

# Print out coffee_corpus
coffee_corpus

# Print the 15th tweet in coffee_corpus
coffee_corpus[[15]]

# Print the contents of the 15th tweet in coffee_corpus
coffee_corpus[[15]][1]

# Now use content to review plain text of the 10th tweet
content(coffee_corpus[[10]])

# Create a DataframeSource from the example text
df_source <- DataframeSource(examples)

# Convert df_source to a volatile corpus
df_corpus <- VCorpus(df_source)

# Examine df_corpus
df_corpus

# Examine df_corpus metadata
meta(df_corpus)

# Compare the number of documents in the vector source
vec_corpus

# Compare metadata in the vector corpus
meta(vec_corpus)