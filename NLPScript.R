# Natural language processing
# Regular expressions!!!!!!!!!!!!!!!!!!!!

text <-c("John's favorite color two colors are blue and red.","John's favorite number is 1111.","John lives at P Sherman, 43 Wallaby Way, Sydney",
         "He is 7 feet tall","John has visted 30 countries","John only has 9 fingers","John has worked eleven different jobs","He can speak 3 languages","John can name 10 facts about himself.")

# Print off each item that contained a numeric number
# d is for digit
grep(pattern = "\\d", x = text, value = TRUE)

# Find all items with a number followed by a space
# d is for digit and s is for space
grep(pattern = "\\d\\s", x = text)

# How many times did you write down 'favorite'?
length(grep(pattern = "favorite", x = text))

# Print off the text for every time you used your boss's name, John
grep('John', x = text, value = TRUE)

# Try replacing all occurences of "John" with He
gsub(pattern = 'John', replacement = 'He', x = text)

# Replace all occurences of "John " with 'He '.
clean_text <- gsub(pattern = 'John\\s', replacement = 'He ', x = text)
clean_text

# Replace all occurences of "John's" with 'His'
gsub(pattern = "John\\'s", replacement = 'His', x = clean_text)

# Tokenization
# Or text preprocessing 
# can be characters words sentences documents or regular expression
# using tidytext package
library(tidytext)
library(dplyr)

df <- read.csv("animal_farm.csv")
df <- as.data.frame(df)

# Split the text_column into sentences
df %>%
  unnest_tokens(output = "sentences", input = text_column, token = "sentences")

# Split the text_column into sentences
df %>%
  unnest_tokens(output = "sentences", input = text_column, token = "sentences") %>%
  # Count sentences using the "chapter" column
  count(chapter)

# Split the text_column using regular expressions
# split the text into sentences whenever period is found
df %>%
  unnest_tokens(output = "sentences", input = text_column,
                token = "regex", pattern = "\\.") %>%
  count(chapter)

#Text cleaning basics
# preprocessing removing stop words
# Tokenize animal farm's text_column column
df$chapter <- as.character(df$chapter)
df$text_column <- as.character(df$text_column)

tidy_df <- df %>%
  unnest_tokens(word, text_column)

# Print the word frequencies - most frequent first!
tidy_df %>%
  count(word, sort = TRUE)

# Remove stop words, using `stop_words` from tidytext
tidy_df %>%
  anti_join(stop_words)

#The root of words are often more important than their endings, especially when it comes to text analysis. 
# Perform stemming on tidy_animal_farm
library(SnowballC)
stemmed_df <- tidy_df %>%
  mutate(word = wordStem(word))

# Print the old word frequencies 
tidy_df %>%
  count(word, sort = TRUE)

# Print the new word frequencies
stemmed_df %>%
  count(word, sort = TRUE)

# CORPUS (A collection of text)
# corpora collection of documents containing natural language text (from tm package as corpus)
# vcorpus text and meta data text 
library(tm)
data("acq")
library(tidytext) # how to unlist list
data("crude")
library(dplyr)

# how to accuess a list
acq[[1]]$meta$places

# how to unlist a list of lists!!!!!!!!!!!!
tidy_data <- tidy(acq)
tidy_data

#create corpus
corpus <- VCorpus(VectorSource(tidy_data$text))

# add meta data
meta(corpus ,'Author') <- tidy_data$author
meta(corpus , 'oldid') <- tidy_data$oldid
head(meta(corpus))

# Print out the corpus
print(crude)

# Print the content of the 10th article
crude[[10]]$content

# Find the first ID
crude[[1]]$meta$id

# Make a vector of IDs
ids <- c()
for(i in c(1:20)){
  ids <- append(ids, crude[[i]]$meta$id)
}
ids

# Create a tibble & Review
# basically creating a dataframe from list of lists
crude_tibble <- tidy(crude)
names(crude_tibble)

crude_counts <- crude_tibble %>%
  # Tokenize by word
  unnest_tokens(word, text) %>%
  # Count by word
  count(word, sort = TRUE) %>%
  # Remove stop words
  anti_join(stop_words)

# Creating a corpus from russian tweets
russian_tweets <- read.csv("russian_1.csv")

# Create a corpus
# taking the text column of the data and making a list of lists basically
# this only contains the content but has the holding places of other columns
tweet_corpus <- VCorpus(VectorSource(russian_tweets$content))

# Attach following and followers by filling the holding places
meta(tweet_corpus, 'following') <- russian_tweets$following
meta(tweet_corpus, 'followers') <- russian_tweets$followers

# Review the meta data
head(meta(tweet_corpus))

# BAG OF WORDS REPRESENTATION!!!!!!!!!!!!!!!!!!!
# Count occurrence by article_id and word
words <- crude_tibble %>%
  unnest_tokens(output = "word", token = "words", input = text) %>%
  anti_join(stop_words) %>%
  count(id, word, sort=TRUE)

# How many different word/article combinations are there?
unique_combinations <- nrow(words)

# Filter to responses with the word "prices"
words_with_prices <- words %>%
  filter(word == 'prices')

# How many articles had the word "prices"?
number_of_price_articles <- nrow(words_with_prices)

# SPARSE MATRICES WITH RUSSIAN TWEETS
russian_tweets$content <- as.character(russian_tweets$content)

# Tokenize and remove stop words in vector
tidy_tweets <- russian_tweets %>%
  unnest_tokens(word, content) %>%
  anti_join(stop_words)

# Count by words 
unique_words <- tidy_tweets %>%
  count(word)

# Count by tweet (tweet_id) and word
unique_words_by_tweet <- tidy_tweets %>%
  count(tweet_id, word)

# Find the size of matrix
size <- nrow(russian_tweets) * nrow(unique_words)

# Find percent of entries that would have a value
percent <- nrow(unique_words_by_tweet) / size
percent

#TFIDF !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# term-frequency inverse document frequency!!!!!!
# idf = log (total number of documents/ number of documents were term appears)
# calculating TFIDF matrix
# Create a tibble with TFIDF values
crude_weights <- crude_tibble %>%
  unnest_tokens(output = "word", token = "words", input = text) %>%
  anti_join(stop_words) %>%
  count(id, word) %>%
  bind_tf_idf(word, id, n)

# Find the highest TFIDF values
crude_weights %>%
  arrange(desc(tf_idf))

# Find the lowest non-zero TFIDF values
crude_weights %>%
  filter(tf_idf != 0) %>%
  arrange(tf_idf)

# COSINE SIMULARITY 
# Because the top weights in crude weights was january with tf_idf of .21 it apeard in 4 articles several times
# we can use these values to asses how similar two articles are
# measure of similarty by measruing by finding angle formed by two vectors(dot prodcut) 
# Lets go back to animal_farm
install.packages("widyr") # calculate cosign similarity 
library(widyr)

# Create word counts
animal_farm_counts <- df %>%
  unnest_tokens(word, text_column) %>%
  count(chapter, word)

# Calculate the cosine similarity by chapter, using words
comparisons <- animal_farm_counts %>%
  pairwise_similarity(chapter, word, n) %>%
  arrange(desc(similarity))

# Print the mean of the similarity values
comparisons %>%
  summarize(mean = mean(similarity))

# cosine similarity example 
# Create word counts 
animal_farm_counts <- df %>%
  unnest_tokens(word, text_column) %>%
  anti_join(stop_words) %>%
  count(chapter, word)

# Create word counts 
animal_farm_counts <- df %>%
  unnest_tokens(word, text_column) %>%
  anti_join(stop_words) %>%
  count(chapter, word) %>%
  bind_tf_idf(chapter, word, n)

# Calculate cosine similarity on word counts
animal_farm_counts %>%
  pairwise_similarity(chapter, word, n) %>%
  arrange(desc(similarity))

# Calculate cosine similarity using tf_idf values
animal_farm_counts %>%
  pairwise_similarity(chapter, word, tf_idf) %>%
  arrange(desc(similarity))

# preparing text for modeling!!!!!!!!!!!!!!!!!!!!!!!!!
# using classification models
#During the 2016 US election, Russian tweet bots were used to constantly distribute political rhetoric to both democrats and republicans. You have been given a dataset of such tweets called russian_tweets

# Stem the tokens
russian_tokens <- russian_tweets %>%
  unnest_tokens(output = "word", token = "words", input = content) %>%
  anti_join(stop_words) %>%
  mutate(word = wordStem(word))

# Create a document term matrix 
tweet_matrix <- russian_tokens %>%
  count(tweet_id, word) %>%
  cast_dtm(document = tweet_id, term = word,
           value = n, weighting = tm::weightTfIdf)

# Print the matrix details 
tweet_matrix

# removing sparse terms because we dont have access to GPU or cloud compute resources
less_sparse_matrix <-  removeSparseTerms(tweet_matrix, sparse = .50)

# Print results
tweet_matrix
less_sparse_matrix

# change the value to .90
less_sparse_matrix2 <-
  removeSparseTerms(tweet_matrix, sparse = .90)

# Print results
matrix
less_sparse_matrix2

# sparse now .99
less_sparse_matrix3 <-
  removeSparseTerms(tweet_matrix, sparse =.99)

# Print results
matrix
less_sparse_matrix3

# final results
less_sparse_matrix4 <-
  removeSparseTerms(tweet_matrix, sparse =.9999)

# Print results
matrix
less_sparse_matrix4

# CLASSIFICATION MODELINNG
library(randomForest)

# Create train/test split
set.seed(1111)
sample_size <- floor(0.75 * nrow(less_sparse_matrix))
train_ind <- sample(nrow(less_sparse_matrix), size = sample_size)
train <- less_sparse_matrix[train_ind, ]
test <- less_sparse_matrix[-train_ind, ]

# Create a random forest classifier
rfc <- randomForest(x = as.data.frame(as.matrix(train)), 
                    y = less_sparse_matrix4[train_ind],
                    nTree = 50)
# Print the results
rfc

# Confusion matricies 
# Percentage correctly labeled "Left"
left <- (350) / (350 + 157)
left

# Percentage correctly labeled "Right"
right <- (436) / (436 + 57)
right

# Percentage correctly labeled:
accuracy <- (350 + 436) / (350 + 157 + 57 + 436)

# TOPIC MODELIMG !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#preparing for LDA
animal_farm_tokens <- df %>%
  unnest_tokens(output= "word",token = "words", input = text_column) %>%
  anti_join(stop_words) %>%
  mutate(word = wordStem(word))

# document -term matrix
animal_farm_matrix <- animal_farm_tokens %>%
  count(chapter,word) %>%
  cast_dtm(document = chapter,term = word,
        value = n, weighting = tm::weightTf)

install.packages("topicmodels")
library(topicmodels)

# Create train/test split
set.seed(1111)
sample_size <- floor(0.75 * nrow(animal_farm_matrix))
train_ind <- sample(nrow(animal_farm_matrix), size = sample_size)
train <- animal_farm_matrix[train_ind, ]
test <- animal_farm_matrix[-train_ind, ]

animal_farm_lda <- LDA(train, k = 4, method = 'Gibbs',
                       control = list(seed = 1111))
animal_farm_lda

animal_farm_betas <- tidy(animal_farm_lda,matrix = "beta")
animal_farm_betas

sum(animal_farm_betas$beta)

# top words per topic
animal_farm_betas %>%
  group_by(topic) %>%
  top_n(10,beta)%>%
  arrange(topic,-beta)%>%
  filter(topic == 1)

animal_farm_betas %>%
  group_by(topic) %>%
  top_n(10,beta) %>%
  arrange(topic, -beta) %>%
  filter(topic ==2)

# labeling documents as topics
# chapter 1 is mostly comprised of topic 2
animal_farm_chapters <- tidy(animal_farm_lda,matrix = "gamma")
animal_farm_chapters %>%
  filter(document == 'Chapter 1')

# LDA in PRACTICE!!!!!!!!!!!!!!!!!!!
# PERPLEXITY
# Peform topic modeling 
lda_model <- LDA(train, k = 5, method = "Gibbs",
                 control = list(seed = 1111))
# Train
perplexity(lda_model, newdata = train) 
# Test
perplexity(lda_model, newdata = test) 

# Peform topic modeling with k as 15
lda_model <- LDA(train, k = 15, method = "Gibbs",
                 control = list(seed = 1111))
# Train
perplexity(lda_model, newdata = train) 
# Test
perplexity(lda_model, newdata = test) 

# Peform topic modeling k as 50
lda_model <- LDA(train, k = 50, method = "Gibbs",
                 control = list(seed = 1111))
# Train
perplexity(lda_model, newdata = train) 
# Test
perplexity(lda_model, newdata = test) 


# Extract the gamma matrix 
gamma_values <- tidy(lda_model, matrix = "gamma")
# Create grouped gamma tibble
grouped_gammas <- gamma_values %>%
  group_by(document) %>%
  arrange(desc(gamma)) %>%
  slice(1) %>%
  group_by(topic)
# Count (tally) by topic
grouped_gammas %>% 
  tally(topic, sort=TRUE)
# Average topic weight for top topic for each sentence
grouped_gammas %>% 
  summarize(avg=mean(gamma)) %>%
  arrange(desc(avg))



# SENTIMENT ANALYSIS 
# prepare data
animal_farm <- read.csv("animal_farm.csv", stringsAsFactors = FALSE)
animal_farm <- as_tibble(animal_farm)

# tokenize and remove stop words
animal_farm_tokens <- animal_farm %>%
  unnest_tokens(output = "word", token = "words", input = text_column) %>%
  anti_join(stop_words)

# afinn lexicon 
animal_farm_tokens <- animal_farm_tokens %>%
  inner_join(get_sentiments("afinn"))

animal_farm_tokens %>%
  inner_join(get_sentiments("afinn")) %>%
  group_by(chapter) %>%
  summarise(sentiment = sum(value)) %>% # instead of value use score
  arrange(sentiment)

# bing lexicon 
get_sentiments("bing") # built in list of sentiments
get_sentiments("nrc") # instead of numbers meaning!
get_sentiments("afinn") # -5 to 5

word_totals <- animal_farm_tokens %>%
  group_by(chapter) %>%
  count()

# portion of negative words used
animal_farm_tokens %>%
  inner_join(get_sentiments("bing")) %>%
  group_by(chapter) %>%
  count(sentiment) %>%
  filter(sentiment == 'negative') %>%
  transform(p = n / word_totals$n) %>%
  arrange(desc(p))

as.data.frame(table(get_sentiments("nrc")$sentiment)) %>%
  arrange(desc(Freq))

fear <- get_sentiments("nrc") %>%
  filter(sentiment == "fear")

animal_farm_tokens %>%
  inner_join(fear) %>%
  count(word,sort = TRUE)


# Print the overall sentiment associated with each pig's sentences
for(name in c("napoleon", "snowball", "squealer")) {
  # Filter to the sentences mentioning the pig
  pig_sentences <- sentences[grepl(name, sentences$sentence), ]
  # Tokenize the text
  napoleon_tokens <- pig_sentences %>%
    unnest_tokens(output = "word", token = "words", input = sentence) %>%
    anti_join(stop_words)
  # Use afinn to find the overall sentiment score
  result <- napoleon_tokens %>% 
    inner_join(get_sentiments("afinn")) %>%
    summarise(sentiment = sum(score))
  # Print the result
  print(paste0(name, ": ", result$sentiment))
}

eft_tokens <- left %>%
  unnest_tokens(output = "word", token = "words", input = content) %>%
  anti_join(stop_words)
# Dictionaries 
anticipation <- get_sentiments("nrc") %>% 
  filter(sentiment == "anticipation")
joy <- get_sentiments("nrc") %>% 
  filter(sentiment == "joy")
# Print top words for Anticipation and Joy
left_tokens %>%
  inner_join(anticipation, by = "word") %>%
  count(word, sort = TRUE)
left_tokens %>%
  inner_join(joy, by = "word") %>%
  count(word, sort = TRUE)