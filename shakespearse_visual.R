

install.packages("textplot")
library(textplot)
install.packages("textdata")
library(textdata)
install.packages('corpus')
library(corpus)
install.packages("cran")
library(cran)
install.packages("tidytext")
library(tidytext)
install.packages("wordcloud")
library(wordcloud)
library(tidyverse)
library(tibble)
install.packages("readr")
library(readr)


#1. load shakespeare.rda into r environment

load("D:\\VIT\\SEM_6\\DataVisualization&Presentation\\lab\\Codes\\shakespeare.rda")
View(shakespeare)

#2. Pipe the shakespeare data frame to the next line
# Use count to find out how many titles/types there are
shakespeare %>% count(title)
print("20BCE0348")

#3. Load tidytext/ tidyverse
library(tidyr)
library(ggplot2)
library(readr)
library(textdata)
library(tibble)
library(dplyr)

linenumbers <- c(1:141067)
tidy_shakespeare<-cbind(tidy_shakespeare,linenumbers)
colnames(tidy_shakespeare)[3] <- "linenumber"

tidy_shakespeare2<-cbind(tidy_shakespeare2,linenumbers)
colnames(tidy_shakespeare2)[4] <- "linenumber"
#4. create and object tidy_shakespeare
# Group by the titles of the plays
# Define a new column linenumber
# Transform the non-tidy text data to tidy text data
library(dplyr)
library(tidyr)

# Group by title and define a new column line number
tidy_shakespeare <- shakespeare %>%
  group_by(title) %>% mutate(linenumber = row_number())

# Transform non-tidy text data to tidy text data
tidy_shakespeare <- tidy_shakespeare %>%
  unnest_tokens(word, text)
tidy_shakespeare

#5. Pipe the tidy Shakespeare data frame to the next line
# Use count to find out how many times each word is used
library(stringr)
text <- shakespeare["text"]
text <- text %>% mutate(words = str_split(text, "\\s+"))
text <- text %>% unnest(words)
word_counts <- text %>% count(words)
word_counts

library(dplyr)
tidy_shakespeare %>%
  count(word, sort = TRUE) %>% group_by(type) %>% ungroup()
print("20BCE0348")
tidy_shakespeare

#6. Sentiment analysis of tidy_shakespeare assin to object shakespeare_sentiment
# Implement sentiment analysis with the "bing" lexicon
?get_sentiments
library(tidytext)

# Get the bing lexicon
lexicon <- get_sentiments("bing")

# Perform sentiment analysis
shakespeare_sentiment <- tidy_shakespeare %>%
  inner_join(lexicon, by = "word") %>%
  group_by(title, linenumber) %>%
  ungroup()
shakespeare_sentiment
print("20BCE0348")

#7. shakespeare_sentiment
# Find how many positive/negative words each play has
library(dplyr)

shakespeare_sentiment %>%
  group_by(title) %>%
  summarize(num_positive = sum(sentiment > 0),
            num_negative = sum(sentiment < 0))


#8. Tragedy or comedy from tidy_shakespeare  assign to sentiment_counts
# Implement sentiment analysis using the "bing" lexicon
# Count the number of words by title, type, and sentiment
library(dplyr)

# Load the bing lexicon
lexicon <- get_sentiments("bing")

tidy_shakespeare2 <- shakespeare %>%
  group_by(title, type) %>% mutate(linenumber = row_number())

tidy_shakespeare2 <- tidy_shakespeare2 %>%
  unnest_tokens(word, text)

tidy_shakespeare2

shakespeare_sentiment2 <- tidy_shakespeare2 %>%
  inner_join(lexicon, by = "word") %>%
  group_by(title, linenumber) %>%
  ungroup()
shakespeare_sentiment2


# Join with the lexicon and group by title, type, and sentiment
sentiment_counts <- tidy_shakespeare2 %>%
  inner_join(lexicon, by = "word") %>%
  group_by(title, type, sentiment) %>%
  summarize(count = n()) %>%
  ungroup()

sentiment_counts
#9. from sentiment_counts
# Group by the titles of the plays
# Find the total number of words in each play
# Calculate the number of words divided by the total
# Filter the results for only negative sentiment then arrange percentage in asc order
library(dplyr)

# Group by title and calculate the total number of words in each play
total_words <- sentiment_counts %>%
  group_by(title) %>%
  summarize(total = sum(count))

# Calculate the percentage of negative words for each play
negative_words <- sentiment_counts %>%
  group_by(title) %>%
  filter(sentiment == "negative") %>%
  summarize(negative = sum(count))

percentages <- total_words %>%
  inner_join(negative_words, by = "title") %>%
  mutate(percentage = negative / total * 100) %>%
  filter(!is.na(percentage)) %>%
  arrange(percentage)

total_words
negative_words
percentages
#10 Most common positive and negative words and assign to word_could
# Implement sentiment analysis using the "bing" lexicon
# Count by word and sentiment
library(dplyr)

# Load the bing lexicon
lexicon <- get_sentiments("bing")

# Join with the lexicon
shakespeare_sentiment <- tidy_shakespeare %>%
  inner_join(lexicon, by = "word")

# Count by word and sentiment
word_count <- shakespeare_sentiment %>%
  count(word, sentiment, sort = TRUE)

# Filter for positive and negative words
word_cloud <- word_count %>%
  filter(sentiment %in% c("positive", "negative")) %>%
  slice_max(n = 20, order_by = n)

wordcloud(words = word_cloud$word, freq = word_cloud$n, min.freq = 1,
          max.words=120, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))

#11. extract top 10 words from word_counts and assing to top_words
# Group by sentiment
# Take the top 10 for each sentiment and ungroup it
# Make word a factor in order of n
library(dplyr)

top_words <- word_count %>%
  group_by(sentiment) %>%
  slice_max(n = 10, order_by = n) %>%
  ungroup() %>%
  mutate(word = factor(word, levels = unique(word[order(n, decreasing = TRUE)])))

top_words
#12 Use aes() to put words on the x-axis and n on the y-axis
# Make a bar chart with geom_col()
# facet_wrap for sentiments and apply scales  as free
#Move x to y and y to x
library(ggplot2)

ggplot(top_words, aes(x = n, y = word, fill = sentiment)) +
  geom_col() +
  facet_wrap(~sentiment, scales = "free") +
  scale_fill_manual(values = c("positive" = "darkgreen", "negative" = "red")) +
  labs(title = "Top 10 Positive and Negative Words in Shakespeare's Plays",
       x = "Frequency", y = NULL) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        axis.text.y = element_text(size = 10, margin = margin(r = 5)),
        axis.text.x = element_text(size = 10, margin = margin(t = 5)),
        axis.title.y = element_text(size = 12, margin = margin(r = 10)),
        axis.title.x = element_text(size = 12, margin = margin(t = 10)))



#13 from tidy_shakespeare Calculating a contribution score
# Count by title and word
# Implement sentiment analysis using the "afinn" lexicon
# Group by title
# Calculate a contribution for each word in each title
library(dplyr)

# Load the AFINN lexicon
lexicon <- get_sentiments("afinn")

# Count by title and word
word_counts <- tidy_shakespeare %>% unnest_tokens(word, text) %>% inner_join(lexicon, by = "word") %>% 
  group_by(title, word) %>% summarise(n = n(), score = sum(value)) %>% ungroup()

# Group by title
title_counts <- word_counts %>%
  group_by(title) %>%
  summarise(total_words = sum(n), total_score = sum(score))

# Calculate a contribution for each word in each title
contribution_scores <- word_counts %>%
  left_join(title_counts, by = "title") %>%
  mutate(contribution = score / total_score * 100)
contribution_scores
