# Load required libraries
library(tm)
library(wordcloud)
library(RColorBrewer)
library(tidyverse)

# Set the path to your corpus directory
corpus_dir <- "corpus"

# Create a text corpus from the directory
corpus <- VCorpus(DirSource(corpus_dir), readerControl = list(language = "en"))

# Preprocess the text
corpus <- tm_map(corpus, content_transformer(tolower))           # Convert to lowercase
corpus <- tm_map(corpus, removePunctuation)                      # Remove punctuation
corpus <- tm_map(corpus, removeNumbers)                          # Remove numbers
corpus <- tm_map(corpus, removeWords, stopwords("en"))           # Remove English stopwords
corpus <- tm_map(corpus, stripWhitespace)                        # Remove extra whitespace

# Create a document-term matrix
dtm <- TermDocumentMatrix(corpus)
m <- as.matrix(dtm)
word_freqs <- sort(rowSums(m), decreasing = TRUE)
df <- data.frame(word = names(word_freqs), freq = word_freqs)

# Plot the wordcloud
set.seed(123) # for reproducibility
wordcloud(
  words = df$word,
  freq = df$freq,
  min.freq = 2,
  max.words = 200,
  random.order = FALSE,
  fixed.asp = TRUE,
  colors = brewer.pal(8, "Dark2")
)


library(wordcloud2)
letterCloud(df, word = "Bug", size = 2)

df2 <- df %>% slice_head(n = 300)


wordcloud2(df2, size = 1, shape = 'circle')
