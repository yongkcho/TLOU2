#### Last of Us Review Analysis ####

# set working dir
wkdir <- "/Users/wang/TLOU2"
setwd(wkdir)

list.files(wkdir)

# import library
library(dplyr)
library(stringi)
library(stringr)
library(ggthemes)
library(ggplot2)
library(igraph)
#library(qgraph)

library(tm)
library(tidytext)
library(tidyverse)
library(topicmodels)
library(lda)
library(Rmpfr)
library(LDAvis)
library(wordcloud)

# import dataset
user_data <- read.csv("all_user_data.csv")
review_data <- read.csv("TLOU_review_200624_processed.csv", encoding ="utf-8")

# define functions
'%!in%' <- function(x, y)!('%in%'(x, y))

#### analysis on user_data

# Q1. how many people just btw 0-1 reivew?
fake_user <- user_data[user_data$rating_num <= 1 & user_data$review_num <= 1, ] 
nrow(fake_user) # 8906 out of 12365 user just left '1' or less reivew(if deleted)

fake_review <- review_data[review_data$user %in% fake_user$user,]
summary(fake_review$score)
h1 <- hist(fake_review$score, breaks = 10) 
table(fake_review$score) # 4210 '0' and 1920 '10'


# let's compare this nums with others
real_user <- user_data[user_data$user %!in% fake_user$user,]
real_review <- review_data[review_data$user %in% real_user$user,]
summary(real_review$score)
hist(real_review$score)
h2 <- hist(real_review$score, breaks = 10)
table(real_review$score)
8
# overlap 2 histograms in one plot to compare
plot(h1, col = rgb(0, 1, 1, 1/4), xlim = c(0,10))
plot(h2, col = rgb(1, 0, 0, 1/4), xlim = c(0,10), add = T)

# since there's diff btw numbers it is very hard to compare
h1$counts <- h1$counts / sum(h1$counts)
h2$counts <- h2$counts / sum(h2$counts)

# let's plot agian
plot(h1, col = rgb(0, 1, 1, 1/4), xlim = c(0,10))
plot(h2, col = rgb(1, 0, 0, 1/4), xlim = c(0,10), add = T)

# how about time series comparison?
all_avg_score <- aggregate(score ~ created_date, data = review_data, mean)
fake_avg_score <- aggregate(score ~ created_date, data = fake_review, mean)
real_avg_score <- aggregate(score ~ created_date, data = real_review, mean)

all_avg_score$type = "all"
fake_avg_score$type = "fake"
real_avg_score$type = "real"

avg_score_plot <- rbind(all_avg_score, fake_avg_score, real_avg_score)
ggplot(avg_score_plot, aes(x = created_date, y = score, group = type)) +
  geom_line(aes(color = type)) +
  geom_point(aes(color = type)) +
  theme_classic()

# Q2. what is the POS and NEG aspect of the game?
h <- hist(review_data$score, breaks = 10) 
h
summary(review_data$score)

# had to delete all non utf8 
review_data$review <- as.character(review_data$review)
review_data$review <- stri_encode(review_data$review, "", "utf-8")

neg_review <- review_data[review_data$score < 3.727,] #lower than mean
pos_review <- review_data[review_data$score >= 3.727,] #higher than mean

# wordcloud with pos_review
pos_corpus <- Corpus(VectorSource(pos_review$review))
pos_corpus <- tm_map(pos_corpus, removePunctuation)
pos_corpus <- tm_map(pos_corpus, removeNumbers)
pos_corpus <- tm_map(pos_corpus, tolower)
pos_corpus <- tm_map(pos_corpus, removeWords, stopwords("english"))
pos_corpus <- tm_map(pos_corpus, stemCompletion, dictionary = DocsCopy, lazy=TRUE)
pos_corpus <- tm_map(pos_corpus, stripWhitespace)

pos_tdm <- TermDocumentMatrix(pos_corpus, control=list(wordLengths=c(4,Inf)))
pos_tdm <- as.data.frame(as.matrix(pos_tdm))

pos_words <- sort(rowSums(pos_tdm), decreasing=TRUE)  
pos_words <- data.frame(word = names(pos_words), freq = pos_words)
rownames(pos_words) <- c()
to_remove <- c(21, 28, 36, 42, 44)
pos_words <- pos_words[-to_remove,]
wordcloud(pos_words$word[1:50], pos_words$freq[1:50], random.order = F, 
          scale = c(2.5, 0.5), rot.per = 0, colors = "darkblue")

# wordcloud with neg_review
neg_corpus <- Corpus(VectorSource(neg_review$review))
neg_corpus <- tm_map(neg_corpus, removePunctuation)
neg_corpus <- tm_map(neg_corpus, removeNumbers)
neg_corpus <- tm_map(neg_corpus, tolower)
neg_corpus <- tm_map(neg_corpus, removeWords, stopwords("english"))
neg_corpus <- tm_map(neg_corpus, stemCompletion, dictionary = DocsCopy, lazy=TRUE)
neg_corpus <- tm_map(neg_corpus, stripWhitespace)

neg_tdm <- TermDocumentMatrix(neg_corpus, control=list(wordLengths=c(4,Inf)))
neg_tdm <- as.data.frame(as.matrix(neg_tdm))

neg_words <- sort(rowSums(neg_tdm), decreasing=TRUE)  
neg_words <- data.frame(word = names(neg_words), freq = neg_words)
rownames(neg_words) <- c()
to_remove <- c(23, 19, 28, 33, 35, 49)
neg_words <- neg_words[-to_remove,]
wordcloud(neg_words$word[1:50], neg_words$freq[1:50], random.order = F, 
          scale = c(2.5, 0.5), rot.per = 0, colors = "darkred")

#word freq comparison using normalization
pos_words$normalize <- scale(pos_words$freq) 
neg_words$normalize <- scale(neg_words$freq)
compare_freq <- merge(pos_words, neg_words, by = "word")
compare_freq <- subset(compare_freq, select = c("word", "normalize.x", "normalize.y"))
colnames(compare_freq) <- c("word", "pos", "neg")
compare_freq <- compare_freq[order(compare_freq$pos, decreasing = TRUE),]
temp_pos <- subset(compare_freq, select = c("word", "pos"))
temp_neg <- subset(compare_freq, select = c("word", "neg"))
colnames(temp_neg)[2] <- "pos"
temp_pos$type <- "pos"
temp_neg$type <- "neg"
temp_neg$pos <- temp_neg$pos * -1
comparison_freq <- rbind(temp_pos[1:50,], temp_neg[1:50,])

# X Axis Breaks and Labels 
comparison_lev <- comparison_freq$word %>% as.character()
comparison_lev <- comparison_lev[!duplicated(comparison_lev)]
comparison_freq$word <- factor(as.character(comparison_freq$word),
                               levels = rev(comparison_lev))
rownames(comparison_freq) = c()

# Plot
ggplot(comparison_freq, aes(x = word, y = pos, fill = type)) +
  geom_bar(stat = "identity", width = .6, position = "dodge") +   # draw the bars
  coord_flip() +  # Flip axes
  labs(title="Pos vs Neg word freq(normalized)") +
  theme_tufte() +  # Tufte theme from ggfortify
  theme(plot.title = element_text(hjust = .5), 
        axis.ticks = element_blank())   # Centre plot title

# network analysis
neg_tfIdf <- TermDocumentMatrix(neg_corpus,
                                control = list(wordLengths = c(3,30), weighting = weightTfIdf))
neg_tfIdf_df <- as.data.frame(as.matrix(neg_tfIdf))
neg_tfIdfResult <- sort(rowSums(neg_tfIdf_df), decreasing=TRUE)  
