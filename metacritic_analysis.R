#### Last of Us Review Analysis ####

# set working dir
wkdir <- "C:/Users/ykun9/TLOU2"

setwd(wkdir)

list.files(wkdir)

# import library
library(dplyr)
library(stringr)
library(ggplot2)

# import dataset
user_data <- read.csv("all_user_data.csv")
review_data <- read.csv("TLOU_review_200620.csv")

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

# overlap 2 histograms in one plot to compare
plot(h1, col = rgb(0, 1, 1, 1/4), xlim = c(0,10))
plot(h2, col = rgb(1, 0, 0, 1/4), xlim = c(0,10), add = T)

# since there's diff btw numbers it is very hard to compare
h1$counts <- h1$counts / sum(h1$counts)
h2$counts <- h2$counts / sum(h2$counts)

# let's plot agian
plot(h1, col = rgb(0, 1, 1, 1/4), xlim = c(0,10))
plot(h2, col = rgb(1, 0, 0, 1/4), xlim = c(0,10), add = T)

