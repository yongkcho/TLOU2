#### Last of Us Review Crawler ####

# from Metacritic

#### import library ####
library(dplyr)
library(RSelenium)
library(rvest)
library(stringr)


#### Crawl Review Data with RSelenium ####
# define remote driver

remDr <- remoteDriver(remoteServerAddr = "localhost",
                      port = 4445L,
                      browserName = "chrome")
remDr$open()


base_url <- "https://www.metacritic.com/game/playstation-4/the-last-of-us-part-ii/user-reviews?page="

# define functions
'%!in%' <- function(x, y)!('%in%'(x, y))

# make empty dataframe
#all_df <- NULL
add_df <- NULL
error_chk <- "There are no user reviews yet - Be first to review The Last of Us Part II."

for(i in 301:400){ #number started from '0'
  review_url <- paste0(base_url, 393-i)
  remDr$navigate(review_url)
  
  temp <- remDr$getPageSource()[[1]] %>% read_html()
  
  is_error <- temp %>% html_nodes(".review_top.review_top_l p") %>% html_text() %>% str_remove_all("\n") %>% trimws()
  if(length(is_error) == 0){is_error <- "it's okay"}
  if(is_error == error_chk){
    Sys.sleep(1)
    next #page does not contain text
    }
  
  score <- temp %>% html_nodes(".body.product_reviews") %>% html_nodes(".review_grade div") %>% html_text() %>% as.integer()
  created_date <- temp %>% html_nodes(".body.product_reviews") %>% html_nodes(".date") %>% html_text()
  review <- temp  %>% html_nodes(".body.product_reviews") %>%  html_nodes(".review_body") %>% html_text %>% str_remove_all("\n") %>% trimws()
  user <- temp %>% html_nodes(".name a") %>% html_text()
  user_url <- temp %>% html_nodes(".name a") %>% html_attr("href")
  user_url <- paste0("https://www.metacritic.com", user_url)
  helpful_count <- temp %>% html_nodes(".body.product_reviews")  %>% html_nodes(".total_ups") %>% html_text() %>% as.integer()
  all_count <- temp %>% html_nodes(".body.product_reviews")  %>% html_nodes(".total_thumbs") %>% html_text() %>% as.integer()
  
  temp_df <- data.frame(created_date = created_date, user = user, user_url = user_url, score = score,
                        review = review, all_count = all_count, helpful_count = helpful_count, 
                        page_url = review_url)
  #all_df <- rbind(all_df, temp_df)
  add_df <- rbind(add_df, temp_df)
  
  message(i, " th page crawled. ;D")
  Sys.sleep(1)
}
#bak <- all_df
all_df <- rbind(all_df, add_df)
all_df <- all_df[!duplicated(all_df),]

write.csv(all_df, "LOU_review_200624.csv")

#### Crawl critic review ####

critic_url <- "https://www.metacritic.com/game/playstation-4/the-last-of-us-part-ii/critic-reviews"

remDr$navigate(critic_url)

temp <- remDr$getPageSource()[[1]] %>% read_html()
user <- temp %>% html_nodes(".reviews.critic_reviews") %>% html_nodes(".source a") %>% html_text()
review <- temp %>% html_nodes(".reviews.critic_reviews") %>% html_nodes(".review_body") %>% html_text() %>% str_remove_all("\n") %>% trimws()
created_date <- temp %>% html_nodes(".reviews.critic_reviews") %>% html_nodes(".date") %>% html_text()
score <- temp %>% html_nodes(".reviews.critic_reviews") %>% html_nodes(".review_grade div") %>% html_text() %>% as.numeric()

user <- user[1:100]
review <- review[1:100]
created_date <- created_date[1:100]
score <- score[1:100]

critic_df <- data.frame(created_date = created_date, user = user, score = score, review = review,
                        url = critic_url)

write.csv(critic_df, "critic_data.csv")

#### Crawl User Data with Selenium ####

all_df$user_url <- all_df$user_url %>% as.character()

#user_bak <- all_user
new_data <- add_df[add_df$user %!in% all_user$user,] 
new_user <- NULL

for(i in 1:length(new_data$user_url)){
  remDr$navigate(new_data$user_url[i])
  
  temp <- remDr$getPageSource()[[1]] %>% read_html()
  rating_num <- temp %>% html_nodes(".total_summary_ratings.mr20") %>% html_nodes(".data") %>% html_text() %>% as.integer()
  review_num <- temp %>% html_nodes(".total_summary_reviews") %>% html_nodes(".data") %>% html_text() %>% as.integer()
  dist <- temp %>% html_nodes(".count_wrap") %>% html_nodes(".count") %>% html_text() %>% as.integer()
  pos_dist <- dist[1]
  mixed_dist <- dist[2]
  neg_dist <- dist[3]
  avg_score <- temp %>% html_nodes(".summary_data") %>% html_text() %>% str_remove_all("\n") %>% 
    str_remove_all("\t") %>% trimws()
  if(length(dist) == 0){ #review is deleted
    temp_df <- data.frame(user = all_df$user[i], user_url = all_df$user_url[i], rating_num = rating_num, review_num = review_num,
                          pos_dist = 0, mixed_dist = 0, neg_dist = 0, avg_score = "0.0")
  } else { #normal case
    temp_df <- data.frame(user = all_df$user[i], user_url = all_df$user_url[i], rating_num = rating_num, review_num = review_num,
                          pos_dist = pos_dist, mixed_dist = mixed_dist, neg_dist = neg_dist, avg_score = avg_score)
  }
  new_user <- rbind(new_user, temp_df)
  
  if(i %% 100 == 0){message(round(i / length(all_df$user_url), digits = 4) * 100, " % is done.")}
  Sys.sleep(1)
}

# all_user <- all_user[!duplicated(all_user),]
# write.csv(all_user, "all_user_data.csv")