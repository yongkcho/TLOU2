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


base_url <- "https://www.metacritic.com/game/playstation-4/the-last-of-us-part-ii/user-reviews?sort-by=date&num_items=100&page="

# define functions
'%!in%' <- function(x, y)!('%in%'(x, y))

# make empty dataframe
#bak <- all_df
add_df <- NULL
error_chk <- "There are no user reviews yet - Be first to review The Last of Us Part II."

for(i in 0:800){ #number started from '0'
  review_url <- paste0(base_url, i)
  remDr$navigate(review_url)
  
  temp <- remDr$getPageSource()[[1]] %>% read_html()
  
  is_error <- temp %>% html_nodes(".review_top.review_top_l p") %>% html_text() %>% str_remove_all("\n") %>% trimws()
  if(length(is_error) == 0){is_error <- "it's okay"}
  if(is_error == error_chk){
    Sys.sleep(1)
    next #page does not contain text
  }
  
  
  is_loading <- temp %>% html_nodes(xpath = '//*[@id="main-message"]/h1/span') %>% html_text()
  if(length(is_loading) == 0){is_loading <- ""}
  if(is_loading == loading_chk){
    Sys.sleep(1)
    remDr$close()
    remDr$open()
    remDr$navigate(review_url)
    temp <- remDr$getPageSource()[[1]] %>% read_html()
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
  
  #if(created_date[100] == "Jun 23, 2020"){break}

  add_df <- rbind(add_df, temp_df)
  
  message(i, " th page crawled. ;D")
  Sys.sleep(2)
}

all_df <- read.csv("TLOU_review_200624.csv")
all_df <- 
#bak <- all_df
all_df <- rbind(all_df, add_df)
all_df <- all_df[!duplicated(all_df),]

write.csv(all_df, "LOU_review_200630.csv")

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
review_data <- all_df
review_data <- review_data[!duplicated(review_data),]
review_data$user_url <- review_data$user_url %>% as.character()

#user_bak <- all_user
new_data <- review_data[review_data$user %!in% all_user$user,] 
nrow(new_data)
new_user <- NULL

error_chk <- "Error 503 Service Unavailable"
delete_chk <- "User not found"
loading_chk <- "페이지가 작동하지 않습니다."
delete_user <- c()

for(i in 16639:21999){#length(new_data$user_url)
  remDr$navigate(new_data$user_url[i])

  temp <- remDr$getPageSource()[[1]] %>% read_html()
  
  is_error <- temp %>% html_nodes("body > h1") %>% html_text()
  if(length(is_error) == 0){is_error <- ""}
  if(is_error == error_chk){
    remDr$refresh()
  }
  is_delete <- temp %>% html_nodes(".name") %>% html_text()
  if(length(is_delete) == 0){is_delete <- ""}
  if(is_delete == delete_chk){
    delete_user <- c(delete_user, new_data$user[i])
    next
  }

  is_loading <- temp %>% html_nodes(xpath = '//*[@id="main-message"]/h1/span') %>% html_text()
  if(length(is_loading) == 0){is_loading <- ""}
  if(is_loading == loading_chk){
    Sys.sleep(3)
    remDr$refresh()
    temp <- remDr$getPageSource()[[1]] %>% read_html()
  }
  if(is_loading == loading_chk){
    Sys.sleep(3)
    remDr$refresh()
    temp <- remDr$getPageSource()[[1]] %>% read_html()
  }
  
  rating_num <- temp %>% html_nodes(".total_summary_ratings.mr20") %>% html_nodes(".data") %>% html_text() %>% as.integer()
  review_num <- temp %>% html_nodes(".total_summary_reviews") %>% html_nodes(".data") %>% html_text() %>% as.integer()
  dist <- temp %>% html_nodes(".count_wrap") %>% html_nodes(".count") %>% html_text() %>% as.integer()
  pos_dist <- dist[1]
  mixed_dist <- dist[2]
  neg_dist <- dist[3]
  avg_score <- temp %>% html_nodes(".summary_data") %>% html_text() %>% str_remove_all("\n") %>% 
    str_remove_all("\t") %>% trimws()
  if(length(dist) == 0){ #review is deleted
    temp_df <- data.frame(user = new_data$user[i], user_url = new_data$user_url[i], rating_num = rating_num, review_num = review_num,
                          pos_dist = 0, mixed_dist = 0, neg_dist = 0, avg_score = "0.0")
  } else { #normal case
    temp_df <- data.frame(user = new_data$user[i], user_url = new_data$user_url[i], rating_num = rating_num, review_num = review_num,
                          pos_dist = pos_dist, mixed_dist = mixed_dist, neg_dist = neg_dist, avg_score = avg_score)
  }
  new_user <- rbind(new_user, temp_df)
  
  if(i %% 100 == 0){
    remDr$close()
    remDr$open()
    message(round(i /22000, digits = 4) * 100, " % is done.") # length(new_data$user_url)
  }
  
  Sys.sleep(1.5)
  if(i %% 1000 == 0){Sys.sleep(60)}
}
all_user <- rbind(all_user,new_user)
all_user <- all_user[!duplicated(all_user),]
write.csv(all_user, "all_user_data_200704_server1.csv")
