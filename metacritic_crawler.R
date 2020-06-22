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

# make empty dataframe
all_df <- NULL

for(i in 0:130){ #number started from '0'
  review_url <- paste0(base_url, i)
  remDr$navigate(review_url)
  
  temp <- remDr$getPageSource()[[1]] %>% read_html()
  
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
  all_df <- rbind(all_df, temp_df)
  
  message(i, " th page crawled. ;D")
  Sys.sleep(1)
}

write.csv(all_df, "LOU_review_200620.csv")

#### Crawl User Data with Selenium ####

all_df$user_url <- all_df$user_url %>% as.character()

all_user <- NULL

for(i in 11285:length(all_df$user_url)){
  remDr$navigate(all_df$user_url[i])
  
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
  all_user <- rbind(all_user, temp_df)
  
  if(i %% 100 == 0){message(round(i / length(all_df$user_url), digits = 4) * 100, " % is done.")}
  Sys.sleep(1)
}

all_user <- all_user[!duplicated(all_user),]
write.csv(all_user, "all_user_data.csv")

