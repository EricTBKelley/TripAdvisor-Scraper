#Web Scraping

# Inspired by
# http://notesofdabbler.github.io/201408_hotelReview/scrapeTripAdvisor.html
   
library(rvest)
library(xlsx)
library(dplyr)
library(stringr)
TripAdvisorReviews <- function(hotelUrl) {
  
  url <- hotelUrl
  
  iterations <- url %>% 
      read_html() %>% 
      html_nodes(".pageNum") %>% 
      tail(1) %>% 
      html_text() %>% 
      as.numeric()
      
  for(x in 1:iterations){
    if (!x == 1) {
      url <- hotelUrl
      num <- as.character((x * 5) - 5)
      split.url <- str_split(url, "-Reviews-", simplify = T) %>% as.vector()
      url <- paste0(split.url[1], "-Reviews-or", num, "-", split.url[2])
    }
    print(url)
    
    reviews <- url %>%
      read_html() %>%
      html_nodes(".hotels-review-list-parts-SingleReview__reviewContainer--d54T4")
    
    id <- reviews %>%
      html_attr("data-reviewid")
    
    quote <- reviews %>% 
      html_node(".hotels-review-list-parts-ReviewTitle__reviewTitleText--3QrTy") %>% 
      html_text()
    
    rating <- reviews %>%
      html_node(".hotels-review-list-parts-RatingLine__bubbles--1oCI4") %>%
      html_node('.ui_bubble_rating') %>% 
      gsub("[^0-9]", "", .) %>% 
      as.numeric(.)/10
    
    date <- reviews %>%
      html_node(".hotels-review-list-parts-EventDate__event_date--CRXs4") %>%
      html_text() %>% 
      gsub("Date of stay: ", "", .)
    
    review <- reviews %>% 
      html_node(".hotels-review-list-parts-ExpandableReview__reviewText--3oMkH") %>% 
      html_text()
      
    data5 <- data.frame(id, quote, rating, date, review, stringsAsFactors = FALSE)
    
    if(x==1) hotel.data <- data5
    else hotel.data <- rbind(hotel.data, data5)
    
  }
  return(hotel.data)
}

JW.Marriot.Indianapolis <- TripAdvisorReviews("http://www.tripadvisor.com/Hotel_Review-g37209-d1762915-Reviews-JW_Marriott_Indianapolis-Indianapolis_Indiana.html")
write.xlsx(JW.Marriot.Indianapolis, "jw_marriot_indianapolis_reviews.xlsx")

airport.inn.seatac <- TripAdvisorReviews("https://www.tripadvisor.com/Hotel_Review-g58732-d126096-Reviews-Americas_Best_Value_Airport_Inn-SeaTac_Washington.html")
