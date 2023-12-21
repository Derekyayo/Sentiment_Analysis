review_words <- data %>%
  unnest_tokens(word, review) %>%
  count(word, sort = TRUE)


review_words

total_words <- review_words %>%
  inner_join(nrc_sentiment) %>%
  count(word, sort = TRUE) %>%
  group_by(word) %>% 
  summarize(total = sum(n))

review_words <- left_join(review_words, sentimental)

review_words

review_words <- na.omit(sentimental)


nrc_sentiment = get_sentiments("nrc")
bing_sentiment = get_sentiments("bing")
afinn_sentiment = get_sentiments("afinn")
