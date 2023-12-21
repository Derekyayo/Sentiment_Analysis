library(janeaustenr)

tidy_books <- austen_books() %>%
  group_by(book) %>%
  mutate(
    linenumber = row_number(),
    chapter = cumsum(str_detect(text, 
                                regex("^chapter [\\divxlc]", 
                                      ignore_case = TRUE))))

view(tidy_books)

tidy_review <- data %>%
  unnest_tokens(word, review) %>%
  count(word, sort = TRUE)



nrc_joy <- get_sentiments("nrc") %>% 
  filter(sentiment == "joy")

tidy_review<- tidy_review %>%
  inner_join(nrc_joy) %>%
  count(word, sort = TRUE)


sentimental <- tidy_review %>%
  inner_join(get_sentiments("bing")) %>%
  group_by(word) %>%
  summarise(sentiment_score = sum(sentiment == "positive") - sum(sentiment == "negative"),
            sentiment = first(sentiment))

sentimental

sentimental
sentiment
review_sentiment_counts

review_sentiment_counts <- tidy_review %>%
  inner_join(get_sentiments("bing")) %>%
  group_by(word) %>%
  summarise(
    positive_words = sum(sentiment == "positive"),
    negative_words = sum(sentiment == "negative"),
    sentiment = first(sentiment))

revie
review_sentiment_counts_wide <- review_sentiment_counts %>%
  mutate(net_sentiment = positive_words - negative_words) %>%
  pivot_wider(names_from = c("positive_words", "negative_words"), values_from = c("positive_words", "negative_words"), values_fill = 0)


review_sentiment_counts_wide



ggplot(review_sentiment_counts_wide, aes(sentiment, net_sentiment, fill = word)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, ncol = 2, scales = "free_x") +
  labs(title = "Sentiment Trajectory Across Plot for Positive and Negative Sentiments",
       x = "Plot Trajectory Index",
       y = "Net Sentiment Score")

  
get_sentiments("nrc") %>% 
  filter(sentiment %in% c("positive", "negative")) %>% 
  count(sentiment)

get_sentiments("bing") %>% 
  count(sentiment)


tidy_review

bing_word_counts <- tidy_review %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts %>%
  group_by(sentiment) %>%
  ungroup() %>%
  ggplot(aes(n, word, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(x = "Contribution to sentiment",
       y = NULL)

tidy_review %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n))

tidy_review %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("gray20", "gray80"),
                   max.words = 100)
