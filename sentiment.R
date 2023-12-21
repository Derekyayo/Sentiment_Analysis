head(Reviews)

head(all_reviews)

cleaned_reviews <- Reviews %>%
  mutate(Reviews = substring(Reviews, 18)) %>%
  separate(Reviews, into = c("Date", "Review"), sep = "\n", extra = "drop")

cleaned_reviews

view(cleaned_reviews)

cleaned_reviews <- cleaned_reviews %>%
  select(-Date)

cleaned_reviews

tibble(cleaned_reviews)

sum(is.na(cleaned_reviews$Review))

sum(is.na(cleaned_reviews$Review))

na_count <- sum(is.na(cleaned_reviews$Review))

na_count
cat("Number of NA values before dropping:", na_count, "\n")

cleaned_reviews <- na.omit(cleaned_reviews)

na_count_after <- sum(is.na(cleaned_reviews$review))

na_count_after
print(cleaned_reviews)

view(cleaned_reviews)

data <- cleaned_reviews

BA_reviews %>%
  unnest_tokens(word, reviews)

cleaned_reviews <- as.data.frame(cleaned_reviews)

view(cleaned_reviews)

view(data)

cleaned_reviews %>%
  unnest_tokens(word,Reviews)

cleaned_reviews %>%
  unnest_tokens(word, text = Reviews)

class(BA_reviews$reviews)

head(cleaned_reviews)

cleaned_reviews$Review <- as.character(cleaned_reviews$Review)

cleaned_reviews$Review <- tolower(cleaned_reviews$Review)

cleaned_reviews<- as.data.frame(cleaned_reviews)

class(cleaned_reviews$Review)


any(is.na(cleaned_reviews$review))

sum(is.na(cleaned_reviews$review))

data$Review<- removeNumbers(cleaned_reviews$Review)

tail(cleaned_reviews)

data <- data %>%
  rename(review = Review)

class(data$Review)

data <- cleaned_reviews

view(data)
data

data$review <- gsub("[[:punct:]]", "", data$review)

data$review <- gsub("[\\ud800-\\udfff]", "", data$review, perl = TRUE)

data <- as.data.frame(data)

cleaned_reviews$review[cleaned_reviews$review == ""] <- NA

class(data$review)









emotions_dict <- list(
  Enjoyment = c('pleasure','joy','happiness','amusement','pride','awe','excitement','ecstasy', 'sorrow'),
  Excitement = c('agitation', 'excitement', 'turmoil', 'upheaval', 'hullabaloo'),
  Sadness = c('lonely','unhappy','hopeless','gloomy','miserable'),
  Fear = c('worried','nervous','anxious','scared','panicked','stressed'),
  Anger = c('annoyed','frustrated','bitter','infuriated','mad','insulted','vengeful'),
  Disgust =c('dislike','revulsion','nauseated','aversion','offended','horrified'))


corpus <- Corpus(VectorSource(data$review))
glimpse(corpus)
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)

corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords("en"))

corpus <- tm_map(corpus, stemDocument)
corpus <- Corpus(VectorSource(data$review))

corpus <- tm_map(corpus, stemDocument)

stemDocument(corpus, language = "english")

tm_map(corpus, stemDocument, lazy = TRUE)

corpus <- tm_map(corpus, stemDocument)


dtm <- DocumentTermMatrix(corpus)
emotions_count <- sapply(emotions_dict, function(synonyms) {
  sum(apply(as.matrix(dtm), 1, function(row) any(row %in% synonyms)))
})

total_words <- sum(as.matrix(dtm))

total_words

emotions_frequency <- emotions_count / total_words


high_frequency_emotions <- names(emotions_frequency[emotions_frequency > 0.1])
medium_frequency_emotions <- names(emotions_frequency[0.05 < emotions_frequency & emotions_frequency <= 0.1])
low_frequency_emotions <- names(emotions_frequency[emotions_frequency <= 0.05])

cat("High-Frequency Emotions:", high_frequency_emotions, "\n")

cat("Medium-Frequency Emotions:", medium_frequency_emotions, "\n")

cat("Low-Frequency Emotions:", low_frequency_emotions, "\n")

view(data)

review_data

data$review<- removeNumbers(data$review)

review_data <- data %>%
  unnest_tokens(word, review) %>%
  anti_join(stop_words)


view(review_data)

head(review_data)

dim(review_data)


review_data %>%
  count(word) %>%
  arrange(desc(n)) %>%
  head()

review_data2 <- data %>%
  unnest_tokens(word, review) %>%
  anti_join(stop_words)

head(review_data2)


review_data2 %>%
  count(word) %>%
  arrange(desc(n)) %>%
  head()


word_counts <- review_data2 %>%
  count(word) %>%
  filter(n>50) %>%
  arrange(desc(n))

ggplot(word_counts, aes(x=word, y=n)) + 
  geom_col() +
  coord_flip() +
  ggtitle("Review Word Counts")


word_counts <- review_data2 %>%
  count(word) %>%
  filter(n>50) %>%
  mutate(word2 = fct_reorder(word, n))

word_counts


ggplot(word_counts, aes(x=word2, y=n)) + 
  geom_col() +
  coord_flip() +
  ggtitle("Review Word Counts")


review_data2 <- review_data2 %>%
  clean_names()

review_data2


FM_review <- review_data2 %>%
  count(word) %>%
  cast_dtm(word, n) %>%
  as.matrix()

head(review_data2)

view(review_data2)

rm(Reviews)

corpus <- Corpus(VectorSource(review_data2$word))

dtm <- DocumentTermMatrix(corpus)

dtm <- dtm[rowSums(as.matrix(dtm)) > 0, ]


dtm_matrix <- as.matrix(dtm)


if (nrow(dtm_matrix) == 0) {
  stop("No documents left after filtering out rows with all zero entries.")
}

review_lda <- LDA(dtm_matrix,
                  k = 4,
                  method = "Gibbs",
                  control = list(seed = 1234))

rm(eview_lda)
glimpse(review_lda)

review_topics <- tidy(review_lda, matrix = "beta")

glimpse(review_topics)


review_topics %>%
  arrange(desc(beta))

top_terms <- review_topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>% 
  ungroup() %>%
  arrange(topic, -beta)

top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()




rm(expression)
rm(remDr)
