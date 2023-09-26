library(tidyverse)
library(stringr)
library(scales)
library(tidytext)
library(textstem)
library(tidyr)
library(ggwordcloud)
library(treemapify)
library(vader)
library(reshape2)
library(wordcloud)
library(igraph)
library(ggraph)
library(forcats)
library(topicmodels)
library(stringi)
library(naivebayes)
library(rpart)
library(rpart.plot)


set.seed(123)

dat <- readxl::read_xlsx("BarbieScrub.xlsx") #Washed data was manually scrubbed in Excel

title <- dat %>% select(ID, thesis)
rating <- dat %>% select(ID, date, rating)

noSpoiler <- dat %>% filter(!grepl("Warning: Spoilers", text))
noSpoiler$review <- sub('.*July 2023', '', noSpoiler$text)
noSpoiler <- noSpoiler %>% subset(select = -c(text, thesis, rating, date))

data("stop_words")
afinn <- get_sentiments("afinn")

#### Word Count ####
tidyDat <- noSpoiler %>% unnest_tokens(word, review)

tidyDat <- tidyDat %>% anti_join(stop_words)
tidyDat$word <- tidyDat$word %>% str_to_lower(locale = "en")
tidyDat$word <- lemmatize_words(tidyDat$word)

wordCount <- tidyDat %>% count(word, sort = T)

wCl_1 <- wordCount %>% 
  head(100) %>%
  ggplot(., aes(label = word, size = n, color = n))+
  geom_text_wordcloud()+
  theme_minimal()+
  scale_size_area(max_size = 20)+
  labs(title = "Top 100 Most Common Words \nUsed in Barbie (2023)'s IMDB Reviews")

# export::graph2png(wCl_1, "Word Count (Cloud).png")

wTr_1 <- wordCount %>% head(100) %>%
  ggplot(., aes(area = n, fill = n, 
                label = paste(word, "\ncount:", n)))+
  geom_treemap()+
  geom_treemap_text(place = "centre")+
  theme(legend.position = "none")+
  labs(title = "Top 100 Most Common Words \nUsed in Barbie (2023)'s IMDB Reviews")
  
# export::graph2png(wTr_1, "Word Count (Tree).png")

wordSent <- inner_join(tidyDat, afinn)
wordSent <- wordSent %>% group_by(word, value) %>% summarise (n = n()) %>% arrange(desc(n))

wordSent$sentiment <- ifelse(wordSent$value < 0, "negative", "positive")

wSe_1 <- wordSent %>% group_by(sentiment) %>% slice_max(n, n = 20) %>% ungroup() %>% mutate(word = reorder(word,n)) %>%
  ggplot(., aes(n, word, fill = value))+
  geom_col()+
  scale_fill_gradient2(position="bottom" ,
                       low = "#BF3100",
                       mid = "#E6E6EA",
                       high = "#009FB7")+
  facet_wrap(~sentiment, scales = "free_y")+
  scale_x_continuous(name = "Count", breaks = pretty_breaks(n = 10))+
  theme_bw()+
  labs(title = "Top 20 Most Common Sentiment Words \nUsed in Barbie (2023)'s IMDB Reviews")

# export::graph2png(wSe_1, "Word Sentiment (Bar).png")

# sCl_1 <- acast(wordSent, word ~ sentiment, value.var = "n", fill = 0) %>% 
#   comparison.cloud(colors = c("#BF3100", "#009FB7"),
#                    max.words = 100)

# export::graph2png(sCl_1, "Word Sentiment (Cloud).png")

#Topic-modeling
dtm_review <- tidyDat %>% 
  count(ID, word) %>%
  cast_dtm(ID, word, n) %>%
  as.matrix()

lda_topics <- LDA(dtm_review, k = 4, method = "Gibbs", control = list(seed = 123)) %>%
  tidy(matrix = "beta")

word_probs <- lda_topics %>% group_by(topic) %>% 
  top_n(15,beta) %>% 
  ungroup() %>%
  mutate(term = fct_reorder(term, beta))

tpMdling <- ggplot(word_probs, aes(term, beta, fill = as.factor(topic)))+
  geom_col(show.legend = F)+
  facet_wrap(~topic, scales = "free")+
  coord_flip()+
  theme_bw()+
  labs(title = "Topic Modeling")

# export::graph2png(tpMdling, "Topic Modeling.png")

#### Bi Gram ####
#Tokenization
bi_gram <- noSpoiler %>% 
  unnest_tokens(bigram, review, token = "ngrams", n = 2) %>%
  filter(!is.na(bigram))
  
#All to lowercase
bi_gram$bigram <- str_to_lower(bi_gram$bigram, locale = "en")

#Remove stop-word
bigrams_separated <- bi_gram %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

bigrams_filtered$word1 <- lemmatize_words(bigrams_filtered$word1)
bigrams_filtered$word2 <- lemmatize_words(bigrams_filtered$word2)

#Word Count
bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)

bigram_counts$word12 <- str_c(bigram_counts$word1, " ", bigram_counts$word2)

wCl_2 <- bigram_counts %>% 
  head(100) %>%
  ggplot(., aes(label = word12, size = n, color = n))+
  geom_text_wordcloud()+
  theme_minimal()+
  scale_color_gradient(low = "darkgreen", high = "lightgreen")+
  scale_size_area(max_size = 17)+
  labs(title = "Top 100 Most Common Bi-Grams \nUsed in Barbie (2023)'s IMDB Reviews")

# export::graph2png(wCl_2, "Bi-Gram Count (Cloud).png")

wTr_2 <- bigram_counts %>% head(100) %>%
  ggplot(., aes(area = n, fill = n, 
                label = paste(word12, "\ncount:", n)))+
  geom_treemap()+
  scale_fill_gradient(low = "darkgreen", high = "lightgreen")+
  geom_treemap_text(place = "centre")+
  theme(legend.position = "none")+
  labs(title = "Top 100 Most Common Bi-Grams \nUsed in Barbie (2023)'s IMDB Reviews")

# export::graph2png(wTr_2, "Bi-Gram Count (Tree).png")

# vader_allbigrams <- vader_df(bigram_counts$word12)
# write.csv(vader_allbigrams, "vader_allbigrams.csv")
vader_allbigrams <- read.csv("vader_allbigrams.csv")
vaderbigrams <- rename(vader_allbigrams, bigram = text)

sentiment_vader <- inner_join(bigram_counts, vaderbigrams, by = c("word12" = "bigram"))
sentiment_noNeutral <- sentiment_vader %>% filter(neu != 1)

sentiment_noNeutral$sentiment <- ifelse(sentiment_noNeutral$compound > 0, "positive", "negative")

wSe_2 <- sentiment_noNeutral %>% group_by(sentiment) %>% slice_max(n, n = 20) %>% ungroup() %>% mutate(word = reorder(word12,n)) %>%
  ggplot(., aes(n, word, fill = compound))+
  geom_col()+
  scale_fill_gradient2(position="bottom" ,
                       low = "#BF3100",
                       mid = "#E6E6EA",
                       high = "#009FB7")+
  facet_wrap(~sentiment, scales = "free_y")+
  scale_x_continuous(name = "Count", breaks = pretty_breaks(n = 10))+
  theme_bw()+
  labs(title = "Top 20 Most Common Sentiment Words \nUsed in Barbie (2023)'s IMDB Reviews")

# export::graph2png(wSe_2, "Bi-Gram Sentiment (Bar).png")

# sCl_2 <- acast(sentiment_noNeutral, word12 ~ sentiment, value.var = "n", fill = 0) %>% 
#   comparison.cloud(colors = c("#BF3100", "#009FB7"),
#                    max.words = 100)

#### Bi-Gram Network ####
bigram_graph <- bigram_counts %>% filter(n > 10) %>% graph_from_data_frame()

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

bigrmNW <- ggraph(bigram_graph, layout = "fr")+
  geom_edge_link(aes(edge_alpha = n), show.legend = F,
                 arrow = a, end_cap = circle(.07, "inches"))+
  geom_node_point(color = "lightblue", size = 5)+
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)+
  theme_void()

# export::graph2png(bigrmNW, "Bi-Gram Network.png")

#### Sentiment ####
review_sentiment <- readxl::read_xlsx("reviewSentiment.xlsx")

forRev <- dat %>% filter(!grepl("Warning: Spoilers", text))
forRev$review <- sub('.*July 2023', '', forRev$text)
forRev <- forRev %>% subset(select = -c(text, thesis))

review_analysis <- full_join(forRev, review_sentiment, by = c("ID", "review"))
review_analysis <- review_analysis %>%  filter(!is.na(sentiment))

review_analysis$date[review_analysis$ID == 549] <- "2023-07-21"

review_analysis$date <- as.Date(review_analysis$date)

rtIncn <- ggplot(review_analysis, aes(as.factor(rating), fill = sentiment))+
  geom_bar()+
  theme_bw()+
  labs(title = "Rating Inconsistency")

# export::graph2png(rtIncn, "Rating Inconsistency.png")

sentCountDate <- review_analysis %>% group_by(sentiment, date) %>% summarise(n = n())
sentSumCount <- review_analysis %>% group_by(date) %>% summarise(n = n())

sentComb <- left_join(sentCountDate, sentSumCount, by = "date")
sentComb$perc <- sentComb$n.x/sentComb$n.y
sentComb$perc_lab <- percent(sentComb$perc)

deltaSenDate <- ggplot(sentComb ,aes(date, perc, fill = sentiment))+
  geom_col()+
  theme_bw()+
  scale_x_date(date_breaks = "1 day", date_labels = "%B %d")+
  labs(title = "IMDB's Review Sentiments One Week After Premier")

# export::graph2png(deltaSenDate, "Sentiment Percentage Date.png")

review_analysis$wordCount <- stri_count_words(review_analysis$review)

## Maybe ANOVA (Regression came out negative so...)

