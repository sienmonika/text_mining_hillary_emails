library(RSQLite)
library(ggplot2)
library(dplyr)
library(tidytext)
library(stringr)
library(tidyr)
library(wordcloud)
library(reshape2)
library(colorRamps)
setwd("C:/Users/Monika Sienkiewicz/Documents/text_mining_hillary")

#### Import ####
#import emails where sender = hillary
db <- dbConnect(dbDriver("SQLite"), "/Users/Monika Sienkiewicz/Documents/text_mining_hillary/database.sqlite")

emails <- dbGetQuery(db, "SELECT ExtractedBodyText EmailBody FROM Emails e INNER JOIN Persons p ON e.SenderPersonId=P.Id WHERE p.Name='Hillary Clinton'  AND e.ExtractedBodyText != '' ORDER BY RANDOM()")
#data frame class is important for tidy format
class(emails)

####CLEANING and INITIAL ANALYSIS ####
#adding id for each email
original_emails <- emails %>%
  mutate(emailnumber = row_number()) %>%
  ungroup()

#tidy format

#filter parts of email address and numbers
custom_stop_words <- data_frame(word = c(as.character(1:3000), "hrod17", "clintonemail.com", "state.gov", "00", "01", "03", "04841", "05", "08", "1.4"))

tidy_emails <- original_emails %>%
  #tokenizing
  unnest_tokens(word, EmailBody) %>%
  #delete stop words
  anti_join(stop_words) %>%
  anti_join(custom_stop_words) 

######## Relationships between words: n-grams and correlations#####

library(ggraph)
library(igraph)

#create bigrams
emails_bigrams <- original_emails %>%
  unnest_tokens(bigram, EmailBody, token = "ngrams", n = 2) 

#eliminate stop words + specific words + filter numbers

head(custom_stop_words)

bigrams_separated <- emails_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word1 %in% custom_stop_words$word) %>%
  filter(!word2 %in% custom_stop_words$word) %>%
  #filter numbers
  filter(!str_detect(word1, "\\d"),
         !str_detect(word2, "\\d")) 

# new bigram counts #for graphs
bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)

head(bigram_counts)

#unite bigrams #for tfidf
bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")

head(bigrams_united)
#plot bigrams
# >5x
bigrams_united %>%
  count(bigram, sort = TRUE) %>%
  filter(n > 5) %>%
  mutate(bigram= reorder(bigram, n)) %>%
  ggplot(aes(bigram, n)) +
  geom_col(fill="deeppink4") +
  xlab(NULL) +
  coord_flip()+
  theme_minimal()

dev.copy(png,'bigramx5.png', height=500)
dev.off()

#>10 x
bigrams_united %>%
  count(bigram, sort = TRUE) %>%
  filter(n > 10) %>%
  mutate(bigram= reorder(bigram, n)) %>%
  ggplot(aes(bigram, n)) +
  geom_col(fill="deeppink4") +
  xlab(NULL) +
  coord_flip()
dev.copy(png,'bigramx10.png')
dev.off()


## bigram TFIDF##
bigram_tf_idf <- bigrams_united %>%
  count(emailnumber, bigram) %>%
  bind_tf_idf(bigram, emailnumber, n) %>%
  arrange(desc(tf_idf))

head(bigram_tf_idf)

bigram_tf_idf_2 <- bigram_tf_idf %>%
  filter(n != 1) %>%
  filter(n != 2) 


bigram_tf_idf_2 %>%
  arrange(desc(tf_idf)) %>%
  mutate(bigram = factor(bigram, levels = rev(unique(bigram)))) %>% 
  group_by(emailnumber) %>% 
  top_n(10, tf_idf) %>% 
  ungroup %>%
  ggplot(aes(bigram, tf_idf, fill = emailnumber)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "TF-IDF") +
  facet_wrap(~emailnumber, ncol = 3, scales ="free") +
  coord_flip() +
  theme_minimal(base_size=18)

dev.copy(png,'bigramtfidfmin3.png', width=1000, height=900)
dev.off()



#negative words
AFINN <- get_sentiments("afinn")
negation_words <- c("not", "no", "never", "without", "don't")

negated_words <- bigrams_separated %>%
  filter(word1 %in% negation_words) %>%
  inner_join(AFINN, by = c(word2 = "word")) %>%
  count(word1, word2, score, sort = TRUE) %>%
  ungroup()

#18 bigrams
head(negated_words)

negated_words %>%
  mutate(contribution = n * score) %>%
  arrange(desc(abs(contribution))) %>%
  mutate(word2 = reorder(word2, contribution)) %>%
  ggplot(aes(word2, n * score, fill = n * score > 0)) +
  geom_col(show.legend = FALSE) +
  theme_minimal()+
  xlab("Słowa poprzedzone \n \"not\", \"no\", \"never\", \n \"without\", \"don't\" ") +
  ylab("Sentyment * Liczba wystąpień") +
  ylim(-5,10) +
  coord_flip()

dev.copy(png,'negativebigram.png', height=200)
dev.off()


#Graphs####

library(igraph)
bigram_counts


bigram_graph <- bigram_counts %>%
  filter(n > 3) %>%
  graph_from_data_frame()

bigram_graph

library(ggraph)
set.seed(123)
a <- grid::arrow(type = "closed", length = unit(.1, "inches"))

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.05, 'inches')) +
  geom_node_point(color = "lightblue", size = 4) +
  geom_node_text(aes(label = name), vjust = 1.5, hjust = 1.5) +
  theme_void(base_size=12)

dev.copy(png,'bigramgraph.png', width=1000)
dev.off()


####Counting and correlating pairs of words with the widyr package####

library(widyr)

email_cors_20 <- tidy_emails %>% 
  group_by(word) %>%
  filter(n() >= 20) %>%
  filter(!str_detect(word, "\\d")) %>%
  pairwise_cor(word, emailnumber, sort = TRUE, upper = FALSE)

email_cors_20

set.seed(123)

#average corr
email_cors_20 %>%
  filter(correlation > .3) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation, edge_width = correlation), edge_colour = "royalblue") +
  geom_node_point(size = 2) +
  geom_node_text(aes(label = name), repel = TRUE,
                 point.padding = unit(0.5, "lines")) +
  theme_void(base_size=15)

dev.copy(png,'corelationbigram.png', width=1000, height=600)
dev.off()

#weak corr
email_cors_20 %>%
  filter(correlation > .1) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation, edge_width = correlation), edge_colour = "royalblue") +
  geom_node_point(size = 2) +
  geom_node_text(aes(label = name), repel = TRUE,
                 point.padding = unit(0.3, "lines")) +
  theme_void(base_size=12)

dev.copy(png,'corelationbigram01.png', width=1000, height=800)
dev.off()

# n>10 without numbers
email_cors_10 <- tidy_emails %>% 
  group_by(word) %>%
  filter(n() >= 10) %>%
  filter(!str_detect(word, "\\d")) %>%
  pairwise_cor(word, emailnumber, sort = TRUE, upper = FALSE) 

email_cors_10

set.seed(123)
email_cors_10 %>%
  filter(correlation > .4) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation, edge_width = correlation), edge_colour = "royalblue") +
  geom_node_point(size = 3) +
  geom_node_text(aes(label = name), repel = TRUE,
                 point.padding = unit(0.1, "lines")) +
  theme_void()
dev.copy(png,'corelationbigram10.png', width=1000, height=800)
dev.off()
