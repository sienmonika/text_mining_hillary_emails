library(RSQLite)
library(ggplot2)
library(dplyr)
library(tidytext)
library(stringr)
library(tidyr)
library(wordcloud)
library(reshape2)
library(colorRamps)
library(igraph)
library(ggraph)
library(topicmodels)
library(tidytext)

#plot descriptions e.g. labs in Polish

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

####Preprocessing for TFIDF####
tidy_emails_words <- tidy_emails %>%
  count(emailnumber, word, sort = TRUE) %>%
  ungroup()

total_emails_words <- tidy_emails_words %>% 
  group_by(emailnumber) %>% 
  summarize(total = sum(n))

emails_words_2 <- left_join(tidy_emails_words, total_emails_words) 

head(emails_words_2)

####tf idf####
#bind tf idf

email_words_bindtfidf <- emails_words_2 %>%
  bind_tf_idf(word, emailnumber, n)

email_words_bindtfidf %>%
  select(-total) %>%
  arrange(desc(tf_idf))

#when both  n and term frequency are equal to 1 for these terms >> these were documents that only had a single word in them. If a document only contains one word, the tf-idf algorithm will think that is a very important word.
#Depending on our analytic goals, it might be a good idea to throw out all documents that have very few words.

#filter out emails with less then 4 words
email_words_bindtfidf3 <- email_words_bindtfidf %>%
  filter(n != 1) %>%
  filter(n != 2) %>%
  filter(n != 3)

email_words_bindtfidf3%>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  group_by(emailnumber) %>% 
  top_n(10, tf_idf) %>% 
  ungroup %>%
  ggplot(aes(word, tf_idf, fill = emailnumber)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~emailnumber, ncol = 4, scales ="free") +
  coord_flip() +
  theme_minimal()

dev.copy(png,'tfidfmin3.png', width=900, height=700)
dev.off()

#filterout emails with less then 3 words
email_words_bindtfidf2 <- email_words_bindtfidf %>%
  filter(n != 1) %>%
  filter(n != 2) 

email_words_bindtfidf2%>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  group_by(emailnumber) %>% 
  top_n(5, tf_idf) %>% 
  ungroup %>%
  ggplot(aes(word, tf_idf, fill = emailnumber)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~emailnumber, ncol = 4, scales ="free") +
  coord_flip() +
  theme_minimal()

dev.copy(png,'tfidfmin2.png', width=900, height=700)
dev.off()


####topic modeling####

library(topicmodels)

#cast_dtm
emails_dtm <- tidy_emails %>%
  count(emailnumber, word, sort = TRUE) %>%
  ungroup()  %>%
  cast_dtm(emailnumber, word, n)

emails_dtm

######choosing k #####
# http://www.rpubs.com/MNidhi/NumberoftopicsLDA
# https://cran.r-project.org/web/packages/ldatuning/vignettes/topics.html 

#Package ldatuning realizes 4 metrics to select perfect number of topics for LDA model.
#install.packages("ldatuning")

library("ldatuning")
emails_dtm

#Calculates different metrics to estimate the most preferable number of topics for LDA model.
result <- FindTopicsNumber(
  emails_dtm,
  topics = seq(from = 2, to = 15, by = 1),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 123),
  mc.cores = 2L,
  verbose = TRUE
)

result
#Result is a number of topics and corresponding values of metrics

#Simple approach in analyze of metrics is to find extremum, more complete description is in corresponding papers:
# minimization:  Arun2010 [1] CaoJuan2009 [2]
# maximization: Deveaud2014 [3] Griffiths2004 [4,5]

FindTopicsNumber_plot(result)

dev.copy(png,'picktopics.png', width=1000)
dev.off()

#####further analyse 5 and 12####

#if needed can analyse topics using method VEM
#result2 <- FindTopicsNumber(
# emails_dtm,
#topics = seq(from = 2, to = 15, by = 1),
#metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
#method = "VEM",
#control = list(seed = 123),
#mc.cores = 2L,
#verbose = TRUE
#)
#'Griffiths2004' is incompatible with 'VEM' method, excluded.

#result2

#FindTopicsNumber_plot(result2)
#not possible to use 1 of max metrics Griffiths. 2nd max one is close to not depending on no of topics. minimising metrics are better the more topic there are


###5 topics Gibbs####

#model
emails_lda_5 <- LDA(emails_dtm, k = 5, method="Gibbs", control = list(seed = 123))
emails_lda_5

emails_topics_5 <- tidy(emails_lda_5, matrix = "beta")
emails_topics_5

####beta per-topic-per-word probabilities
#top terms in each of topics
emails_top_terms_5 <- emails_topics_5 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

emails_top_terms_5 %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  theme_minimal()+
  coord_flip() +
  labs(y = expression(beta))

dev.copy(png,'5topicsbeta.png', height=400)
dev.off()

#####gamma per-document-per-topic probabilities

emails_documents_5 <- tidy(emails_lda_5, matrix = "gamma")
emails_documents_5

#plot
ggplot(emails_documents_5, aes(gamma)) +
  geom_histogram() +
  scale_y_log10() +
  theme_minimal()+
  labs(title = "Rozkład prawdopodobieństw dla wszystkich tematów",
       y = "Liczba emaili [skala log]", x = expression(gamma))

dev.copy(png,'gamma1_5.png', height=200)
dev.off()

#plot2 distribution per topic
ggplot(emails_documents_5, aes(gamma, fill = as.factor(topic))) +
  geom_histogram(show.legend = FALSE) +
  facet_wrap(~ topic) +
  scale_y_log10() +
  theme_minimal()+
  labs(title = "Rozkład prawdopodobieństwa należenia do danego tematu",
       y = "Liczba emaili", x = expression(gamma))
dev.copy(png,'gamma2_5.png', height=300)
dev.off()

#plot # of messages where this was the highest % topic

emails_lda_5 %>%
  tidy(matrix = "gamma") %>%
  ggplot(aes(factor(topic), gamma)) +
  geom_boxplot() +
  theme_minimal() +
  labs(x = "Temat",
       y = "% wiadomości, dla których \n jest to najważniejszy temat")

dev.copy(png,'topicproc5.png', height=200)
dev.off()

#interactive viz - two dimensional distance between topics and showing the words which are most strongly associated with those topics.
doc.length = rowSums(as.matrix(emails_dtm))
term.frequency = colSums(as.matrix(emails_dtm))
vocab = colnames(emails_dtm)
P = posterior(emails_lda_5 )
phi = P$term
theta = P$topics

# create the JSON object to feed the visualization:
email_vis_5= list(phi = phi,theta = theta,doc.length = doc.length, vocab = vocab,term.frequency = term.frequency)
library(LDAvis)
library(servr)
email_json_5 = createJSON(phi = email_vis_5$phi,theta = email_vis_5$theta,doc.length = email_vis_5$doc.length, vocab =colnames(phi),  term.frequency = email_vis_5$term.frequency)
serVis(email_json, out.dir = './', open.browser = TRUE)


###12 topics Gibbs####

#model
emails_lda_12 <- LDA(emails_dtm, k = 12, method="Gibbs", control = list(seed = 123))

emails_lda_12

emails_topics_12 <- tidy(emails_lda_12, matrix = "beta")
emails_topics_12


####beta per-topic-per-word probabilities
#top terms in each of topics
emails_top_terms_12 <- emails_topics_12 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

emails_top_terms_12 %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  theme_minimal(base_size=15)+
  coord_flip() +
  labs(y = expression(beta))

dev.copy(png,'12topicsbeta.png', height=750, width=1000)
dev.off()

#####gamma per-document-per-topic probabilities

emails_documents_12 <- tidy(emails_lda_12, matrix = "gamma")
emails_documents_12

#plot
ggplot(emails_documents_12, aes(gamma)) +
  geom_histogram() +
  scale_y_log10() +
  theme_minimal()+
  labs(title = "Rozkład prawdopodobieństw dla wszystkich tematów",
       y = "Liczba emaili [skala log]", x = expression(gamma))

dev.copy(png,'gamma1_12.png', height=200)
dev.off()

#plot2 distribution per topic
ggplot(emails_documents_12, aes(gamma, fill = as.factor(topic))) +
  geom_histogram(show.legend = FALSE) +
  facet_wrap(~ topic) +
  scale_y_log10() +
  theme_minimal()+
  labs(title = "Rozkład prawdopodobieństwa należenia do danego tematu",
       y = "Liczba emaili", x = expression(gamma))
dev.copy(png,'gamma2_12.png', height=300)
dev.off()

#plot # of messages where this was the highest % topic

emails_lda_12 %>%
  tidy(matrix = "gamma") %>%
  ggplot(aes(factor(topic), gamma)) +
  geom_boxplot() +
  theme_minimal() +
  labs(x = "Temat",
       y = "% wiadomości, dla których \n jest to najważniejszy temat")

dev.copy(png,'topicproc12.png', height=200)
dev.off()

#interactive viz - two dimensional distance between topics and showing the words which are most strongly associated with those topics.
doc.length = rowSums(as.matrix(emails_dtm))
term.frequency = colSums(as.matrix(emails_dtm))
vocab = colnames(emails_dtm)
P = posterior(emails_lda_12 )
phi = P$term
theta = P$topics

# create the JSON object to feed the visualization:
email_vis_12= list(phi = phi,theta = theta,doc.length = doc.length, vocab = vocab,term.frequency = term.frequency)
library(LDAvis)
library(servr)
email_json_12 = createJSON(phi = email_vis_12$phi,theta = email_vis_12$theta,doc.length = email_vis_12$doc.length, vocab =colnames(phi),  term.frequency = email_vis_12$term.frequency)
serVis(email_json, out.dir = './', open.browser = TRUE)
