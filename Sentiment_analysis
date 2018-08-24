library(RSQLite)
library(ggplot2)
library(dplyr)
library(tidytext)
library(stringr)
library(tidyr)
library(wordcloud)
library(reshape2)
library(colorRamps)
library(countrycode)
library(qdapDictionaries)
setwd("C:/Users/Monika Sienkiewicz/Documents/text_mining_hillary")

### Import ####
#import emails where sender = hillary
db <- dbConnect(dbDriver("SQLite"), "/Users/Monika Sienkiewicz/Documents/text_mining_hillary/database.sqlite")
emails <- dbGetQuery(db, "SELECT ExtractedBodyText EmailBody FROM Emails e INNER JOIN Persons p ON e.SenderPersonId=P.Id WHERE p.Name='Hillary Clinton'  AND e.ExtractedBodyText != '' ORDER BY RANDOM()")

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
  
#stop words are e.g. ...
stop_words %>%
  count(word) %>%
  with(wordcloud(word, max.words=500, scale=c(4,.5), use.r.layout=FALSE,random.order=TRUE, rot.per=0.35, colors=brewer.pal(8, "Dark2") ) )
dev.copy(png,'stopwordcloud.png')
dev.off()

#simple word counts plot
library(ggplot2)
tidy_emails %>%
  count(word, sort = TRUE) %>%
  filter(n > 30) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col(fill="darkcyan") +
  xlab(NULL) +
  coord_flip() +
  theme_light(base_size=12) +
  ylab("Najczęściej występujące słowa") + 
  geom_text(aes(label=n), colour="white",hjust=1.1, size=3)
dev.copy(png,'commonwords.png', height=650)
dev.off()

####Word clouds all emails####
tidy_emails %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100, colors=(rainbow(10)) ))
dev.copy(png,'wordcloudGenrainbow.png')
dev.off()

#### SENTIMENT ANALYSIS ####
##sentiment dictionaries general analysis
bingdraft <- get_sentiments("bing") %>% 
  count(sentiment)

nrcdraft <- get_sentiments("nrc") %>% 
  count(sentiment)

nrcdraftPN <- get_sentiments("nrc") %>% 
  filter(sentiment %in% c("positive", 
  "negative","joy", "")) %>% 
  count(sentiment)

affindraft <- get_sentiments("afinn") 

loughrandraft <- get_sentiments("loughran") %>% 
  count(sentiment)

###BING sentiment ####

bing_word_counts <- tidy_emails %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  theme_light(base_size=13) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Udział słowa w sentymencie [Bing]",
       x = NULL) +
  coord_flip()
dev.copy(png,'bing sentiment.png',height=200)
dev.off()

#word cloud
library(reshape2)
tidy_emails %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("red2", "green3"),
                   max.words = 1000, random.order = TRUE, title.size = 8) 

dev.copy(png,'BingSentimentRedGreen.png', height=500, width=500)
dev.off()

#####NRC sentiment ####
nrc_word_counts <- tidy_emails %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

nrc_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  theme_light(base_size=15) +
  theme(strip.text=element_text(color="black"), strip.background=element_rect(fill="grey96"))+
  facet_wrap(~sentiment, scales = "free", ncol=3) +
  labs(y = "Udział słowa w sentymencie [NRC]",
       x = NULL) +
  coord_flip()
dev.copy(png,'NRC Sentiment.png', height=850, width=1000)
dev.off()


####financial dictionary loughran sentiment####
tidy_emails %>%
  count(word) %>%
  inner_join(get_sentiments("loughran"), by = "word") %>%
  group_by(sentiment) %>%
  top_n(5, n) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col() +
  theme_light(base_size=18) +
  scale_fill_discrete(name="Sentyment",
                      breaks=c("constraining", "litigious", "negative", "positive", "superfluous", "uncertainty"),
                      labels=c("ograniczające", "prawnicze", "negatywne", "pozytywne", "zbędne", "niepewne")) +
  coord_flip() +
  facet_wrap(~ sentiment, scales = "free_y", ncol = 2) +
  ylab("Częstość słów danego typu [Loughran]") +
  xlab(NULL)

dev.copy(png,'Fin Sentiment.png', height=550, width=1000)
dev.off()

####affin sentiment####
afinn_wiz <- tidy_emails %>% 
  inner_join(get_sentiments("afinn")) %>% 
  #recalculate values from (-5,5) into positive / negative categories
  mutate(sentiment_2= ifelse(score>0, "Positive","Negative")) %>%
  count(word, sentiment_2, sort = TRUE) %>%
  ungroup()

head(afinn_wiz)

afinn_wiz %>%
  group_by(sentiment_2) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment_2)) +
  geom_col(show.legend = FALSE) +
  theme_light(base_size=13) +
  facet_wrap(~sentiment_2, scales = "free") +
  labs(y = "Udział słowa w sentymencie [Afinn]",
       x = NULL) +
  coord_flip()

dev.copy(png,'afinn Sentiment.png', height=200)
dev.off()

####compare 4 sentiment dictionaries - sentiment per 1 mail ####

#data cleaning
afinn_1 <- tidy_emails %>% 
  inner_join(get_sentiments("afinn")) %>% 
  group_by(emailnumber) %>% 
  summarise(sentiment = sum(score)) %>% 
  mutate(method = "AFINN")

head(afinn_1)

bing_and_nrc_1 <- bind_rows(tidy_emails %>% 
                              inner_join(get_sentiments("bing")) %>%
                              mutate(method = "Bing et al."),
                            tidy_emails %>% 
                              inner_join(get_sentiments("nrc") %>% 
                                           filter(sentiment %in% c("positive", 
                                                                   "negative"))) %>%
                              mutate(method = "NRC")) %>%
  count(method, emailnumber, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

fin_dic_1 <- tidy_emails %>% 
  inner_join(get_sentiments("loughran") %>% 
               filter(sentiment %in% c("positive", 
                                       "negative"))) %>%
  mutate(method = "Loughran") %>%
  count(method, emailnumber, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

#plot - it isn't very easy to analyse
bind_rows(afinn_1, bing_and_nrc_1, fin_dic_1) %>%
  ggplot(aes(emailnumber, sentiment, fill = method)) +
  geom_col(show.legend = FALSE) +
  theme_light(base_size=18) +
  ylab("Sentyment")+
  xlab("Numer Emaila")+
  facet_wrap(~method, ncol = 1, scales = "free_y") +
  theme(strip.text=element_text(color="black"), strip.background=element_rect(fill="grey96"))+
  scale_fill_brewer( palette = "Dark2")
  
dev.copy(png,'Compare4dict.png', width=1000, height=1000)
dev.off()

#plot 2 - based on smoothing GAM - much easier to analyse
bind_rows(afinn_1, bing_and_nrc_1, fin_dic_1) %>%
  ggplot(aes(emailnumber, sentiment, fill = method)) +
  theme_light(base_size=48) +
  ylab("Sentyment")+
  stat_smooth(show.legend = FALSE) +
  xlab("Numer Emaila")+
  facet_wrap(~method, ncol = 1) +
  theme(strip.text=element_text(color="black"), strip.background=element_rect(fill="grey96"))
  
dev.copy(png,'Compare4dictsmoothbig.png', width=2480, height=3508)
dev.off()

####COUNTRY SENTIMENT######

# Sentiment analysis from ghassent's script:
# https://www.kaggle.com/ghassent/hillary-clinton-emails/sentiments-text-mining-and-more/discussion

# Country analysis from Olalekan's script:
# https://www.kaggle.com/ampaho/hillary-clinton-emails/countries-the-most-mentioned-by-hillary

# country coding cleansing, courtesy Olalekan
count.occurences <- function(needle, haystack)
{
  sapply(regmatches(haystack, gregexpr(needle, haystack, ignore.case=T, perl=T)), length)
}
#changed countrycode_data to codelist
data("codelist")

countrycode_data_without_atf <- codelist[-83,]
countries <- countrycode_data_without_atf[, c("country.name.en", "country.name.en.regex", "iso2c", "iso3c")]
#changed country.name.en and country.name.en.regex
countries$other <- NA
countries[countries$country.name.en=="United Kingdom",]$other <- "UK"
words_to_remove <- rbind(DICTIONARY[nchar(DICTIONARY$word)==2,], DICTIONARY[nchar(DICTIONARY$word)==3,])
words_to_be_removed <- toupper(c(words_to_remove$word, "RE", "FM", "TV", "AL", "AQ", "LA", "BEN"))

#add "RE" because the iso3 code for Reunion islands.. but it appears a lot in emails to indicate the RE(sponses) to previous emails.
#AL is a given name and also ISO2 for Albania
#BEN is a given name and also ISO3 for Benin
#LA is Los angeles and iso 2 for Liberia
#AQ is abbreviation of "As Quoted" and iso 2 for Antarctica


# Now process all e-mails
allAnalysis <- NULL
for (n in 1:nrow(emails)) {
  if ((n %% 50) == 0) print(paste(trunc(100*n/nrow(emails)),"%"))
  email <- emails$EmailBody[n]
  sentiment <- get_sentiments(email, method="nrc")
  
  for(i in 1:nrow(countries)) {
    n_occurences <- 0
    tmp <- 0
    # Find countries by matching a regexp
    if(!is.na(countries$country.name.en.regex[i])) {
      tmp <- count.occurences(countries$country.name.en.regex[i], email)
      n_occurences <- n_occurences + tmp
    }
    # Find countries by abbreviation #remove words that are ISO2 iso3 country codes
    for (countryAlt in c(3:5)) {
      if( (! (countries[i,countryAlt] %in% words_to_be_removed) ) && (!is.na(countries[i,countryAlt]))  ) {
        iso_boundary <- paste0("\\s", countries[i,countryAlt], "\\s")
        tmp <- count.occurences(iso_boundary, email)
        n_occurences <- n_occurences + tmp
      }
    }
    # Find countries by literal match
    if(tmp <= 0) {
      tmp <- count.occurences(countries$country.name.en[i], email)
      n_occurences <- n_occurences + tmp
    }
    
    if (n_occurences > 0) { 
      country <- countries$country.name.en[i]
      if (is.null(allAnalysis)) {
        allAnalysis <- cbind(sentiment, country)
      } else {
        allAnalysis <- rbind(allAnalysis, cbind(sentiment, country))
      }
    }
  }
}



df <- group_by(allAnalysis, country) %>% 
  summarise(count=n(),
            Sentiment=sum(joy)+sum(trust)-sum(anger)-sum(disgust)-sum(fear)-sum(sadness),
            anger=sum(anger), 
            anticipation=sum(anticipation),
            disgust=sum(disgust), 
            fear=sum(fear), 
            joy=sum(joy), 
            sadness=sum(sadness),
            surprise=sum(surprise),
            trust=sum(trust), 
            negative=sum(negative), 
            positive=sum(positive))%>% 
  group_by(Sentiment)


df$Sentiment <- df$Sentiment / (max(df$Sentiment)-min(df$Sentiment))

df2 <- df %>%
  group_by(Sentiment) 

#plot
p3 <- ggplot(filter(df2, count >= 2), aes(x=reorder(country, -Sentiment), y=count, fill=Sentiment))+
  geom_bar(stat="identity", position = "identity")+
  labs(x=NULL, y="Liczba wystąpień", title="Kraje a sentyment emaili Hillary Clinton") + 
  theme_minimal(base_size=18)+
  theme(axis.text.x = element_text(size  = 16, angle = 45, hjust = 1, vjust = 1)) +
  scale_fill_gradient(low="blue",high="gold")

print(p3)
dev.copy(png,'countrysentiment3.png', width=1000, height=750)
dev.off()


#Now process all e-mails again - changed way to calculate sentiment - nrc had diffrent outcome on dict comparision above. now based on bing
allAnalysis_bing <- NULL
for (n in 1:nrow(emails)) {
  if ((n %% 50) == 0) print(paste(trunc(100*n/nrow(emails)),"%"))
  email <- emails$EmailBody[n]
  sentiment <- get_sentiment(email, method="bing")

  for(i in 1:nrow(countries)) {
    n_occurences <- 0
    tmp <- 0
    # Find countries by matching a regexp
    if(!is.na(countries$country.name.en.regex[i])) {
      tmp <- count.occurences(countries$country.name.en.regex[i], email)
      n_occurences <- n_occurences + tmp
    }
    # Find countries by abbreviation #remove words that are ISO2 iso3 country codes
    for (countryAlt in c(3:5)) {
      if( (! (countries[i,countryAlt] %in% words_to_be_removed) ) && (!is.na(countries[i,countryAlt]))  ) {
        iso_boundary <- paste0("\\s", countries[i,countryAlt], "\\s")
        tmp <- count.occurences(iso_boundary, email)
        n_occurences <- n_occurences + tmp
      }
    }
    # Find countries by literal match
    if(tmp <= 0) {
      tmp <- count.occurences(countries$country.name.en[i], email)
      n_occurences <- n_occurences + tmp
    }

    if (n_occurences > 0) {
      country <- countries$country.name.en[i]
      if (is.null(allAnalysis_bing)) {
        allAnalysis_bing <- cbind(sentiment, country)
      } else {
        allAnalysis_bing <- rbind(allAnalysis_bing, cbind(sentiment, country))
      }
    }
  }
}

library(sqldf)
df_bing2 <- sqldf("select country, count(country) as count, sum(sentiment) as Sentiment from df_bing group by country;")

df_bing2$Sentiment <- df_bing2$Sentiment / (max(df_bing2$Sentiment)-min(df_bing2$Sentiment))

df_bing3 <- df_bing2 %>%
  group_by(Sentiment)

#plot for bing
p3 <- ggplot(filter(df_bing3, count >= 2), aes(x=reorder(country, -Sentiment), y=count, fill=Sentiment))+
  geom_bar(stat="identity", position = "identity")+
  labs(x=NULL, y="Liczba wystąpień", title="Kraje a sentyment emaili Hillary Clinton [Bing]") +
  theme_minimal(base_size=18)+
  theme(axis.text.x = element_text(size  = 16, angle = 45, hjust = 1, vjust = 1)) +
  scale_fill_gradient2(low="darkorchid3",mid="gold2", high="darkorange1")

print(p3)
dev.copy(png,'countrysentiment3_bing.png', width=1000, height=750)
dev.off()
