# text_mining_hillary_emails

This is part of my Master Thesis at SGH.

It contains text mining analysis of e-mails of Hillary Clinton (data source: Kaggle). During the US presidential election, the Democratic Party nominated Hillary Clinton. While she was the Secretary of State under Barack Obama, she used a personal email server. During her candidacy for president, the opposition party launched an investigation into whether classified information was not properly secured, which constitutes a crime. As part of the government investigation, thousands of emails were released to the public.

Analysis with R and SQL.

R
- Main analysis based on tidy data principles:
> library(tidytext)

- Data base requests:
> library(RSQLite)

- Organising text, data: 
> library(tidytext)
> library(dplyr)
> library(stringr)
> library(tidyr)
> library(reshape2)

- Sentiment analysis:
> library(tidytext)
> library(countrycode)
> library(qdapDictionaries)

- Topic modelling:
> library(tidytext)
> library(topicmodels)
> library(ldatuning)
> library(LDAvis)
> library(servr)

- Graphs analysis:
> library(igraph) 
> library(ggraph)

- Vizualizations:
> library(ggplot2)
> library(wordcloud)
> library(colorRamps)
> library(LDAvis)
> library(servr)
> library(ggraph)
