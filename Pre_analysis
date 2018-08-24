setwd("C:/Users/Monika Sienkiewicz/Documents/text_mining_hillary")
library(RSQLite)
library(dplyr)
library(ggplot2)

#### Import ####
#Import emails where sender = Hillary as data frame
db <- dbConnect(dbDriver("SQLite"), "/Users/Monika Sienkiewicz/Documents/text_mining_hillary/database.sqlite")
emails <- dbGetQuery(db, "SELECT ExtractedBodyText EmailBody FROM Emails e INNER JOIN Persons p ON e.SenderPersonId=P.Id WHERE p.Name='Hillary Clinton'  AND e.ExtractedBodyText != '' ORDER BY RANDOM()")


#Show tables in data set
tables <- dbGetQuery(db, "SELECT Name FROM sqlite_master WHERE type='table'")
colnames(tables) <- c("Name")
tables <- tables %>%
  rowwise() %>%
  mutate(RowCount=dbGetQuery(db, paste0("SELECT COUNT(Id) RowCount FROM ", Name))$RowCount[1])
print.table(tables)


##Common Senders
commonSenders <- dbGetQuery(db, "
                            SELECT p.Name, COUNT(p.Name) NumEmailsSent
                            FROM Emails e
                            INNER JOIN Persons p ON e.SenderPersonId=p.Id
                            GROUP BY p.Name
                            ORDER BY COUNT(p.Name) DESC
                            LIMIT 10")

 ggplot(commonSenders, aes(x=reorder(Name, NumEmailsSent), y=NumEmailsSent)) +
  geom_bar(stat="identity", fill="#53cfff") +
  coord_flip() + 
  theme_light(base_size=12) +
  xlab("") +
  ylab("No of sent e-mails") + 
  theme(plot.title=element_text(size=12))+
  scale_y_continuous(limits = c(0, 6000), breaks=c(0, 1000, 2000, 3000, 4000, 5000, 6000))

dev.copy(png,'sent.png', height=200)
dev.off()

##Common Recipients
commonRecipients <- dbGetQuery(db, "
SELECT p.Name, COUNT(p.Name) NumEmailsReceived
FROM Emails e
INNER JOIN EmailReceivers r ON r.EmailId=e.Id
INNER JOIN Persons p ON r.PersonId=p.Id
GROUP BY p.Name
ORDER BY COUNT(p.Name) DESC
LIMIT 10")

ggplot(commonRecipients, aes(x=reorder(Name, NumEmailsReceived), y=NumEmailsReceived)) +
  geom_bar(stat="identity", fill="#53cfff") +
  coord_flip() + 
  theme_light(base_size=12) +
  xlab("") + 
  ylab("No of received e-mails") + 
  theme(plot.title=element_text(size=10)) +
  scale_y_continuous(limits = c(0, 6000), breaks=c(0, 1000, 2000, 3000, 4000, 5000, 6000))

dev.copy(png,'received.png', height=200)
dev.off()


## Most common subjects
commonSubjects <- dbGetQuery(db, "
SELECT MetadataSubject Subject,
       COUNT(Id) NumberOfOccurences
FROM Emails
GROUP BY MetaDataSubject
ORDER BY COUNT(Id) DESC")

commonSubjects2 <- commonSubjects %>%
  filter(NumberOfOccurences >= 15)

library(ggplot2)
ggplot(commonSubjects2, aes(x=reorder(Subject, NumberOfOccurences), y=NumberOfOccurences)) +
  geom_bar(stat="identity", fill="royalblue2") +
  coord_flip() + 
  theme_light(base_size=12) +
  xlab("") + 
  ylab("Common subjects") + 
  theme(plot.title=element_text(size=12))

dev.copy(png,'commonsubjects.png', height=400)
dev.off()

##Predicting gender based on a name

#install.packages("gender")
#install.packages("genderdata", repos = "http://packages.ropensci.org")
#vignette(topic = "predicting-gender", package = "gender")

library(gender)
library(genderdata)
library(tidyr)

##predict gender of recipient
commonRecipients2 <- commonRecipients %>%
  extract(Name, c("FirstName", "LastName"), "([^ ]+) (.*)")
  
#method = "ssa": United States from 1930 to 2012. Drawn from Social Security Administration data.
CommonRecipients_gender <- gender(commonRecipients2$FirstName, years=c(1930, 1975), method="ssa")
print(CommonRecipients_gender)

CommonRecipients_gender1 <- CommonRecipients_gender %>%
  select(name, proportion_male, proportion_female)

library(reshape2)
DF1 <- melt(CommonRecipients_gender1, id.var="name")

library(ggplot2)
ggplot(DF1, aes(x=name, y=value, fill=variable)) +
  geom_bar(stat="identity") +
  coord_flip() + 
  theme_light(base_size=10) +
  xlab("") + 
  ylab("Płeć") + 
  scale_fill_brewer( palette = "Oranges") +
  theme(plot.title=element_text(size=10))
