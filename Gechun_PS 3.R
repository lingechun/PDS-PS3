#1
#download and subset data
library(ggplot2)
primaryPolls<-read.csv('https://jmontgomery.github.io/PDS/Datasets/president_primary_polls_feb2020.csv', stringsAsFactors = F)
primaryPolls$start_date<-as.Date(primaryPolls$start_date, "%m/%d/%Y")
primaryPolls<-primaryPolls[primaryPolls$state%in%c("Alabama", "Arkansas", "California", "Colorado", "Maine", "Massachusetts", "Minnesota", "North Carolina", "Oklahoma", "Tennessee", "Texas", "Utah", "Vermont", "Virginia"),]
primaryPolls<-primaryPolls[primaryPolls$candidate_name%in%unique(primaryPolls$candidate_name),]
#plot by state
ggplot(data=primaryPolls)+
  scale_shape_manual(values = 1:nlevels(primaryPolls$candidate_name))+
  geom_point(mapping = aes(x=start_date, y=pct,  color=candidate_name), alpha=.8)+
  facet_wrap(~ state, nrow=5)
#change to the minimial theme
pl <- ggplot(data=primaryPolls)+
  scale_shape_manual(values = 1:nlevels(primaryPolls$candidate_name))+
  geom_point(mapping = aes(x=start_date, y=pct,  color=candidate_name), alpha=.8)+
  facet_wrap(~ state, nrow=5)
pl + theme_minimal()
#change the axis labels and legends
pl + labs(x="Start Date", y="Percentage") + theme(legend.position="bottom")

#2
#re-organize the dataset so that there is only one row for each candidate-state dyad
primaryPolls<-read.csv('https://jmontgomery.github.io/PDS/Datasets/president_primary_polls_feb2020.csv', stringsAsFactors = F)
primaryPolls$start_date<-as.Date(primaryPolls$start_date, "%m/%d/%Y")
candidate.state <- summarise(group_by(primaryPolls, candidate_name, state), count=n())

#compare the size of this dataset to our original dataset using the object_size command
print(object.size(primaryPolls),units="auto")
print(object.size(candidate.state), units="auto")

#3
library(fivethirtyeight)
library(tidyverse)
library(dplyr)
polls <- read_csv(file="https://jmontgomery.github.io/PDS/Datasets/president_primary_polls_feb2020.csv")
Endorsements <- endorsements_2020
#change the Endorsements variable name endorsee to candidate_name
Endorsements <- rename(Endorsements, candidate_name=endorsee)
#change the Endorsements dataframe into a tibble object
as_tibble(Endorsements)
#filter the poll variable to only include the following 6 candidates
polls.6candidate <- filter(polls, candidate_name %in% c("Amy Klobuchar", "Bernard Sanders", "Elizabeth Warren", "Joseph R. Biden Jr.", "Michael Bloomberg", "Pete Buttigieg"))
#subset the dataset to the following 5 variables
select(polls.6candidate, candidate_name, sample_size, start_date, party, pct)
#compare the candidate names
unique(Endorsements$candidate_name)
unique(polls.6candidate$candidate_name)
#make these names the same across datasets
Endorsements <- Endorsements %>%
  mutate(candidate_name=str_replace(Endorsements$candidate_name, "Bernie Sanders", "Bernard Sanders")) 
Endorsements <- Endorsements %>%
  mutate(candidate_name=str_replace(Endorsements$candidate_name, "Joe Biden", "Joseph R. Biden Jr."))
#combine the two datasets by candidate name
combine <- Endorsements %>%
  inner_join(polls.6candidate, by="candidate_name")
#create a variable which indicates the number of endorsements for each of the five candidates
combine.counts <- combine %>% count(candidate_name)
#plot the number of endorsement each of the 5 candidates have
p <- ggplot(data=combine.counts)+
  scale_shape_manual(values = 1:nlevels(combine.counts$candidate_name))+
  geom_point(mapping = aes(x=candidate_name, y=n,  color=candidate_name), alpha=.8)
p
p + theme_dark()
#using the knowledge from the last step change the label of the X and Y axes to be more informative, add a title, and use your favorite theme
p + labs(x="Candidates", y="Number of Endorsements", title="Each Candidate's Endorsements") + theme_light()


#4
library(RColorBrewer)
library(tidyverse) 
library(tm)  
library(lubridate) 
library(wordcloud)
library(SnowballC)
tweets <- read_csv('https://politicaldatascience.com/PDS/Datasets/trump_tweets.csv')
#separate the created_at variable into two new variables where the date and the time are in separate columns
tweets <- tweets %>%
  separate(created_at, c("date", "time"), " ")
#report the range of dates that is in this dataset
range(tweets$date)
#subset the data to only include original tweets
original.tweets <- filter(tweets, is_retweet=="FALSE")
#show the text of the President’s top 5 most popular and most retweeted tweets
popular <- original.tweets %>% mutate(popular.rank = rank(desc(favorite_count), ties.method="first"))
retweeted <- original.tweets %>% mutate(retweeted.rank = rank(desc(retweet_count), ties.method="first"))
filter(popular, popular.rank%in%c(1:5))$text
filter(retweeted, retweeted.rank%in%c(1:5))$text
#remove punctuation and number
a <- str_replace_all(string=original.tweets$text, pattern = "[&â€¦™ðŸ¥]", replacement="")
text<- Corpus(VectorSource(a))
text <- tm_map(text, removePunctuation)
text <- tm_map(text, removeNumbers)
#convert to lower case
text <-tm_map(text, content_transformer(tolower))
#remove the standard english stop words
text <- tm_map(text,removeWords, stopwords("english"))
#remove the following words
text <- tm_map(text, removeWords, c("see", "people","new","want","one",
                                         "even","must","need", "done","back",
                                         "just","going", "know", "can", "said",
                                         "like","many","like","realdonaldtrump"))
#remove extraneous whitespace
text <- tm_map(text, stripWhitespace)
text <- tm_map(text, stemDocument)

#unlist the text and split them into words
words <- str_c(unlist(text))
words <- str_split(words, pattern = " ")
words <- unlist(words)
words <- as.tibble(words)
words <- rename(words, word=value)
words <- words %>% count(word)
top50 <- words %>% mutate(rank = rank(desc(n), ties.method="first")) %>% filter(rank%in%c(1:50))  
#visualize the top 50 words
wordcloud(words = top50$word, freq = top50$n, min.freq = 3, max.words=50,
          random.order=FALSE, rot.per=0.1, 
          colors=brewer.pal(8, "Accent"))
#create a document term matrix called DTM
DTM <- TermDocumentMatrix(text, control = list(weighting = weightTfIdf))

#report the 50 words with the the highest tf.idf scores using a lower frequency bound of .8
DTM
