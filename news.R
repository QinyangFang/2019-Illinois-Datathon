getwd()
NYTIMES_KEY = "Y029ASiPvi3SRmLqAD4dA3zNGKxp8Xyk"
#devtools::install_github("mkearney/nytimes")
#install.packages(c("jsonlite", "tidytext", "dplyr","bindrcpp", "curl"))
library(jsonlite)
library(dplyr)
x <- fromJSON("http://api.nytimes.com/svc/search/v2/articlesearch.json?q=honeywell&api-key=Y029ASiPvi3SRmLqAD4dA3zNGKxp8Xyk",flatten=TRUE) %>%
  data.frame()
term <- "Synchrony Financial"
begin_date<-"20180101"
end_date<-"20181231"
baseurl <- paste0("http://api.nytimes.com/svc/search/v2/articlesearch.json?q=",term,
                  "&begin_date=",begin_date,"&end_date=",end_date,
                  "&facet_filter=true&api-key=",NYTIMES_KEY, sep="")
initialQuery <- fromJSON(baseurl)
maxPages <- round((initialQuery$response$meta$hits[1] / 10)-1) 
pages <- list()
for(i in 0:maxPages){
  nytSearch <- fromJSON(paste0(baseurl, "&page=", i), flatten = TRUE) %>% data.frame() 
  message("Retrieving page ", i)
  pages[[i+1]] <- nytSearch 
  Sys.sleep(1) 
}


#combine to one page
allNYTSearch <- rbind_pages(pages)

#remove duplicate articles
allNYTSearch <- unique(allNYTSearch)
#isolate snippets
snippets <- allNYTSearch$response.docs.snippet

#break apart sentences
#install.packages("tm")
library(dplyr)
library(tidyr)
library(tm)

words <-allNYTSearch %>% 
  mutate(word=strsplit(as.character(snippets), " ")) %>% 
  unnest(word)


#Bing lexicon
library(tidytext)
bing <- sentiments %>%
  filter(lexicon == "bing") %>%
  select(-score)

snippets_ensentiment3 <- words %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, index = response.docs.snippet, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

#merge with BING lexicon
words.bing <- merge(bing, words, by ="word")

#create subset of snippets and dates
dates <- subset(words.bing, select = c(response.docs.snippet, response.docs.pub_date))
#only_dates = substr(dates$response.docs.pub_date, start = 1, stop = 10)

#dates = merge(dates, only_dates, by = "response.docs.pub_date")


#merge dates with words.bing 1 (has sentiment score per snippet)
sentimentScores <- merge(words.bing, dates, by = "response.docs.snippet")
sentimentScores <- unique(sentimentScores)

#colnames(sentimentScores) <- c("snippet", "word", "negative", "positve", "dummy", "score", "pub_date")

#sentimentScores[, `Date` = NA]
#sentimentScores[, "response.docs.pub_date" = as.numeric("response.docs.pub_date")]
#setDT(sentimentScores)[, `Date` := format(as.Date(pub_date), "%Y/%m/%d") ]
#sentimentScores[,`Date` := as.Date(`Date`)]

library(ggplot2)
scores = merge(snippets_ensentiment3, sentimentScores, by.x = "index", by.y = "response.docs.snippet")
scores$response.docs.pub_date.x = substr(scores$response.docs.pub_date.x, start = 1, stop = 10)

#dates <- scores$response.docs.pub_date.x
#betterDates <- as.Date(dates, "%m/%d/%y")

ggplot(scores,aes(response.docs.pub_date.x, sentiment.x)) + geom_bar(stat = "identity", show.legend = FALSE,colour="orange")+ 
  xlab("Date") + ylab("Sentiment by Article") +
  ggtitle("Net Sentiment Score by article") +
  theme(axis.text.x = element_text(angle = 90, hjust = 5))

#install.packages("wordcloud")
library(wordcloud)
words_frequency <- as.data.frame(table(words$word))

words_frequency <- words_frequency[!(words_frequency$Var1 == "the" | words_frequency$Var1 == "and" | words_frequency$Var1 == "was"
                                     |words_frequency$Var1 == "to" | words_frequency$Var1 == "The" | words_frequency$Var1 == "of"
                                   |words_frequency$Var1 == "for" |words_frequency$Var1 == "as" | words_frequency$Var1 == "has" |words_frequency$Var1 == "we"
                                   |words_frequency$Var1 == "after" |words_frequency$Var1 == "a" |words_frequency$Var1 == "on"
                                   |words_frequency$Var1 == "be" |words_frequency$Var1 == "in" |words_frequency$Var1 == "an"
                                   |words_frequency$Var1 == "that" |words_frequency$Var1 == "must" |words_frequency$Var1 == "could"
                                   |words_frequency$Var1 == "will"|words_frequency$Var1 == "its" |words_frequency$Var1 == "Its"
                                   |words_frequency$Var1 == "it" |words_frequency$Var1 == "up" |words_frequency$Var1 == "each"
                                   |words_frequency$Var1 == "new" |words_frequency$Var1 == "have"|words_frequency$Var1 == "his"
                                   |words_frequency$Var1 == "is"|words_frequency$Var1 == "at"|words_frequency$Var1 == "are"|words_frequency$Var1 == "by"
                                   |words_frequency$Var1 == "But"|words_frequency$Var1 == "this"|words_frequency$Var1 == "A"
                                   |words_frequency$Var1 == "In"|words_frequency$Var1 == "I"|words_frequency$Var1 == "can"
                                   |words_frequency$Var1 == "with"|words_frequency$Var1 == "who"|words_frequency$Var1 == "from"
                                   |words_frequency$Var1 == "but"|words_frequency$Var1 == "also"|words_frequency$Var1 == "not"
                                   |words_frequency$Var1 == "where"|words_frequency$Var1 == "been"|words_frequency$Var1 == "had"
                                   |words_frequency$Var1 == "An"|words_frequency$Var1 == "he"|words_frequency$Var1 == "him"
                                   |words_frequency$Var1 == "their"|words_frequency$Var1 == "many"|words_frequency$Var1 == "there"
                                   |words_frequency$Var1 == "still" |words_frequency$Var1 == "about"|words_frequency$Var1 == "Mr."),]

#removePunctuation(words_frequency)
#removeNumbers(words_frequency)
#removeWords(words_frequency, stopwords("en"))

wordcloud(words = words_frequency$Var1, freq = words_frequency$Freq, min.freq = 1,
          max.words=80, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

