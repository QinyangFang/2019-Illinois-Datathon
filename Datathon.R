install.packages(c("tidytext", "bindrcpp", "ngram"))


#demo = read.csv("Honeywell-sample.csv")
demo_text = scan("Honeywell-sample.csv", "character", sep = " ")
demo_text

library("ngram")
library("tm")
library(coreNLP)
library(openNLP)
library(tidyverse)
library(topicmodels)
test = strsplit(demo_text, split = "")

library(tidytext)
library(janeaustenr)
library(dplyr)
tidy_text = demo_text %>%
  unnest_tokens(word, text)

demo_text_tbl = tally(demo_text)
count(demo_text_tbl, sort = TRUE)

tidy_demo= demo_text %>%
  anti_join(get_stopwords())

tidy_demo %>%
  count(text, sort = TRUE)

library(tidyr)
get_sentiments("bing")