#connect all libraries
library(twitteR)
library(ROAuth)
library(plyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(purrr)
library(tm)
library(SnowballC)
library(lubridate)
library(scales)
library(tidyr)
library(tidytext)
library(broom)
library(wordcloud)
#connect to API

setup_twitter_oauth("IPOmcmCGR7bhD9kACIk2V1M7w",
                    "hbm6GQvF8Pw7Olg8Y5yXJ6ni6xlttuNQSFX0ScwBKoyUo2VU1m",
                    "3092398967-Xj4Ku5JphlEqYdGKC0pz5KL9Ysg55TB6NKAZe5g",
                    "9rRZlPEzMtd73wVf6JLhQjfxjwhkc50wGko1gH4gsKcW7")

#comparing russian and english sentiments
homeopathy_ru_tweets=searchTwitter("Гомеопатия",lang="ru",n=81,since='2000-04-5')
homeopathy_tweets=searchTwitter("Homeopathy",lang="en",n=81,since='2000-04-5')

tweets_to_text <- function(homeopathy_ru_tweets_, gsub) {
  homeopathy_ru_list=sapply (homeopathy_ru_tweets_, function(x) x$getText())
  #homeopathy_ru_list[3]
  homeopathy_ru_list=gsub("https://t.co/[A-Za-z\\d]+|&amp;", " ", homeopathy_ru_list)
  #homeopathy_ru_list[3]
  homeopathy_ru_list = gsub(gsub, " ", homeopathy_ru_list)
  homeopathy_ru_list=gsub("com", " ", homeopathy_ru_list)
  #homeopathy_ru_list[3]
  return(homeopathy_ru_list)
}

homeopathy_ru_list <- tweets_to_text(homeopathy_ru_tweets, "[^А-Яа-я]")
homeopathy_list <- tweets_to_text(homeopathy_tweets, "[^A-Za-z]") 

tweet_to_corpus <- function(homeopathy_ru_list_, language) {
  homeopathy_ru_corpus=Corpus(VectorSource(homeopathy_ru_list))
  # Convert to lower-case
  homeopathy_ru_corpus=tm_map(homeopathy_ru_corpus,tolower)
  # Remove stopwords; other words that are not relevant to the analysis such as http(s) can also be included
  homeopathy_ru_corpus=tm_map(homeopathy_ru_corpus,function(x) removeWords(x,stopwords(kind=language)))
  # convert corpus to a Plain Text Document
  homeopathy_ru_corpus=tm_map(homeopathy_ru_corpus, PlainTextDocument)
}
# Create corpus
#homeopathy_corpus <- tweet_to_corpus(homeopathy_list, "en")
#homeopathy_ru_corpus <- tweet_to_corpus(homeopathy_ru_list, "ru")

#homeopathy_ru_tweets_df <- tbl_df(map_df(homeopathy_ru_tweets, as.data.frame))
#homeopathy_tweets_df <- tbl_df(map_df(homeopathy_tweets, as.data.frame))

score.sentiment = function(sentences, pos.words, neg.words)
{
  scores = laply(sentences,
                 function(sentence, pos.words, neg.words)
                 {
                   # remove punctuation - using global substitute
                   sentence = gsub("[[:punct:]]", "", sentence)
                   # remove control characters
                   sentence = gsub("[[:cntrl:]]", "", sentence)
                   # remove digits
                   sentence = gsub('\\d+', '', sentence)
                   # to lower case
                   sentence = sapply(sentence, tolower)
                   # split sentence into words with str_split (stringr package)
                   word.list = str_split(sentence, "\\s+")
                   words = unlist(word.list)
                   # compare words to the dictionaries of positive & negative terms
                   pos.matches = match(words, pos.words)
                   neg.matches = match(words, neg.words)
                   # get the position of the matched term or NA
                   # we just want a TRUE/FALSE
                   pos.matches = !is.na(pos.matches)
                   neg.matches = !is.na(neg.matches)
                   # final score
                   score = sum(pos.matches) - sum(neg.matches)
                   return(score)
                 }, pos.words, neg.words)
  # data frame with scores for each sentence
  scores.df = data.frame(text=sentences, score=scores)
  return(scores.df)
}

pos = readLines("negative_words")
neg = readLines("positive_words")
pos_ru = readLines("negative_words_ru.txt")
neg_ru = readLines("positive_words_ru.txt")

scores_ru = score.sentiment(homeopathy_ru_list, pos_ru, neg_ru)
scores = score.sentiment(homeopathy_list, pos, neg)
#typeof(homeopathy_list)
#homeopathy_list[1]
#scores_ru

#Display
dev.off()
par(mar=c(3,3,5,1))   # extra large right margin
my.bin.width <- 1
plot.new()
par(xpd=TRUE)
par(mar=c(3,3,5,5))   # extra large bottom margin
my.bin.width<-1
hist(main = "Distribution of sentiment by language",scores_ru[,2],axes = F, xlab=" ", col="green", breaks=seq(-4,3, by=my.bin.width),border=F)
hist(scores[,2], axes = F, col=scales::alpha('skyblue',.6), xlab=" ", breaks=seq(-4, 4, by=my.bin.width), add=T, border=F)
axis(1, at = c(-4, 4), labels = c("negative", "positive"), pos = 3, tick = FALSE)
legend("right", inset=.05, title="",
       c("russian","english"), fill=c("green", "skyblue"), 
       xjust = 0.5, border = FALSE, text.width = 1, ncol = 1, horiz = FALSE,
       bty='n', cex=0.75)
#
#
###################Take more egnlish tweets for deeper analysis##################

homeopathy_en_tweets=searchTwitter("Homeopathy",lang="en",n=2000,since='2000-04-5')
homeopathy_tweets_df <- tbl_df(map_df(homeopathy_en_tweets, as.data.frame))
#homeopathy_ru_list=sapply(homeopathy_ru_tweets,function(x) x$getText())
cleaned_text <- tweets_to_text(homeopathy_en_tweets, "[^A-Za-z]")
homeopathy_tweets_df[,'cleaned'] <- cleaned_text
#adding column with sentiment to df
scores_en = score.sentiment(homeopathy_tweets_df[,'cleaned'][[1]], pos, neg)
homeopathy_tweets_df[,'score'] <- scores_en[,2]

#identify source
tweets <- homeopathy_tweets_df %>%
  select(id, statusSource, text, cleaned, score, created) %>%
  extract(statusSource, "source", "Twitter for (.*?)<") 

#split tweets by word
reg <- "([^A-Za-z\\d#@']|'(?![A-Za-z\\d#@]))"
tweet_words <- tweets %>%
  filter(!str_detect(cleaned, '^"')) %>%
  unnest_tokens(word, cleaned, token = "regex", pattern = reg) %>%
  filter(!word %in% stop_words$word,
         str_detect(word, "[a-z]"))

#adding sentiments
tweet_words[,'score'] <- ifelse(tweet_words$word %in% pos, 1, ifelse(tweet_words$word %in% neg, -1, 0))
tweet_words <- tweet_words[!is.na(tweet_words[,4]),]

#splitting by resource
android <- na.omit(tweet_words[tweet_words[,2] == "Android" ,c(2,3,4,6)])
iphone <- na.omit(tweet_words[tweet_words[,2] == "iPhone" ,c(2,3,4,6)])
mac_and_ipad <- na.omit(tweet_words[tweet_words[,2] == "Mac" | tweet_words[,2] == "iPad",c(2,3,4,6)])
#android[,3] <- sapply(android[,3], function(x) as.numeric(x))
#typeof(android[1,3]$score)

dev.off()
par(mar=c(3,3,5,1))   # extra large bottom margin
my.bin.width <- 1
par(xpd=TRUE)
plot.new()
hist(main = "Distribution by source", android$score, axes = F, xlab=" ",col=scales::alpha('blue',.5),breaks=seq(-1.5,1.5,by=my.bin.width), border=F, add=F)
legend(-1,4500, inset=.05, title="",
       c("android","iphone","mac and ipad"), fill=c("blue", "pink", "purple"), 
       xjust = 0.5, border = FALSE, text.width = 1, ncol = 1, horiz = FALSE,
       bty='n', cex=0.75)
axis(1, at = c(-1.7, 1.3), labels = c("negative", "positive"), pos = 10, tick = FALSE)

hist(main = "blue: iphone", iphone$score, axes = F, col=scales::alpha('pink',.5),border=F, xlab=" ",breaks=seq(-1.6,1.4,by=my.bin.width), add=T)

hist(main = "pink: mac and ipad", mac_and_ipad$score, axes = F, col=scales::alpha('purple',.5),border=F, xlab=" ",breaks=seq(-1.7,1.3,by=my.bin.width), add=T)


#group by words 
by_words <- tweet_words %>%
  group_by(word) %>%
  mutate(total_words = n()) %>%
  ungroup() %>%
  distinct(word, total_words) %>%
  arrange(desc(total_words))

#by_words
by_words <- by_words[-c(1,2,3), ]

#filter only the words from NRC
nrc <- sentiments %>%
  filter(lexicon == "nrc") %>%
  dplyr::select(word, sentiment)

#join tweet words with NRC
by_sentiment <- tweet_words %>%
  inner_join(nrc, by = "word") %>%
  count(sentiment, id) %>%
  ungroup() %>%
  complete(sentiment, id, fill = list(n = 0)) %>%
  group_by(sentiment) %>%
  summarize(words = sum(n)) %>%
  ungroup() %>%
  arrange(desc(words))

#head(by_sentiment)
#word_sentiment[1,]


pie(by_sentiment$words, by_sentiment$sentiment, main="Top emotions")

pie(by_words[1:20,]$total_words, by_words[1:20,]$word, main="Top popular words")
