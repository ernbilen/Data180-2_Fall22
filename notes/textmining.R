
#############
# Text Mining
# nomination speech

library(tm) # text mining package

# Instead of that, just read in the speech
charVector <- scan("https://raw.githubusercontent.com/ernbilen/Data180-2_Fall22/main/data/speech.txt", character(0), sep = "\n")
head(charVector)


posWords <- scan("https://raw.githubusercontent.com/ernbilen/Data180-2_Fall22/main/data/positive-words.txt", character(0), sep = "\n")  # 2006 items
negWords <- scan("https://raw.githubusercontent.com/ernbilen/Data180-2_Fall22/main/data/negative-words.txt", character(0), sep = "\n")  # 4783 items
head(posWords,15)
head(negWords,15)



# cleaning part!!
wordVector <- VectorSource(charVector) # from tm package, convert to vector
class(wordVector); typeof(wordVector); length(wordVector)
wordCorpus <- Corpus(wordVector) # convert to corpus
class(wordCorpus); typeof(wordCorpus); length(wordCorpus)


# first step transformation: make all of the letters in "wordCorpus" lowercase
wordCorpus <- tm_map(wordCorpus, content_transformer(tolower))
# second step transformation: remove the punctuation in "wordCorpus"
wordCorpus <- tm_map(wordCorpus, removePunctuation)
# third step transformation: remove numbers in "wordCorpus"
wordCorpus <- tm_map(wordCorpus, removeNumbers)
# final step transformation: take out the "stop" words, such as "the", "a" and "at"
wordCorpus <- tm_map(wordCorpus, removeWords, stopwords("english"))

wordCorpus[["1"]][["content"]] # Review what’s left of the first paragraph
# wordVector[1] # to compare
# wordCorpus$content to see its content

# create a term-document matrix "tdm"
tdm <- TermDocumentMatrix(wordCorpus)
# view term-document matrix "tdm"
tdm
# shows sparsity. Zeros. Non sparse division shows sparsity too.
# i is the index number of the term in question, j is what document it belongs to
# v how many times the term occurs in the respective document
# A list of document names and terms is under dimnames

fTerms <- findFreqTerms(tdm, lowfreq = 20) # from tm
fTerms
# Common terms: "tonight"  "new"      "people"   "children" "family"   "will"     "families" "america" 
findAssocs(tdm, fTerms, 0.4) # for fTerms, what comes next?

Zipf_plot(tdm)

# Create a list of counts for each word
# convert tdm into a matrix called "m"
m <- as.matrix(tdm)
dim(m) # 1210 x 166, 1210 unique words
# get total count for each word
wordCounts <- rowSums(m)
# sort words in "wordCounts" by frequency
wordCounts <- sort(wordCounts, decreasing=TRUE)
# check the first several items in "wordCounts" to see if it is built correctly
head(wordCounts)
totalWords <- sum(wordCounts) # total occurance of words

barplot(wordCounts)
barplot(table(wordCounts))
# to restrict: wordCounts[1:20]

# identify positive words
matchedP <- match(names(wordCounts), posWords, nomatch = 0)
matchedP <- wordCounts[matchedP != 0]
barplot(matchedP,las=2,cex.names=0.75)
sum(matchedP)/totalWords

# Exercise: Generate the Negative Word Matches and Interpret Results
matchedN <- match(names(wordCounts), negWords, nomatch = 0)
str(matchedN); head(matchedN,50) # What is matchedN right now?
matchedN <- matchedN != 0 # A boolean map
str(matchedN); head(matchedN,50) # How about now?
matchedN <- wordCounts[matchedN] # Use matchedN as a selector
str(matchedN); head(matchedN,50) # And finally, what about now?
barplot(matchedN,las=2,cex.names=0.75)
sum(matchedN)/totalWords

# Interpret the results.
# What is the proportion of negative words?
# Are you surprised?
# What are the implications in terms of campaign speeches?



######################################################################
# Part of Speech (POS) Tagging and Topic Modeling
#
# You need a Python environment on your computer to run spaCy functions
# An easy way to start is to install miniconda, which is a package manager
# generic across languages including R and Python. This will enable
# spacy_install() to bring down the spacy package for Python.
# Note that spacy_install() does a lot, so leave plenty of time.

# very handy to subset further down to verb, noun..
# helps get around the perplexity problem.

#install.packages("spacyr") # should only need to do this once.
library("spacyr")
#spacy_install()
# if things give error, or get stuck, try
#library("reticulate")
#reticulate::conda_install(envname = "spacy_condaenv", packages = 'spacy')
#spacy_install()
#library("spacyr")

spacy_initialize() # Start the spacy session

inText <- c(doc1="Thanksgiving was always special at my house. Sarah pointed to my sisters and said, 'They are eating apples.'",
            doc2="Then Sarah pointed to the bowl that held the fruit and said, 'They are eating-apples.'")

startTime <- Sys.time()
outTokens <- spacy_parse(inText)
Sys.time() - startTime # Takes 0.36 seconds, or about 0.18 seconds per sentence

outTokens

outTokens2 <- spacy_parse(inText, lemma = FALSE)
entity_extract(outTokens2) # get person


############################################################################################
# national anthems
library(tm)

# Read in anthems
anthems <- read.csv('https://raw.githubusercontent.com/lucas-de-sa/national-anthems-clustering/master/datasets/anthems.csv')

charVector <- anthems$Anthem
head(charVector)


posWords <- scan("positive-words.txt", character(0), sep = "\n")  # 2006 items
negWords <- scan("negative-words.txt", character(0), sep = "\n")  # 4783 items
head(posWords,15)
head(negWords,15)


# Make into a bag of words with two class transformations
# install.packages('tm') # text mining package

wordVector <- VectorSource(charVector) # converts it into a vector
class(wordVector); typeof(wordVector); length(wordVector)
wordCorpus <- Corpus(wordVector) # converts it into a corpus
class(wordCorpus); typeof(wordCorpus); length(wordCorpus) # handy for cleaning.
# wordCorpus$content to see its content

# cleaning part!!
# first step transformation: make all of the letters in "wordCorpus" lowercase
wordCorpus <- tm_map(wordCorpus, content_transformer(tolower))
# second step transformation: remove the punctuation in "wordCorpus"
wordCorpus <- tm_map(wordCorpus, removePunctuation)
# third step transformation: remove numbers in "wordCorpus"
wordCorpus <- tm_map(wordCorpus, removeNumbers)
# final step transformation: take out the "stop" words, such as "the", "a" and "at"
wordCorpus <- tm_map(wordCorpus, removeWords, stopwords("english"))

wordCorpus[["1"]][["content"]] # Review what's left of the first paragraph


# create a term-document matrix "tdm"
tdm <- TermDocumentMatrix(wordCorpus)
# view term-document matrix "tdm"
tdm
# sparsity shows how "empty" tdm matrix is

# i is the index number of the term in question, j is what document it belongs to
# v how many times the term occurs in the respective document
# A list of document names and terms is under dimnames
m <- as.matrix(tdm)
# create a list of counts for each word named "wordCounts"
wordCounts <- rowSums(m)
# sort words in "wordCounts" by frequency
wordCounts <- sort(wordCounts, decreasing=TRUE)
# check the first several items in "wordCounts" to see if it is built correctly
head(wordCounts)
totalWords <- sum(wordCounts)

barplot(wordCounts[wordCounts>50],cex.names=0.95) # most popular words
barplot(table(wordCounts),las=2,cex.names=0.75) # frequency of occurences per anthem

# identify positive words
matchedP <- match(names(wordCounts), posWords, nomatch = 0)
matchedP <- matchedP != 0
matchedP <- wordCounts[matchedP]
str(matchedP); head(matchedP,50) # Positive

barplot(matchedP,las=2,cex.names=0.75)
sum(matchedP)/totalWords # 17% positive words!

# Exercise: Generate the Negative Word Matches and Interpret Results
matchedN <- match(names(wordCounts), negWords, nomatch = 0)
str(matchedN); head(matchedN,50) # What is matchedN right now?
matchedN <- matchedN != 0 # A boolean map
str(matchedN); head(matchedN,50) # How about now?
matchedN <- wordCounts[matchedN] # Use matchedN as a selector
str(matchedN); head(matchedN,50) # And finally, what about now?
barplot(matchedN,las=2,cex.names=0.75)
sum(matchedN)/totalWords # 5% negative words

######################################################

library("quanteda") # Put the package in memory
library("corpus")
# let's use corpus package, alternative to tm package
term_stats(charVector, ngrams = 1) # get term stats, for single word. Can change ngrams.
term_stats(charVector, ngrams = 1,filter = text_filter(drop_punct = TRUE, drop_symbol = TRUE, drop = stopwords_en))
# clean on the fly.
# support is the number of anthems containing the word

# Convert to corpus, reshape to paragraphs
anthemcorpus <- corpus(charVector, docnames=anthems$country)
paras <- corpus_reshape(anthemcorpus, to="paragraphs")
anthem_dtm <- dfm(paras, stem=TRUE, remove_punct=TRUE, remove_symbols = TRUE, remove_numbers = TRUE, remove=c(stopwords("english")))
anthem_dtm <- dfm_remove(anthem_dtm, c("s","?","?",'thi'))
anthem_dtm <- dfm_trim(anthem_dtm, min_termfreq=20) # to trim
anthem_dtm
colnames(anthem_dtm)

# make a wordcloud
library("quanteda.textplots")
textplot_wordcloud(anthem_dtm,adjust=.6)
# color palette:
#textplot_wordcloud(anthem_dtm,color = rev(RColorBrewer::brewer.pal(10, "RdBu")))



######################################################
# Here's where the LDA topic modeling occurs
library("topicmodels")
library('tidytext')
library('dplyr')

# get tdm matrix, anthem_dtm is a dfm, document feature matrix, which is the transpose of dtm
anthem_topics <- convert(anthem_dtm, to = "topicmodels") # same as tdm earlier

topic_model <- LDA(anthem_topics, method = "VEM", k=5)
terms(topic_model,5)

# Beta is the probability that a given term appears in a particular topic. 
# Higher probability terms "define" the topic best.
tidy_topics <- tidy(topic_model, matrix = "beta")
tidy_topics

# can show, adds up to 1 for each topic
# tidy_topics %>% group_by(topic) %>% summarize(sum_beta = sum(beta))


# no need to understand this as much!: can copy paste when needed. (true for
# rest of the file.)
anthem_top_topics <- tidy_topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 2) %>% # cool func, gets the max n for each topic group
  ungroup() %>% # to get the tibble without group tag
  arrange(topic, -beta) # sort by topic, beta decreasing

anthem_top_topics %>%
  mutate(term = reorder_within(term, beta, topic)) %>% # this hack is to order for facet
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~topic, scales = "free") + # scales="free" allows x-y scales to be free.
  scale_y_reordered() # used in combo with reorder_within

# now the same for gamma
# Gamma is the probability that a given topic appears in a particular document.
# Higher probability documents show the main topics.
tidy_anthems <- tidy(topic_model, matrix = "gamma")
tidy_anthems

# show piece by piece that for a given doc, gammas add up to 1
#tidy_anthems %>% filter(document=='text1') %>% select(gamma) %>% sum()

# try to name?
tidy_anthems <- tidy_anthems %>% 
  mutate(topicname = ifelse(topic==1, '1: glory,homeland',ifelse(topic==2,'2: god,nation',ifelse(topic==3,'3: land,love',ifelse(topic==4,'4: us,one',ifelse(topic==5,'5: may,eternal','.'))))))

# gamma plots
top_anthems <- tidy_anthems %>%
  group_by(topicname) %>%
  slice_max(gamma, n = 10) %>% 
  ungroup() %>%
  arrange(document, -gamma)

top_anthems %>%
  mutate(document = reorder_within(document, gamma, topicname)) %>%
  ggplot(aes(gamma, document, fill = factor(topicname))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~topicname, scales = "free") +
  scale_y_reordered() # takes care of labels

# amazing:
tidy_anthems %>% filter(document=='text129')
# notice adds up to 1.

######################################################

# let's make a map!
# we need country names!!
library('stringr') # to get part of string from column, for code
tidy_anthems <- tidy_anthems %>% mutate(countryid=as.numeric(str_extract(tidy_anthems$document, "[0-9]+"))) # gets code
anthems <- anthems %>% 
  mutate(countryid = factor(rownames(anthems))) # add code as new var

df <- merge(tidy_anthems,anthems,on='countryid')


#install.packages("rnaturalearth")
#install.packages("rnaturalearthdata")

library("rnaturalearth")
library("rnaturalearthdata")
library("sf")

world <- ne_countries(scale = "medium", returnclass = "sf") # to load world map
world$CountryCode = world$su_a3 # has bunch of codes
df$CountryCode = df$Alpha.3 # making sure df also has same code

out <- merge(df, world, on = 'CountryCode') # merge all on code
topic1 <- out %>% filter(topic==1) %>% mutate(gamma=round(gamma,2))
# convert to spatial object
topic1 <- st_as_sf(topic1)

library(mapview) # mapview!
mapview(topic1,zcol='gamma', col.regions = blues9, label=paste(topic1$CountryCode ,', gamma= ', topic1$gamma),layer.name ='glory')
# notice the "paste" in label.

######################################################
# How many groups? Aka optimal k?
maxTopics <- 10 # Max number of topics we will investigate
set.seed(1)

perList <- NULL # We are going to make a list of perplexity values
# Trying to find the lowest perplexity value.

# Loop: Try every number of topics from 2 up to maxTopics
for (i in 2:maxTopics) {
  topic_model <- LDA(anthem_topics, method = "VEM", k=i)  # Latent Dirichlet Allocation
  perList[i-1] <- perplexity(topic_model)
}

# Label and plot the perplexity list: Look for the knee
names(perList) <- as.character(2:maxTopics)
perList_df <- as.data.frame(perList)
library(ggplot2)
ggplot(data=perList_df,aes(x=as.numeric(rownames(perList_df)), y=perList)) + geom_point() + geom_line(linetype='dashed') + labs(x='num topics',y='perplexity')
# elbow plot gives 3 as the optimal k

######################################################
# Exercise: Redo the topic model with the ideal number of topics

topic_model <- LDA(anthem_topics, method = "VEM", k=3)
terms(topic_model,3)

# Beta is the probability that a given term appears in a particular topic. 
# Higher probability terms "define" the topic best.
tidy_topics <- tidy(topic_model, matrix = "beta")
tidy_topics

anthem_top_topics <- tidy_topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>% 
  ungroup() %>%
  arrange(topic, -beta)

anthem_top_topics %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()

#gamma
tidy_anthems <- tidy(topic_model, matrix = "gamma")
tidy_anthems

# name etc, which can be skipped.
tidy_anthems <- tidy_anthems %>% 
  mutate(topicname = ifelse(topic==1, '1: glory,homeland',ifelse(topic==2,'2: god,nation',ifelse(topic==3,'3: land,love',ifelse(topic==4,'4: us,one',ifelse(topic==5,'5: may,eternal','.'))))))


top_anthems <- tidy_anthems %>%
  group_by(topicname) %>%
  slice_max(gamma, n = 10) %>% 
  ungroup() %>%
  arrange(document, -gamma)

top_anthems %>%
  mutate(document = reorder_within(document, gamma, topicname)) %>%
  ggplot(aes(gamma, document, fill = factor(topicname))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~topicname, scales = "free") +
  scale_y_reordered() # takes care of labels



# How Many Documents for Each Topic?
top_anthems %>% group_by(topic) %>% summarize(count=n(),av_gamma = mean(gamma)) %>% ggplot(.,aes(x=topic,y=count)) + geom_col()





