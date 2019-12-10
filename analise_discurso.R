## AUTHOR: GUSTAVO VENTURI
## CLASS: TEXT ANALISYS WITH R AND PYTHON
## PROF: JASMINE MOREIRA
## DATE: 11 DECEMBER 2019
## CURRENT R VERSION: 3.6.1

#---------------------------#
#        UPDATING R         #
#---------------------------#

#version                       #Check current version. If current version is older than 3.6.1, install a newer version.
#install.packages("installr")  #Install package to install packages
#library(installr)             #Call the library "installr"
#updateR()                     #function to update R


#---------------------------#
#   INSTALLING PACKAGES     #
#---------------------------#


# install.packages("pdftools")        
# install.packages("tibble")          
# install.packages("tm")
# install.packages("dplyr")
# install.packages("tidytext")
# install.packages("wordcloud")
# install.packages("SnowballC")
# install.packages("readr")
# install.packages("gdata")
# install.packages("ggplot2")
# install.packages("tidyr")
# install.packages("igraph")
# install.packages("ggraph")
# install.packages("widyr")
# install.packages("Rfacebook")
# install.packages("topicmodels")
# install.packages("ldatuning")
#install.packages("rtweet")
#install.packages("stringr")


#---------------------------#
#    LOADING LIBRARIES      #
#---------------------------#


library(pdftools)
library(tibble)
library(tm)
library(dplyr)
library(tidytext)
library(wordcloud)
library(SnowballC)
library(readr)
library(gdata)
library(tidyr)
library(ggplot2)
library(tidyr)
library(igraph)
library(ggraph)
library(widyr)
library(topicmodels)
library(ldatuning)
library(rtweet)
library(stringr)


#---------------------------#
#     LOADING STOPWORDS     #
#---------------------------#


##LOAD A STOPWORDS IN PT-BR

stp_words <- 
read_delim("C:\\Users\\gusta\\OneDrive\\Documentos\\RStudio\\UP_Pos_RScript\\stopwords.csv",
           ";",escape_double = FALSE, trim_ws = TRUE, locale = locale(encoding = "latin1"))

##ADD NEW STOPWORDS

stp_words <- add_row(stp_words, word = "forma")
stp_words <- add_row(stp_words, word = "dizer")
stp_words <- add_row(stp_words, word = "pois")
stp_words <- add_row(stp_words, word = "coisa")
stp_words <- add_row(stp_words, word = "coisas")
stp_words <- add_row(stp_words, word = "ficou")
stp_words <- add_row(stp_words, word = "the")
stp_words <- add_row(stp_words, word = "of")
stp_words <- add_row(stp_words, word = "and")
stp_words <- add_row(stp_words, word = "pp")
stp_words <- add_row(stp_words, word = "press")
stp_words <- add_row(stp_words, word = "?")
stp_words <- add_row(stp_words, word = "mt")
stp_words <- add_row(stp_words, word = "tjs")
stp_words <- add_row(stp_words, word = "gee")
stp_words <- add_row(stp_words, word = "fez")
stp_words <- add_row(stp_words, word = "ie")
stp_words <- add_row(stp_words, word = "diz")
stp_words <- add_row(stp_words, word = "cap?tulo")
stp_words <- add_row(stp_words, word = "por?m")
stp_words <- add_row(stp_words, word = "dc")
stp_words <- add_row(stp_words, word = "ne")
stp_words <- add_row(stp_words, word = "nhor")
stp_words <- add_row(stp_words, word = "se")


#---------------------------#
#  LOADING FOLDER WITH DATA #
#---------------------------#


##Set a work directory. It's a folder with data

setwd("C:\\Users\\gusta\\OneDrive\\Documentos\\RStudio\\UP_Pos_RScript\\livros")


#Create a function to load files into "PDF" folder.

pdf2txt <- function(files){
  sapply(paste0('./PDF/', files),
         function(f){return(paste(pdf_text(f), collapse = " "))},
         USE.NAMES = FALSE)} 

#Load all file's name inside the folders and subfolders into a table.

alltexts <- tibble(fname = list.files('./PDF',recursive = TRUE)) %>%
  mutate(category = sub('/[^/]*$', '', fname)) %>%
  mutate(txt = pdf2txt(fname))

#Load folders names inside the main folder ("PDF")

categories <- unique(alltexts$category)

#Filter a category
  ## Categories available: "Outros" and "SenhorDosAneis"

viewCategory <- "SenhorDosAneis"
text <- alltexts %>%
  filter(str_detect(category, viewCategory))


##If you want to load only one text, choice a line below and run.

##text <- paste(pdf_text("C:\\Users\\gusta\\OneDrive\\Documentos\\RStudio\\UP_Pos_RScript\\livros\\PDF\\SenhorDosAneis\\1ASociedadeDoAnel.pdf"), " ")
##text <- paste(pdf_text("C:\\Users\\gusta\\OneDrive\\Documentos\\RStudio\\UP_Pos_RScript\\livros\\PDF\\SenhorDosAneis\\2AsDuasTorres.pdf"), " ")
##text <- paste(pdf_text("C:\\Users\\gusta\\OneDrive\\Documentos\\RStudio\\UP_Pos_RScript\\livros\\PDF\\SenhorDosAneis\\3RetornoDoRei.pdf"), " ")


#Replace all the "-". Expect join splited words.
#text <- replace_na(text,"...","")

#Split wich sentence by separator '.'
text <- unlist(strsplit(text$txt,"[.]"))

#Put wich sentence into a table with a index.
text <- tibble(sentence = text) 

##Remove punctuation, numbers and blank spaces.
text$sentence <- text$sentence %>%
  removePunctuation() %>%
  stripWhitespace() %>%
  removeNumbers() 
  
##Split each sentence in words.
tokens <- text%>%
  mutate(linenumber = row_number()) %>% #Create a column with sentence number.
  unnest_tokens(word,sentence) %>%      #Split each word into a column called 'word'.
  anti_join(stp_words)                  #Remove the stop word.
  
  
##Count the words
tokens_count <- tokens %>%
  count(word,sort = TRUE) ##Order by count of tokens

##Create a palette with 15 colors, called 'Set2'
numRegister <- 15 ## Number of colors.
mycolors <- colorRampPalette(brewer.pal(8,"Set2"))(numRegister) 

#Make a word cloud.
wordcloud(tokens_count$word,tokens_count$n, 
          max.words = 50, scale = c(4,0.5,.25),
          colors = brewer.pal(10,"Spectral"))
### The result of word cloud is 100 words. The scale put the words the most repeating words bigger than other words.

#Plot the results in a bar chart with tokens_count
tokens_count %>%
  mutate(word = reorder(word,n)) %>%
  head(numRegister) %>%
  ggplot(aes(word,n,fill=factor(word)))+
  scale_fill_manual(values = mycolors)+
  geom_col()+
  xlab(NULL)+
  coord_flip()
### The result put the most frequent word in order descending. Usualy in story books, the most frequent words is the main characters.


#Split the sentences in 2 words.
bigrams <- text %>%
  unnest_tokens(bigram, sentence, token = "ngrams", n = 2) %>%
  separate(bigram,c("word1","word2"),sep = " ") %>%
  filter(!word1 %in% as.vector(t(stp_words$word))) %>%
  filter(!word2 %in% as.vector(t(stp_words$word))) %>% 
  unite(bigram,word1,word2,sep = " ") %>%
  count(bigram, sort = TRUE)

#Plot the results in a bar chart.
bigrams %>%
  mutate(bigram = reorder(bigram,n)) %>%
  head(numRegister) %>%
  ggplot(aes(bigram,n,fill=factor(bigram)))+
  scale_fill_manual(values = mycolors)+
  geom_col()+
  xlab(NULL)+
  coord_flip()
### The result usualy is related a quoted text of main character or speaker.


#Split the sentences in 3 words.
trigrams <- text %>%
  unnest_tokens(trigram, sentence, token = "ngrams", n = 3) %>%
  separate(trigram,c("word1","word2","word3"), sep = " ") %>%   #Important: Only the first and last word were removed the stop words.
  filter(!word1 %in% as.vector(t(stp_words$word))) %>%          #in case of trigrams, que word 1 and 3.
  filter(!word3 %in% as.vector(t(stp_words$word))) %>%          #
  unite(trigram,word1,word2,word3, sep = " ") %>%
  count(trigram, sort = TRUE)

#Plot the results in a bar chart.
trigrams %>%
  mutate(trigram = reorder(trigram,n)) %>%
  head(numRegister) %>%
  ggplot(aes(trigram,n,fill = factor(trigram)))+
  scale_fill_manual(values = mycolors)+
  geom_col()+
  xlab(NULL)+
  coord_flip()

#Split the sentences in 4 words.
quadgrams <- text %>%
  unnest_tokens(quadgram, sentence, token = "ngrams", n = 4) %>%
  separate(quadgram,c("word1","word2","word3","word4"), sep = " ") %>%
  filter(!word1 %in% as.vector(t(stp_words$word))) %>%
  
  filter(!word4 %in% as.vector(t(stp_words$word))) %>%
  unite(quadgram,word1,word2,word3,word4, sep = " ") %>%
  count(quadgram, sort = TRUE)

#Plot the results in a bar chart.
quadgrams %>%
  mutate(quadgram = reorder(quadgram,n)) %>%
  head(numRegister) %>%
  ggplot(aes(quadgram,n,fill = factor(quadgram)))+
  scale_fill_manual(values = mycolors)+
  geom_col()+
  xlab(NULL)+
  coord_flip()

#Correlate the pairs of words
word_cords <- tokens %>%
  group_by(word) %>%
  filter(n()>10) %>%
  pairwise_cor(word,linenumber,sort = TRUE)

#-------------------------------#
#     SENTIMENT ANALISYS        #
#-------------------------------#

##Load a sentiment words dictionary (sentiment table)
affin_pt <- 
  read_delim("C:\\Users\\gusta\\OneDrive\\Documentos\\RStudio\\UP_Pos_RScript\\affin_pt.csv",
             ";",escape_double = FALSE, trim_ws = TRUE, locale = locale(encoding = "latin1"))

##Put the sentence words in the sentiment dictionary with inner join, excluding words doesn't countain in sentiment dictionary.
affin_pt <- tokens %>%
  inner_join(affin_pt) %>%
  count(index=linenumber %/% 1000, sentiment) %>% #The number 1000, represents the number of words 
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positivo - negativo) 

#Plot result in a chart, positive values mean positive sentiments.
ggplot(affin_pt, aes(index, sentiment))+
  geom_col(show.legend = TRUE)


#-------------------------------#
#       TOPICS ANALISYS         #
#-------------------------------#


#Create an array with results of distance between words.
#Groups of words create a topic.

dtm <- tokens %>%
  count(linenumber,word, sort = TRUE) %>%
  cast_dtm(linenumber, word, n)

corpus_lda <- dtm %>%
  LDA(k = 12, control = list(seed=1234))

#Show the terms, splited in 15 topics
get_terms(corpus_lda, 15) 


#The result of FindTopicsNumber is a calculate of topics. Ploting the results in a line chart, the visual analisys showing a peak, can be a number of topics.
result <- FindTopicsNumber(
  dtm,
  topics = seq(from = 2, to = 15, by = 1),
  metrics = c("Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed=77),
  mc.cores = 2L,
  verbose = TRUE
  )

#Plot the results in a line chart.
FindTopicsNumber_plot(result)

#Calculate 'beta'. Higher beta is more representative topic. 
corpus_topics <- tidy(corpus_lda, matrix = "beta")
corpus_top_terms <- corpus_topics %>%
  group_by(topic) %>%
  top_n(15, beta) %>%
  arrange(topic, -beta) %>%
  do(head(., n = 15)) %>%
  ungroup() %>%
  mutate(term=reorder(term,beta)) %>%
  mutate(order = row_number())

#Plot the bar charts. Check the 'Beta' in scale charts, the higher value show the most important topic.
corpus_top_terms %>%
  ggplot(aes(order, beta, fill = factor(topic))) + 
  geom_bar(stat = "identity", show.legend = FALSE) + 
  facet_wrap(~ topic, scales = "free", nrow = 3 ) + 
  xlab("Termos") + 
  ylab("Beta") +
  scale_x_continuous(
    breaks = corpus_top_terms$order,
    labels = corpus_top_terms$term,
    expand = c(0,0),
    trans = "reverse"
      ) + coord_flip()