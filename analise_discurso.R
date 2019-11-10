#install.packages("installr")
#library(installr)
#updateR()

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

##CARREGA A LISTA DE PALAVRAS QUE DEVEM SAIR DO TEXTO
stp_words <- 
read_delim("C:\\Users\\gusta\\OneDrive\\Documentos\\RStudio\\UP_Pos_RScript\\stopwords.csv",
           ";",escape_double = FALSE, trim_ws = TRUE, locale = locale(encoding = "latin1"))
##ADICIONA MAIS PALAVRAS A LISTA DE PALAVRAS
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
stp_words <- add_row(stp_words, word = "ó")
stp_words <- add_row(stp_words, word = "mt")
stp_words <- add_row(stp_words, word = "tjs")
stp_words <- add_row(stp_words, word = "gee")
stp_words <- add_row(stp_words, word = "fez")
stp_words <- add_row(stp_words, word = "ie")
stp_words <- add_row(stp_words, word = "diz")
stp_words <- add_row(stp_words, word = "capítulo")
stp_words <- add_row(stp_words, word = "porém")
stp_words <- add_row(stp_words, word = "dc")
stp_words <- add_row(stp_words, word = "ne")
stp_words <- add_row(stp_words, word = "nhor")
stp_words <- add_row(stp_words, word = "se")




##CARREGA O PDF DO LIVRO
text <- paste(pdf_text("C:\\Users\\gusta\\OneDrive\\Documentos\\RStudio\\UP_Pos_RScript\\biblia.pdf"), " ")
#text <- replace_na(text,"-","")
text <- unlist(strsplit(text,"[.]")) ##SEPARA O TEXTO POR CADA OCORRENCIA DO PONTO FINAL (SEPARAÇÃO POR FRASE)
text <- tibble(sentence = text) ##FAZ UMA TABELA COM AS FRASES E ENUMERAÇÃO

##REMOVE A PONTUAÇÃO, ESPAÇOS EM BRANCO E NUMEROS
text$sentence <- text$sentence %>%
  removePunctuation() %>%
  stripWhitespace() %>%
  removeNumbers() 
  
##TOKENIZAÇÃO DO TEXTO (SEPARAÇÃO POR PALAVRAS)
tokens <- text %>%
  mutate(linenumber = row_number()) %>% ##ENUMERA AS PALAVRAS
  unnest_tokens(word,sentence) %>% ##DESANINHA AS FRASES EM PALAVRAS
  anti_join(stp_words) ##REMOVE AS PALAVRAS QUE ESTÃO NA LISTA DE STOPWORDS.

##CONTA AS PALAVRAS
tokens_count <- tokens %>%
  count(word,sort = TRUE) ##ORDENA PELA CONTAGEM DE PALAVRAS

##CRIAR UMA LISTA DE CORES PARA OS GRAFICOS COM 15 CORES.
numRegister <- 15 ## NUMERO DE CORES
mycolors <- colorRampPalette(brewer.pal(8,"Set2"))(numRegister) ##CRIA UMA NOVA PALETA DE CORES CHAMADA "SET2"

#wordcloud(tokens_count$word,tokens_count$n, 
#          max.words = 100, scale = c(2,0.5,5),
#          colors = brewer.pal(10,"Spectral"))



tokens_count %>%
  mutate(word = reorder(word,n)) %>%
  head(numRegister) %>%
  ggplot(aes(word,n,fill=factor(word)))+
  scale_fill_manual(values = mycolors)+
  geom_col()+
  xlab(NULL)+
  coord_flip()

bigrams <- text %>%
  unnest_tokens(bigram, sentence, token = "ngrams", n = 2) %>%
  separate(bigram,c("word1","word2"),sep = " ") %>%
  filter(!word1 %in% as.vector(t(stp_words$word))) %>%
  filter(!word2 %in% as.vector(t(stp_words$word))) %>% 
  unite(bigram,word1,word2,sep = " ") %>%
  count(bigram, sort = TRUE)

bigrams %>%
  mutate(bigram = reorder(bigram,n)) %>%
  head(numRegister) %>%
  ggplot(aes(bigram,n,fill=factor(bigram)))+
  scale_fill_manual(values = mycolors)+
  geom_col()+
  xlab(NULL)+
  coord_flip()

trigrams <- text %>%
  unnest_tokens(trigram, sentence, token = "ngrams", n = 3) %>%
  separate(trigram,c("word1","word2","word3"), sep = " ") %>%
  filter(!word1 %in% as.vector(t(stp_words$word))) %>%
  
  filter(!word3 %in% as.vector(t(stp_words$word))) %>%
  unite(trigram,word1,word2,word3, sep = " ") %>%
  count(trigram, sort = TRUE)

trigrams %>%
  mutate(trigram = reorder(trigram,n)) %>%
  head(numRegister) %>%
  ggplot(aes(trigram,n,fill = factor(trigram)))+
  scale_fill_manual(values = mycolors)+
  geom_col()+
  xlab(NULL)+
  coord_flip()

quadgrams <- text %>%
  unnest_tokens(quadgram, sentence, token = "ngrams", n = 4) %>%
  separate(quadgram,c("word1","word2","word3","word4"), sep = " ") %>%
  filter(!word1 %in% as.vector(t(stp_words$word))) %>%
  
  filter(!word4 %in% as.vector(t(stp_words$word))) %>%
  unite(quadgram,word1,word2,word3,word4, sep = " ") %>%
  count(quadgram, sort = TRUE)

quadgrams %>%
  mutate(quadgram = reorder(quadgram,n)) %>%
  head(numRegister) %>%
  ggplot(aes(quadgram,n,fill = factor(quadgram)))+
  scale_fill_manual(values = mycolors)+
  geom_col()+
  xlab(NULL)+
  coord_flip()

word_cords <- tokens %>%
  group_by(word) %>%
  filter(n()>10) %>%
  pairwise_cor(word,linenumber,sort = TRUE)

##ANALISE DE SENTIMENTO


##CARREGA LISTA DE PALAVRAS DE SENTIMENTOS
affin_pt <- 
  read_delim("C:\\Users\\gusta\\OneDrive\\Documentos\\RStudio\\UP_Pos_RScript\\affin_pt.csv",
             ";",escape_double = FALSE, trim_ws = TRUE, locale = locale(encoding = "latin1"))


affin_pt <- tokens %>%
  inner_join(affin_pt) %>%
  #count(index = sentiment) %>%
  count(index=linenumber %/% 200, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positivo - negativo) 
  #summarise(positivo = sum(positivo), negativo = sum(negativo))
  
#barplot(c(affin_pt$positivo,affin_pt$negativo))
ggplot(affin_pt, aes(index, sentiment))+
  geom_col(show.legend = TRUE)

#analise de tópicos  
#distancia euclidiana (formula da reta) dentro fo grupo esparços

dtm <- tokens %>%
  count(linenumber,word, sort = TRUE) %>%
  cast_dtm(linenumber, word, n)

corpus_lda <- dtm %>%
  LDA(k = 12, control = list(seed=1234))

get_terms(corpus_lda, 15)


#calcula a curva de tópicos "joelho". O joelho é a quantidade "ideal" de tópicos.
result <- FindTopicsNumber(
  dtm,
  topics = seq(from = 2, to = 50, by = 1),
  metrics = c("Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed=77),
  mc.cores = 2L,
  verbose = TRUE
  )

#plota os resultados no gráfico
FindTopicsNumber_plot(result)

corpus_topics <- tidy(corpus_lda, matrix = "beta")
corpus_top_terms <- corpus_topics %>%
  group_by(topic) %>%
  top_n(15, beta) %>%
  arrange(topic, -beta) %>%
  do(head(., n = 15)) %>%
  ungroup() %>%
  mutate(term=reorder(term,beta)) %>%
  mutate(order = row_number())

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