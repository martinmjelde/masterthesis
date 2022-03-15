# Loading packages and importing data

# read in the libraries we're going to use

if (!require("tidyverse"))
  install.packages("tidyverse")
library(tidyverse) # general utility & workflow functions

if (!require("reshape2"))
  install.packages("reshape2")
library(reshape2) # additional utility & workflow functions

if (!require("tidytext"))
  install.packages("tidytext")
library(tidytext) # tidy implimentation of NLP methods

if (!require("tm"))
  install.packages("tm")
library(tm)# general text mining functions, making document term matrices

if (!require("topicmodels"))
  install.packages("topicmodels")
library(topicmodels) # for LDA topic modelling 

if (!require("SnowballC"))
  install.packages("SnowballC")
library(SnowballC) # for stemming

if (!require("NLP"))
  install.packages("NLP")
library(NLP) # Natural Language Processing Infrstructure

if (!require("randomForest"))
  install.packages("randomForest")
library(randomForest) # Algorithm called RandomForest, a regression and classification algorithm

if(!require("caret"))
  install.packages("caret")
library(caret) 

if(!require("reticulate"))
  install.packages("reticulate")
library(reticulate)

if(!require("quanteda"))
  install.packages("quanteda")
library(quanteda)

if(!require("quanteda.textmodels"))
  install.packages("quanteda.textmodels")
library(quanteda.textmodels)

library(readr)
# Årsak
årsak <- read_csv("~/Documents/Fritidsbåtplattformen/Delt opp/årsak.csv")
spec(årsak)

# Fartøy
fartøy <- read_csv("~/Documents/Fritidsbåtplattformen/Delt opp/fartøy.csv", 
                   col_types = cols(imonr = col_character(), 
                                    bruttotonnasje = col_character(),
                                    sikkerhetstonnasje = col_character(),
                                    bredde = col_character(),
                                    lengde_loa = col_character(),
                                    lengde_lpp = col_character(),
                                    sisteombygningsår = col_character(),
                                    fartsområde_kode = col_character(),
                                    hinkode = col_character(),
                                    ulykkested_kode = col_character(),
                                    fartøyhastighet = col_character(),
                                    sjøkart_kode = col_character(),
                                    lastingprosent = col_character(),
                                    skrogposisjon_kode = col_character(),
                                    skrogskadehøydebunn = col_character(),
                                    distanseaptilx = col_character()
                                    )
                   )
spec(fartøy)

# Konsekvens
konsekvens <- read_csv("~/Documents/Fritidsbåtplattformen/Delt opp/konsekvens.csv")
spec(konsekvens)

# Miljøskade
miljøskade <- read_csv("~/Documents/Fritidsbåtplattformen/Delt opp/miljøskade.csv",
                       col_types = cols(fnnummer = col_character()
                                        )
                       )
spec(miljøskade)

# Person
person <- read_csv("~/Documents/Fritidsbåtplattformen/Delt opp/person.csv",
                   col_types = cols(pusulykkenummer = col_character(),
                                    
                                    )
                   )
spec(person)

# Personskade
personskade <- read_csv("~/Documents/Fritidsbåtplattformen/Delt opp/personskade.csv")
spec(personskade)

# Personverneutstyr
personverneutstyr <- read_csv("~/Documents/Fritidsbåtplattformen/Delt opp/personverneutstyr.csv")
spec(personverneutstyr)

# Tilrådning
tilrådning <- read_csv("~/Documents/Fritidsbåtplattformen/Delt opp/tilrådning.csv")
spec(tilrådning)

# Tilrådningstiltak
tilrådningstiltak <- read_csv("~/Documents/Fritidsbåtplattformen/Delt opp/tilrådningstiltak.csv")
spec(tilrådningstiltak)

# Ulykke
ulykke <- read_csv("~/Documents/Fritidsbåtplattformen/Delt opp/ulykke.csv",
                   col_types = cols(posisjon_breddegrad = col_character()
                                    )
                   )
spec(ulykke)

# ÅrsakDetalj
årsaksdetalj <- read_csv("~/Documents/Fritidsbåtplattformen/Delt opp/årsaksdetaljer.csv")
spec(årsaksdetalj)

# Commands that might prove useful:
readr::tokenizer_csv()

#### Descriptive statistics ####
ulykke %>% 
  mutate(ulykkesår = format((years(as.Date(ulykkedato, format = "%d.%m.%Y")))))  %>% 
  count(ulykkesår) %>% 
  ggplot(aes(ulykkesår, n)) +
  geom_col()+
  theme_minimal() +
  theme(
    axis.text.x = element_text(
      angle = 90,
      size = 10,
      vjust = 0.5,
      color = "black"
    ),
    axis.text.y = element_text(size = 10),
    axis.title.y = element_text(size = 12.5),
    axis.title = element_text(size = 10),
    legend.position = "bottom"
  ) +
  scale_y_continuous(breaks = seq(0,2000, by=250))+
  labs(x = "Year", y = "Number of entries per year")

ulykke %>%
  mutate(ulykkedato = as.Date(ulykkedato)) %>% 
  count(ulykkedato) %>%
  ggplot(aes(ulykkedato, n)) +
  geom_col() +
  labs(x = "Year", y = "Number of operations per decade")


##### Functions #####
# function to get & plot the most informative terms by a specificed number
# of topics, using LDA
top_terms_by_topic_LDA <- function(input_text, # should be a columm from a dataframe
                                   plot = T, # return a plot? TRUE by defult
                                   number_of_topics = 4) # number of topics (4 by default)
{    
  # create a corpus (type of object expected by tm) and document term matrix
  Corpus <- Corpus(VectorSource(input_text)) # make a corpus object
  DTM <- DocumentTermMatrix(Corpus) # get the count of words/document
  
  # remove any empty rows in our document term matrix (if there are any 
  # we'll get an error when we try to run our LDA)
  unique_indexes <- unique(DTM$i) # get the index of each unique value
  DTM <- DTM[unique_indexes,] # get a subset of only those indexes
  
  # preform LDA & get the words/topic in a tidy text format
  lda <- LDA(DTM, k = number_of_topics, control = list(seed = 1234))
  topics <- tidy(lda, matrix = "beta")
  
  # get the top ten terms for each topic
  top_terms <- topics  %>% # take the topics data frame and..
    group_by(topic) %>% # treat each topic as a different group
    top_n(10, beta) %>% # get the top 10 most informative words
    ungroup() %>% # ungroup
    arrange(topic, -beta) # arrange words in descending informativeness
  
  # if the user asks for a plot (TRUE by default)
  if(plot == T){
    # plot the top ten terms for each topic in order
    top_terms %>% # take the top terms
      mutate(term = reorder(term, beta)) %>% # sort terms by beta value 
      ggplot(aes(term, beta, fill = factor(topic))) + # plot beta by theme
      geom_col(show.legend = FALSE) + # as a bar plot
      facet_wrap(~ topic, scales = "free") + # which each topic in a seperate plot
      labs(x = NULL, y = "Beta") + # no x label, change y label 
      coord_flip() # turn bars sideways
  }else{ 
    # if the user does not request a plot
    # return a list of sorted terms instead
    return(top_terms)
  }
}

#### LDA modeling ####
#### Årsak ####

# We create a term matrix we can clean up
årsaksCorpus_ <- Corpus(VectorSource(årsak$direkteårsak_person_fritekst)) 
årsaksDTM <- DocumentTermMatrix(årsaksCorpus_)

# convert the document term matrix to a tidytext corpus
årsaksDTM_tidy <- tidy(årsaksDTM)

# I'm going to add my own custom stop words that I don't think will be
# very informative in these reports
custom_stop_words <- tibble(word = c("på", ""))

norwegian_stop_words <- tibble(word = tm::stopwords(kind = "no"))


# remove stopwords
årsaksDTM_tidy_cleaned <- årsaksDTM_tidy %>% # take our tidy dtm and...
  anti_join(norwegian_stop_words, by = c("term" = "word")) %>% # remove Norwegian stopwords and
  anti_join(stop_words, by = c("term" = "word")) %>% # remove English stopwords as well
  anti_join(custom_stop_words, by = c("term" = "word")) # remove custom stopwords

# reconstruct cleaned documents (so that each word shows up the correct number of times)
cleaned_documents <- årsaksDTM_tidy_cleaned %>%
  group_by(document) %>% 
  mutate(terms = toString(rep(term, count))) %>%
  select(document, terms) %>%
  unique()

# check out what the cleaned documents look like (should just be a bunch of content words)
# in alphabetic order
head(cleaned_documents)

top_terms_by_topic_LDA(cleaned_documents$terms, number_of_topics = 4)

# stem the words (e.g. convert each word to its stem, where applicable)
årsaksDTM_tidy_cleaned_stem <- årsaksDTM_tidy_cleaned %>% 
  mutate(stem = wordStem(term))

# reconstruct our documents
cleaned_documents_stem <- årsaksDTM_tidy_cleaned_stem %>%
  group_by(document) %>% 
  mutate(terms = toString(rep(stem, count))) %>%
  select(document, terms) %>%
  unique()

# now let's look at the new most informative terms
top_terms_by_topic_LDA(cleaned_documents_stem$terms, number_of_topics = 4)

#### Årsak2 ####

