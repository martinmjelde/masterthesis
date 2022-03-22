#### Loading packages and importing data ####

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
library(NLP) # Natural Language Processing Infrastructure

if (!require("randomForest"))
  install.packages("randomForest")
library(randomForest) # Algorithm called RandomForest, a regression and classification algorithm

if (!require("caret"))
  install.packages("caret")
library(caret) # Machine learning training and application

if (!require("reticulate"))
  install.packages("reticulate")
library(reticulate) # Python integration

if (!require("quanteda"))
  install.packages("quanteda")
library(quanteda) # Quantitative statistics with text data

if (!require("quanteda.textmodels"))
  install.packages("quanteda.textmodels")
library(quanteda.textmodels) # Machine learning with text data

if (!require("tidymodels"))
  install.packages("tidymodels")
library(tidymodels) # Machine learning applications in the tidy format

if (!require("chron"))
  install.packages("chron")
library(chron) # excellent package for managing time variables

if (!require("textrecipes"))
  install.packages("textrecipes")
library(textrecipes) # Defining the recipe for the machine learning algorithm

##### Functions #####
# function to get & plot the most informative terms by a readr::specificed number
# of topics, using LDA
top_terms_by_topic_LDA <-
  function(input_text,
           # should be a columm from a dataframe
           plot = T,
           # return a plot? TRUE by defult
           number_of_topics = 4)
    # number of topics (4 by default)
  {
    # create a corpus (type of object expected by tm) and document term matrix
    Corpus <-
      Corpus(VectorSource(removePunctuation(input_text))) # make a corpus object and remove punctuation
    DTM <-
      DocumentTermMatrix(Corpus) # get the count of words/document
    
    # remove any empty rows in our document term matrix (if there are any
    # we'll get an error when we try to run our LDA)
    unique_indexes <-
      unique(DTM$i) # get the index of each unique value
    DTM <-
      DTM[unique_indexes, ] # get a subset of only those indexes
    
    # preform LDA & get the words/topic in a tidy text format
    lda <-
      LDA(DTM, k = number_of_topics, control = list(seed = 1234))
    topics <- tidy(lda, matrix = "beta")
    
    # get the top ten terms for each topic
    top_terms <- topics  %>% # take the topics data frame and..
      group_by(topic) %>% # treat each topic as a different group
      top_n(10, beta) %>% # get the top 10 most informative words
      ungroup() %>% # ungroup
      arrange(topic, -beta) # arrange words in descending informativeness
    
    # if the user asks for a plot (TRUE by default)
    if (plot == T) {
      # plot the top ten terms for each topic in order
      top_terms %>% # take the top terms
        mutate(term = reorder(term, beta)) %>% # sort terms by beta value
        ggplot(aes(term, beta, fill = factor(topic))) + # plot beta by theme
        geom_col(show.legend = FALSE) + # as a bar plot
        facet_wrap( ~ topic, scales = "free") + # which each topic in a seperate plot
        labs(x = NULL, y = "Beta") + # no x label, change y label
        coord_flip() # turn bars sideways
    } else{
      # if the user does not request a plot
      # return a list of sorted terms instead
      return(top_terms)
    }
  }

#### Importing data ####

library(readr)
# Årsak
årsak <-
  read_csv("~/Documents/Fritidsbåtplattformen/Delt opp/årsak.csv")
readr::spec(årsak)

# Fartøy
fartøy <-
  read_csv(
    "~/Documents/Fritidsbåtplattformen/Delt opp/fartøy.csv",
    col_types = cols(
      imonr = col_character(),
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
readr::spec(fartøy)

# Konsekvens
konsekvens <-
  read_csv("~/Documents/Fritidsbåtplattformen/Delt opp/konsekvens.csv")
readr::spec(konsekvens)

# Miljøskade
miljøskade <-
  read_csv(
    "~/Documents/Fritidsbåtplattformen/Delt opp/miljøskade.csv",
    col_types = cols(fnnummer = col_character())
  )
readr::spec(miljøskade)

# Person
person <-
  read_csv(
    "~/Documents/Fritidsbåtplattformen/Delt opp/person.csv",
    col_types = cols(pusulykkenummer = col_character(),)
  )
readr::spec(person)

# Personskade
personskade <-
  read_csv("~/Documents/Fritidsbåtplattformen/Delt opp/personskade.csv")
readr::spec(personskade)

# Personverneutstyr
personverneutstyr <-
  read_csv("~/Documents/Fritidsbåtplattformen/Delt opp/personverneutstyr.csv")
readr::spec(personverneutstyr)

# Tilrådning
tilrådning <-
  read_csv("~/Documents/Fritidsbåtplattformen/Delt opp/tilrådning.csv")
readr::spec(tilrådning)

# Tilrådningstiltak
tilrådningstiltak <-
  read_csv("~/Documents/Fritidsbåtplattformen/Delt opp/tilrådningstiltak.csv")
readr::spec(tilrådningstiltak)

# Ulykke
ulykke <-
  read_csv(
    "~/Documents/Fritidsbåtplattformen/Delt opp/ulykke.csv",
    col_types = cols(posisjon_breddegrad = col_character())
  )
readr::spec(ulykke)

# ÅrsakDetalj
årsaksdetalj <-
  read_csv("~/Documents/Fritidsbåtplattformen/Delt opp/årsaksdetaljer.csv")
readr::spec(årsaksdetalj)

# Commands that might prove useful:
readr::tokenizer_csv()

#### Descriptive statistics ####
# Using a colorblind palette for the graphs, so
# we're importing some colorblind-friendly colors
safe_colorblind_palette <-
  c(
    "#88CCEE",
    "#CC6677",
    "#DDCC77",
    "#117733",
    "#332288",
    "#AA4499",
    "#44AA99",
    "#999933",
    "#882255",
    "#661100",
    "#6699CC",
    "#888888"
  )

## By year

library(chron)
ulykke %>%
  mutate(ulykkesår = format((years(
    as.Date(ulykkedato, format = "%d.%m.%Y")
  ))))  %>%
  count(ulykkesår) %>%
  ggplot(aes(ulykkesår, n)) +
  geom_col() +
  theme_bw() +
  theme(
    axis.text.x = element_text(
      angle = 60,
      size = 10,
      vjust = 1,
      hjust = 1,
      color = "black"
    ),
    axis.text.y = element_text(size = 10),
    axis.title.y = element_text(size = 12.5),
    axis.title.x = element_text(size = 12.5),
    axis.title = element_text(size = 10),
    legend.position = "bottom"
  ) +
  scale_y_continuous(breaks = seq(0, 2000, by = 250)) +
  labs(
    x = "Year",
    y = "Number of entries per year",
    title = paste0("Reported accidents in Sdir's dataset. N = ", nrow(ulykke[!is.na(ulykke$ulykkedato), ]))
  ) +
  scale_fill_manual(values = c(safe_colorblind_palette))


## By month
month.name <- as.factor(month.name)

ulykke$ulykkesmåned <- str_squish(format(factor(months(
  as.Date(ulykke$ulykkedato, format = "%d.%m.%Y")
))))
ulykke$ulykkesmåned

ulykke %>%
  mutate(ulykkesmåned = str_squish(format((months(
    as.Date(ulykkedato, format = "%d.%m.%Y")
  )))))  %>%
  count(ulykkesmåned) %>%
  mutate(ulykkesmåned = factor(ulykkesmåned, levels = month.name)) %>%
  ggplot(aes(ulykkesmåned, n)) +
  geom_col() +
  theme_bw() +
  theme(
    axis.text.x = element_text(
      angle = 60,
      size = 10,
      vjust = 1,
      hjust = 1,
      color = "black"
    ),
    axis.text.y = element_text(size = 10),
    axis.title.y = element_text(size = 12.5),
    axis.title.x = element_text(size = 12.5),
    axis.title = element_text(size = 10),
    legend.position = "bottom"
  ) +
  scale_y_continuous(breaks = seq(0, 4000, by = 500)) +
  labs(
    x = "Month",
    y = "Number of entries per month",
    title = paste0(
      "Reported accidents (1981-2022) in Sdir's dataset. N = ",
      nrow(ulykke[!is.na(ulykke$ulykkedato), ])
    )
  ) +
  scale_fill_manual(values = c(safe_colorblind_palette))

#### Stemming or no stemming ####
tidy_årsak <-   årsak %>%
  unnest_tokens(word, direkteårsak_person_fritekst) %>%
  anti_join(get_stopwords("no")) %>%
  anti_join(get_stopwords("en"))

tidy_årsak %>%
  count(word, sort = TRUE)


#### Building a regression model ####
library(tidymodels)
set.seed(1234)
årsak_split <-   årsak %>%
  mutate(direkteårsak_person_fritekst = str_remove_all(direkteårsak_person_fritekst, "'")) %>%
  initial_split()

årsak_train <- training(årsak_split)
årsak_test <- testing(årsak_split)

library(textrecipes)

årsak_rec <- recipe( ~ text, data = scotus_train) %>%
  step_tokenize(text) %>%
  step_tokenfilter(text, max_tokens = 1e3) %>%
  step_tfidf(text) %>%
  step_normalize(all_predictors())

scotus_rec

#### LDA modeling ####
#### Årsak ####

# We create a term matrix we can clean up
årsaksCorpus_ <-
  Corpus(VectorSource(removePunctuation(årsak$direkteårsak_person_fritekst)))
årsaksDTM <- DocumentTermMatrix(årsaksCorpus_)

# convert the document term matrix to a tidytext corpus
årsaksDTM_tidy <- tidy(årsaksDTM)

# I'm going to add my own custom stop words that I don't think will be
# very informative in these reports
custom_stop_words <- tibble(word = c("på", ""))

norwegian_stop_words <- tibble(word = tm::stopwords(kind = "no"))


# remove stopwords
årsaksDTM_tidy_cleaned <-
  årsaksDTM_tidy %>% # take our tidy dtm and...
  anti_join(norwegian_stop_words, by = c("term" = "word")) %>% # remove Norwegian stopwords and
  anti_join(stop_words, by = c("term" = "word")) %>% # remove English stopwords as well
  anti_join(custom_stop_words, by = c("term" = "word")) # remove custom stopwords

# reconstruct cleaned documents (so that each word shows up the correct number of times)
cleaned_documents <-          årsaksDTM_tidy_cleaned %>%
  group_by(document) %>%
  mutate(terms = toString(rep(term, count))) %>%
  select(document, terms) %>%
  unique()

# check out what the cleaned documents look like (should just be a bunch of content words)
# in alphabetic order
head(cleaned_documents)

top_terms_by_topic_LDA(cleaned_documents$terms, number_of_topics = 4)

# stem the words (e.g. convert each word to its stem, where applicable)
årsaksDTM_tidy_cleaned_stem <-          årsaksDTM_tidy_cleaned %>%
  mutate(stem = wordStem(term))

# reconstruct our documents
cleaned_documents_stem <-          årsaksDTM_tidy_cleaned_stem %>%
  group_by(document) %>%
  mutate(terms = toString(rep(stem, count))) %>%
  select(document, terms) %>%
  unique()

# now let's look at the new most informative terms
top_terms_by_topic_LDA(cleaned_documents_stem$terms, number_of_topics = 4)

#### Modeling ####

# Concenating the free text fields, most of which are from årsak. Missing one
# from årsaksdetalj, fritekst.

årsak <- årsak %>%
  pivot_longer(
    cols = c(direkteårsak_person_fritekst,
             direkteårsak_ytre_fritekst,
             direkteårsak_utstyr_fritekst,
             indirekteårsak_person_fritekst,
             indirekteårsak_arbeidsmiljø_fritekst,
             indirekteårsak_ytre_fritekst,
             indirekteårsak_utstyr_fritekst,
             bakenforårsak_ledelse_fritekst,
             bakenforårsak_prosedyre_fritekst),
    values_drop_na = TRUE
  ) %>%
  group_by(årsak_id) %>%
  summarise(ALL = toString(unique(value))) %>%
  left_join(årsak)


# Based in the årsak data set, how well can we predict the amount of dead or
# injured people?
årsak_dataset <-
  inner_join(årsak, årsaksdetalj, ulykke, by = 'årsak_id')
fartøy_dataset <-
  full_join(fartøy, konsekvens, miljøskade, by = 'fartøy_id')
person_dataset <-
  full_join(personskade, personverneutstyr, ulykke, by = 'person_id')
tilrådning_dataset <-
  full_join(tilrådning, tilrådningstiltak, by = 'tilrådning_id')

full_data <-
  inner_join(årsak_dataset, fartøy, ulykke, by = 'ulykke_id')

personskade$person_id.x <- personskade$person_id
personskade$person_id.y <- personskade$person_id

full_data <-
  inner_join(full_data, personskade, by = c('person_id.x', 'person_id.y'))

#### Creating the text variables ####

# Freetext causes

# full_data <- full_data %>%
#   mutate(
#     freetext = paste(
#       direkteårsak_person_fritekst,
#       direkteårsak_ytre_fritekst,
#       direkteårsak_utstyr_fritekst,
#       indirekteårsak_person_fritekst,
#       indirekteårsak_arbeidsmiljø_fritekst,
#       indirekteårsak_ytre_fritekst,
#       indirekteårsak_utstyr_fritekst,
#       bakenforårsak_ledelse_fritekst,
#       bakenforårsak_prosedyre_fritekst,
#       fritekst
#     , sep = " //// ")
#
#   ) %>%
#   mutate(freetext = str_squish(str_remove_all(freetext, pattern = "NA")))

full_data <- full_data %>%
  pivot_longer(
    cols = c(direkteårsak_person_fritekst,
    direkteårsak_ytre_fritekst,
    direkteårsak_utstyr_fritekst,
    indirekteårsak_person_fritekst,
    indirekteårsak_arbeidsmiljø_fritekst,
    indirekteårsak_ytre_fritekst,
    indirekteårsak_utstyr_fritekst,
    bakenforårsak_ledelse_fritekst,
    bakenforårsak_prosedyre_fritekst,
    fritekst),
    values_drop_na = TRUE
  ) %>%
  group_by(person_id) %>%
  summarise(ALL = toString(unique(value))) %>%
  left_join(full_data)

length(table(full_data$ALL))
# 2099

length(table(full_data$direkteårsak_person_fritekst))
length(table(full_data$direkteårsak_ytre_fritekst))
length(table(full_data$direkteårsak_utstyr_fritekst))
length(table(full_data$indirekteårsak_person_fritekst))
length(table(full_data$indirekteårsak_arbeidsmiljø_fritekst))
length(table(full_data$indirekteårsak_ytre_fritekst))
length(table(full_data$indirekteårsak_utstyr_fritekst))
length(table(full_data$bakenforårsak_ledelse_fritekst))
length(table(full_data$bakenforårsak_prosedyre_fritekst))


#### tf_idf ####

a <-
  c(
    årsak,
    årsaksdetalj,
    fartøy,
    'konsekvens',
    'miljøskade',
    person,
    'personskade',
    personverneutstyr,
    tilrådning,
    tilrådningstiltak,
    ulykke
  )
