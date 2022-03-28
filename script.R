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

if (!require("ranger"))
  install.packages("ranger")
library(ranger) # additional workflow functions with random forests

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

top_terms_by_topic_tfidf <- function(text_df, text_column, group_column, plot = T){
  # name for the column we're going to unnest_tokens_ to
  # (you only need to worry about enquo stuff if you're
  # writing a function using using tidyverse packages)
  group_column <- enquo(group_column)
  text_column <- enquo(text_column)
  
  # get the count of each word in each review
  words <- text_df %>%
    unnest_tokens(word, !!text_column) %>%
    count(!!group_column, word) %>% 
    ungroup()
  
  # get the number of words per text
  total_words <- words %>% 
    group_by(!!group_column) %>% 
    summarize(total = sum(n))
  
  # combine the two dataframes we just made
  words <- left_join(words, total_words)
  
  # get the tf_idf & order the words by degree of relevence
  tf_idf <- words %>%
    bind_tf_idf(word, !!group_column, n) %>%
    select(-total) %>%
    arrange(desc(tf_idf)) %>%
    mutate(word = factor(word, levels = rev(unique(word))))
  
  if(plot == T){
    # convert "group" into a quote of a name
    # (this is due to funkiness with calling ggplot2
    # in functions)
    group_name <- quo_name(group_column)
    
    # plot the 10 most informative terms per topic
    tf_idf %>% 
      group_by(!!group_column) %>% 
      top_n(10) %>% 
      ungroup %>%
      ggplot(aes(word, tf_idf, fill = as.factor(group_name))) +
      geom_col(show.legend = FALSE) +
      labs(x = NULL, y = "tf-idf") +
      facet_wrap(reformulate(group_name), scales = "free") +
      coord_flip()
  }else{
    # return the entire tf_idf dataframe
    return(tf_idf)
  }
}


#### Importing data ####

library(readr)
# Årsak
årsak <-
  read_csv("~/Documents/Fritidsbåtplattformen/Delt opp/årsak.csv")
readr::spec(årsak)

# Fartøy
# fartøy <-
#   read_csv(
#     "~/Documents/Fritidsbåtplattformen/Delt opp/fartøy.csv",
#     col_types = cols(
#       imonr = col_character(),
#       bruttotonnasje = col_character(),
#       sikkerhetstonnasje = col_character(),
#       bredde = col_character(),
#       lengde_loa = col_character(),
#       lengde_lpp = col_character(),
#       sisteombygningsår = col_character(),
#       fartsområde_kode = col_character(),
#       hinkode = col_character(),
#       ulykkested_kode = col_character(),
#       fartøyhastighet = col_character(),
#       sjøkart_kode = col_character(),
#       lastingprosent = col_character(),
#       skrogposisjon_kode = col_character(),
#       skrogskadehøydebunn = col_character(),
#       distanseaptilx = col_character()
#     )
#   )
# readr::spec(fartøy)

# Konsekvens
# konsekvens <-
#   read_csv("~/Documents/Fritidsbåtplattformen/Delt opp/konsekvens.csv")
# readr::spec(konsekvens)

# Miljøskade
# miljøskade <-
#   read_csv(
#     "~/Documents/Fritidsbåtplattformen/Delt opp/miljøskade.csv",
#     col_types = cols(fnnummer = col_character())
#   )
# readr::spec(miljøskade)

# Person
person <-
  read_csv(
    "~/Documents/Fritidsbåtplattformen/Delt opp/person.csv",
    col_types = cols(pusulykkenummer = col_character(),
                     yrkeskodefisker = col_character(),
                     yrkeskodefisker_kode = col_character()
                     )
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
# tilrådning <-
#   read_csv("~/Documents/Fritidsbåtplattformen/Delt opp/tilrådning.csv")
# readr::spec(tilrådning)

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

a <- mean(table(format(years(
  as.Date(ulykke$ulykkedato, format = "%d.%m.%Y")))))

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
  geom_hline(yintercept = a) +
  labs(
    x = "Year",
    y = "Number of entries per year",
    title = paste0("Reported accidents in Sdir's dataset. N = ", nrow(ulykke[!is.na(ulykke$ulykkedato), ]))
  ) +
  scale_fill_manual(values = c(safe_colorblind_palette))

rm(a)
## By month
month.name <- as.factor(month.name)

ulykke$ulykkesmåned <- str_squish(format(factor(months(
  as.Date(ulykke$ulykkedato, format = "%d.%m.%Y")
))))
ulykke$ulykkesmåned
a <- mean(table(ulykke$ulykkesmåned))


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
  geom_hline(yintercept = a)+
  labs(
    x = "Month",
    y = "Number of entries per month",
    title = paste0(
      "Reported accidents (1981-2022) in Sdir's dataset. N = ",
      nrow(ulykke[!is.na(ulykke$ulykkedato), ])
    )
  ) +
  scale_fill_manual(values = c(safe_colorblind_palette))

# I just added it manually. Might create a prettier solution later.
rm(a)

#### Creating a new variable of free text fields ####

# Concenating the free text fields, most of which are from årsak.

årsak_with_detail <- full_join(x = årsak, y = årsaksdetalj, by = "årsak_id", keep = T)
årsak_with_detail$årsak_id <- årsak_with_detail$årsak_id.x

årsak_with_detail <- årsak_with_detail %>%
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
  group_by(årsak_id) %>%
  summarise(ALL = toString(unique(value))) %>%
  left_join(årsak)

full_data <- full_join(x = årsak_with_detail, y = ulykke, by = 'ulykke_id', keep = T)

#### Stemming/stopwords or not? ####
tidy_full <-   full_data %>%
  unnest_tokens(word, ALL) %>%
  anti_join(get_stopwords("no")) %>%
  anti_join(get_stopwords("en"))

tidy_full %>%
  count(word, sort = TRUE)

full_data %>%
  unnest_tokens(word, ALL) %>%
  count(word, sort = TRUE)

#### Building a regression model ####
# From chapter 6 https://smltar.com/mlregression.html#firstmlregression

library(tidymodels)
set.seed(1234)
full_data_split <-   full_data %>%
  mutate(ALL = str_remove_all(ALL, "'")) %>%
  initial_split()

full_data_train <- training(full_data_split)
full_data_test <- testing(full_data_split)

library(textrecipes)

# We can add more predictors here, such as

full_data_rec <- recipe(antall_skadet ~ ALL, data = full_data_train) %>%
  step_tokenize(ALL) %>%
  step_stopwords(language = "no", custom_stopword_source = norwegian_stop_words) %>%
  step_tokenfilter(ALL, max_tokens = 1e3) %>%
  step_tfidf(ALL) %>%
  step_normalize(all_predictors())

full_data_rec

full_data_prep <- prep(full_data_rec)
full_data_bake <- bake(full_data_prep, new_data = NULL)

dim(full_data_bake)

# Creating a workflow

full_data_wf <- workflow() %>%
  add_recipe(full_data_rec)

full_data_wf

svm_spec <- svm_linear() %>%
  set_mode("regression") %>%
  set_engine("LiblineaR")


svm_fit <- full_data_wf %>%
  add_model(svm_spec) %>%
  fit(data = full_data_train)

# Extracting results

svm_fit %>%
  pull_workflow_fit() %>%
  tidy() %>%
  arrange(-estimate)

# The term Bias here means the same thing as an intercept. We see here what
# terms contribute to a rescue mission having more injured

svm_fit %>%
  pull_workflow_fit() %>%
  tidy() %>%
  arrange(estimate)

# Creating folds to validate 

set.seed(123)
full_data_folds <- vfold_cv(full_data_train)

full_data_folds

# Resampling - rs = resample here

set.seed(123)
svm_rs <- fit_resamples(
  full_data_wf %>% add_model(svm_spec),
  full_data_folds,
  control = control_resamples(save_pred = TRUE)
)

svm_rs

collect_metrics(svm_rs)

svm_rs %>%
  collect_predictions() %>%
  ggplot(aes(antall_skadet, .pred, color = id)) +
  geom_abline(lty = 2, color = "gray80", size = 1.5) +
  geom_point(alpha = 0.3) +
  labs(
    x = "Actual number of injured",
    y = "Predicted number of injured",
    color = NULL,
    title = "Predicted and true number of injured",
    subtitle = "Each cross-validation fold is shown in a different color"
  )

# Removing the outlier #10

# svm_rs %>%
#   collect_predictions() %>%
#   ggplot(aes(antall_skadet, .pred, color = id)) +
#   geom_abline(lty = 2, color = "gray80", size = 1.5) +
#   geom_point(alpha = 0.3) +
#   labs(
#     x = "Actual number of injured",
#     y = "Predicted number of injured",
#     color = NULL,
#     title = "Predicted and true years for Supreme Court opinions",
#     subtitle = "Each cross-validation fold is shown in a different color"
#   ) +
#   xlim(0, 22)


# Compare to null model

null_regression <- null_model() %>%
  set_engine("parsnip") %>%
  set_mode("regression")

null_rs <- fit_resamples(
  full_data_wf %>% add_model(null_regression),
  full_data_folds,
  metrics = metric_set(rmse)
)

null_rs

# Remember that this is using unigrams, but it is in fact worse than the null
# model

collect_metrics(null_rs)


# Compare to random forest

# Changing from 1000 to 100 trees due to the time it took the calculations

rf_spec <- rand_forest(trees = 100) %>%
  set_engine("ranger") %>%
  set_mode("regression")

rf_spec

library(ranger)

rf_rs <- fit_resamples(
  full_data_wf %>% add_model(rf_spec),
  full_data_folds,
  control = control_resamples(save_pred = TRUE)
)

collect_metrics(rf_rs)

collect_predictions(rf_rs) %>%
  ggplot(aes(antall_skadet, .pred, color = id)) +
  geom_abline(lty = 2, color = "gray80", size = 1.5) +
  geom_point(alpha = 0.3) +
  labs(
    x = "Actual number of injured",
    y = "Predicted number of injured",
    color = NULL,
    title = "Predicted and true number of injured people using, a random forest model",
    subtitle = "Each cross-validation fold is shown in a different color"
  )

#### LDA modeling ####
#### Årsak ####

# We create a term matrix we can clean up
årsaksCorpus_ <-
  Corpus(VectorSource(removePunctuation(årsak$ALL)))
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



# Based in the årsak data set, how well can we predict the amount of dead or
# injured people?
årsak_dataset <-
  left_join(ulykke, årsak, årsaksdetalj, by = 'ulykke_id')

fartøy_dataset <-
  full_join(fartøy, konsekvens, miljøskade, by = 'fartøy_id')

person_dataset <-
  full_join(personskade, personverneutstyr, ulykke, by = 'person_id')

tilrådning_dataset <-
  full_join(tilrådning, tilrådningstiltak, by = 'tilrådning_id')

full_data <-
  left_join(årsak_dataset, fartøy, personskade, by = 'ulykke_id')

personskade$person_id.x <- personskade$person_id
personskade$person_id.y <- personskade$person_id

full_data <-
  left_join(full_data, personskade, by = c('person_id.x', 'person_id.y'))


#### n-grams ####
# not finished

# get just the tf-idf output for the type of vehicles
tfidf_bygroup_direkteårsak <- top_terms_by_topic_tfidf(text_df = årsak, 
                                                  text_column = ALL, 
                                                  group = direkte_årsak, 
                                                  plot = F)

# do our own plotting
reviews_tfidf_byHotel  %>% 
  group_by(hotel) %>% 
  top_n(5) %>% 
  ungroup %>%
  ggplot(aes(word, tf_idf, fill = hotel)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~hotel, ncol = 4, scales = "free", ) +
  coord_flip()


årsak %>% 
  unnest_tokens(word, text, token = "ngrams", n = 2) %>% 
  separate(word, c("word1", "word2"), sep = " ") %>% 
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>% 
  unite(word, word1, word2, sep = " ") %>% 
  count(word, sort = TRUE) %>% 
  slice(1:10) %>% 
  ggplot() + geom_bar(aes(word, n), stat = "identity", fill = "#de5833") +
  theme_minimal() +
  coord_flip() +
  labs(title = "Top Bigrams of Hotel Reviews",
       subtitle = "using Tidytext in R",
       caption = "Data Source: kaggle.com/rtatman")

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

