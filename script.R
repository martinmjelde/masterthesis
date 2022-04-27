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
library(tidytext) # tidy implementation of NLP methods

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
        theme_bw() + 
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
      theme_bw() +
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
# Ãrsak
årsak <-
  read_csv("~/Master/delt_opp/årsak.csv")
readr::spec(årsak)

# FartÃ¸y
# fartÃ¸y <-
#   read_csv(
#     "~/Master/delt_opp/fartÃ¸y.csv",
#     col_types = cols(
#       imonr = col_character(),
#       bruttotonnasje = col_character(),
#       sikkerhetstonnasje = col_character(),
#       bredde = col_character(),
#       lengde_loa = col_character(),
#       lengde_lpp = col_character(),
#       sisteombygningsÃ¥r = col_character(),
#       fartsomrÃ¥de_kode = col_character(),
#       hinkode = col_character(),
#       ulykkested_kode = col_character(),
#       fartÃ¸yhastighet = col_character(),
#       sjÃ¸kart_kode = col_character(),
#       lastingprosent = col_character(),
#       skrogposisjon_kode = col_character(),
#       skrogskadehÃ¸ydebunn = col_character(),
#       distanseaptilx = col_character()
#     )
#   )
# readr::spec(fartÃ¸y)

# Konsekvens
# konsekvens <-
#   read_csv("~/Master/delt_opp/konsekvens.csv")
# readr::spec(konsekvens)

# Miljøskade
# miljøskade <-
#   read_csv(
#     "~/Master/delt_opp/miljøskade.csv",
#     col_types = cols(fnnummer = col_character())
#   )
# readr::spec(miljøskade)

# Person
person <-
  read_csv(
    "~/Master/delt_opp/person.csv",
    col_types = cols(pusulykkenummer = col_character(),
                     yrkeskodefisker = col_character(),
                     yrkeskodefisker_kode = col_character()
                     )
  )
readr::spec(person)

# Personskade
personskade <-
  read_csv("~/Master/delt_opp/personskade.csv")
readr::spec(personskade)
# Personverneutstyr
personverneutstyr <-
  read_csv("~/Master/delt_opp/personverneutstyr.csv")
readr::spec(personverneutstyr)

# Tilrådning
# tilrådning <-
#   read_csv("~/Master/delt_opp/tilrådning.csv")
# readr::spec(tilrådning)

# Tilrådningstiltak
tilrådningstiltak <-
  read_csv("~/Master/delt_opp/tilrådningstiltak.csv")
readr::spec(tilrådningstiltak)

# Ulykke
ulykke <-
  read_csv(
    "~/Master/delt_opp/ulykke.csv",
    col_types = cols(posisjon_breddegrad = col_character())
  )
readr::spec(ulykke)

# ÅrsakDetalj
årsaksdetalj <-
  read_csv("~/Master/delt_opp/årsaksdetaljer.csv")
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

#### Creating a new variable of free text fields ####

# Concatenating the free text fields, most of which are from årsak.

årsak_with_detail <- full_join(x = årsak, y = årsaksdetalj, by = "årsak_id", keep = TRUE)
årsak_with_detail$årsak_id <- årsak_with_detail$årsak_id.x

# Using full_join, all values that exist in either dataset x or y is included.

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
    values_drop_na = FALSE
  ) %>%
  group_by(årsak_id) %>%
  summarise(ALL = toString(unique(value))) %>%
  full_join(årsak)

# Add hendelsesforløp
full_data <- full_join(x = årsak_with_detail, y = ulykke, by = 'ulykke_id', keep = TRUE)

full_data$ulykke_id <- full_data$ulykke_id.x

# Continue here, add person$personskade and keep uniques

full_data <- full_data %>%
  pivot_longer(
    cols = c(hendelsesforløp, 
             ALL),
    values_drop_na = FALSE
  ) %>%
  group_by(ulykke_id) %>% 
  summarise(ALL = toString(unique(value))) %>%
  full_join(full_data)

full_data <- full_join(x = full_data, y = person, by = 'person_id', keep = TRUE)

full_data$ulykke_id <- full_data$ulykke_id.x

full_data <- full_data %>%
  pivot_longer(
    cols = c(personskade, 
             ALL),
    values_drop_na = FALSE
  ) %>%
  group_by(ulykke_id) %>% 
  summarise(ALL = toString(unique(value))) %>%
  full_join(full_data)


full_data$person_id <- full_data$person_id.x

full_data <- full_join(x = full_data, y = personskade, by = 'person_id', keep = TRUE)

full_data <- full_data %>%
  pivot_longer(
    cols = c(annenskade, 
             ALL),
    values_drop_na = FALSE
  ) %>%
  group_by(ulykke_id) %>% 
  summarise(ALL = toString(unique(value))) %>%
  full_join(full_data)

# Removing datasets we combined to save some space.

rm(årsak)
rm(årsak_with_detail)
rm(årsaksdetalj)
rm(person)
rm(personskade)
rm(personverneutstyr)
rm(tilrådningstiltak)

## By year
library(chron)

a <- mean(table(format(years(
  as.Date(ulykke$ulykkedato, format = "%d.%m.%Y")))))

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
  geom_hline(yintercept = a, color = safe_colorblind_palette[2]) +
  geom_label(aes(x = 1, y = a, label = paste("Mean =", round(a))), nudge_x = 3.1, nudge_y = 50, size = 5, family = "Times")+
  scale_y_continuous(breaks = seq(0, 2000, by = 250)) +
  labs(
    x = "Year",
    y = "Number of entries per year",
    title = paste0("Reported accidents in Sdir's dataset. N = ", nrow(ulykke[!is.na(ulykke$ulykkedato), ]))
  ) +
  scale_fill_manual(values = c(safe_colorblind_palette))

rm(a)

## By month
month.name <- as.factor(month.name)

full_data$ulykkesmåned <- str_squish(format(factor(months(
  as.Date(full_data$ulykkedato, format = "%d.%m.%Y")
))))
full_data$ulykkesmåned
a <- mean(table(full_data$ulykkesmåned))


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
  #scale_y_continuous(breaks = seq(0, 4000, by = 500)) +
  geom_hline(yintercept = a)+
  geom_label(aes(x = 0, y = a, label = paste("Mean =", round(a))), nudge_x = 6.5, nudge_y = 110, size = 5, family = "Times")+
  labs(
    x = "Month",
    y = "Number of entries per month",
    title = paste0(
      "Reported accidents (1981-2022) in Sdir's dataset. N = ",
      nrow(full_data[!is.na(full_data$ulykkedato), ])
    )
  ) +
  scale_fill_manual(values = c(safe_colorblind_palette))

# I just added it manually. Might create a prettier solution later.
rm(a)

# How many injured and dead

ulykke %>% 
  count(antall_skadet) %>%
  ggplot(aes(antall_skadet, n))+
  geom_col()+
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
  labs(
    x = "Injured people per accident report",
    y = "Count",
    title = "Frequency of injured people from each report"
  ) +
  scale_x_continuous(breaks = seq(0, 75, by = 5)) +
  geom_text(aes(label = n), nudge_y = 1000, nudge_x = -0.2, angle = 90, size = 3)+
  scale_fill_manual(values = c(safe_colorblind_palette))
  
# Only deceased

ulykke %>% 
  count(antall_omkommet) %>%
  ggplot(aes(antall_omkommet, n, label = antall_omkommet))+
  geom_col()+
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
  labs(
    x = "Number of deceased people",
    y = "Count",
    title = "Distribution of deceased people from each report"
  ) +
  scale_x_continuous(breaks = seq(0, 20, by = 1)) +
  geom_text(aes(label = n), nudge_y = 600)+
  scale_fill_manual(values = c(safe_colorblind_palette))

ulykke %>% 
  mutate(antall_antatt_døde = as.numeric(antall_savnet) + as.numeric(antall_omkommet)) %>%
  count(antall_antatt_døde) %>%
  ggplot(aes(antall_antatt_døde, n))+
  geom_col()+
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
  labs(
    x = "Number of deceased or missing",
    y = "Count",
    title = "Distribution of deceased or missing people from each report"
  ) +
  scale_x_continuous(breaks = seq(0, 20, by = 1)) +
  geom_text(aes(label = n), nudge_y = 500)+
  scale_fill_manual(values = c(safe_colorblind_palette))

# Also done with "ulykke"

rm(ulykke)

# A bit of cleaning, removing punctuation etc.

# This creates too big of a file, so I'll have to postpone doing the full data
# set, splitting it up



full_data <- full_data %>%
  filter(!is.na(ALL)) %>%
  select(ALL, antall_skadet, antall_omkommet) %>%
  mutate(ALL_cleaned = str_replace_all(string = ALL, pattern =  "\n", replacement = "")) %>%
  mutate(ALL_cleaned2 = str_squish(removePunctuation(ALL_cleaned, preserve_intra_word_contractions = T, preserve_intra_word_dashes = T))) %>%
  select(ALL_cleaned2, antall_skadet, antall_omkommet) %>%
  rename(c("ALL" = "ALL_cleaned2"))



#### Stemming/stopwords or not? ####

# Without removing stopwords

full_data %>%
  unnest_tokens(word, ALL) %>%
  count(word, sort = TRUE)

full_data %>%
  unnest_tokens(word, ALL) %>%
  anti_join(get_stopwords("no")) %>%
  anti_join(get_stopwords("en")) %>%
  count(word, sort = TRUE)

# Removing custom words

custom_words <-
  c(
    "pus",
    "ca",
    "på",
    "g09",
    "g11",
    "kl",
    "fritekster",
    "konvertert",
    "fikk",
    "ulykkesbeskrivelse",
    "m",
    "skadebeskrivelse",
    "077",
    "106",
    "na",
    "dama",
    NA,
    "NA"
  )

full_data %>%
  unnest_tokens(word, ALL) %>%
  anti_join(get_stopwords("no")) %>%
  anti_join(get_stopwords("en")) %>%
  filter(!word %in% custom_words) %>%
  count(word, sort = TRUE)

# Stemming


#### Building a regression model ####
# From chapter 6 https://smltar.com/mlregression.html#firstmlregression

# I think the data set is too extensive as it is right now, extracting a smaller
# sample first

# small_data <- full_data[!is.na(full_data$antall_skadet),]

full_data <- full_data %>%
  mutate(ALL = str_replace_all(string = ALL, pattern =  "NA", replacement = "")) %>%
  mutate(ALL = str_squish(ALL))

# The algorithm can't handle missing values, so we're editing them to 0

full_data$antall_skadet[is.na(full_data$antall_skadet)] <- 0

library(tidymodels)
set.seed(1234)
full_data_split <-   full_data %>%
  mutate(ALL = removeNumbers(ALL)) %>%
  initial_split()

full_data_train <- training(full_data_split)
full_data_test <- testing(full_data_split)

library(textrecipes)


# We can add more predictors here, such as

full_data_rec <- recipe(antall_skadet ~ ALL, data = full_data_train) %>%
  step_tokenize(ALL) %>%
  step_stopwords(language = "no") %>%
  step_stopwords(language = "en") %>%
  step_stopwords(custom_stopword_source = custom_words) %>%
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
  extract_fit_parsnip() %>%
  tidy() %>%
  arrange(-estimate)

# The term Bias here means the same thing as an intercept. We see here what
# terms contribute to a rescue mission having more injured

svm_fit %>%
  extract_fit_parsnip() %>%
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
    title = "Predicted and true number of injured, linear SVM",
    subtitle = "Each cross-validation fold is shown in a different color"
  )+
  theme_bw()+
  scale_fill_manual(values = c(safe_colorblind_palette))

# Removing the outlier #10

svm_rs %>%
  collect_predictions() %>%
  ggplot(aes(antall_skadet, .pred, color = id)) +
  geom_abline(lty = 2, color = "gray80", size = 1.5) +
  geom_point(alpha = 0.3) +
  labs(
    x = "Actual number of injured",
    y = "Predicted number of injured",
    color = NULL,
    title = "Predicted and true number of injured, linear SVM",
    subtitle = "Each cross-validation fold is shown in a different color"
  ) +
  theme_bw()+
  scale_fill_manual(values = c(safe_colorblind_palette))+
  xlim(0, 22)


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

# Remember that this is using unigrams, but it is in fact a little better than
# the null model

collect_metrics(null_rs)
collect_metrics(svm_rs)


# Compare to random forest

# Changing from 1000 to 100 trees due to the time it took the calculations
library(ranger)

rf_spec <- rand_forest(trees = 100) %>%
  set_engine("ranger") %>%
  set_mode("regression")

rf_spec

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
    subtitle = "Each cross-validation fold is shown in a different color, using 1000 trees"
  )+
  #xlim(0,22)+
  theme_bw()

# Using n-grams for modeling
ngram_rec <- function(ngram_options) {
  recipe(antall_skadet ~ ALL, data = full_data_train) %>%
    step_stopwords(language = "no") %>%
    step_stopwords(custom_stopword_source = custom_words) %>%
    step_stopwords(language = "en") %>%
    step_tokenize(ALL, token = "ngrams", options = ngram_options) %>%
    step_tokenfilter(ALL, max_tokens = 1e3) %>%
    step_tfidf(ALL) %>%
    step_normalize(all_predictors())
}

svm_wf <- workflow() %>%
  add_model(svm_spec)

fit_ngram <- function(ngram_options) {
  fit_resamples(
    svm_wf %>% add_recipe(ngram_rec(ngram_options)),
    full_data_folds
  )
}

# Seeing the differences between uni-, bi- and trigrams for SVM

set.seed(123)
unigram_rs <- fit_ngram(list(n = 1))

set.seed(234)
bigram_rs <- fit_ngram(list(n = 2, n_min = 1))

set.seed(345)
trigram_rs <- fit_ngram(list(n = 3, n_min = 1))

set.seed(567)
quadgram_rs <- fit_ngram(list(n = 3, n_min = 1))

list(`1` = unigram_rs,
     `1 and 2` = bigram_rs,
     `1, 2, and 3` = trigram_rs,
     `1, 2, 3 and 4` = quadgram_rs) %>%
  map_dfr(collect_metrics, .id = "name") %>%
  filter(.metric == "rmse") %>%
  ggplot(aes(name, mean, color = name)) +
  geom_crossbar(aes(ymin = mean - std_err, ymax = mean + std_err), alpha = 0.6) +
  geom_point(size = 3, alpha = 0.8) +
  theme_bw()+
  theme(legend.position = "none") +
  labs(
    x = "Degree of n-grams",
    y = "RMSE",
    title = "Model performance for different degrees of n-gram tokenization",
    subtitle = "For the same number of tokens, unigrams performed best"
  )

# Comparing mean absolute % error for each resample

svm_rs %>%
  collect_predictions() %>%
  group_by(id) %>%
  mae(antall_skadet, .pred)


# Tuning the model and changing the number of tokens we use

final_rec <- recipe(antall_skadet ~ ALL, data = full_data_train) %>%
  step_tokenize(ALL, token = "ngrams", options = list(n = 2, n_min = 1)) %>%
  step_tokenfilter(ALL, max_tokens = tune()) %>%
  step_stopwords(language = "no") %>%
  step_stopwords(custom_stopword_source = custom_words) %>%
  step_stopwords(language = "en") %>%
  step_tfidf(ALL) %>%
  step_normalize(all_predictors())

final_rec

svm_spec <- svm_linear() %>%
  set_mode("regression") %>%
  set_engine("LiblineaR")

svm_spec

tune_wf <- workflow() %>%
  add_recipe(final_rec) %>%
  add_model(svm_spec)

tune_wf

final_grid <- grid_regular(
  max_tokens(range = c(1e3, 6e3)),
  levels = 6
)

final_grid

final_rs <- tune_grid(
  tune_wf,
  full_data_folds,
  grid = final_grid,
  metrics = metric_set(rmse, mae),
  control = control_resamples(save_pred = TRUE)
)

final_rs

# Grapichally represent differences
final_rs %>%
  collect_metrics() %>%
  ggplot(aes(max_tokens, mean, color = .metric)) +
  geom_line(size = 1.5, alpha = 0.5) +
  geom_point(size = 2, alpha = 0.9) +
  facet_wrap(~.metric, scales = "free_y", ncol = 1) +
  theme(legend.position = "none") +
  labs(
    x = "Number of tokens",
    title = "Linear SVM performance across number of tokens",
    subtitle = "Performance improves as we include more tokens"
  )

chosen_mae <- final_rs %>%
  select_by_pct_loss(metric = "mae", max_tokens, limit = 3)

chosen_mae

final_wf <- finalize_workflow(tune_wf, chosen_mae)

final_wf

# Evaluating it on real data

final_fitted <- last_fit(final_wf, full_data_split)

collect_metrics(final_fitted)

full_data_fit <- pull_workflow_fit(final_fitted$.workflow[[1]])

full_data_fit %>%
  tidy() %>%
  filter(term != "Bias") %>%
  #filter(term != "fartøyets") %>%
  mutate(
    sign = case_when(estimate > 0 ~ "More (than mean injured)",
                     TRUE ~ "Less (than mean injured)"),
    estimate = abs(estimate),
    term = str_remove_all(term, "tfidf_ALL_")
  ) %>%
  group_by(sign) %>%
  top_n(20, estimate) %>%
  ungroup() %>%
  ggplot(aes(x = estimate,
             y = fct_reorder(term, estimate),
             fill = sign)) +
  geom_col(show.legend = FALSE) +
  scale_x_continuous(expand = c(0, 0)) +
  facet_wrap(~sign, scales = "free") +
  labs(
    y = NULL,
    title = paste("Variable importance for predicting amount of",
                  "injured people in accident reports"),
    subtitle = paste("These features are the most importance",
                     "in predicting the amount of injured")
  )+
  theme_bw()+
  scale_fill_manual(values = c(safe_colorblind_palette))

final_fitted %>%
  collect_predictions() %>%
  ggplot(aes(antall_skadet, .pred)) +
  geom_abline(lty = 2, color = "gray80", size = 1.5) +
  geom_point(alpha = 0.3) +
  labs(
    x = "Actual",
    y = "Predicted amount of injured people",
    title = paste("Predicted and actual amount of injured for the testing set of",
                  "accident reports"),
    subtitle = "For the testing set, predictions are most accurate between 0-5 injured"
  ) +
  scale_x_continuous(breaks = seq(0,22, by = 1))+
  theme_bw()

full_data_bind <- collect_predictions(final_fitted) %>%
  bind_cols(full_data_test %>% select(-antall_skadet)) %>%
  filter(abs(antall_skadet - .pred) > 1)

full_data_bind %>%
  arrange(-antall_skadet) %>%
  select(antall_skadet, .pred, ALL)

#### LDA modeling ####
#### Ãrsak ####


# We create a term matrix we can clean up
full_data_Corpus <-
  Corpus(VectorSource(removePunctuation(full_data$ALL)))
full_data_DTM <- DocumentTermMatrix(full_data_Corpus)

# convert the document term matrix to a tidytext corpus
full_data_DTM_tidy <- tidy(full_data_DTM)

# I'm going to add my own custom stop words that I don't think will be
# very informative in these reports
custom_words

norwegian_stop_words <- rbind(tibble(word = tm::stopwords(kind = "no")), tibble(word = custom_words))


# remove stopwords
full_data_DTM_tidy_cleaned <-
  full_data_DTM_tidy %>% # take our tidy dtm and...
  anti_join(norwegian_stop_words, by = c("term" = "word")) %>% # remove Norwegian stopwords and custom words
  anti_join(stop_words, by = c("term" = "word")) # remove English stopwords as well

# reconstruct cleaned documents (so that each word shows up the correct number of times)
cleaned_documents <-          full_data_DTM_tidy_cleaned %>%
  group_by(document) %>%
  mutate(terms = toString(rep(term, count))) %>%
  select(document, terms) %>%
  unique()

# check out what the cleaned documents look like (should just be a bunch of
# content words) in alphabetic order
head(cleaned_documents)

top_terms_by_topic_LDA(cleaned_documents$terms, number_of_topics = 3)

# stem the words (e.g. convert each word to its stem, where applicable)
full_data_DTM_tidy_cleaned_stem <-  full_data_DTM_tidy_cleaned %>%
  mutate(stem = wordStem(term))

# reconstruct our documents
cleaned_documents_stem <- full_data_DTM_tidy_cleaned_stem %>%
  group_by(document) %>%
  mutate(terms = toString(rep(stem, count))) %>%
  select(document, terms) %>%
  unique()


# now let's look at the new most informative terms

top_terms_by_topic_LDA(cleaned_documents_stem$terms, number_of_topics = 4)

#### Modeling ####

# get just the tf-idf output for the type of vehicles

# In order to do supervised modeling, we need to label stuff, such as accident
# type


#### tf_idf ####


full_data$ulykketype[is.na(full_data$ulykketype)] <- "Ukjent"



tfidf_bygroup_ALL <- top_terms_by_topic_tfidf(text_df = full_data, 
                                                  text_column = ALL, 
                                                  group = ulykketype, 
                                                  plot = F)

# do our own plotting
tfidf_bygroup_ALL  %>% 
  group_by(ulykketype) %>% 
  top_n(5) %>% 
  ungroup %>%
  ggplot(aes(word, tf_idf, fill = ulykketype)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  theme_bw()+
  facet_wrap(~ulykketype, ncol = 4, scales = "free", ) +
  coord_flip()




#### n-grams ####
# not finished

full_data %>% 
  unnest_tokens(word, ALL, token = "ngrams", n = 2) %>% 
  filter(!is.na(word)) %>%
  separate(word, c("word1", "word2"), sep = " ") %>% 
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word1 %in% norwegian_stop_words$word) %>%
  filter(!word2 %in% norwegian_stop_words$word) %>%
  unite(word, word1, word2, sep = " ") %>% 
  count(word, sort = TRUE) %>% 
  slice(1:10) %>% 
  ggplot() + 
  geom_bar(aes(word, n), stat = "identity", fill = "#de5833", position = "dodge") +
  theme_bw() +
  coord_flip() +
  labs(title = "Top Bigrams of accident reports, using all free text fields")


# Attempting a sentiment analysis


full_data %>%
  unnest_tokens(word, ALL) %>%
  inner_join(get_sentiments("bing")) %>% # pull out only sentiment words
  count(sentiment) %>% # count the # of positive & negative words
  spread(sentiment, n, fill = 0) %>% # made data wide rather than narrow
  mutate(sentiment = positive - negative) # # of positive words - # of negative words

# plot of sentiment over time & automatically choose a method to model the change
full_data %>%
  mutate(ulykkesår = format((years(
    as.Date(ulykkedato, format = "%d.%m.%Y")
  ))))  %>%
  unnest_tokens(word, ALL) %>%
  inner_join(get_sentiments()) %>% # pull out only sentiment words
  count(sentiment) %>% # count the # of positive & negative words
  spread(sentiment, n, fill = 0) %>% # made data wide rather than narrow
  mutate(sentiment = positive - negative) # of positive words - # of negative words
  ggplot(sentiments, aes(x = as.numeric(ulykkesår), y = sentiment)) + 
  geom_point(aes(color = ulykkesår))+ # add points to our plot, color-coded by president
  geom_smooth(method = "auto") # pick a method & fit a model
  
  
  small_data <- full_data[!is.na(full_data$ALL),]
  
  small_data <- small_data %>%
    unnest_tokens(word, ALL) %>%
    anti_join(get_stopwords("no")) %>%
    anti_join(get_stopwords("en")) %>%
    filter(!word %in% custom_words)
  
  ## https://towardsdatascience.com/beginners-guide-to-lda-topic-modelling-with-r-e57a5a8e7a25
 
# Doesn't work, probably not necessary tho. 
  
#   small_data <- small_data %>%
#     mutate(ID = row_number())
#   
# small_data <- small_data %>% mutate(ind = row_number())
# 
# small_data <- small_data %>%
#     tidyr::pivot_wider(key = ID, value = ALL)
# 
# small_data [is.na(small_data)] <- ""
# 
# small_data <- tidyr::unite(small_data, text,-ID,sep =" " )
# 
# small_data$word <- trimws(small_data$word)

#create DTM
library(textmineR)
dtm <- CreateDtm(small_data$word,
                 ngram_window = c(1, 3))
#, stem_lemma_function = function(x) SnowballC::wordStem(x, language = "norwegian"
  
tf <- TermDocFreq(dtm = dtm)
  
original_tf <- tf %>% select(term, term_freq,doc_freq)
  
original_tf <- rowid_to_column(original_tf, var = "rowid") # Eliminate words appearing less than 2 times or in more than half of the
  # documents
vocabulary <- tf$term[ tf$term_freq > 1 & tf$doc_freq < nrow(dtm) / 2 ]
  
k_list <- seq(1, 20, by = 1)
setwd("~/Master/masterthesis")

model_dir <- paste0("models_", digest::digest(vocabulary, algo = "sha1"))
if(!dir.exists(model_dir)) dir.create(model_dir)
  
library(parallel)
model_list <- TmParallelApply(cpus = 4, X = k_list, FUN = function(k){
    filename = file.path(model_dir, paste0(k, "_topics.rda"))
    
if(!file.exists(filename)) {
      m <- FitLdaModel(dtm = dtm, k = k, iterations = 500)
      m$k <- k
      m$coherence <- CalcProbCoherence(phi = m$phi, dtm = dtm, M = 5)
      save(m, file = filename)
    } else {
      load(filename)
    }
    
    m
  }, export=c("dtm", "model_dir")) # export only needed for Windows machines

#model tuning
  
#choosing the best model
coherence_mat <- data.frame(k = sapply(model_list, function(x) nrow(x$phi)), 
                              coherence = sapply(model_list, function(x) mean(x$coherence)), 
                              stringsAsFactors = FALSE)


ggplot(coherence_mat, aes(x = k, y = coherence)) +
  geom_point() +
  geom_line(group = 1) +
  ggtitle("Best topics by coherence score") +
  theme_bw() +
  scale_x_continuous(breaks = seq(1, 20, by = 1)) + 
  scale_y_continuous(limits = c(-0.008, 0), breaks = seq(-0.008, 0, by = 0.001)) + 
  ylab("Coherence")
  
# Top 20 terms, describing what this topic is about.
model <- model_list[which.max(coherence_mat$coherence)][[ 1 ]]

model$top_terms <- GetTopTerms(phi = model$phi, M = 20)

top20_wide <- as.data.frame(model$top_terms)

allterms <-data.frame(t(model$phi))

allterms$word <- rownames(allterms)

rownames(allterms) <- 1:nrow(allterms)

allterms <- melt(allterms,idvars = "word") 

allterms <- allterms %>% rename(topic = variable)

FINAL_allterms <- allterms %>% group_by(topic) %>% arrange(desc(value))

# r^2 for LDA ####

library(textmineR)
r2 <- CalcTopicModelR2(dtm = dtm, phi = model_list[[19]]$phi, theta = model_list[[19]]$theta, cpus = 4)

# 0.0007624563

# summary of document lengths
doc_lengths <- rowSums(dtm)

summary(doc_lengths)

# what words are most associated with more than mean injured?

# remove any tokens that were in 3 or fewer documents
dtm2 <- dtm[ , colSums(dtm > 0) > 3 ]

tf2 <- tf[ tf$term %in% colnames(dtm2) , ]

# look at the most frequent bigrams
tf_bigrams <- tf2[ stringr::str_detect(tf2$term, "_") , ]

tf_bigrams <- tf_bigrams[ tf_bigrams$term %in% colnames(dtm2) , ]

tf_meanantallskadet <- list(less = TermDocFreq(dtm[full_data$antall_skadet < 1.01 , ]),
                            more = TermDocFreq(dtm[full_data$antall_skadet > 1.01 , ]))

head(tf_meanantallskadet$less[ order(tf_meanantallskadet$less$term_freq, decreasing = TRUE) , ], 10)

head(tf_meanantallskadet$more[ order(tf_meanantallskadet$more$term_freq, decreasing = TRUE) , ], 10)


# let's reweight by probability by class
p_words <- colSums(dtm) / sum(dtm) 

tf_meanantallskadet$less$conditional_prob <- 
  tf_meanantallskadet$less$term_freq / sum(tf_meanantallskadet$less$term_freq)

tf_meanantallskadet$less$prob_lift <- tf_meanantallskadet$less$conditional_prob - p_words

tf_meanantallskadet$more$conditional_prob <- 
  tf_meanantallskadet$more$term_freq / sum(tf_meanantallskadet$more$term_freq)

tf_meanantallskadet$more$prob_lift <- tf_meanantallskadet$more$conditional_prob - p_words

# let's look again with new weights
head(tf_meanantallskadet$less[ order(tf_meanantallskadet$less$prob_lift, decreasing = TRUE) , ], 10)

head(tf_meanantallskadet$more[ order(tf_meanantallskadet$more$prob_lift, decreasing = TRUE) , ], 10)

# what about bi-grams?
tf_meanantallskadet_bigram <- lapply(tf_meanantallskadet, function(x){
  x <- x[ stringr::str_detect(x$term, "_") , ]
  x[ order(x$prob_lift, decreasing = TRUE) , ]
})

head(tf_meanantallskadet_bigram$less, 10)

head(tf_meanantallskadet_bigram$more, 10)


# Calculating cosine similarity and distance

# TF-IDF and cosine similarity

full_data_Corpus <-
  Corpus(VectorSource(removePunctuation(full_data$ALL)))
full_data_DTM <- DocumentTermMatrix(full_data_Corpus, 
                                    control = list(weighting = "weightTfIdf", 
                                                   removeNumbers = TRUE))


full_data_DTM

# Didn't prove very useful. I can't calculate the cosine distance/values
# because of how large the data set is.

# Get the top terms of each topic
model$top_terms <- GetTopTerms(phi = model$phi, M = 5)

model$top_terms

# Get the prevalence of each topic
# You can make this discrete by applying a threshold, say 0.05, for
# topics in/out of docuemnts. 
model$prevalence <- colSums(model$theta) / sum(model$theta) * 100

# prevalence should be proportional to alpha
plot(model$prevalence, model$alpha, xlab = "prevalence", ylab = "alpha")


# textmineR has a naive topic labeling tool based on probable bigrams
model$labels <- LabelTopics(assignments = model$theta > 0.05, 
                            dtm = dtm,
                            M = 1)

head(model$labels)


# put them together, with coherence into a summary table
model$summary <- data.frame(topic = rownames(model$phi),
                            label = model$labels,
                            coherence = round(model$coherence, 3),
                            prevalence = round(model$prevalence,3),
                            top_terms = apply(model$top_terms, 2, function(x){
                              paste(x, collapse = ", ")
                            }),
                            stringsAsFactors = FALSE)


model$summary[ order(model$summary$prevalence, decreasing = TRUE) , ][ 1:10 , ]


# predictions with gibbs
assignments <- predict(model, dtm,
                       method = "gibbs", 
                       iterations = 200,
                       burnin = 180,
                       cpus = 2)

# predictions with dot
assignments_dot <- predict(model, dtm2,
                           method = "dot")


# compare
barplot(rbind(assignments[10,], assignments_dot[10,]),
        col = c("red", "blue"), las = 2, beside = TRUE)
legend("topright", legend = c("gibbs", "dot"), col = c("red", "blue"), 
       fill = c("red", "blue"))


# Exporting top 20 terms

write.csv(top20_wide, "top20_wide.csv")
  
# Dendrogram for calculating similarities
  model$topic_linguistic_dist <- CalcHellingerDist(model$phi)
  
  model$hclust <- hclust(as.dist(model$topic_linguistic_dist), "ward.D2")
  
  model$hclust$labels <- paste(model$hclust$labels, model$labels[ , 1])
  
  plot(model$hclust, xlab = "Topic relationships", sub = "")
  
  #visualising topics of words based on the max value of phi
  
  set.seed(1234)
  
  final_summary_words <- data.frame(top_terms = t(model$top_terms))
  
  final_summary_words$topic <- rownames(final_summary_words)
  
  rownames(final_summary_words) <- 1:nrow(final_summary_words)
  
  final_summary_words <- final_summary_words %>% melt(id.vars = c("topic"))
  
  final_summary_words <- final_summary_words %>% rename(word = value) %>% select(-variable)
  
  final_summary_words <- left_join(final_summary_words, allterms)
  
  final_summary_words <- final_summary_words %>% group_by(topic,word) %>%
    arrange(desc(value))
  
  final_summary_words <- final_summary_words %>% group_by(topic, word) %>% filter(row_number() == 1) %>% 
    ungroup() %>% tidyr::separate(topic, into =c("t","topic")) %>% select(-t)
  
  word_topic_freq <- left_join(final_summary_words, original_tf, by = c("word" = "term"))
  
  library(wordcloud)
  pdf("cluster.pdf")
  
  for(i in 1:length(unique(final_summary_words$topic)))
  {  wordcloud(words = subset(final_summary_words ,topic == i)$word, freq = subset(final_summary_words ,topic == i)$value, min.freq = 1,
               max.words=200, random.order=FALSE, rot.per=0.35, 
               colors=brewer.pal(8, "Dark2"))}
  dev.off()

# Single wordcloud

 wordcloud::wordcloud(words = subset(final_summary_words ,topic == 15)$word, freq = subset(final_summary_words ,topic == i)$value, min.freq = 1,
            max.words=200, random.order=FALSE, rot.per=0.25, 
            colors=brewer.pal(8, "Dark2"))  
 
 

 
# other stuff
  cleaned_full_data <- full_data %>%
    unnest_tokens(word, ALL) %>%
    anti_join(get_stopwords("no")) %>%
    anti_join(get_stopwords("en")) %>%
    filter(!word %in% custom_words)
  
  cleaned_full_data$word <- stemDocument(cleaned_full_data$word, language = "norwegian")
  
  top_terms_by_topic_LDA(input_text = cleaned_full_data$word, plot = T, number_of_topic = 20)
  
  top_terms_by_topic_tfidf(text_df = full_data)
  
  tfidf_bygroup_ALL <- top_terms_by_topic_tfidf(text_df = full_data, 
                                                text_column = ALL, 
                                                group = ulykketype, 
                                                plot = T)
  