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
# 
# if (!require("party"))
#   install.packages("party")
# library(textrecipes) # Create decision trees

if (!require("themis"))
  install.packages("themis")
library(themis) # For classifying when the classes are unbalanced

if (!require("discrim"))
  install.packages("discrim")
library(discrim) # Naive Bayes classifiers

if (!require("naivebayes"))
  install.packages("naivebayes")
library(naivebayes) # Naive Bayes classifiers

if (!require("textmineR"))
  install.packages("textmineR")
library(textmineR) # Naive Bayes classifiers

if (!require("parallel"))
  install.packages("parallel")
library(parallel) # Naive Bayes classifiers

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
      DTM[unique_indexes,] # get a subset of only those indexes
    
    # preform LDA & get the words/topic in a tidy text format
    lda <-
      LDA(DTM, k = number_of_topics, control = list(seed = 1234))
    topics <- tidy(lda, matrix = "beta")
    
    # get the top ten terms for each topic
    top_terms <- topics  %>% # take the topics data frame and..
      group_by(topic) %>% # treat each topic as a different group
      top_n(10, beta) %>% # get the top 10 most informative words
      ungroup() %>% # ungroup
      arrange(topic,-beta) # arrange words in descending informativeness
    
    # if the user asks for a plot (TRUE by default)
    if (plot == T) {
      # plot the top ten terms for each topic in order
      top_terms %>% # take the top terms
        mutate(term = reorder(term, beta)) %>% # sort terms by beta value
        ggplot(aes(term, beta, fill = factor(topic))) + # plot beta by theme
        theme_bw() +
        geom_col(show.legend = FALSE) + # as a bar plot
        facet_wrap(~ topic, scales = "free") + # which each topic in a seperate plot
        labs(x = NULL, y = "Beta") + # no x label, change y label
        coord_flip() # turn bars sideways
    } else{
      # if the user does not request a plot
      # return a list of sorted terms instead
      return(top_terms)
    }
  }

top_terms_by_topic_tfidf <-
  function(text_df, text_column, group_column, plot = T) {
    # name for the column we're going to unnest_tokens_ to
    # (you only need to worry about enquo stuff if you're
    # writing a function using using tidyverse packages)
    group_column <- enquo(group_column)
    text_column <- enquo(text_column)
    
    # get the count of each word in each review
    words <- text_df %>%
      unnest_tokens(word,!!text_column) %>%
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
      bind_tf_idf(word,!!group_column, n) %>%
      select(-total) %>%
      arrange(desc(tf_idf)) %>%
      mutate(word = factor(word, levels = rev(unique(word))))
    
    if (plot == T) {
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
    } else{
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
    col_types = cols(
      pusulykkenummer = col_character(),
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
  read_csv("~/Master/delt_opp/ulykke.csv",
           col_types = cols(posisjon_breddegrad = col_character()))
readr::spec(ulykke)

# ÅrsakDetalj
årsaksdetalj <-
  read_csv("~/Master/delt_opp/årsaksdetaljer.csv")
readr::spec(årsaksdetalj)

# Commands that might prove useful:
readr::tokenizer_csv()



#### Creating a new variable of free text fields ####

# Concatenating the free text fields, most of which are from årsak.
# In order to do that, we need to merge some data sets. We find the variables
# that are shared in order to sort by the same variables.

intersect(x = labels(årsak)[[2]], y = labels(årsaksdetalj)[[2]])

# Årsaks-ID
sum(table(intersect(årsak$årsak_id,  årsaksdetalj$årsak_id)))

# Fartøy-ID
sum(table(intersect(årsak$fartøy_id,  årsaksdetalj$fartøy_id)))

# Ulykke-ID
sum(table(intersect(årsak$ulykke_id,  årsaksdetalj$ulykke_id)))

# Person-ID
sum(table(intersect(årsak$ulykke_id,  årsaksdetalj$ulykke_id)))

# Using full_join, all values that exist in either dataset x or y is included.

årsak_with_detail <- full_join(
  x =  årsak,
  y =  årsaksdetalj,
  by = "årsak_id",
  keep = FALSE
)

# The variable årsak_id is the one with the most shared values, so we'll use
# that one to concatenate the data sets

table(årsak$årsak_id %in%  årsaksdetalj$årsak_id)

# 24 of the årsaks_ids in årsaksdetalj are not present in årsak.

table(årsak_with_detail$årsak_id %in%  årsak$årsak_id)

# But in the new data set årsak_with_detail, some of the IDs are repeated.
# That's why we get 28105 results here, when the initial results were lower

# But a lot of the årsak_ids are repeated, presumably because of multiple
# registrations per cause, one per person. This is the case in årsaksdetalj,
# and is also the case for person_id, but much more so in årsaksdetalj.

table(duplicated(årsak$årsak_id))
table(duplicated(årsaksdetalj$årsak_id))

# person_id
table(duplicated(årsak$person_id))
table(duplicated(årsaksdetalj$person_id))

# We're gonna check which grouping variables are present in both datasets:

intersect(x = labels(årsak_with_detail)[[2]], y = labels(ulykke)[[2]])

# No variables are repeated, which seems strange. A manual check is in order
labels(årsak_with_detail)[[2]]
labels(ulykke)[[2]]

grep("_id", labels(ulykke)[[2]])
# Only one label with the suffix _id, ulykke_id. This is also present in
# årsak_with_detail, but it is based on a previous dataset.

table(årsak_with_detail$ulykke_id.x %in% ulykke$ulykke_id)
table(årsak_with_detail$ulykke_id.y %in% ulykke$ulykke_id)

# So 28105 of the ulykke_ids in the combined dataset is present in the ulykke
# dataset.

årsak_with_detail$ulykke_id <- årsak_with_detail$ulykke_id.x

# We'll make that our new grouping variable

full_data <-
  full_join(
    x =  årsak_with_detail,
    y = ulykke,
    by = 'ulykke_id',
    keep = FALSE
  )

# We see that we now have 48907 cases and none of them are NA. This number is
# the amount of cases in the dataset with the most different årsak_ids.
table(complete.cases(full_data$ulykke_id))

sum(table(intersect(
  full_data$ulykke_id, ulykke$ulykke_id)))

# The intersect of the årsak_ids is the årsak_ids that are repeated in both data
# sets, both årsak_with_detail and årsak.

table(duplicated(full_data$ulykke_id))

# 11071 of the cases are duplicated. That's about the same as before we merged
# them, but it stems from when we concatenated the cases, we wanted to only keep
# unique values, so in the case of non-uniques, they were dropped and we were
# able to drop them.


# Our new data set has 48907 complete cases and no NA values present. This might
# be considered a little strange, as the data set ulykke "only" has 37836 cases,
# but this is because of the full_join function, where some of the cases were
# not present in one or the other data sets.

intersect(x = labels(full_data)[[2]], y = labels(person)[[2]])

full_data <-
  full_join(x = full_data,
            y = person,
            by = 'ulykke_id',
            keep = FALSE)

table(duplicated(full_data$ulykke_id))

# 37836 cases are not repeated

intersect(x = labels(full_data)[[2]], y = labels(personskade)[[2]])

# We have not worked with person_id before, so let's take a look at it.

table(complete.cases(full_data$person_id))
table(duplicated(full_data$person_id))

table(personskade$person_id %in% full_data$person_id)

# 39615 cases from person_id in the personskade-dataset is available in
# full_data, which is also the full extent of the person_ids.

full_data <-
  full_join(x = full_data,
            y = personskade,
            by = 'person_id',
            keep = FALSE)


# Before we merge the free-text fields, let's have a look at which grouping
# variable we should use.

table(duplicated(full_data$ulykke_id))
table(duplicated(full_data$person_id))
table(duplicated(full_data$årsak_id))

# We quickly see that ulykke_id is the one that's duplicated the least, meaning
# that we should be able to use this one to group the following part

full_data <- full_data %>%
  pivot_longer(
    cols = c(
      direkteårsak_person_fritekst,
      direkteårsak_ytre_fritekst,
      direkteårsak_utstyr_fritekst,
      indirekteårsak_person_fritekst,
      indirekteårsak_arbeidsmiljø_fritekst,
      indirekteårsak_ytre_fritekst,
      indirekteårsak_utstyr_fritekst,
      bakenforårsak_ledelse_fritekst,
      bakenforårsak_prosedyre_fritekst,
      fritekst,
      hendelsesforløp,
      personskade,
      annenskade
    ),
    values_drop_na = TRUE
  ) %>%
  group_by(ulykke_id) %>%
  summarise(ALL = str_squish(toString(unique(value)))) %>%
  inner_join(full_data)

# We ended up on a total of 79800 observations.

head(full_data$ALL)

# Some of them are repeated, so we'll just remove those instances.

full_data <- full_data[!duplicated(full_data$ALL), ]

head(full_data$ALL)

table(duplicated(full_data$ulykke_id))

# We ended up with 37830 observations, of which none of the ulykke_ids are
# repeated. In total we lost 6 observations from the ulykke dataset, but that's 
# not an issue.

# Removing datasets we combined to save some space.

rm(årsak)
rm(årsak_with_detail)
rm(årsaksdetalj)
rm(person)
rm(personskade)
rm(personverneutstyr)
rm(tilrådningstiltak)
rm(ulykke)

gc()

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
  as.Date(full_data$ulykkedato, format = "%d.%m.%Y")
))))

full_data %>%
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
      hjust = 1.1,
      color = "black"
    ),
    axis.text.y = element_text(size = 10),
    axis.title.y = element_text(size = 12.5),
    axis.title.x = element_text(size = 12.5),
    axis.title = element_text(size = 10),
    legend.position = "bottom"
  ) +
  geom_hline(yintercept = a, color = safe_colorblind_palette[2]) +
  geom_label(
    aes(
      x = 1,
      y = a,
      label = paste("Mean =", round(a))
    ),
    nudge_x = 3.1,
    nudge_y = 50,
    size = 5,
    family = "Times"
  ) +
  scale_y_continuous(breaks = seq(0, 2000, by = 250)) +
  labs(
    x = "Year",
    y = "Number of entries per year",
    title = paste0("Reported accidents in Sdir's dataset. N = ", nrow(full_data[!is.na(full_data$ulykkedato),]))
  ) +
  scale_fill_manual(values = c(safe_colorblind_palette))

rm(a)

## By month
month_name <-
  factor(
    month.name,
    ordered = TRUE, 
    levels = c(
      "January",
      "February",
      "March",
      "April",
      "May",
      "June",
      "July",
      "August",
      "September",
      "October",
      "November",
      "December"
    )
  )

# Setting the date format to English rather than Norwegian

Sys.setlocale("LC_TIME", "English")

full_data$ulykkedato2 <-
  as.POSIXct(full_data$ulykkedato, format = "%d.%m.%Y")

full_data$ulykkesmåned <- months(full_data$ulykkedato2)

full_data$ulykkesmåned <- factor(
  full_data$ulykkesmåned,
  ordered = TRUE,
  levels = c(
    "January",
    "February",
    "March",
    "April",
    "May",
    "June",
    "July",
    "August",
    "September",
    "October",
    "November",
    "December"
  )
)

a <- mean(table(full_data$ulykkesmåned))

ggplot(full_data,
       aes(ulykkesmåned)) +
  geom_bar() +
  theme_bw() +
  theme(
    axis.text.x = element_text(
      angle = 60,
      size = 10,
      vjust = 1,
      hjust = 1.1,
      color = "black"
    ),
    axis.text.y = element_text(size = 10),
    axis.title.y = element_text(size = 12.5),
    axis.title.x = element_text(size = 12.5),
    axis.title = element_text(size = 10),
    legend.position = "bottom"
  ) +
  scale_y_continuous(breaks = seq(0, 4000, by = 500)) +
  geom_hline(yintercept = 3152) +
  geom_label(
    aes(
      x = 0,
      y = 3152,
      label = "Mean = 3 152.5"
    ),
    nudge_x = 6.5,
    nudge_y = 110,
    size = 5
  ) +
  labs(
    x = "Month",
    y = "Number of entries per month",
    title = "Reported accidents (1981-2022) in Sdir's dataset. N = 37 830")

# I just added it manually. Might create a prettier solution later.
rm(a)

# How many injured and dead

full_data %>%
  count(antall_skadet) %>%
  ggplot(aes(antall_skadet, n)) +
  geom_col() +
  theme_bw() +
  theme(
    axis.text.x = element_text(
      angle = 60,
      size = 10,
      vjust = 1,
      hjust = 1.1,
      color = "black"
    ),
    axis.text.y = element_text(size = 10),
    axis.title.y = element_text(size = 12.5),
    axis.title.x = element_text(size = 12.5),
    axis.title = element_text(size = 10),
    legend.position = "bottom"
  ) +
  labs(x = "Injured people per accident report",
       y = "Count",
       title = "Frequency of injured people from each report") +
  scale_x_continuous(breaks = seq(0, 75, by = 5)) +
  geom_text(
    aes(label = n),
    nudge_y = 1000,
    nudge_x = -0.2,
    angle = 90,
    size = 3
  ) +
  scale_fill_manual(values = c(safe_colorblind_palette))

# Only deceased

full_data %>%
  count(antall_omkommet) %>%
  ggplot(aes(antall_omkommet, n, label = antall_omkommet)) +
  geom_col() +
  theme_bw() +
  theme(
    axis.text.x = element_text(
      angle = 60,
      size = 10,
      vjust = 1,
      hjust = 1.1,
      color = "black"
    ),
    axis.text.y = element_text(size = 10),
    axis.title.y = element_text(size = 12.5),
    axis.title.x = element_text(size = 12.5),
    axis.title = element_text(size = 10),
    legend.position = "bottom"
  ) +
  labs(x = "Number of deceased people",
       y = "Count",
       title = "Distribution of deceased people from each report") +
  scale_x_continuous(breaks = seq(0, 20, by = 1)) +
  geom_text(aes(label = n), nudge_y = 600) +
  scale_fill_manual(values = c(safe_colorblind_palette))

full_data %>%
  mutate(antall_antatt_døde = as.numeric(antall_savnet) + as.numeric(antall_omkommet)) %>%
  count(antall_antatt_døde) %>%
  ggplot(aes(antall_antatt_døde, n)) +
  geom_col() +
  theme_bw() +
  theme(
    axis.text.x = element_text(
      angle = 60,
      size = 10,
      vjust = 1,
      hjust = 1.1,
      color = "black"
    ),
    axis.text.y = element_text(size = 10),
    axis.title.y = element_text(size = 12.5),
    axis.title.x = element_text(size = 12.5),
    axis.title = element_text(size = 10),
    legend.position = "bottom"
  ) +
  labs(x = "Number of deceased or missing",
       y = "Count",
       title = "Distribution of deceased or missing people from each report") +
  scale_x_continuous(breaks = seq(0, 20, by = 1)) +
  geom_text(aes(label = n), nudge_y = 500) +
  scale_fill_manual(values = c(safe_colorblind_palette))

# Visualising accident types

full_data %>%
  count(ulykketype) %>%
  ggplot(aes(ulykketype, n, label = ulykketype)) +
  geom_col() +
  theme_bw() +
  theme(
    axis.text.x = element_text(
      angle = 60,
      size = 10,
      vjust = 1,
      hjust = 1.1,
      color = "black"
    ),
    axis.text.y = element_text(size = 10),
    axis.title.y = element_text(size = 12.5),
    axis.title.x = element_text(size = 12.5),
    axis.title = element_text(size = 10),
    legend.position = "bottom"
  ) +
  labs(x = "Accident types (in Norwegian)",
       y = "Count",
       title = "Distribution of accident types from each report") +
  geom_text(aes(label = n), nudge_y = 650) +
  scale_fill_manual(values = c(safe_colorblind_palette))

full_data %>%
  count(ulykketype) %>%
  ggplot(aes(ulykketype, n, label = ulykketype)) +
  geom_col() +
  theme_bw() +
  theme(
    axis.text.x = element_text(
      angle = 60,
      size = 10,
      vjust = 1,
      hjust = 1.1,
      color = "black"
    ),
    axis.text.y = element_text(size = 10),
    axis.title.y = element_text(size = 12.5),
    axis.title.x = element_text(size = 12.5),
    axis.title = element_text(size = 10),
    legend.position = "bottom"
  ) +
  labs(x = "Accident types (in Norwegian)",
       y = "Count",
       title = "Distribution of accident types from each report") +
  geom_text(aes(label = n), nudge_y = 650) +
  scale_fill_manual(values = c(safe_colorblind_palette))

# Faceting accident type over years

full_data$years <- years(full_data$ulykkedato2)

full_data$years <- as.numeric(as.character(full_data$years))

table(full_data$years)

table(full_data$ulykketype)

p <- ggplot(data = full_data, aes(years)) +
  geom_histogram(stat = "count", binwidth = "unscaled x") +
  theme_bw()+
  theme(
    axis.text.x = element_text(
      size = 8,
      color = "black"
    ),
    axis.text.y = element_text(size = 10),
    axis.title.y = element_text(size = 12.5),
    axis.title = element_text(size = 10),
    legend.position = "bottom"
  ) +
  #guides(x = guide_axis(n.dodge = 2, angle = 50), y.sec = guide_axis(), x.sec()) +
  scale_x_continuous(breaks = seq(1981, 2022, by = 2))+
  guides(x = guide_axis(angle = 50))+ #, x.sec = guide_axis(angle = 50))+
  labs(
    x = "Year",
    y = "Number of entries per year",
    title = paste0("Reported accidents in Sdir's dataset, by accident type. N = ", nrow(full_data[!is.na(full_data$ulykketype),]))
  ) +
  scale_fill_manual(values = c(safe_colorblind_palette))
p

# Adding a margin or a total plot.

df <- rbind(full_data, transform(full_data, ulykketype = "All combined"))

# The package "lemon" let's us add x-ticks to the plot with the command below,
# facet_rep_wrap.

library(lemon)

p %+% df + facet_rep_wrap(.~ ulykketype, ncol = 3, scales = "free_y", repeat.tick.labels = "bottom")

p2 <- ggplot(data = full_data, aes(ulykkesmåned)) +
  geom_histogram(stat = "count", binwidth = "unscaled x") +
  theme_bw()+
  theme(
    axis.text.x = element_text(
      size = 8,
      color = "black"
    ),
    axis.text.y = element_text(size = 10),
    axis.title.y = element_text(size = 12.5),
    axis.title = element_text(size = 10),
    legend.position = "bottom"
  ) +
  #guides(x = guide_axis(n.dodge = 2, angle = 50), y.sec = guide_axis(), x.sec()) +
  #scale_x_continuous(breaks = seq(1981, 2022, by = 2))+
  guides(x = guide_axis(angle = 50))+ #, x.sec = guide_axis(angle = 50))+
  labs(
    x = "Year",
    y = "Number of entries per year",
    title = paste0("Reported accidents in Sdir's dataset, by accident type. N = ", nrow(full_data[!is.na(full_data$ulykketype),]))
  ) +
  scale_fill_manual(values = c(safe_colorblind_palette))
p2

# Adding a margin or a total plot.

df <- rbind(full_data, transform(full_data, ulykketype = "All combined"))

# The package "lemon" let's us add x-ticks to the plot with the command below,
# facet_rep_wrap.

library(lemon)

p2 %+% df + facet_rep_wrap(.~ ulykketype, ncol = 3, scales = "free_y", repeat.tick.labels = "bottom")


# A sneak peak at the MTO variable:


# Creating a new variable
full_data$MTO <- NA

# Man

full_data$MTO[full_data$ulykketype == "Arbeidsulykke/Personulykke"] <- "Man"
full_data$MTO[full_data$ulykketype == "Annen ulykke"] <- "Man"

# Technology

full_data$MTO[full_data$ulykketype == "Maskinhavari"] <- "Technology"
full_data$MTO[full_data$ulykketype == "Brann/Eksplosjon"] <- "Technology"
full_data$MTO[full_data$ulykketype == "Kantring"] <- "Technology"
full_data$MTO[full_data$ulykketype == "Kollisjon"] <- "Technology"
full_data$MTO[full_data$ulykketype == "Lekkasje"] <- "Technology"
full_data$MTO[full_data$ulykketype == "Stabilitetssvikt uten kantring"] <- "Technology"

# Organisation

full_data$MTO[full_data$ulykketype == "Feil på redningsmidler"] <- "Organisation"
full_data$MTO[full_data$ulykketype == "Hardtværskade"] <- "Organisation"
full_data$MTO[full_data$ulykketype == "Kontaktskade, Kaier, Broer etc"] <- "Organisation"
full_data$MTO[full_data$ulykketype == "Miljøskade/Forurensing"] <- "Organisation"
full_data$MTO[full_data$ulykketype == "Grunnstøting"] <- "Organisation"
full_data$MTO[full_data$ulykketype == "Fartøyet er savnet, forsvunnet"] <- "Organisation"


full_data$MTO <- as.factor(as.character(full_data$MTO))


p3 <- ggplot(data = full_data, aes(years)) +
  geom_histogram(stat = "count", binwidth = "unscaled x") +
  theme_bw()+
  theme(
    axis.text.x = element_text(
      size = 8,
      color = "black"
    ),
    axis.text.y = element_text(size = 10),
    axis.title.y = element_text(size = 12.5),
    axis.title = element_text(size = 10),
    legend.position = "bottom"
  ) +
  #guides(x = guide_axis(n.dodge = 2, angle = 50), y.sec = guide_axis(), x.sec()) +
  scale_x_continuous(breaks = seq(1981, 2022, by = 2))+
  guides(x = guide_axis(angle = 50))+ #, x.sec = guide_axis(angle = 50))+
  labs(
    x = "Year",
    y = "Number of entries per year",
    title = paste0("Reported accidents in Sdir's dataset, by accident type (MTO). N = ", nrow(full_data[!is.na(full_data$ulykketype),]))
  ) +
  scale_fill_manual(values = c(safe_colorblind_palette))
p3

# Adding a margin or a total plot.

df2 <- rbind(full_data, transform(full_data, MTO = "All combined"))

p3 %+% df2 + facet_rep_wrap(.~ MTO, ncol = 2, scales = "free_y", repeat.tick.labels = "bottom")

p4 <- ggplot(data = full_data, aes(ulykkesmåned)) +
  geom_histogram(stat = "count", binwidth = "unscaled x") +
  theme_bw()+
  theme(
    axis.text.x = element_text(
      size = 8,
      color = "black"
    ),
    axis.text.y = element_text(size = 10),
    axis.title.y = element_text(size = 12.5),
    axis.title = element_text(size = 10),
    legend.position = "bottom"
  ) +
  #guides(x = guide_axis(n.dodge = 2, angle = 50), y.sec = guide_axis(), x.sec()) +
  #scale_x_continuous(breaks = seq(1981, 2022, by = 2))+
  guides(x = guide_axis(angle = 50))+ #, x.sec = guide_axis(angle = 50))+
  labs(
    x = "Year",
    y = "Number of entries per year",
    title = paste0("Reported accidents in Sdir's dataset, by accident type (MTO). N = ", nrow(full_data[!is.na(full_data$ulykketype),]))
  ) +
  scale_fill_manual(values = c(safe_colorblind_palette))
p4

# Adding a margin or a total plot.

df2 <- rbind(full_data, transform(full_data, MTO = "All combined"))

p4 %+% df2 + facet_rep_wrap(.~ MTO, ncol = 2, scales = "free_y", repeat.tick.labels = "bottom")

# Removing objects no longer in use:

rm(df)
rm(p)
rm(p2)
rm(p3)
rm(p4)

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

language_identification_words <-
  c(
    "pus",
    "g09",
    "g11",
    "m",
    "077",
    "106",
    "na",
    "dama",
    NA,
    "NA",
    "konvertert",
    "fra"
  )

full_data %>%
  unnest_tokens(word, ALL) %>%
  anti_join(get_stopwords("no")) %>%
  anti_join(get_stopwords("en")) %>%
  filter(!word %in% language_identification_words) %>%
  count(word, sort = TRUE)

#### Trying to detect languages ####
##### Google's text recognition #####
require(cld3)
library(pbapply)

# We should not remove all stopwords before identifying languages, as they might
# indicate a language.

all_stop_words <-
  rbind(
    tibble(word = tm::stopwords(kind = "no")),
    tibble(word = tm::stopwords(kind = "en")),
    tibble(word = custom_words)
  )

language_identification_words <- paste(language_identification_words, collapse = "|")

# Removing all stopwords from the list of all stopwords and the custom words we
# added. Using pblapply to add a progress bar to lapply, as it takes a minute

full_data$ALL_cleaned <- tolower(as.character(full_data$ALL))

full_data$ALL_cleaned <- pblapply(full_data$ALL_cleaned, tm::removeWords, language_identification_words)

full_data$ALL_cleaned <- unlist(full_data$ALL_cleaned)

full_data$ALL_cleaned <- removeNumbers(full_data$ALL_cleaned)

full_data$ALL_cleaned <- removePunctuation(full_data$ALL_cleaned)

full_data$ALL_cleaned <-
  str_remove_all(string = full_data$ALL_cleaned, pattern = "konvertert fra")

full_data$ALL_cleaned <- str_squish(full_data$ALL_cleaned)

full_data$ALL_cleaned <- trimws(full_data$ALL_cleaned)

# Inspecting to check if done correctly

head(full_data$ALL_cleaned)

# Length of the cleaned version

full_data$length_ALL_cleaned <- str_count(full_data$ALL_cleaned, pattern = boundary("word"))

table(full_data$length_ALL_cleaned)
mean(full_data$length_ALL_cleaned, na.rm = T)

# Length of the uncleaned version
full_data$length_ALL <- str_count(full_data$ALL, pattern = boundary("word"))

table(full_data$length_ALL)
mean(full_data$length_ALL, na.rm = T)


# Using the automated version:

full_data$languages <- cld3::detect_language(full_data$ALL_cleaned)

# Investigating and cleaning:

table(full_data$languages, useNA = "always")

# A lot of languages are "bs" or 

head(full_data$ALL_cleaned[full_data$languages == "bs"])

full_data$languages <- as.character(full_data$languages)

table(full_data$languages[full_data$ALL_cleaned == "overflateslagskade"])

# All of the instances where "Overflateslagskade" is the only word is incorrectly
# labeled as "bs", rather than "no".

full_data$languages[full_data$ALL_cleaned == "overflateslagskade"] <- "no"

table(full_data$languages)

table(head(full_data$ALL_cleaned[full_data$languages == "da"], 100))

# Is it perhaps one of the people writing the reports?
table(full_data$registrert_av.x[full_data$languages == "da"])

# Several operators have written these reports

table(years(full_data$ulykkedato2)[full_data$languages == "da"])

# Over several years

# These reports are a mix of norwegian and english

full_data$languages[
  isTRUE(
    grepl(
      full_data$ALL,
      pattern = "Indre skader Overflateslagskade"))] <- "no"

full_data$languages[
  isTRUE(
    grepl(
      full_data$ALL,
      pattern = "Indre skader"))] <- "no"

table(full_data$ALL_cleaned[full_data$languages == "de"])

# These reports in german are actually norwegian

full_data$languages[full_data$languages == "de"] <- "no"

head(full_data$ALL_cleaned[full_data$languages == "sv"], 100)

full_data$languages[full_data$ALL_cleaned == "forbrenning"] <- "no"

table(full_data$ALL_cleaned[full_data$languages == "sv"])

# Looks like most of it is actually swedish

table(full_data$ALL_cleaned[full_data$languages == "sv" & full_data$length_ALL_cleaned > 100])

# Quite informative reports, so I'll keep them in for now.

head(full_data$ALL_cleaned[full_data$languages == "en"])

# Some of these are just NA, why is that? Let's see how many of the reports are 
# simply one word

table(full_data$ALL_cleaned[full_data$length_ALL_cleaned == 1], 
      useNA = "always")

# This seems very odd. Let's look at one of the smaller, but important 
# categories - drowning

full_data$ALL[full_data$ALL_cleaned == "drukning"]

table(full_data$antall_skadet[full_data$ALL_cleaned == "drukning"])

table(full_data$antall_omkommet[full_data$ALL_cleaned == "drukning"])

# These reports are seemingly all from the database PUS. Was that an old system?

table(years(full_data$ulykkedato2)[full_data$ALL_cleaned == "drukning"])

# Let's look at who made the registered entry

table(years(full_data$ulykkedato2)[full_data$registrert_av.x == "Konvertering"])


# A quick look at the accident types as well

table(full_data$ulykketype[full_data$registrert_av.x == "Konvertering"])

# Still not very promising results... Let's look at another method with a pre-
# trained model
##### fastText ######

library(fastText)

file_path <- file.path("lid.176.bin")

full_data$language_fasttext <- fastText::language_identification(input_obj = full_data$ALL_cleaned,
                                                        pre_trained_language_model_path = file_path,
                                                        k = 1,
                                                        th = 0.0,
                                                        threads = 1,
                                                        verbose = TRUE)

full_data$language_detected_fasttext <- paste0(full_data$language_fasttext$iso_lang_1)
full_data$language_probability_fasttext <- paste0(full_data$language_fasttext$prob_1)

table(full_data$language_detected_fasttext)

head(full_data$ALL_cleaned[full_data$language_detected_fasttext == "da"], 100)

# The word "fremmedlegeme" is wrongfully assumed to be danish
sum(table(full_data$ALL_cleaned[full_data$ALL_cleaned == "fremmedlegeme"]))

# 1115 of the free text fields are just the word fremmedlegeme.

table(full_data$antall_skadet[full_data$ALL_cleaned == "fremmedlegeme"])
table(full_data$ulykketype[full_data$ALL_cleaned == "fremmedlegeme"])

# And all of the cases are work accidents with only 1 injured person
# This does not seem like a good solution, let's go back to the drawing board.

# We'll do it for each variable before we merge them, maybe that will help.

full_data$languages_direkteårsak_person_fritekst <- detect_language(removePunctuation(removeNumbers(full_data$direkteårsak_person_fritekst)))
full_data$languages_direkteårsak_ytre_fritekst <- detect_language(removePunctuation(removeNumbers(full_data$direkteårsak_ytre_fritekst)))
full_data$languages_direkteårsak_utstyr_fritekst <- detect_language(removePunctuation(removeNumbers(full_data$direkteårsak_utstyr_fritekst)))
full_data$languages_indirekteårsak_person_fritekst <- detect_language(removePunctuation(removeNumbers(full_data$indirekteårsak_person_fritekst)))
full_data$languages_indirekteårsak_arbeidsmiljø_fritekst <- detect_language(removePunctuation(removeNumbers(full_data$indirekteårsak_arbeidsmiljø_fritekst)))
full_data$languages_indirekteårsak_ytre_fritekst <- detect_language(removePunctuation(removeNumbers(full_data$indirekteårsak_ytre_fritekst)))
full_data$languages_indirekteårsak_utstyr_fritekst <- detect_language(removePunctuation(removeNumbers(full_data$indirekteårsak_utstyr_fritekst)))
full_data$languages_bakenforårsak_ledelse_fritekst <- detect_language(removePunctuation(removeNumbers(full_data$bakenforårsak_ledelse_fritekst)))
full_data$languages_bakenforårsak_prosedyre_fritekst <- detect_language(removePunctuation(removeNumbers(full_data$bakenforårsak_prosedyre_fritekst)))
full_data$languages_fritekst <- detect_language(removePunctuation(removeNumbers(full_data$fritekst)))
full_data$languages_hendelsesforløp <- detect_language(removePunctuation(removeNumbers(full_data$hendelsesforløp)))
full_data$languages_personskade <- detect_language(removePunctuation(removeNumbers(full_data$personskade)))
full_data$languages_annenskade <- detect_language(removePunctuation(removeNumbers(full_data$annenskade)))

table(full_data$languages_direkteårsak_person_fritekst)
table(full_data$languages_direkteårsak_ytre_fritekst)
table(full_data$languages_direkteårsak_utstyr_fritekst)
table(full_data$languages_indirekteårsak_person_fritekst)
table(full_data$languages_indirekteårsak_arbeidsmiljø_fritekst)
table(full_data$languages_indirekteårsak_ytre_fritekst)
table(full_data$languages_indirekteårsak_utstyr_fritekst)
table(full_data$languages_bakenforårsak_ledelse_fritekst)
table(full_data$languages_bakenforårsak_prosedyre_fritekst)
table(full_data$languages_fritekst)
table(full_data$languages_hendelsesforløp)
table(full_data$languages_personskade)
table(full_data$languages_annenskade)

languages_before_merge <- full_data %>%
  gather("key", "value",
         languages_direkteårsak_person_fritekst,
         languages_direkteårsak_ytre_fritekst,
         languages_direkteårsak_utstyr_fritekst,
         languages_indirekteårsak_person_fritekst,
         languages_indirekteårsak_arbeidsmiljø_fritekst,
         languages_indirekteårsak_ytre_fritekst,
         languages_indirekteårsak_utstyr_fritekst,
         languages_bakenforårsak_ledelse_fritekst,
         languages_bakenforårsak_prosedyre_fritekst,
         languages_fritekst,
         languages_hendelsesforløp,
         languages_personskade,
         languages_annenskade
  ) %>%
  group_by(value) %>%
  summarise(n = n())

languages_before_merge$n[languages_before_merge$value == "no"]

cols <- c(
  "languages_direkteårsak_person_fritekst",
  "languages_direkteårsak_ytre_fritekst",
  "languages_direkteårsak_utstyr_fritekst",
  "languages_indirekteårsak_person_fritekst",
  "languages_indirekteårsak_arbeidsmiljø_fritekst",
  "languages_indirekteårsak_ytre_fritekst",
  "languages_indirekteårsak_utstyr_fritekst",
  "languages_bakenforårsak_ledelse_fritekst",
  "languages_bakenforårsak_prosedyre_fritekst",
  "languages_fritekst",
  "languages_hendelsesforløp",
  "languages_personskade",
  "languages_annenskade"
)


full_data$languages_combined <- apply( full_data[ , cols ] , 1 , paste , collapse = "-" )

full_data <- full_data[ , !( names(full_data) %in% cols ) ]

full_data$languages_combined_count <- str_count(full_data$languages_combined, pattern = "no")

table(full_data$languages_combined_count)

# 1024 results have 0 instances of norwegian detected. This is perhaps not the 
# only way to determine languages, but it seems to be the best one after much
# trial and error.

# Removing numbers and puntuation from the variables:

full_data$direkteårsak_person_fritekst <- removePunctuation(removeNumbers(full_data$direkteårsak_person_fritekst))
full_data$direkteårsak_ytre_fritekst <- removePunctuation(removeNumbers(full_data$direkteårsak_ytre_fritekst))
full_data$direkteårsak_utstyr_fritekst <- removePunctuation(removeNumbers(full_data$direkteårsak_utstyr_fritekst))
full_data$indirekteårsak_person_fritekst <- removePunctuation(removeNumbers(full_data$indirekteårsak_person_fritekst))
full_data$indirekteårsak_arbeidsmiljø_fritekst <- removePunctuation(removeNumbers(full_data$indirekteårsak_arbeidsmiljø_fritekst))
full_data$indirekteårsak_ytre_fritekst <- removePunctuation(removeNumbers(full_data$indirekteårsak_ytre_fritekst))
full_data$indirekteårsak_utstyr_fritekst <- removePunctuation(removeNumbers(full_data$indirekteårsak_utstyr_fritekst))
full_data$bakenforårsak_ledelse_fritekst <- removePunctuation(removeNumbers(full_data$bakenforårsak_ledelse_fritekst))
full_data$bakenforårsak_prosedyre_fritekst <- removePunctuation(removeNumbers(full_data$bakenforårsak_prosedyre_fritekst))
full_data$fritekst <- removePunctuation(removeNumbers(full_data$fritekst))
full_data$hendelsesforløp <- removePunctuation(removeNumbers(full_data$hendelsesforløp))
full_data$personskade <- removePunctuation(removeNumbers(full_data$personskade))
full_data$annenskade <- removePunctuation(removeNumbers(full_data$annenskade))

# Trying fastText's pre-trained model once:

full_data$languages2_direkteårsak_person_fritekst <-
  fastText::language_identification(
    input_obj = full_data$direkteårsak_person_fritekst,
    pre_trained_language_model_path = file_path,
    k = 1,
    th = 0.0,
    threads = 1,
    verbose = TRUE
  )

full_data$languages2_direkteårsak_ytre_fritekst <-
  language_identification(
    input_obj = full_data$direkteårsak_ytre_fritekst,
    pre_trained_language_model_path = file_path,
    k = 1,
    th = 0.0,
    threads = 1,
    verbose = TRUE
  )

full_data$languages2_direkteårsak_utstyr_fritekst <-
  language_identification(
    input_obj = full_data$direkteårsak_utstyr_fritekst,
    pre_trained_language_model_path = file_path,
    k = 1,
    th = 0.0,
    threads = 1,
    verbose = TRUE
  )

full_data$languages2_indirekteårsak_person_fritekst <-
  language_identification(
    input_obj = full_data$indirekteårsak_person_fritekst,
    pre_trained_language_model_path = file_path,
    k = 1,
    th = 0.0,
    threads = 1,
    verbose = TRUE
  )

full_data$languages2_indirekteårsak_arbeidsmiljø_fritekst <-
  language_identification(
    input_obj = full_data$indirekteårsak_arbeidsmiljø_fritekst,
    pre_trained_language_model_path = file_path,
    k = 1,
    th = 0.0,
    threads = 1,
    verbose = TRUE
  )

full_data$languages2_indirekteårsak_ytre_fritekst <-
  language_identification(
    input_obj = full_data$indirekteårsak_ytre_fritekst,
    pre_trained_language_model_path = file_path,
    k = 1,
    th = 0.0,
    threads = 1,
    verbose = TRUE
  )

full_data$languages2_indirekteårsak_utstyr_fritekst <-
  language_identification(
    input_obj = full_data$indirekteårsak_utstyr_fritekst,
    pre_trained_language_model_path = file_path,
    k = 1,
    th = 0.0,
    threads = 1,
    verbose = TRUE
  )

full_data$languages2_bakenforårsak_ledelse_fritekst <-
  language_identification(
    input_obj = full_data$bakenforårsak_ledelse_fritekst,
    pre_trained_language_model_path = file_path,
    k = 1,
    th = 0.0,
    threads = 1,
    verbose = TRUE
  )

full_data$languages2_bakenforårsak_prosedyre_fritekst <-
  language_identification(
    input_obj = full_data$bakenforårsak_prosedyre_fritekst,
    pre_trained_language_model_path = file_path,
    k = 1,
    th = 0.0,
    threads = 1,
    verbose = TRUE
  )

full_data$languages2_fritekst <-
  language_identification(
    input_obj = full_data$fritekst,
    pre_trained_language_model_path = file_path,
    k = 1,
    th = 0.0,
    threads = 1,
    verbose = TRUE
  )

# Does not work.
full_data$hendelsesforløp <- as.character(str_squish(full_data$hendelsesforløp))

full_data$languages2_hendelsesforløp <-
  language_identification(
    input_obj = full_data$hendelsesforløp,
    pre_trained_language_model_path = file_path,
    k = 1,
    th = 0.0,
    threads = 1,
    verbose = TRUE
  )

full_data$languages2_personskade <-
  language_identification(
    input_obj = full_data$personskade,
    pre_trained_language_model_path = file_path,
    k = 1,
    th = 0.0,
    threads = 1,
    verbose = TRUE
  )

full_data$languages2_annenskade <-
  language_identification(
    input_obj = full_data$annenskade,
    pre_trained_language_model_path = file_path,
    k = 1,
    th = 0.0,
    threads = 1,
    verbose = TRUE
  )

# Getting the languages predicted:

full_data$languages2_direkteårsak_person_fritekst_detected <- paste0(full_data$languages2_direkteårsak_person_fritekst$iso_lang_1)
full_data$languages2_direkteårsak_ytre_fritekst_detected <- paste0(full_data$languages2_direkteårsak_ytre_fritekst$iso_lang_1)
full_data$languages2_direkteårsak_utstyr_fritekst_detected <- paste0(full_data$languages2_direkteårsak_utstyr_fritekst$iso_lang_1)
full_data$languages2_indirekteårsak_person_fritekst_detected <- paste0(full_data$languages2_indirekteårsak_person_fritekst$iso_lang_1)
full_data$languages2_indirekteårsak_arbeidsmiljø_fritekst_detected <- paste0(full_data$languages2_indirekteårsak_arbeidsmiljø_fritekst$iso_lang_1)
full_data$languages2_indirekteårsak_ytre_fritekst_detected <- paste0(full_data$languages2_indirekteårsak_ytre_fritekst$iso_lang_1)
full_data$languages2_indirekteårsak_utstyr_fritekst_detected <- paste0(full_data$languages2_indirekteårsak_utstyr_fritekst$iso_lang_1)
full_data$languages2_bakenforårsak_ledelse_fritekst_detected <- paste0(full_data$languages2_bakenforårsak_ledelse_fritekst$iso_lang_1)
full_data$languages2_bakenforårsak_prosedyre_fritekst_detected <- paste0(full_data$languages2_bakenforårsak_prosedyre_fritekst$iso_lang_1)
full_data$languages2_fritekst_detected <- paste0(full_data$languages2_fritekst$iso_lang_1)
full_data$languages2_hendelsesforløp_detected <- paste0(full_data$languages2_hendelsesforløp$iso_lang_1)
full_data$languages2_personskade_detected <- paste0(full_data$languages2_personskade$iso_lang_1)
full_data$languages2_annenskade_detected <- paste0(full_data$languages2_annenskade$iso_lang_1)

# Getting the probabilites
full_data$languages2_direkteårsak_person_fritekst_probability <- paste0(full_data$languages2_direkteårsak_person_fritekst$prob_1)
full_data$languages2_direkteårsak_ytre_fritekst_probability <- paste0(full_data$languages2_direkteårsak_ytre_fritekst$prob_1)
full_data$languages2_direkteårsak_utstyr_fritekst_probability <- paste0(full_data$languages2_direkteårsak_utstyr_fritekst$prob_1)
full_data$languages2_indirekteårsak_person_fritekst_probability <- paste0(full_data$languages2_indirekteårsak_person_fritekst$prob_1)
full_data$languages2_indirekteårsak_arbeidsmiljø_fritekst_probability <- paste0(full_data$languages2_indirekteårsak_arbeidsmiljø_fritekst$prob_1)
full_data$languages2_indirekteårsak_ytre_fritekst_probability <- paste0(full_data$languages2_indirekteårsak_ytre_fritekst$prob_1)
full_data$languages2_indirekteårsak_utstyr_fritekst_probability <- paste0(full_data$languages2_indirekteårsak_utstyr_fritekst$prob_1)
full_data$languages2_bakenforårsak_ledelse_fritekst_probability <- paste0(full_data$languages2_bakenforårsak_ledelse_fritekst$prob_1)
full_data$languages2_bakenforårsak_prosedyre_fritekst_probability <- paste0(full_data$languages2_bakenforårsak_prosedyre_fritekst$prob_1)
full_data$languages2_fritekst_probability <- paste0(full_data$languages2_fritekst$prob_1)
full_data$languages2_hendelsesforløp_probability <- paste0(full_data$languages2_hendelsesforløp$prob_1)
full_data$languages2_personskade_probability <- paste0(full_data$languages2_personskade$prob_1)
full_data$languages2_annenskade_probability <- paste0(full_data$languages2_annenskade$prob_1)



languages2_before_merge2 <- full_data %>%
  gather("key", "value",
         languages2_direkteårsak_person_fritekst_detected,
         languages2_direkteårsak_ytre_fritekst_detected,
         languages2_direkteårsak_utstyr_fritekst_detected,
         languages2_indirekteårsak_person_fritekst_detected,
         languages2_indirekteårsak_arbeidsmiljø_fritekst_detected,
         languages2_indirekteårsak_ytre_fritekst_detected,
         languages2_indirekteårsak_utstyr_fritekst_detected,
         languages2_bakenforårsak_ledelse_fritekst_detected,
         languages2_bakenforårsak_prosedyre_fritekst_detected,
         languages2_fritekst_detected,
         languages2_hendelsesforløp_detected,
         languages2_personskade_detected,
         languages2_annenskade_detected
  ) %>%
  group_by(value) %>%
  summarise(n = n())

languages2_before_merge2$n[languages2_before_merge2$value == "no"]

cols2 <- c(
  "languages2_direkteårsak_person_fritekst_detected",
  "languages2_direkteårsak_ytre_fritekst_detected",
  "languages2_direkteårsak_utstyr_fritekst_detected",
  "languages2_indirekteårsak_person_fritekst_detected",
  "languages2_indirekteårsak_arbeidsmiljø_fritekst_detected",
  "languages2_indirekteårsak_ytre_fritekst_detected",
  "languages2_indirekteårsak_utstyr_fritekst_detected",
  "languages2_bakenforårsak_ledelse_fritekst_detected",
  "languages2_bakenforårsak_prosedyre_fritekst_detected",
  "languages2_fritekst_detected",
  "languages2_hendelsesforløp_detected",
  "languages2_personskade_detected",
  "languages2_annenskade_detected"
)

full_data$languages_combined2 <- apply( full_data[ , cols2 ] , 1 , paste , collapse = "-" )

full_data <- full_data[ , !( names(full_data) %in% cols2 ) ]

full_data$languages_combined_count2 <- str_count(full_data$languages_combined2, pattern = "no")

table(full_data$languages_combined_count2)

table(full_data$languages_combined_count)

# Manually checking the reports for Norwegian

head(full_data$ALL_cleaned[full_data$languages_combined_count2 == 0], 50)

head(full_data$ALL_cleaned[full_data$languages_combined_count == 0], 50)

# Google's version is better with 7/50 and the pre-trained model with 15/50.

full_data %>%
  count(languages) %>%
  ggplot(aes(languages, n, label = languages)) +
  geom_col() +
  theme_bw() +
  labs(x = "Language codes",
       y = "Count",
       title = "Distribution of languages in the reports") +
  geom_text(aes(label = n), nudge_y = 600) +
  scale_fill_manual(values = c(safe_colorblind_palette))

table(full_data$length_ALL_cleaned[full_data$languages_combined_count == 0])

full_data <- full_data[!duplicated(full_data$ALL), ]

tail(full_data$ALL[full_data$languages_combined_count == 0])

table(full_data$languages_combined_count)

table(full_data$languages_combined_count2)

full_data <- full_data[full_data$languages_combined_count != 0, ]

#### Classification model ####

zeroNAs <- names(which(colSums(is.na(full_data))<=0))
lessthanhundred <- names(which(colSums(is.na(full_data))<100))
lessthanthousand <- names(which(colSums(is.na(full_data))<1000))

colnames(full_data)[!apply(full_data, 2, anyNA)]

# These are the variables with 0 NA values and we should try to use them.

full_data <- full_data %>%
  mutate(ALL = str_replace_all(
    string = ALL,
    pattern =  "Konvertert fra PUS:",
    replacement = ""
  )) %>%
  mutate(ALL = str_squish(ALL))

# The algorithm can't handle missing values, so we're editing them to 0

# full_data$antall_skadet[is.na(full_data$antall_skadet)] <- 0

library(tidymodels)
set.seed(12345)
full_data_split <-   full_data %>%
  mutate(ALL = removeNumbers(ALL)) %>%
  initial_split()

full_data_train <- training(full_data_split)
full_data_test <- testing(full_data_split)

library(textrecipes)

# For multiple classes for classification, which are rather unbalanced:
library(themis)

# custom stopwords to remove, in addition to the ones already used in language
# identification:

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


unbalanced_full_data_rec_ulykketype <- recipe(ulykketype ~ ALL_cleaned, data = full_data_train) %>%
  step_tokenize(ALL_cleaned) %>%
  step_stopwords(language = "no") %>%
  step_stopwords(language = "en") %>%
  step_stopwords(custom_stopword_source = custom_words) %>%
  step_tokenfilter(ALL_cleaned, max_tokens = 1e4) %>%
  step_tfidf(ALL_cleaned) %>%
  step_downsample(ulykketype)

unbalanced_full_data_rec_ulykketype

unbalanced_vfolds <- vfold_cv(full_data_train)

multi_spec <- multinom_reg(penalty = tune(), mixture = 1) %>%
  set_mode("classification") %>%
  set_engine("glmnet")

multi_spec

library(hardhat)
# Is packed with the other packages

sparse_bp <- default_recipe_blueprint(composition = "dgCMatrix")


# Single lasso

lasso_spec <- logistic_reg(penalty = 0.01, mixture = 1) %>%
  set_mode("classification") %>%
  set_engine("glmnet")

lasso_spec

lasso_wf <- workflow() %>%
  add_recipe(unbalanced_full_data_rec_ulykketype) %>%
  add_model(lasso_spec)

lasso_wf

# Multiple lasso

multi_lasso_wf <- workflow() %>%
  add_recipe(unbalanced_full_data_rec_ulykketype, blueprint = sparse_bp) %>%
  add_model(multi_spec)

multi_lasso_wf

lambda_grid <- grid_regular(penalty(), levels = 30)
lambda_grid

smaller_lambda <- grid_regular(penalty(range = c(-5, 0)), levels = 20)
smaller_lambda

multi_lasso_rs_ulykketype <- tune_grid(
  multi_lasso_wf,
  unbalanced_vfolds,
  grid = lambda_grid,
  control = control_resamples(save_pred = TRUE)
)

multi_lasso_rs_ulykketype

autoplot(multi_lasso_rs_ulykketype) +
  labs(
    title = "Lasso model performance across regularisation penalties",
    subtitle = "Performance metrics can be used to identity the best penalty"
  )+
  theme_bw()

# Best ROC-AUC

multi_lasso_rs_ulykketype %>%
  show_best("roc_auc")

# What's the best accuracy?

best_acc_ulykketype <- multi_lasso_rs_ulykketype %>%
  show_best("accuracy")

best_acc_ulykketype

# Making a confusion matrix

multi_lasso_rs_ulykketype %>%
  collect_predictions() %>%
  filter(penalty == best_acc_ulykketype$penalty) %>%
  #filter(id == "Fold01") %>%
  conf_mat(ulykketype, .pred_class) %>%
  autoplot(type = "heatmap") +
  scale_y_discrete(labels = function(x) str_wrap(x, 20)) +
  scale_x_discrete(labels = function(x) str_wrap(x, 25)) +
  theme(
    axis.text.x = element_text(
      angle = 60,
      size = 10,
      vjust = 1,
      hjust = 1.1,
      color = "black"
    ))

# Removing all the correctly predicted observations:

multi_lasso_rs_ulykketype %>%
  collect_predictions() %>%
  filter(penalty == best_acc$penalty) %>%
  # filter(id == "Fold01") %>%
  filter(.pred_class != ulykketype) %>%
  conf_mat(ulykketype, .pred_class) %>%
  autoplot(type = "heatmap") +
  scale_y_discrete(labels = function(x) str_wrap(x, 20)) +
  scale_x_discrete(labels = function(x) str_wrap(x, 25)) + 
  theme(
    axis.text.x = element_text(
      angle = 60,
      size = 10,
      vjust = 1,
      hjust = 1.1,
      color = "black"
    ))

# Using a smaller lambda

multi_lasso_rs_small_ulykketype <- tune_grid(
  multi_lasso_wf,
  unbalanced_vfolds,
  grid = smaller_lambda,
  control = control_resamples(save_pred = TRUE)
)

multi_lasso_rs_small_ulykketype

collect_metrics(multi_lasso_rs_small_ulykketype)

multi_lasso_rs_small_ulykketype %>%
  show_best("roc_auc")

# What's the best accuracy?

best_acc_small_ulykketype <- multi_lasso_rs_small_ulykketype %>%
  show_best("accuracy")

best_acc_small_ulykketype

# Making a confusion matrix

multi_lasso_rs_small_ulykketype %>%
  collect_predictions() %>%
  filter(penalty == best_acc_small$penalty) %>%
  #filter(id == "Fold01") %>%
  conf_mat(ulykketype, .pred_class) %>%
  autoplot(type = "heatmap") +
  scale_y_discrete(labels = function(x) str_wrap(x, 20)) +
  scale_x_discrete(labels = function(x) str_wrap(x, 25)) +
  theme(
    axis.text.x = element_text(
      angle = 60,
      size = 10,
      vjust = 1,
      hjust = 1.1,
      color = "black"
    ))

# No differences between the different sizes of lambda regularisation

# Using a sparse encoding:

tune_spec <- logistic_reg(penalty = tune(), mixture = 1) %>%
  set_mode("classification") %>%
  set_engine("glmnet")

tune_spec

# Only three cases

# Adding the different classes to the data

# Creating a new variable
full_data$MTO <- NA

# Man

full_data$MTO[full_data$ulykketype == "Arbeidsulykke/Personulykke"] <- "Man"
full_data$MTO[full_data$ulykketype == "Annen ulykke"] <- "Man"

# Technology

full_data$MTO[full_data$ulykketype == "Maskinhavari"] <- "Technology"
full_data$MTO[full_data$ulykketype == "Brann/Eksplosjon"] <- "Technology"
full_data$MTO[full_data$ulykketype == "Kantring"] <- "Technology"
full_data$MTO[full_data$ulykketype == "Kollisjon"] <- "Technology"
full_data$MTO[full_data$ulykketype == "Lekkasje"] <- "Technology"
full_data$MTO[full_data$ulykketype == "Stabilitetssvikt uten kantring"] <- "Technology"

# Organisation

full_data$MTO[full_data$ulykketype == "Feil på redningsmidler"] <- "Organisation"
full_data$MTO[full_data$ulykketype == "Hardtværskade"] <- "Organisation"
full_data$MTO[full_data$ulykketype == "Kontaktskade, Kaier, Broer etc"] <- "Organisation"
full_data$MTO[full_data$ulykketype == "Miljøskade/Forurensing"] <- "Organisation"
full_data$MTO[full_data$ulykketype == "Grunnstøting"] <- "Organisation"
full_data$MTO[full_data$ulykketype == "Fartøyet er savnet, forsvunnet"] <- "Organisation"


full_data$MTO <- as.factor(as.character(full_data$MTO))


library(tidymodels)
set.seed(1234)
MTO_full_data_split <-   full_data %>%
  mutate(ALL = removeNumbers(ALL)) %>%
  initial_split(strata = MTO)

MTO_full_data_train <- training(MTO_full_data_split)
MTO_full_data_test <- testing(MTO_full_data_split)

library(textrecipes)

# Naive Bayes
# We increased the max tokens to 10 000, due to the Naive Bayes' ability to work
# with more data than the other algorithms. 

MTO_full_data_rec <-
  recipe(MTO ~ ALL, data = MTO_full_data_train) %>%
  step_tokenize(ALL) %>%
  step_stopwords(language = "no") %>%
  step_stopwords(language = "en") %>%
  step_stopwords(custom_stopword_source = custom_words) %>%
  step_tokenfilter(ALL, max_tokens = 1e3) %>%
  step_tfidf(ALL)

MTO_full_data_rec

# Creating a workflow

MTO_full_data_wf <- workflow() %>%
  add_recipe(MTO_full_data_rec)

MTO_full_data_wf

library(discrim)

nb_spec <- naive_Bayes() %>%
  set_mode("classification") %>%
  set_engine("naivebayes")

nb_spec

library(naivebayes)

MTO_nb_fit <- MTO_full_data_wf %>%
  add_model(nb_spec) %>%
  parsnip::fit(data = MTO_full_data_train)

set.seed(234)
MTO_full_data_folds <- vfold_cv(MTO_full_data_train)

MTO_full_data_folds

MTO_nb_wf <- workflow() %>%
  add_recipe(MTO_full_data_rec) %>%
  add_model(nb_spec)

MTO_nb_wf

# Adding resamples

MTO_nb_rs <- fit_resamples(
  MTO_nb_wf,
  MTO_full_data_folds,
  control = control_resamples(save_pred = TRUE)
)

MTO_nb_rs_metrics <- collect_metrics(MTO_nb_rs)
MTO_nb_rs_predictions <- collect_predictions(MTO_nb_rs)

MTO_nb_rs_metrics

MTO_nb_rs_acc <- MTO_nb_rs %>%
  show_best("accuracy")

MTO_nb_rs %>%
  #filter(id == "Fold01") %>% 
  collect_predictions() %>%
  conf_mat(MTO, .pred_class) %>%
  #group_by(id) %>%
  #roc_curve(truth = MTO, .pred_Man) %>%
  autoplot(type = "heatmap") +
  labs(
    color = NULL,
    title = "Confusion matrix of MTO using Naive Bayes"
  )

conf_mat_resampled(MTO_nb_rs, tidy = FALSE) %>%
  autoplot(type = "heatmap")

multi_lasso_rs_small %>%
  collect_predictions() %>%
  filter(penalty == best_acc_small$penalty) %>%
  #filter(id == "Fold01") %>%
  conf_mat(ulykketype, .pred_class) %>%
  autoplot(type = "heatmap") +
  scale_y_discrete(labels = function(x) str_wrap(x, 20)) +
  scale_x_discrete(labels = function(x) str_wrap(x, 25)) +
  theme(
    axis.text.x = element_text(
      angle = 60,
      size = 10,
      vjust = 1,
      hjust = 1.1,
      color = "black"
    ))

# That was not useful, as most cases were appointed to the Man category.

# Let's compare to the null model:

null_classification <- null_model() %>%
  set_engine("parsnip") %>%
  set_mode("classification")

null_rs <- workflow() %>%
  add_recipe(MTO_full_data_rec) %>%
  add_model(null_classification) %>%
  fit_resamples(
    MTO_full_data_folds
  )

null_rs %>%
  collect_metrics()

# Splitting it up into man made and structural origins

full_data$MTO_man <- NA
full_data$MTO_man[full_data$MTO == "Man"] <- "Man" 
full_data$MTO_man[full_data$MTO != "Man"] <- "Structures"

full_data$MTO_man <- as.factor(as.character(full_data$MTO_man))
table(full_data$MTO_man)  

# Splitting and creating a new train/test set

MTO_man_full_data_split <- full_data %>%
  mutate(ALL = removeNumbers(ALL)) %>%
  initial_split(strata = MTO_man)

MTO_man_full_data_train <- training(MTO_man_full_data_split)
MTO_man_full_data_test <- testing(MTO_man_full_data_split)

MTO_man_full_data_rec <-
  recipe(MTO_man ~ ALL, data = MTO_man_full_data_train) %>%
  step_tokenize(ALL) %>%
  step_stopwords(language = "no") %>%
  step_stopwords(language = "en") %>%
  step_stopwords(custom_stopword_source = custom_words) %>%
  step_tokenfilter(ALL, max_tokens = 1e4) %>%
  step_tfidf(ALL)

MTO_man_full_data_rec

# Creating a workflow

MTO_man_full_data_wf <- workflow() %>%
  add_recipe(MTO_man_full_data_rec)

MTO_man_full_data_wf

library(discrim)
library(naivebayes)

MTO_man_nb_fit <- MTO_man_full_data_wf %>%
  add_model(nb_spec) %>%
  parsnip::fit(data = MTO_man_full_data_train)

set.seed(234)
MTO_man_full_data_folds <- vfold_cv(MTO_man_full_data_train)

MTO_man_full_data_folds

MTO_man_nb_wf <- workflow() %>%
  add_recipe(MTO_man_full_data_rec) %>%
  add_model(nb_spec)

MTO_man_nb_wf

# Adding resamples

MTO_man_nb_rs <- fit_resamples(
  MTO_man_nb_wf,
  MTO_man_full_data_folds,
  control = control_resamples(save_pred = TRUE)
)

MTO_man_nb_rs_metrics <- collect_metrics(MTO_man_nb_rs)
MTO_man_nb_rs_predictions <- collect_predictions(MTO_man_nb_rs)

MTO_man_nb_rs_metrics

MTO_man_nb_rs %>%
  show_best("accuracy")

MTO_man_nb_rs_predictions %>%
  #  filter(id == "Fold01") %>% 
  group_by(id) %>% 
  roc_curve(truth = MTO_man, .pred_Man) %>%
  autoplot() +
  labs(
    color = NULL,
    title = "ROC curve for the accident type based on the simplified MTO framework",
    subtitle = "Each resample fold is shown in a different color"
  )


conf_mat_resampled(MTO_man_nb_rs, tidy = FALSE) %>%
  #filter(.id == "Fold01")
  autoplot(type = "heatmap")

# Did not get very much better results, unfortunately.

# Trying a lasso model which is more robust for data disparity
# with multiple classes for classification, which are rather unbalanced:
library(themis)

MTO_unbalanced_full_data_rec <- recipe(MTO ~ ALL, data = MTO_full_data_train) %>%
  step_tokenize(ALL) %>%
  step_stopwords(language = "no") %>%
  step_stopwords(language = "en") %>%
  step_stopwords(custom_stopword_source = custom_words) %>%
  step_tokenfilter(ALL, max_tokens = 1e3) %>%
  step_tfidf(ALL) %>%  
  step_downsample(MTO)

MTO_unbalanced_vfolds <- vfold_cv(MTO_full_data_train)

multi_spec <- multinom_reg(penalty = tune(), mixture = 1) %>%
  set_mode("classification") %>%
  set_engine("glmnet")


multi_spec

library(hardhat)
# Is packed with the other packages

sparse_bp <- default_recipe_blueprint(composition = "dgCMatrix")


# Single lasso

lasso_spec <- logistic_reg(penalty = 0.01, mixture = 1) %>%
  set_mode("classification") %>%
  set_engine("glmnet")

lasso_spec

lasso_wf <- workflow() %>%
  add_recipe(MTO_unbalanced_full_data_rec) %>%
  add_model(lasso_spec)

lasso_wf

# Lasso

multi_lasso_wf <- workflow() %>%
  add_recipe(MTO_unbalanced_full_data_rec, blueprint = sparse_bp) %>%
  add_model(multi_spec)

multi_lasso_wf

lambda_grid <- grid_regular(penalty(), levels = 30)
lambda_grid

smaller_lambda <- grid_regular(penalty(range = c(-5, 0)), levels = 20)
smaller_lambda

multi_lasso_rs <- tune_grid(
  multi_lasso_wf,
  MTO_unbalanced_vfolds,
  grid = lambda_grid,
  control = control_resamples(save_pred = TRUE)
)

multi_lasso_rs

# What's the best accuracy?

best_acc <- multi_lasso_rs %>%
  show_best("accuracy")

best_acc

best_roc <- multi_lasso_rs %>%
  show_best("roc_auc")

best_roc  

# Making a confusion matrix

multi_lasso_rs %>%
  collect_predictions() %>%
  filter(penalty == best_acc$penalty) %>%
  #filter(id == "Fold01") %>%
  conf_mat(MTO, .pred_class) %>%
  autoplot(type = "heatmap") +
  scale_y_discrete(labels = function(x) str_wrap(x, 20)) +
  scale_x_discrete(labels = function(x) str_wrap(x, 25))
#theme_bw(legend = "none")
# theme(
#   axis.text.x = element_text(
#     angle = 60,
#     size = 10,
#     vjust = 1,
#     hjust = 1.1,
#     color = "black"
#   ))

# Removing all the correctly predicted observations:

multi_lasso_rs %>%
  collect_predictions() %>%
  filter(penalty == best_acc$penalty) %>%
  # filter(id == "Fold01") %>%
  filter(.pred_class != MTO) %>%
  conf_mat(MTO, .pred_class) %>%
  autoplot(type = "heatmap") +
  scale_y_discrete(labels = function(x) str_wrap(x, 20)) +
  scale_x_discrete(labels = function(x) str_wrap(x, 25)) 
# theme(
#   axis.text.x = element_text(
#     angle = 60,
#     size = 10,
#     vjust = 1,
#     hjust = 1.1,
#     color = "black"
#   ))

# Using a smaller lambda

multi_lasso_rs_small <- tune_grid(
  multi_lasso_wf,
  MTO_unbalanced_vfolds,
  grid = smaller_lambda,
  control = control_resamples(save_pred = TRUE)
)

multi_lasso_rs_small

best_acc_small <- multi_lasso_rs_small %>% show_best("accuracy")

multi_lasso_rs_small %>%
  collect_predictions() %>%
  filter(penalty == best_acc_small$penalty) %>%
  # filter(id == "Fold01") %>%
  #filter(.pred_class != MTO) %>%
  conf_mat(MTO, .pred_class) %>%
  autoplot(type = "heatmap") +
  scale_y_discrete(labels = function(x) str_wrap(x, 20)) +
  scale_x_discrete(labels = function(x) str_wrap(x, 25)) 
# theme(
#   axis.text.x = element_text(
#     angle = 60,
#     size = 10,
#     vjust = 1,
#     hjust = 1.1,
#     color = "black"
#   ))


# Trying a sparse version which would cut down on computing time

tune_spec2 <- multinom_reg(penalty = tune(), mixture = 1) %>%
  set_mode("classification") %>%
  set_engine("glmnet")

tune_spec2

sparse_wf <- workflow() %>%
  add_recipe(MTO_unbalanced_full_data_rec, blueprint = sparse_bp) %>%
  add_model(tune_spec2)

tune_wf <- workflow() %>%
  add_recipe(MTO_unbalanced_full_data_rec, blueprint = sparse_bp) %>%
  add_model(tune_spec2)

set.seed(2022)
tune_rs <- tune_grid(
  tune_wf,
  MTO_unbalanced_vfolds,
  grid = lambda_grid,
  control = control_resamples(save_pred = TRUE)
)

tune_rs

collect_metrics(tune_rs)

autoplot(tune_rs) +
  labs(
    title = "Lasso model performance across regularization penalties",
    subtitle = "Performance metrics can be used to identity the best penalty"
  )+
  theme_bw()

tune_rs %>%
  show_best("roc_auc")

tune_rs %>%
  show_best("accuracy")

chosen_auc <- tune_rs %>%
  select_by_one_std_err(metric = "roc_auc", -penalty)

chosen_auc

# Continue here.

final_lasso <- finalize_workflow(tune_wf, chosen_auc)

fitted_lasso <- parsnip::fit(final_lasso, MTO_full_data_train)

fitted_lasso %>%
  pull_workflow_fit() %>%
  tidy() %>%
  arrange(-estimate)

# Factors that assume structures. Higher number = higher estimate

fitted_lasso %>%
  pull_workflow_fit() %>%
  tidy() %>%
  arrange(estimate)


# Tuning the recipe a little further:
# Removing words that appear fewer than 10 times, possible typos etc.

MTO_unbalanced_rec_v2 <- recipe(MTO ~ ALL, data = MTO_full_data_train) %>%
  step_tokenize(ALL) %>%
  step_stopwords(language = "no") %>%
  step_stopwords(language = "en") %>%
  step_stopwords(custom_stopword_source = custom_words) %>%
  step_tokenfilter(ALL, max_tokens = tune(), min_times = 10) %>%
  step_tfidf(ALL) %>%
  step_downsample(MTO)

sparse_wf_v2 <- MTO_full_data_wf %>%
  update_recipe(MTO_unbalanced_rec_v2, blueprint = sparse_bp) %>%
  add_model(multi_spec)


# Tuning it further:

final_grid <- grid_regular(
  penalty(range = c(-4, 0)),
  max_tokens(range = c(1e3, 5e3)),
  levels = c(penalty = 20, max_tokens = 3)
)

final_grid


set.seed(2020)
tune_rs <- tune_grid(
  sparse_wf_v2,
  MTO_full_data_folds,
  grid = final_grid,
  metrics = metric_set(accuracy, sensitivity, specificity)
)

collect_metrics(tune_rs)

tune_rs %>%
  show_best("accuracy")

tune_rs %>%
  show_best("sensitivity")

tune_rs %>%
  show_best("specificity")

autoplot(tune_rs) +
  labs(
    color = "Number of tokens",
    title = "Model performance across regularisation penalties and tokens",
    subtitle = paste("We can choose a simpler model with higher regularisation")
  )+
  theme_bw()


choose_acc <- tune_rs %>%
  select_by_one_std_err(metric = "accuracy", -penalty)

choose_acc

final_wf <- finalize_workflow(sparse_wf_v2, choose_acc)
final_wf

final_fitted <-
  last_fit(final_wf,
           MTO_full_data_split,
           metrics = metric_set(accuracy, sensitivity, specificity, roc_auc))

collect_metrics(final_fitted)

# Confusion matrix

final_fitted %>%
  collect_predictions() %>%
  conf_mat(truth = MTO, estimate = .pred_class) %>%
  autoplot(type = "heatmap") +
  ggtitle("Fitting the trained and tuned multiple lasso-model to the test data.
Accuracy = 0.928, ROCAUC = 0.979")
 

final_fitted$splits

final_fitted %>%
  collect_metrics()

# This is for the testing data, and an accuracy of .903 is pretty good.


##### SVM Classification: #####
# We're swapping from using parsnip to using e1071
library(e1071)

library(RTextTools)

# Creating the DTM:
tock_2 <- Sys.time()

dtMatrix <-
  create_matrix(
    full_data["ALL"],
    weighting = tm::weightTfIdf,
    removeNumbers = TRUE,
    removePunctuation = TRUE,
    removeStopwords = all_stop_words$word,
    toLower = TRUE,
    stripWhitespace = TRUE,
    ngramLength = c(1:3)
  )

as.matrix(dtMatrix)

# Configure the training data
container <-
  create_container(
    dtMatrix,
    as.factor(full_data$ulykketype),
    trainSize = 1:print(round(nrow(full_data)*0.75)),
    testSize = print((round(nrow(full_data)*0.75)+1)):nrow(full_data),
    virgin = FALSE
  )

# train a SVM Model

model <- train_model(container, "SVM", kernel = "linear", cost = 1, verbose = TRUE, cross = 10)

svm_predictions <- predict(model, container@classification_matrix, probability = T)

svm_accuracy <- svm_predictions == full_data$ulykketype[1:print(round(nrow(full_data)*0.75))]
table(svm_accuracy)

model_results <- classify_model(container, model, s = lambda_grid)

tock_2 <- Sys.time()
tick_tock_2 <- tick_2 - tock_2 

# Have to weight the training, not the prediction
w <- t(model_results$coefs) %*% model_results$SVM_PROB           # weight vectors

summary(w)

w <- apply(w, 2, function(v){sqrt(sum(v^2))})  # weight

w <- sort(w, decreasing = T)

print(w)

tock <- Sys.time()
tock

# class 1 vs. 14
# class 1 has n[1] SV, class 14 has n[14]
# rows of n[1], column 2 = [1vs2, 1vs3*]
# rows of n[3], column 1 = [3vs1*, 3vs2]
coef1 = c(model$coefs[1:n[1],2],model$coefs[(sum(n[1:2])+1):sum(n),1])
SVs1 = rbind(model$SV[1:n[1],],model$SV[(sum(n[1:2])+1):sum(n),])
w1 = t(SVs1)%*%coef1
w1 <- apply(w1, 2, function(v){sqrt(sum(v^2))})  # weight
w1 <- sort(w1, decreasing = T)

print(w1)

coef14 = c(model$coefs[1:n[14],2], model$coefs[(sum(n[1:13])+1):sum(n),1])
SVs14 = rbind(model$SV[1:n[14],],model$SV[(sum(n[1:13])+1):sum(n),])


# rho stores the b's, [1vs2, 1vs3, 2vs3]
b1 = -model$rho[2]
b14 = -model$rho[15]

plot(rbind(full_data[1:print(round(nrow(full_data) * 0.75))], full_data[print((round(nrow(full_data) * 0.75) + 1)):nrow(full_data)]), col = full_data$ulykketype)
abline(-b1/w1[2], -w1[1]/w1[2], col=4)


# Configure the training data
MTO_container <-
  create_container(
    dtMatrix,
    as.factor(full_data$MTO),
    trainSize = 1:print(round(nrow(full_data)*0.75)),
    testSize = print((round(nrow(full_data)*0.75)+1)):nrow(full_data),
    virgin = FALSE
  )


MTO_model <- train_model(MTO_container, "SVM", kernel = "linear", cost = 1, verbose = TRUE)

MTO_model_results <- classify_model(MTO_container, model, s = lambda_grid)

tick <- Sys.time()
tick - tock

# Trying another approach:

text_model_svm_spec <- svm_linear("classification") %>% 
  set_engine("LiblineaR")

# Re-using one of the classification recipes:

unbalanced_full_data_rec_ulykketype

MTO_text_model_svm_wf <- workflow() %>% 
  add_recipe(MTO_unbalanced_full_data_rec) %>% 
  add_model(text_model_svm_spec)

svm_MTO <- tune::fit_resamples(
  MTO_text_model_svm_wf,
  MTO_unbalanced_vfolds
)


fit_svm_model <- fit(text_model_svm_wf, unbalanced_vfolds)

predictions_SVM <- predict(fit_svm_model, full_data_test)

full_data_test$ulykketype <- as.factor(full_data_test$ulykketype)

bind_cols(full_data_test, predictions_SVM) %>% accuracy(truth = ulykketype, estimate = .pred_class)

roc_SVM <-
  bind_cols(full_data_test, predictions_SVM) %>% 
  roc_curve(ulykketype, .pred___label__1) %>% 
  mutate(Model = "SVM")

roc_SVM %>%
  ggplot(aes(
    x = 1 - specificity,
    y = sensitivity,
    color = Model
  )) +
  geom_path() + geom_abline(lty = 3) +
  coord_equal() + theme_bw() +
  ggtitle(
    "ROC SVM vs Naive bayes",
    "Receiver Operator curve comparing support vector machine \nand naive bayes without hyperparameter tunning"
  )

# Compare to random forest

# Changing from 1000 to 100 trees due to the time it took the calculations
library(ranger)

rf_classification_spec <- rand_forest(trees = 1000) %>%
  set_engine("ranger") %>%
  set_mode("classification")

rf_classification_spec

classification_full_data_rec_mto <-
  recipe(MTO ~ ALL, data = full_data_train) %>%
  step_tokenize(ALL) %>%
  step_stopwords(language = "no") %>%
  step_stopwords(language = "en") %>%
  step_stopwords(custom_stopword_source = custom_words) %>%
  step_tokenfilter(ALL, max_tokens = 1e3) %>%
  step_tfidf(ALL) %>%
  step_downsample(MTO)



rf_classification_wf <- workflow() %>%
  add_recipe(classification_full_data_rec_mto) %>%
  add_model(rf_classification_spec)

rf_classification_rs <- fit_resamples(rf_classification_wf,
                       full_data_folds,
                       control = control_resamples(save_pred = TRUE))

collect_metrics(rf_classification_rs)

library(performanceEstimation)

best_acc_rf_rs <- rf_classification_rs %>%
  show_best("roc_auc")

rf_classification_rs %>%
  collect_predictions() %>%
  #filter(id == "Fold01") %>%
  conf_mat(MTO, .pred_class) %>%
  autoplot(type = "heatmap") +
  scale_y_discrete(labels = function(x) str_wrap(x, 20)) +
  scale_x_discrete(labels = function(x) str_wrap(x, 25)) +
  theme(
    axis.text.x = element_text(
      angle = 60,
      size = 10,
      vjust = 1,
      hjust = 1.1,
      color = "black"
    ))+
  ggtitle(label = " Random forest of 1000 trees predicting MTO accident reports. \n Best accuracy: .906, ROC-AUC: .974")

rf_classification_rs_pred <- rf_classification_rs %>%
  collect_predictions()

rf_classification_rs %>%
  autoplot()

# That was not useful


#### Regression model ####
# From chapter 6 https://smltar.com/mlregression.html#firstmlregression

# I think the data set is too extensive as it is right now, extracting a smaller
# sample first



# small_data <- full_data[!is.na(full_data$antall_skadet),]

full_data <- full_data %>%
  mutate(ALL = str_replace_all(
    string = ALL,
    pattern =  "Konvertert fra PUS:",
    replacement = ""
  )) %>%
  mutate(ALL = str_squish(ALL))

# The algorithm can't handle missing values, if there had been any NA values, 
# this is how one could edit them to 0

# full_data$antall_skadet[is.na(full_data$antall_skadet)] <- 0

library(tidymodels)
set.seed(1234)
full_data_split <-   full_data %>%
  mutate(ALL = removeNumbers(ALL)) %>%
  initial_split()

full_data_train <- training(full_data_split)
full_data_test <- testing(full_data_split)

library(textrecipes)


# We can add more predictors here, such as

full_data_regression_rec <-
  recipe(antall_skadet ~ ALL, data = full_data_train) %>%
  step_tokenize(ALL) %>%
  step_stopwords(language = "no") %>%
  step_stopwords(language = "en") %>%
  step_stopwords(custom_stopword_source = custom_words) %>%
  step_tokenfilter(ALL, max_tokens = 1e3) %>%
  step_tfidf(ALL) %>%
  step_normalize(all_predictors())

full_data_regression_rec

full_data_prep <- prep(full_data_regression_rec)
full_data_bake <- bake(full_data_prep, new_data = NULL)

dim(full_data_bake)

# Creating a workflow

full_data_regression_wf <- workflow() %>%
  add_recipe(full_data_rec)

full_data_regression_wf

svm_regression_spec <- svm_linear() %>%
  set_mode("regression") %>%
  set_engine("LiblineaR")


svm_regression_fit <- full_data_regression_wf %>%
  add_model(svm_regression_spec) %>%
  fit(data = full_data_train)

# Extracting results

svm_regression_fit %>%
  extract_fit_parsnip() %>%
  tidy() %>%
  arrange(-estimate)

# The term Bias here means the same thing as an intercept. We see here what
# terms contribute to a rescue mission having more injured

svm_regression_fit %>%
  extract_fit_parsnip() %>%
  tidy() %>%
  arrange(estimate)

# Creating folds to validate

set.seed(123)
full_data_folds <- vfold_cv(full_data_train)

full_data_folds

# Resampling - rs = resample here

set.seed(123)
svm_regression_rs <- fit_resamples(full_data_regression_wf %>% add_model(svm_regression_spec),
                        full_data_folds,
                        control = control_resamples(save_pred = TRUE))

svm_regression_rs

collect_metrics(svm_regression_rs)

svm_regression_rs %>%
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
  theme_bw() +
  scale_fill_manual(values = c(safe_colorblind_palette))

# Removing the outlier #10

svm_regression_rs %>%
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
  theme_bw() +
  scale_fill_manual(values = c(safe_colorblind_palette)) +
  xlim(0, 22)


# Compare to null model

null_regression <- null_model() %>%
  set_engine("parsnip") %>%
  set_mode("regression")

null_rs <- fit_resamples(full_data_regression_wf %>% add_model(null_regression),
                         full_data_folds,
                         metrics = metric_set(rmse))

null_rs

# Remember that this is using unigrams, but it is in fact a little better than
# the null model

collect_metrics(null_rs)
collect_metrics(svm_regression_rs)


last_fit(full_data_regression_wf, full_data_test)

# Compare to random forest

# Changing from 1000 to 100 trees due to the time it took the calculations
library(ranger)

rf_regression_spec <- rand_forest(trees = 1000) %>%
  set_engine("ranger") %>%
  set_mode("regression")

rf_regression_spec

rf_regression_rs <- fit_resamples(full_data_regression_wf %>% add_model(rf_regression_spec),
                       full_data_folds,
                       control = control_resamples(save_pred = TRUE))

collect_metrics(rf_regression_rs, summarize = F)


collect_predictions(rf_regression_rs) %>%
  ggplot(aes(antall_skadet, .pred, color = id)) +
  geom_abline(lty = 2, color = "gray80", size = 1.5) +
  geom_point(alpha = 0.3) +
  labs(
    x = "Actual number of injured",
    y = "Predicted number of injured",
    color = NULL,
    title = "Predicted and true number of injured people using, a random forest model",
    subtitle = "Each cross-validation fold is shown in a different color, using 1000 trees"
  ) +
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

svm_regression_wf <- workflow() %>%
  add_model(svm_regression_spec)

fit_ngram <- function(ngram_options) {
  fit_resamples(svm_wf %>% add_recipe(ngram_rec(ngram_options)),
                full_data_folds)
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

list(
  `1` = unigram_rs,
  `1 and 2` = bigram_rs,
  `1, 2, and 3` = trigram_rs,
  `1, 2, 3 and 4` = quadgram_rs
) %>%
  map_dfr(collect_metrics, .id = "name") %>%
  filter(.metric == "rmse") %>%
  ggplot(aes(name, mean, color = name)) +
  geom_crossbar(aes(ymin = mean - std_err, ymax = mean + std_err), alpha = 0.6) +
  geom_point(size = 3, alpha = 0.8) +
  theme_bw() +
  theme(legend.position = "none") +
  labs(
    x = "Degree of n-grams",
    y = "RMSE",
    title = "Model performance for different degrees of n-gram tokenization",
    subtitle = "For the same number of tokens, unigrams performed best"
  )

# Comparing mean absolute % error for each resample

svm_regression_rs %>%
  collect_predictions() %>%
  group_by(id) %>%
  mae(antall_skadet, .pred)


# Tuning the model and changing the number of tokens we use

final_regression_rec <- 
  recipe(antall_skadet ~ ALL, data = full_data_train) %>%
  step_tokenize(ALL, token = "ngrams", options = list(n = 2, n_min = 1)) %>%
  step_tokenfilter(ALL, max_tokens = tune::tune()) %>%
  step_stopwords(language = "no") %>%
  step_stopwords(language = "en") %>%
  step_stopwords(custom_stopword_source = custom_words) %>%
  step_tfidf(ALL) %>%
  step_normalize(all_predictors())


final_regression_rec

svm_spec <- svm_linear() %>%
  set_mode("regression") %>%
  set_engine("LiblineaR")

svm_spec

tune_regression_wf <- workflow() %>%
  add_recipe(final_regression_rec) %>%
  add_model(svm_spec)

tune_wf

final_grid <- grid_regular(max_tokens(range = c(1e3, 10e3)),
                           levels = 10)

final_grid

final_regression_rs <- tune_grid(
  tune_regression_wf,
  full_data_folds,
  grid = final_grid,
  metrics = metric_set(rmse, mae, rsq_trad, rsq),
  control = control_resamples(save_pred = TRUE)
)

final_regression_rs

# Grapichally represent differences
final_regression_rs %>%
  collect_metrics() %>%
  ggplot(aes(max_tokens, mean, color = .metric)) +
  geom_line(size = 1.5, alpha = 0.5) +
  geom_point(size = 2, alpha = 0.9) +
  facet_wrap( ~ .metric, scales = "free_y", ncol = 1) +
  theme_bw()+
  scale_x_continuous(breaks = seq(0, 10000, by = 1000))+
  theme(legend.position = "none") +
  labs(x = "Number of tokens",
       title = "Linear SVM performance across number of tokens",
       subtitle = "Performance decreases in all measures as more tokens are included")

chosen_mae <- final_regression_rs %>%
  select_by_pct_loss(metric = "mae", max_tokens, limit = 3)

# check std error

chosen_mae

final_regression_wf <- finalize_workflow(tune_regression_wf, chosen_mae)

final_wf

# Evaluating it on real data

final_regression_fitted <- last_fit(final_regression_wf, full_data_split)

collect_metrics(final_regression_fitted)

full_data_regression_fit <- pull_workflow_fit(final_regression_fitted$.workflow[[1]])

full_data_regression_fit %>%
  tidy() %>%
  filter(term != "Bias") %>%
  #filter(term != "for") %>%
  mutate(
    sign = case_when(
      estimate > 0 ~ "More (than 1 injured)",
      TRUE ~ "Fewer (than or equal to 1 injured)"
    ),
    estimate = abs(estimate),
    term = str_remove_all(term, "tfidf_ALL_")
  ) %>%
  group_by(sign) %>%
  top_n(20, estimate) %>%
  ungroup() %>%
  ggplot(aes(
    x = estimate,
    y = fct_reorder(term, estimate),
    fill = sign
  )) +
  geom_col(show.legend = FALSE) +
  scale_x_continuous(expand = c(0, 0)) +
  facet_wrap( ~ sign, scales = "free") +
  labs(
    y = NULL,
    title = paste(
      "Variable importance for predicting amount of",
      "injured people in accident reports"
    ),
    subtitle = paste(
      "These features are the most importance",
      "in predicting the amount of injured"
    )
  ) +
  theme_bw() +
  scale_fill_manual(values = c(safe_colorblind_palette))

final_regression_fitted %>%
  collect_predictions() %>%
  ggplot(aes(antall_skadet, .pred)) +
  geom_abline(lty = 2, color = "gray80", size = 1.5) +
  geom_point(alpha = 0.3) +
  labs(
    x = "Actual",
    y = "Predicted amount of injured people",
    title = paste(
      "Predicted and actual amount of injured for the testing set of",
      "accident reports"
    ),
    subtitle = "For the testing set, predictions are most accurate between 0-5 injured"
  ) +
  #scale_x_continuous(breaks = seq(0, 22, by = 1)) +
  theme_bw()

full_data_bind <- collect_predictions(final_regression_fitted) %>%
  bind_cols(full_data_test %>% select(-antall_skadet)) %>%
  filter(abs(antall_skadet - .pred) > 1)

full_data_bind %>%
  arrange(-antall_skadet) %>%
  select(antall_skadet, .pred, ALL)

#### OL ####
# Maybe we can aggregate some specific terms?



avg_ol_hits  <- aggregate(eu_hits ~ Month, date_eu_hits, mean)


#### Deceased regression #####
# Trying it out with deceased rather than injured and adding some predictor
# variables

# Tuning the model and changing the number of tokens we use

final_deceased_regression_rec <- recipe(antall_omkommet ~ ALL + ulykketype, data = full_data_train) %>%
  step_tokenize(ALL, token = "ngrams", options = list(n = 2, n_min = 1)) %>%
  step_tokenfilter(ALL, max_tokens = tune()) %>%
  step_stopwords(language = "no") %>%
  step_stopwords(custom_stopword_source = custom_words) %>%
  step_stopwords(language = "en") %>%
  step_tfidf(ALL) %>%
  step_normalize(all_predictors())

final_deceased_regression_rec

svm_spec <- svm_linear() %>%
  set_mode("regression") %>%
  set_engine("LiblineaR")

svm_spec

tune_deceased_regression_wf <- workflow() %>%
  add_recipe(final_deceased_regression_rec) %>%
  add_model(svm_spec)

tune_deceased_regression_wf

final_grid <- grid_regular(max_tokens(range = c(1e3, 1e10)),
                           levels = 10)

final_grid

final_deceased_regression_rs <- tune_grid(
  tune_deceased_regression_wf,
  full_data_folds,
  grid = final_grid,
  metrics = metric_set(rmse, mae),
  control = control_resamples(save_pred = TRUE)
)

final_decased_regression_rs

# Grapichally represent differences
final_deceased_regression_rs %>%
  collect_metrics() %>%
  ggplot(aes(max_tokens, mean, color = .metric)) +
  geom_line(size = 1.5, alpha = 0.5) +
  geom_point(size = 2, alpha = 0.9) +
  facet_wrap( ~ .metric, scales = "free_y", ncol = 1) +
  theme_bw()+
  scale_x_continuous(breaks = seq(0, 10000, by = 2000))+
  theme(legend.position = "none") +
  labs(x = "Number of tokens",
       title = "Linear SVM performance across number of tokens",
       subtitle = "Performance decreases (????) as more tokens are included")

chosen_deceased_mae <- final_deceased_regression_rs %>%
  select_by_pct_loss(metric = "mae", max_tokens, limit = 3)

# check std error

chosen_deceased__mae

final_deceased_regression_wf <- finalize_workflow(tune_deceased_regression_wf, chosen_deceased_mae)

final_deceased_regression_wf

# Evaluating it on real data

final_deceased_regression_fitted <- last_fit(final_deceased_regression_wf, full_data_split)

collect_metrics(final_deceased_regression_fitted)

full_data_deceased_fit <- pull_workflow_fit(final_deceased_regression_fitted$.workflow[[1]])

full_data_deceased_fit %>%
  tidy() %>%
  filter(term != "Bias") %>%
  #filter(term != "fartøyets") %>%
  mutate(
    sign = case_when(
      estimate > 0 ~ "More (than mean deceased)",
      TRUE ~ "Less (than mean deceased)"
    ),
    estimate = abs(estimate),
    term = str_remove_all(term, "tfidf_ALL_")
  ) %>%
  group_by(sign) %>%
  top_n(20, estimate) %>%
  ungroup() %>%
  ggplot(aes(
    x = estimate,
    y = fct_reorder(term, estimate),
    fill = sign
  )) +
  geom_col(show.legend = FALSE) +
  scale_x_continuous(expand = c(0, 0)) +
  facet_wrap( ~ sign, scales = "free") +
  labs(
    y = NULL,
    title = paste(
      "Variable importance for predicting amount of",
      "injured people in accident reports"
    ),
    subtitle = paste(
      "These features are the most importance",
      "in predicting the amount of injured"
    )
  ) +
  theme_bw() +
  scale_fill_manual(values = c(safe_colorblind_palette))

full_data_deceased_fit %>%
  collect_predictions() %>%
  ggplot(aes(antall_skadet, .pred)) +
  geom_abline(lty = 2, color = "gray80", size = 1.5) +
  geom_point(alpha = 0.3) +
  labs(
    x = "Actual",
    y = "Predicted amount of injured people",
    title = paste(
      "Predicted and actual amount of injured for the testing set of",
      "accident reports"
    ),
    subtitle = "For the testing set, predictions are most accurate between 0-5 injured"
  ) +
  #scale_x_continuous(breaks = seq(0, 22, by = 1)) +
  theme_bw()

full_data_deceased_bind <- collect_predictions(final_deceased_regression_fitted) %>%
  bind_cols(full_data %>% select(-antall_omkommet)) %>%
  filter(abs(antall_omkommet - .pred) > 1)

full_data_deceased_bind %>%
  arrange(-antall_omkommet) %>%
  select(antall_omkommet, .pred, ALL)



#### LDA modeling ####
#### Ãrsak ####
library(tm)

# We create a term matrix we can clean up
full_data_Corpus <-
  Corpus(VectorSource(removePunctuation(full_data$ALL)))
full_data_DTM <- DocumentTermMatrix(full_data_Corpus)

# convert the document term matrix to a tidytext corpus
full_data_DTM_tidy <- tidy(full_data_DTM)

# I'm going to add my own custom stop words that I don't think will be
# very informative in these reports
custom_words

norwegian_stop_words <-
  rbind(tibble(word = tm::stopwords(kind = "no")), tibble(word = custom_words))


# remove stopwords
full_data_DTM_tidy_cleaned <-
  full_data_DTM_tidy %>% # take our tidy dtm and...
  anti_join(norwegian_stop_words, by = c("term" = "word")) %>% # remove Norwegian stopwords and custom words
  anti_join(stop_words, by = c("term" = "word")) %>% # remove English stopwords as well
  removeNumbers() %>%
  removePunctuation()

# reconstruct cleaned documents (so that each word shows up the correct number of times)
cleaned_documents <-  full_data_DTM_tidy_cleaned %>%
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


#full_data$ulykketype[is.na(full_data$ulykketype)] <- "Ukjent"



tfidf_bygroup_ALL <- top_terms_by_topic_tfidf(
  text_df = full_data,
  text_column = ALL_cleaned,
  group = ulykketype,
  plot = T
)

# do our own plotting
tfidf_bygroup_ALL  %>%
  group_by(ulykketype) %>%
  top_n(5) %>%
  ungroup %>%
  ggplot(aes(word, tf_idf, fill = ulykketype)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  theme_bw() +
  facet_wrap( ~ ulykketype, ncol = 4, scales = "free",) +
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
  # Added custom_words to norwegian_stop_words earlier
  #filter(!word1 %in% custom_words) %>%
  #filter(!word2 %in% custom_words) %>%
  unite(word, word1, word2, sep = " ") %>%
  count(word, sort = TRUE) %>%
  top_n(10) %>%
  slice(1:10) %>%
  ggplot() +
  geom_bar(aes(word, n),
           stat = "identity",
           fill = "#de5833",
           position = "dodge") +
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
  geom_point(aes(color = ulykkesår)) + # add points to our plot, color-coded by president
  geom_smooth(method = "auto") # pick a method & fit a model


small_data <- full_data[!is.na(full_data$ALL), ]

full_data_cleaned <- full_data %>%
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
dtm <- CreateDtm(full_data_cleaned$word,
                 ngram_window = c(1, 3))
#, stem_lemma_function = function(x) SnowballC::wordStem(x, language = "norwegian"

tf <- TermDocFreq(dtm = dtm)

original_tf <- tf %>% select(term, term_freq, doc_freq)

original_tf <-
  rowid_to_column(original_tf, var = "rowid") # Eliminate words appearing less than 2 times or in more than half of the
# documents
vocabulary <-
  tf$term[tf$term_freq > 1 & tf$doc_freq < nrow(dtm) / 2]

model_dir <-
  paste0("models_", digest::digest(vocabulary, algo = "sha1"))
if (!dir.exists(model_dir))
  dir.create(model_dir)

k_list <- seq(1, 196, by = 1)
setwd("~/Master/masterthesis/models_b6f60a5ca3dbd516b99a26f66ee8276ad1ed2829")

library(parallel)
tick <- Sys.time()

model_dir <- paste0("models_b6f60a5ca3dbd516b99a26f66ee8276ad1ed2829")

model_list <-
  TmParallelApply(
    cpus = 8,
    X = k_list,
    FUN = function(k) {
      filename = file.path(model_dir, paste0(k, "_topics.rda"))
      
      if (!file.exists(filename)) {
        m <- FitLdaModel(dtm = dtm,
                         k = k,
                         iterations = 500)
        m$k <- k
        m$coherence <-
          CalcProbCoherence(phi = m$phi,
                            dtm = dtm,
                            M = 5)
        save(m, file = filename)
      } else {
        load(filename)
      }
      
      m
    },
    export = c("dtm", "model_dir")
  ) # export only needed for Windows machines

#model tuning

file_names <- as.list(dir(path = model_dir, pattern="*_topics.rda"))

cores <- makeCluster(detectCores()/2)

model_list <- parLapply(file_names, function(x){get(load(x,.GlobalEnv))}, cl = cores)


#choosing the best model
coherence_mat <-
  data.frame(
    k = sapply(model_list, function(x)
      nrow(x$phi)),
    coherence = sapply(model_list, function(x)
      mean(x$coherence)),
    stringsAsFactors = FALSE
  ) %>%


ggplot(coherence_mat, aes(x = k, y = coherence)) +
  geom_point() +
  geom_line(group = 1) +
  ggtitle("Best topics by coherence score, k = 1-181") +
  theme_bw() +
  #scale_x_continuous(breaks = seq(1, 20, by = 1)) +
  #scale_y_continuous(limits = c(-0.008, 0),
  #breaks = seq(-0.008, 0, by = 0.001)) +
  ylab("Coherence")

# Top 20 terms, describing what this topic is about.
model <- model_list[which.max(coherence_mat$coherence)][[1]]

model$top_terms <- GetTopTerms(phi = model$phi, M = 20)

top20_wide <- as.data.frame(model$top_terms)

allterms <- data.frame(t(model$phi))

allterms$word <- rownames(allterms)

rownames(allterms) <- 1:nrow(allterms)

allterms <- melt(allterms, idvars = "word")

allterms <- allterms %>% rename(topic = variable)

FINAL_allterms <-
  allterms %>% group_by(topic) %>% arrange(desc(value))

tock <- Sys.time()

write_file(x = as.character(tock), file = "done.txt")

time_spent <- tock-tick
# r^2 for LDA ####

library(textmineR)
r2 <-
  CalcTopicModelR2(
    dtm = dtm,
    phi = model_list[[118]]$phi,
    theta = model_list[[118]]$theta,
    cpus = 12
  )

# 0.0007624563

# summary of document lengths
doc_lengths <- rowSums(dtm)

summary(doc_lengths)

# what words are most associated with more than mean injured?

# remove any tokens that were in 3 or fewer documents
dtm2 <- dtm[, colSums(dtm > 0) > 3]

tf2 <- tf[tf$term %in% colnames(dtm2) ,]

# look at the most frequent bigrams
tf_bigrams <- tf2[stringr::str_detect(tf2$term, "_") ,]

tf_bigrams <- tf_bigrams[tf_bigrams$term %in% colnames(dtm2) ,]

tf_meanantallskadet <-
  list(less = TermDocFreq(dtm[full_data$antall_skadet < 1.01 ,]),
       more = TermDocFreq(dtm[full_data$antall_skadet > 1.01 ,]))

head(tf_meanantallskadet$less[order(tf_meanantallskadet$less$term_freq, decreasing = TRUE) ,], 10)

head(tf_meanantallskadet$more[order(tf_meanantallskadet$more$term_freq, decreasing = TRUE) ,], 10)


# let's reweight by probability by class
p_words <- colSums(dtm) / sum(dtm)

tf_meanantallskadet$less$conditional_prob <-
  tf_meanantallskadet$less$term_freq / sum(tf_meanantallskadet$less$term_freq)

tf_meanantallskadet$less$prob_lift <-
  tf_meanantallskadet$less$conditional_prob - p_words

tf_meanantallskadet$more$conditional_prob <-
  tf_meanantallskadet$more$term_freq / sum(tf_meanantallskadet$more$term_freq)

tf_meanantallskadet$more$prob_lift <-
  tf_meanantallskadet$more$conditional_prob - p_words

# let's look again with new weights
head(tf_meanantallskadet$less[order(tf_meanantallskadet$less$prob_lift, decreasing = TRUE) ,], 10)

head(tf_meanantallskadet$more[order(tf_meanantallskadet$more$prob_lift, decreasing = TRUE) ,], 10)

# what about bi-grams?
tf_meanantallskadet_bigram <-
  lapply(tf_meanantallskadet, function(x) {
    x <- x[stringr::str_detect(x$term, "_") ,]
    x[order(x$prob_lift, decreasing = TRUE) ,]
  })

head(tf_meanantallskadet_bigram$less, 10)

head(tf_meanantallskadet_bigram$more, 10)


# Calculating cosine similarity and distance is not possible due to the size of
# the DTM.

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
plot(model$prevalence,
     model$alpha,
     xlab = "prevalence",
     ylab = "alpha")


# textmineR has a naive topic labeling tool based on probable bigrams
model$labels <- LabelTopics(assignments = model$theta > 0.05,
                            dtm = dtm,
                            M = 1)

head(model$labels)


# put them together, with coherence into a summary table
model$summary <- data.frame(
  topic = rownames(model$phi),
  label = model$labels,
  coherence = round(model$coherence, 3),
  prevalence = round(model$prevalence, 3),
  top_terms = apply(model$top_terms, 2, function(x) {
    paste(x, collapse = ", ")
  }),
  stringsAsFactors = FALSE
)


model$summary[order(model$summary$prevalence, decreasing = TRUE) ,][1:10 ,]


# predictions with gibbs
assignments <- predict(
  model,
  dtm,
  method = "gibbs",
  iterations = 200,
  burnin = 180,
  cpus = 2
)

# predictions with dot
assignments_dot <- predict(model, dtm2,
                           method = "dot")


# compare
barplot(
  rbind(assignments[10, ], assignments_dot[10, ]),
  col = c("red", "blue"),
  las = 2,
  beside = TRUE
)
legend(
  "topright",
  legend = c("gibbs", "dot"),
  col = c("red", "blue"),
  fill = c("red", "blue")
)


# Exporting top 20 terms

write.csv(top20_wide, "top20_wide.csv")

# Dendrogram for calculating similarities
model$topic_linguistic_dist <- CalcHellingerDist(model$phi)

model$hclust <-
  hclust(as.dist(model$topic_linguistic_dist), "ward.D2")

model$hclust$labels <-
  paste(model$hclust$labels, model$labels[, 1])

plot(model$hclust, xlab = "Topic relationships", sub = "")

#visualising topics of words based on the max value of phi

set.seed(1234)

final_summary_words <- data.frame(top_terms = t(model$top_terms))

final_summary_words$topic <- rownames(final_summary_words)

rownames(final_summary_words) <- 1:nrow(final_summary_words)

final_summary_words <-
  final_summary_words %>% melt(id.vars = c("topic"))

final_summary_words <-
  final_summary_words %>% rename(word = value) %>% select(-variable)

final_summary_words <- left_join(final_summary_words, allterms)

final_summary_words <-
  final_summary_words %>% group_by(topic, word) %>%
  arrange(desc(value))

final_summary_words <-
  final_summary_words %>% group_by(topic, word) %>% filter(row_number() == 1) %>%
  ungroup() %>% tidyr::separate(topic, into = c("t", "topic")) %>% select(-t)

word_topic_freq <-
  left_join(final_summary_words, original_tf, by = c("word" = "term"))

library(wordcloud)
pdf("cluster.pdf")

for (i in 1:length(unique(final_summary_words$topic)))
{
  wordcloud(
    words = subset(final_summary_words , topic == i)$word,
    freq = subset(final_summary_words , topic == i)$value,
    min.freq = 1,
    max.words = 200,
    random.order = FALSE,
    rot.per = 0.35,
    colors = brewer.pal(8, "Dark2")
  )
}
dev.off()

# Single wordcloud

wordcloud::wordcloud(
  words = subset(final_summary_words , topic == 15)$word,
  freq = subset(final_summary_words , topic == i)$value,
  min.freq = 1,
  max.words = 200,
  random.order = FALSE,
  rot.per = 0.25,
  colors = brewer.pal(8, "Dark2")
)




# other stuff
cleaned_full_data <- full_data %>%
  unnest_tokens(word, ALL) %>%
  anti_join(get_stopwords("no")) %>%
  anti_join(get_stopwords("en")) %>%
  filter(!word %in% custom_words)

cleaned_full_data$word <-
  stemDocument(cleaned_full_data$word, language = "norwegian")

top_terms_by_topic_LDA(
  input_text = cleaned_full_data$word,
  plot = T,
  number_of_topic = 20
)

top_terms_by_topic_tfidf(text_df = full_data)

tfidf_bygroup_ALL <-
  top_terms_by_topic_tfidf(
    text_df = full_data,
    text_column = ALL,
    group = ulykketype,
    plot = T
  )
