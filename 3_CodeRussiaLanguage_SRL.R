# Project:    Russia- Friend or Foe?
# Task:       Code semantic roles of Russia and conflictuality
# Author:     Ch Rauh (April 30 2022)


# Packages
library(tidyverse)
library(extrafont)
library(quanteda)
library(countrycode)
library(LSX)


# Data ####

# UN sentences with meta data
ungd <- read_rds("./Data/ungd_sentences.rds") %>% 
  mutate(id = row_number())

# Semantic roles in UNGD sentences
# Sentence ID matches row number in UN sentences data
srl <- read_rds("./Data/ungd_sentences_srl.rds")

# Add metadata to SRL results
srl <- srl %>% 
  left_join(ungd, by = "id")


# Identify Russia related sentences ####

# RegEx / Dictionary
russ.dict <- "(russia[a-z]*)|(soviet[a-z]*)|brezhnev([^a-z]|$)|andropov([^a-z]|$)|chernenko([^a-z]|$)|gorbachev([^a-z]|$)|yeltsin([^a-z]|$)|medvedev([^a-z]|$)|putin([^a-z]|$)|kremlin([^a-z]|$)"


# Russia as an agent

# putin <- srl %>% 
#   filter(str_detect(tolower(agent), "putin[^a-z]"))

russ.ag <- srl %>% 
  filter(str_detect(tolower(agent), russ.dict))


# There are duplicated sentences - why?
duplicate.ids <- russ.ag %>% filter(duplicated(id)) %>% select(id)
duplicates <- russ.ag %>% filter(id %in% duplicate.ids$id)
# All cases with either multiple agents or multiple verbs

# Remove duplicates 
russ.ag <- russ.ag %>% 
  select(-c(agent, negation, verb, patient, sentence)) %>% 
  unique()
nrow(duplicate.ids <- russ.ag %>% filter(duplicated(id)) %>% select(id)) == 0

# Inspect examples
i <- sample(1:nrow(russ.ag), 1)
i
russ.ag$text[i]

test <- srl %>% filter(id == russ.ag$id[i])

# Russia as a patient

# putin <- srl %>%
#   filter(str_detect(tolower(patient), "putin[^a-z]"))

russ.pa <- srl %>% 
  filter(str_detect(tolower(patient), russ.dict))


# There are duplicated sentences - why?
duplicate.ids <- russ.pa %>% filter(duplicated(id)) %>% select(id)
duplicates <- russ.pa %>% filter(id %in% duplicate.ids$id)
# Again either multiple patients or multiple verbs

# Remove duplicates 
russ.pa <- russ.pa %>% 
  select(-c(agent, negation, verb, patient, sentence)) %>% 
  unique()
nrow(duplicate.ids <- russ.pa %>% filter(duplicated(id)) %>% select(id)) == 0

# One problem here is that Russia will often be a patient of a verb for which it is also an agent
# I thus filter such, that this sentence set refers only to instances where Russia was solely (!) a patient

russ.pa <- russ.pa %>% 
  filter(!(id %in% russ.ag$id))

# test <- srl %>% filter(id == 408779) # mmmh ....

# Inspect examples
i <- sample(1:nrow(russ.pa), 1)
i
russ.pa$text[i]
russ.pa$id[i]

test <- srl %>% filter(id == russ.pa$id[i])

# Many nominalizations
# The adherence of Russia, the conclusion by Russia

# What's up with cases in which an agent has no patient, or a patient has no agent?
# What would I be filtering this way?


# Add Russia codes to full UNGD Sentences ####

ungd$russia.agent <- ifelse(ungd$id %in% russ.ag$id, TRUE, FALSE)
sum(ungd$russia.agent) == nrow(russ.ag)
ungd$russia.patient <- ifelse(ungd$id %in% russ.pa$id, TRUE, FALSE)
sum(ungd$russia.patient) == nrow(russ.pa)

ungd$russia.pres <- str_detect(tolower(ungd$sentence), russ.dict)

ungd %>% 
  group_by(russia.pres, russia.agent, russia.patient) %>% 
  summarize(n=n())


# Check cases in which russia occurs, buit in no role

unspecified <- ungd %>% 
  filter(russia.pres & !russia.agent & !russia.patient)

i <- sample(1:nrow(unspecified), 1)
i
unspecified$sentence[i]
unspecified$id[i]

test <- srl %>% filter(id == unspecified$id[i])

# Many true negatives
# But also nominalizations and sometimes undetected verbs





# Apply conflictual/cooperative language model ####

rm(srl, russ.pa, russ.ag, test, unspecified, duplicates, duplicate.ids, putin, i)
gc()


tmod_lss <- read_rds("./Data/lss_model.rds")

corp <- corpus(ungd$sentence)
tok <- tokens(corp,
                   remove_punct = TRUE, remove_symbols = TRUE, 
                   remove_numbers = TRUE, remove_url = TRUE) %>% 
  tokens_remove(stopwords("en"))
dfm <- tok %>% 
  dfm() %>% 
  dfm_remove(pattern = "")

ungd$lss.fit <- predict(tmod_lss, newdata = dfm)
hist(ungd$lss.fit)

# Export data ####
write_rds(ungd, "./Data/ungd_sentences_RussiaCodes.rds")

