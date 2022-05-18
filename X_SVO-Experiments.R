
# TEST POS identification



####################################################################
# Project:  Russia - friend or foe?
# Task:     Face validiation how the US talks about its enemies
# Author:   Christian Rauh 29.03.2022
####################################################################


# Packages ####
library(tidyverse)
library(quanteda)
library(quanteda.textmodels)
library(LSX)
library(extrafont)
library(countrycode)
library(spacyr)

# Grün: #619933
# Pink: #9e3173
# Blau: #0380b5


# UNGA sentences ####
sent <- read_rds("./Data/ungd_sentences.rds")


# Predict conflictual/cooperative language ####

# The trained model
tmod_lss <- read_rds("./Data/lss_model.rds")

# Dfm of sentences
corp_sent <- corpus(sent$sentence)
tok_sent <- tokens(corp_sent,
                   remove_punct = TRUE, remove_symbols = TRUE, 
                   remove_numbers = TRUE, remove_url = TRUE) %>% 
  tokens_remove(stopwords("en"))
dfm_sent <- tok_sent %>% 
  dfm() %>% 
  dfm_remove(pattern = "")

# Predict polarity based on LSS model
sent$lss.fit <- predict(tmod_lss, newdata = dfm_sent)
hist(sent$lss.fit)



# Detect country names in sentences ####
# countries <- countrycode::codelist$country.name.en %>% 
#   tolower() %>% 
#   paste(collapse = "|")
# 
# sent$cmentions <- str_extract_all(tolower(sent$sentence), countries)

countries <- countrycode::codelist


# Sentences from the US ####
us <- sent %>% filter(country == "USA")

# ... about other countries
iran <- us %>% 
  filter(str_detect(tolower(sentence), "iran")) %>% 
  select(sentence, year, lss.fit) %>% 
  mutate(target = "Iran")

iraq <- us %>% 
  filter(str_detect(tolower(sentence), "iraq")) %>% 
  select(sentence, year, lss.fit) %>% 
  mutate(target = "Iraq")

syria <- us %>% 
  filter(str_detect(tolower(sentence), "syria")) %>% 
  select(sentence, year, lss.fit) %>% 
  mutate(target = "Syria")

sarabia <- us %>% 
  filter(str_detect(tolower(sentence), "saudi arabia")) %>% 
  select(sentence, year, lss.fit) %>% 
  mutate(target = "Saudi Arabia")


china <- us %>% 
  filter(str_detect(tolower(sentence), "china|chinese")) %>% 
  select(sentence, year, lss.fit) %>% 
  mutate(target = "China")

russia <- us %>% 
  filter(str_detect(tolower(sentence), "russia|soviet")) %>% 
  select(sentence, year, lss.fit) %>% 
  mutate(target = "Soviet Union / Russia")

germany <- us %>% 
  filter(str_detect(tolower(sentence), "germany|german ")) %>% 
  select(sentence, year, lss.fit) %>% 
  mutate(target = "Germany")

uk <- us %>% 
  filter(str_detect(tolower(sentence), "united kingdom|great britain|british")) %>% 
  select(sentence, year, lss.fit) %>% 
  mutate(target = "United Kingdom")

ustargets <- rbind(uk, germany, russia, china,
                   iran, iraq, syria, sarabia) %>% 
  mutate(target = factor(target, levels = c("China", "Soviet Union / Russia", "Germany", "United Kingdom",
                                            "Iran", "Iraq", "Syria", "Saudi Arabia")))


# Test cases ####
# US messages on Iraq in 2003 should be conflictual

test <- iraq %>% 
  filter(year == 2002) %>% 
  filter(lss.fit > 0)


# SPO Triplets

spacy_initialize()

text <- c("The Russian federal government has attacked the Ukranian opposition.",
          "Russia attacked.",
          "If this goes off, Russia is to blame.",
          "It is Russia's fault",
          "This war was started by Russia.")

parsed <- spacy_parse(text, 
            dependency = T, nounphrase = T, lemma = F)
# nounphrase_consolidate(parsed)
# 
# nps <- spacy_extract_nounphrases(text) %>% 
#   mutate(end_id = start_id + (length-1)) # Final token id of noun phrase
# Get subjects and objects from parsed text
# subjects <- parsed %>% filter(str_detect(dep_rel, "subj"))
# objects <- parsed %>% filter(str_detect(dep_rel, "obj"))
  
# A noun phrase consolidator
# That tells me whether it is obj or sub

# Prepare the parsed output

# Row id for joining
parsed$row <- 1:nrow(parsed) 

# npgroups <- data.frame(row = which(str_detect(parsed$nounphrase, "beg")), # Row in which a new nounphrase begins
#                        groupnum = 1:length(which(str_detect(parsed$nounphrase, "beg")))) # Number of nounphrase group
# parsed <- parsed %>% left_join(npgroups, by=("row"))


# Mark potential subject-predicate-object phrases
parsed$spo <- parsed$nounphrase
parsed$spo[parsed$spo == "" & str_detect(parsed$dep_rel, "subj")] <- "beg"
parsed$spo[parsed$spo == "" & str_detect(parsed$dep_rel, "obj")] <- "beg"
parsed$spo[parsed$spo == "" & parsed$dep_rel == "ROOT"] <- "beg"

npgroups <- data.frame(row = which(str_detect(parsed$spo, "beg")), # Row in which a new nounphrase begins
                       groupnum = 1:length(which(str_detect(parsed$spo, "beg")))) # Number of nounphrase group
parsed <- parsed %>% left_join(npgroups, by=("row"))

# Fill up group num over length of nounphrase
for (i in 1:nrow(parsed)) {
  if(!is.na(parsed$groupnum[i])) {next}
  if(parsed$nounphrase[i] == "") {next}
  parsed$groupnum[i] <- parsed$groupnum[i-1]
}

# Consolidate nounphrase and store dependency relation
nps <- parsed %>% 
  filter(!is.na(groupnum)) %>% 
  group_by(groupnum) %>% 
  summarise(doc_id = unique(doc_id),
            sentence_id = unique(sentence_id),
            nounphrase = paste(token, collapse = " "),
            dependencies = paste(dep_rel, collapse = " ")) %>% 
  mutate(type = ifelse(str_detect(dependencies, "subj"), 
                       "subject", 
                       ifelse(str_detect(dependencies, "obj"), 
                              "object", 
                              ifelse(dependencies == "ROOT", 
                                     "predicate",
                                     NA)))) %>% 
  select(-c(dependencies, groupnum))


# Cases where agency does not lie with the subject
spacy_parse("It is Russia's fault", 
            dependency = T, lemma = F, nounphrase = T)
spacy_parse("If there is one force that can achieve this, it is the EU.", 
            dependency = T, lemma = F, nounphrase = T)


# Check the 'attr' tag
# https://stackoverflow.com/questions/62895997/nlp-what-is-exactly-a-grammar-dependence-tag-attr
# https://emorynlp.github.io/ddr/doc/pages/attr.html

# Agency in passive constructions
spacy_parse("Ukraine was attacked by Russia.", 
            dependency = T, lemma = F, nounphrase = T)