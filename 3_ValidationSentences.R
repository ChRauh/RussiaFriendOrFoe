
# Project:  Russia Friend or Foe
# Task:     Get a set of sentences 
#           To create manual coding benchmarks for the LSS score


# Packages #####
library(tidyverse)
library(quanteda)
library(countrycode)
library(spacyr)
library(LSX)

# UNGD corpus
ungd <- read_rds("./Data/ungd.rds")


# Russia references ####
ungd$russ.ref <- str_count(tolower(ungd$text), "(russia[a-z]*)|(soviet[a-z]*)")
sum(ungd$russ.ref > 0)


# Sentence tokenization ####

spacy_initialize(model = "en_core_web_sm")

# Target data frame  
sentences <- data.frame()

# Loop over speeches and tokenize

for (i in 1:nrow(ungd)) {
  
  # Progress
  print(paste0(i, "/", nrow(ungd)))
  
  # Tokenize current speech
  current <- spacy_parse(ungd$text[i]) %>% 
    group_by(sentence_id) %>% 
    summarise(sentence = paste(token, collapse = " ") %>% 
                str_trim()) %>% 
    filter(str_detect(sentence, "[a-z]")) %>% # Drops empty lines and para numbers
    mutate(doc_id = ungd$doc_id[i],
           country = ungd$country[i],
           state = ungd$state[i],
           year = ungd$year[i])
  
  # Append to target
  sentences <- rbind(sentences, current)
}

# Keep sentence level data on disk 
write_rds(sentences, "./Data/ungd_sentences.rds")



# Apply conflict-cooperation scale ####

# The trained model
tmod_lss <- read_rds("./Data/lss_model.rds")

# Dfm of sentences
corp_sent <- corpus(sentences$sentence, docvars = sentences[,c("doc_id", "sentence_id")])
tok_sent <- tokens(corp_sent,
                   remove_punct = TRUE, remove_symbols = TRUE, 
                   remove_numbers = TRUE, remove_url = TRUE) %>% 
  tokens_remove(stopwords("en"))
dfm_sent <- tok_sent %>% 
  dfm() %>% 
  dfm_remove(pattern = "")

# Predict polarity based on LSS model
sentences$lss.fit <- predict(tmod_lss, newdata = dfm_sent)
hist(sentences$lss.fit)

# Examples
sentences %>% 
  arrange(lss.fit) %>% 
  head(10) %>% 
  select(c(sentence, lss.fit))

sentences %>% 
  arrange(desc(lss.fit)) %>% 
  head(10) %>% 
  select(c(sentence, lss.fit))




# Sample sentences for human coding ####

# Sample
set.seed(20220325)
df <- sentences[sample(1:nrow(sentences), 150), ]
hist(df$lss.fit)

# Examples
df %>% 
  arrange(desc(lss.fit)) %>% 
  head(10) %>% 
  select(c(sentence, lss.fit))

# Text cleaning
df$sentence <- df$sentence %>% 
  str_replace_all("\\s+", " ")

# Split reasonable sample sizes
write_rds(df[1:50, ], "./Data/Validation/Set1.rds")
write_rds(df[51:100, ], "./Data/Validation/Set2.rds")
write_rds(df[101:150, ], "./Data/Validation/Set3.rds")


# Sentences referring to Russia somehow ####
russ <- sentences %>% 
  filter(str_detect(tolower(sentence), "(russia[a-z]*)|(soviet[a-z]*)"))

russ %>% 
  arrange(lss.fit) %>% 
  head(10) %>% 
  select(c(sentence, lss.fit))











  
  



