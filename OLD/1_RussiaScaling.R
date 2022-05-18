

# Packages #####
library(tidyverse)
library(quanteda)
library(countrycode)
library(LSX)


# Corpus ####
ungd <- read_rds("./Data/ungd.rds")


# Russia references ####

ungd$russ.ref <- str_count(tolower(ungd$text), "(russia[a-z]*)|(soviet[a-z]*)")
sum(ungd$russ.ref > 0)

hist(ungd$russ.ref)
# russ.ref.eu <- ungd %>%
#   filter(eu == 1) %>%
#   group_by(year, country) %>%
#   summarise(russ = sum(russ.ref))


# Quanteda objects  ###

# Corpus
ungd$id <- 1:nrow(ungd)
ungd_corpus <- corpus(ungd$text, docvars = ungd[ ,c("id", "session", "country")])

# Sentence tokenization
corp_sent <- corpus_reshape(ungd_corpus, to =  "sentences")
toks_sent <- corp_sent %>% 
  tokens(remove_punct = TRUE, remove_symbols = TRUE, 
         remove_numbers = TRUE, remove_url = TRUE) %>% 
  tokens_remove(stopwords("en")) %>% 
  tokens_remove(countrycode::codelist$country.name.en) # Remove english country names - they should not affect the scale

# Doc frequency matrix (sentences)
dfmat_sent <- toks_sent %>% 
  dfm() %>% 
  dfm_remove(pattern = "") %>% 
  dfm_trim(min_termfreq = 5)

topfeatures(dfmat_sent, 20)


# Seed dictionary ####
# Aim is to measure whether Russia is presented as friend or foe

conflict_terms <- list(
  positive = c("friend", "partner", "ally", "peace", "peaceful",  "friendly", "cooperative"),
  negative = c("foe", "opponent", "enemy", "war", "aggressive", "hostile", "uncooperative")
)

conflict_dict <- dictionary(conflict_terms)

seed <- as.seedwords(conflict_dict)
print(seed)

# Run LSS model
# Intentionally no context words
tmod_lss <- textmodel_lss(dfmat_sent, seeds = seed,
                          k = 300, cache = TRUE)

head(coef(tmod_lss), 20) # most cooperative words
tail(coef(tmod_lss), 20) # most adversarial words

write_rds(tmod_lss, "./Data/lss_model.rds")

# Illustarte word weights
# textplot_terms(tmod_lss, unlist(conflict_terms), max_words = 5000)

# Maybe construct a nicer plot, highlighting random 'learned' words in different color
# Export mdoel to that end


# Text around references to Russia ####

# KWIC

russ.kwic <-
  kwic(
    ungd_corpus,
    pattern = "(russia[a-z]*)|(soviet[a-z]*)",
    window = 40,
    valuetype = "regex",
    separator = " ",
    case_insensitive = TRUE
  )

russ.kwic$russ.text <- 
  paste(russ.kwic$pre, russ.kwic$post, sep = " ") %>% 
  str_remove_all(fixed("Union")) %>% 
  str_remove_all(fixed("Federation"))

russ.kwic$rid <- 1:nrow(russ.kwic)


# DFM
corp_russ <- corpus(russ.kwic$russ.text, docvars = russ.kwic[,c("rid")])
tok_russ <- tokens(corp_russ,
                   remove_punct = TRUE, remove_symbols = TRUE, 
                   remove_numbers = TRUE, remove_url = TRUE) %>% 
  tokens_remove(stopwords("en"))
dfm_russ <- tok_russ %>% 
  dfm() %>% 
  dfm_remove(pattern = "")

# Predict polarity based on LSS model

russ.kwic$lss.fit <- predict(tmod_lss, newdata = dfm_russ)

hist(russ.kwic$lss.fit)

# Store this in the bigger corpus ####

# Summarise estimate of cooperative language
# on the level of speeches
russ.lang <-
  russ.kwic %>% 
  as.data.frame() %>% 
  mutate(id = as.numeric(str_remove(docname, fixed("text")))) %>% 
  select(c("id", "lss.fit")) %>% 
  group_by(id) %>% 
  summarise(coop.language = mean(lss.fit))

# Should match the number of speeches with russia reference as per the above
nrow(russ.lang) == sum(ungd$russ.ref > 0)

# Merge and export
ungd.russ <- ungd %>% 
  left_join(russ.lang, by = "id") %>% 
  select(-c("doc_id", "text", "state", "eec"))

write_rds(ungd.russ, "./Data/UNGA_RussiaLanguage.RDS")

