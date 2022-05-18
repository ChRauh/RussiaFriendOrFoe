# Task: Try to ompliment semantic role labelling in python
#       Based on Ash et al. relatio modules


# Packages #####
library(reticulate)
library(tidyverse)


# Choose python build here
# I am using a 3.7 version, in which realtio is installed
# along the instructions at https://github.com/relatio-nlp/relatio
use_python("C:/Users/chris/AppData/Local/Programs/Python/Python37/python.exe", required = TRUE)

# Load python shell
# (also check which one its is)
# repl_python()

# Test text ####

text <- c("Russia attacked the Ukraine yesterday morning.",
          "The Ukraine was attacked by Russia.",
          "It is a long standing issue that Russia does not comply with the non-proliferation treaty.",
          "There are other countries like Russia.",
          "The US does not care, so Russia does not care.",
          "The Russian people are not to blame, its the Russian government that is acting in bad faith!",
          "Germany will stop Russian energy imports, so that the Russian government cannot finance this war any longer.")

# Get text into python environment
py$text <- r_to_py(text, convert = T)

# text <- py$text # Get python object back into R


# Semantic role labelling
# with relatio in python
py_run_string("from relatio.wrappers import run_srl
srl_res = run_srl(
    path=\"https://storage.googleapis.com/allennlp-public-models/openie-model.2020.03.26.tar.gz\", # pre-trained model
    sentences=text,
    progress_bar=True,
)")

# Get srl results back into r
srl <- py$srl_res

# Clean up ####

# triplets <- character()
# for (i in 1:length(srl)) {
#   
#   # There may be several verbs in each sentence verbs[[j]],
#   # and each results in different semantic roles - so more triplets than sentences
#   # need to find a way to carry the id through here (i is this id!)!
#   
#   current <- srl[[i]]$verbs[[1]]$description
#   triplets <- c(triplets, current)
# }


triplets <-data.frame()

for (i in 1:length(srl)) {
  
  # There may be several verbs in each sentence verbs[[j]],
  # and each results in different semantic roles - so more triplets than sentences
  
  # Carry the sentence id through
  id <- i
  
  # Get number of verbs in sentence
  nverbs <- length(srl[[i]]$verbs)
  
  # object to hold all triplets
  current <- character()
  
  # Loop over verbs
  for (j in 1:nverbs) {
    triplet <- srl[[i]]$verbs[[j]]$description
    current <- c(current, triplet)
  }
  
  # Assemble and append
  df <- data.frame(id = rep(id, length(current)),
                   raw = current,
                   text = paste(srl[[i]]$words, collapse = " "))
  triplets <- rbind(triplets, df)
}

# Separate the semantic roles (brute force stringr)
triplets <- triplets %>% 
  mutate(agent = str_extract_all(raw, "\\[ARG0.*?\\]"),
         negation =str_extract_all(raw, "\\[ARGM-NEG.*?\\]"),
         verb = str_extract_all(raw, "\\[V.*?\\]"),
         patient = str_extract_all(raw, "\\[ARG1.*?\\]"))# Additional labels available!

triplets2 <- triplets %>% 
  mutate_at(vars(4, 5, 6, 7), ~str_remove_all(., "(\\[.*?:)|(\\])") %>% 
              str_trim()) %>% 
  select(-raw)

triplets2[triplets2 == "character(0)"] <- NA
write_rds(triplets2, "./Data/SRL-Examples.rds")



