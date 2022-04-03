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


# Plot ####

mean(sent$lss.fit, na.rm = T)
sd(sent$lss.fit, na.rm = T)

ustargets$posneg <- "neutral"
ustargets$posneg[ustargets$lss.fit > .5] <- "cooperative"
ustargets$posneg[ustargets$lss.fit < -.5] <- "conflictual"
ustargets$posneg <- factor(ustargets$posneg, levels = c("conflictual", "neutral", "cooperative"))

ggplot(ustargets, aes(x = year, y = lss.fit))+
  geom_hline(yintercept = 0, linetype = "dashed", size =.5)+
  #stat_smooth(method = "loess")+
  geom_jitter(width = .2, alpha = .6, shape = 16, aes(color = posneg))+ 
  # stat_summary(geom = "line", fun = mean)+
  # scale_colour_gradient2(low = "#9e3173",
  #                        mid = "grey50",
  #                        high = "#619933")+
  scale_colour_manual(values = c("red", "grey70", "green"))+
  facet_wrap(.~target, nrow = 2)+
  labs(title = "How the US spoke about other countries in the United Nations General Assembly",
       subtitle = "Estimated level of conflictual vs. cooperative language in individual sentences from US speeches mentioning selected countries.",
       y = "Latent Semantic Scaling estimate\nfor conflictual vs cooperative language\n",
       x = "\nYear of the UN General Assembly session",
       color = "Estimated tendency of sentence: ")+
  theme_bw()+
  theme(legend.position = "bottom",
        axis.text = element_text(color = "black"),
        text = element_text(family = "Dahrendorf"))

ggsave("./Plots/HumanValidation/UStragets.png", width = 26, height = 16, units = "cm")



# Look at examples ####

test <- syria %>% filter(year >=2010)
  


