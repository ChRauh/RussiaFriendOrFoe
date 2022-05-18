# install.packages("sentiment.ai")
library(sentiment.ai)
library(data.table)
library(tidyverse)
library(extrafont)
# install_sentiment.ai() # Only once, installs the python backend




# Initiate the model once
# This will create the sentiment.ai.embed model
# Do this so it can be reused without recompiling - especially on GPU!
init_sentiment.ai()

text <- c(
  "We continue to engage with Russia on a good basis.",
  "Russia'S approach is fundamentally different than ours.",
  "We cannot accept anymore Russian interference. That goes against our core values.",
  "Peace with Russia, that's our aim.",
  "We hate Russia!")


sentiment.ai.score <- sentiment_score(text)

cooperative <- c("friend", "partner", "ally", "peace", "peaceful",  "friendly", "cooperative")
conflictual <- c("foe", "opponent", "enemy", "war", "aggressive", "hostile", "uncooperative")

my_categories <- list(coperative = "a reliable partner", conflictual = "a dangerous enemy")

sentiment_match(x = text, phrases = my_categories)

sentiments <- as.data.frame(data(default))


# Trying semantic polarity 
# as per https://github.com/BenWiseman/sentiment.ai/discussions/6

text <- c(
  "We continue to engage with Russia on a good basis.",
  "Russia'S approach is fundamentally different than ours.",
  "We cannot accept anymore Russian interference. That goes against our core values.",
  "Peace with Russia, that's our aim.",
  "We hate Russia!",
  "Russia is our best partner and neighbour!")

cooperative <- c("friend", "partner", "ally", "peace", "peaceful",  "friendly", "cooperative")
conflictual <- c("foe", "opponent", "enemy", "war", "aggressive", "hostile", "uncooperative")

my_categories <- list(cooperative = cooperative, conflictual = conflictual)

text_emb   <- embed_text(text)
coop_emb   <- embed_text(cooperative)
confl_emb   <- embed_text(conflictual)
all_emb    <- rbind(coop_emb, confl_emb)
result_raw <- cosine_match(text_emb, all_emb)

# avg similarity
# create table of categories
ref_dt <- data.table(phrase = c(cooperative, conflictual), 
                     topic   = c(rep("cooperative", length(cooperative)),
                                 rep("conflictual", length(conflictual))))
# join
result_raw <- ref_dt[result_raw, on = c(phrase = "reference")]


# summarise and viola!
result <- result_raw[, .(avg_similarity = mean(similarity)), by = c("target", "topic")]

# Polarity
resultw <- result %>% 
  pivot_wider(id_cols = target, names_from = topic, values_from = avg_similarity) %>% 
  mutate(polarity = cooperative - conflictual) %>% 
  arrange(desc(polarity))




# Human codes #####

# Human coding results
# Directly from the manually downloaded csvs

coder1 <- rbind(read_csv("./data/Validation/Coder1-JS/IndividualData1.csv") %>% select(-coder),
                read_csv("./data/Validation/Coder1-JS/IndividualData2.csv") %>% select(-coder),
                read_csv("./data/Validation/Coder1-JS/IndividualData3.csv") %>% select(-coder)) %>% 
  rename(coder1 = human)


coder2 <- rbind(read_csv("./data/Validation/Coder2-CR/IndividualData1.csv") %>% select(-coder),
                read_csv("./data/Validation/Coder2-CR/IndividualData2.csv") %>% select(-coder),
                read_csv("./data/Validation/Coder2-CR/IndividualData3.csv") %>% select(-coder)) %>% 
  rename(coder2 = human)

hc <- coder1 %>% 
  left_join(coder2 %>% select(sentence, coder2), by = "sentence")



# Score sentiment

sents <- sentiment_score(hc$sentence)
hc$sentai <- sents

plot(hc$lss.fit, hc$sentai)


hc$coder1fac <- hc$coder1 %>% 
  factor(levels = c("Conflictual", "Somewhat conflictual", "Neutral", "Somewhat cooperative", "Cooperative"))

ggplot(hc, aes(y = sentai, x = coder1fac))+
  geom_boxplot(fill = NA,outlier.shape = NA)+
  geom_jitter(width = .2, alpha = .2, shape = 16)+
  stat_summary(geom = "point", fun = mean, color = "red")+
  stat_summary(geom = "line", fun = mean, color = "red", aes(group =1))+
  labs(title = "Coder 1 vs. the machine",
       subtitle = "150 random sentences from UN General Assembly speeches",
       y= "\nEstimate of Sentiment.AI model\n",
       x= "Choices\nof coder 1 (JS)\n",
       caption = "Red markers indicate sentiment.ai means by coder choice.")+
  coord_flip()+
  theme_minimal()+
  theme(legend.position = "bottom",
        axis.text = element_text(color = "black"),
        text = element_text(family = "Dahrendorf"))

ggsave("./Plots/HumanValidation/Coder1vsMachine.SentimentAI.png", width = 20, height = 14, units = "cm")


# Coder 2
hc$coder2fac <- hc$coder2 %>% 
  factor(levels = c("Conflictual", "Somewhat conflictual", "Neutral", "Somewhat cooperative", "Cooperative"))

ggplot(hc, aes(y = sentai, x = coder2fac))+
  geom_boxplot(fill = NA,outlier.shape = NA)+
  geom_jitter(width = .2, alpha = .2, shape = 16)+
  stat_summary(geom = "point", fun = mean, color = "red")+
  stat_summary(geom = "line", fun = mean, color = "red", aes(group =1))+
  labs(title = "Coder 2 vs. the machine",
       subtitle = "150 random sentences from UN General Assembly speeches",
       y= "\nEstimate of sentiment.ai model\n",
       x= "Choices\nof coder 2 (CR)\n",
       caption = "Red markers indicate sentiment.ai means by coder choice.")+
  coord_flip()+
  theme_minimal()+
  theme(legend.position = "bottom",
        axis.text = element_text(color = "black"),
        text = element_text(family = "Dahrendorf"))

ggsave("./Plots/HumanValidation/Coder2vsMachine_sentimentAI.png", width = 20, height = 14, units = "cm")  




# Polarity - the SentAI approach ####

cooperative <- c("friend", "partner", "ally", "peace", "peaceful",  "friendly", "cooperative")
conflictual <- c("foe", "opponent", "enemy", "war", "aggressive", "hostile", "uncooperative")

my_categories <- list(cooperative = cooperative, conflictual = conflictual)

text_emb   <- embed_text(hc$sentence)
coop_emb   <- embed_text(cooperative)
confl_emb   <- embed_text(conflictual)
all_emb    <- rbind(coop_emb, confl_emb)
result_raw <- cosine_match(text_emb, all_emb)

# avg similarity
# create table of categories
ref_dt <- data.table(phrase = c(cooperative, conflictual), 
                     topic   = c(rep("cooperative", length(cooperative)),
                                 rep("conflictual", length(conflictual))))
# join
result_raw <- ref_dt[result_raw, on = c(phrase = "reference")]


# summarise and viola!
result <- result_raw[, .(avg_similarity = mean(similarity)), by = c("target", "topic")]

# Polarity
resultw <- result %>% 
  pivot_wider(id_cols = target, names_from = topic, values_from = avg_similarity) %>% 
  mutate(polarity = cooperative - conflictual)

# Write to human codes (order retained)
hc$sentai.pol <- resultw$polarity


# Plot ####
ggplot(hc, aes(x=lss.fit, y = sentai.pol))+
  geom_point()+
  stat_smooth(method = "lm")


hc$coder1fac <- hc$coder1 %>% 
  factor(levels = c("Conflictual", "Somewhat conflictual", "Neutral", "Somewhat cooperative", "Cooperative"))

ggplot(hc, aes(y = sentai.pol, x = coder1fac))+
  geom_boxplot(fill = NA,outlier.shape = NA)+
  geom_jitter(width = .2, alpha = .2, shape = 16)+
  stat_summary(geom = "point", fun = mean, color = "red")+
  stat_summary(geom = "line", fun = mean, color = "red", aes(group =1))+
  labs(title = "Coder 1 vs. the machine",
       subtitle = "150 random sentences from UN General Assembly speeches",
       y= "\nEstimate of Sentiment.AI model\nseeded with conflictual and cooperative terms",
       x= "Choices\nof coder 1 (JS)\n",
       caption = "Red markers indicate sentiment.ai means by coder choice.")+
  coord_flip()+
  theme_minimal()+
  theme(legend.position = "bottom",
        axis.text = element_text(color = "black"),
        text = element_text(family = "Dahrendorf"))

ggsave("./Plots/HumanValidation/Coder1vsMachine.SentimentAI-Polarity.png", width = 20, height = 14, units = "cm")


# Coder 2
hc$coder2fac <- hc$coder2 %>% 
  factor(levels = c("Conflictual", "Somewhat conflictual", "Neutral", "Somewhat cooperative", "Cooperative"))

ggplot(hc, aes(y = sentai.pol, x = coder2fac))+
  geom_boxplot(fill = NA,outlier.shape = NA)+
  geom_jitter(width = .2, alpha = .2, shape = 16)+
  stat_summary(geom = "point", fun = mean, color = "red")+
  stat_summary(geom = "line", fun = mean, color = "red", aes(group =1))+
  labs(title = "Coder 2 vs. the machine",
       subtitle = "150 random sentences from UN General Assembly speeches",
       y= "\nEstimate of Sentiment.AI model\nseeded with conflictual and cooperative terms",
       x= "Choices\nof coder 2 (CR)\n",
       caption = "Red markers indicate sentiment.ai means by coder choice.")+
  coord_flip()+
  theme_minimal()+
  theme(legend.position = "bottom",
        axis.text = element_text(color = "black"),
        text = element_text(family = "Dahrendorf"))

ggsave("./Plots/HumanValidation/Coder2vsMachine_sentimentAI-Polarity.png", width = 20, height = 14, units = "cm")  



