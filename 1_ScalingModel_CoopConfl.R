

# Packages #####
library(tidyverse)
library(quanteda)
library(countrycode)
library(LSX)
library(extrafont)


# Corpus ####
ungd <- read_rds("./Data/ungd.rds")


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
# Aim is to scale cooperative/conflictive language

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


# Illustrate text model

mod <- read_rds("./Data/lss_model.rds")

head(coef(mod),20)
tail(coef(mod),20)

df <- data.frame(words = names(mod$beta),
                 polarity = mod$beta,
                 freq = mod$frequency)

df.anchors <- df %>% 
  filter(words %in% c("friend", "partner", "ally", "peace", "peaceful",  "friendly", "cooperative",
                      "foe", "opponent", "enemy", "war", "aggressive", "hostile", "uncooperative")) %>% 
  mutate(group = "Beispiele für vom Forscher gesetzte Ankerbegriffe")

df.learned <- df %>% 
  filter(words %in% c("coexistence", "harmonious", "respectful", "partnership", "ties", "win-win", "state-to-state", "legitimate", "stable",
                      "security", "community", "global", "international", "national", "unemployed", "geopolitics", "misperceptions", "issue", "conducive", 
                      "militaristic", "provocative", "propaganda", "irresponsible", "warmongering", "unacceptable", "military", "tension", "regret", "stop")) %>% 
  mutate(group = "Beispiele für vom Algorithmus automatisch skalierte Begriffe")

df.highlight <- rbind(df.anchors, df.learned)
df$highlight$group <- fct_rev(factor(df.highlight$group, levels = c("Beispiele für vom Forscher gesetzte Ankerbegriffe", "Beispiele für vom Algorithmus automatisch skalierte Begriffe")))

# Plot textmodel illustration
set.seed(2147483647)
ggplot()+
  geom_text(data = df[sample(nrow(df), 6000, replace = F), ], alpha= .4, color = "grey60", aes(y=log(freq), x = polarity, label = words)) +
  geom_vline(xintercept = 0, linetype = "dashed")+
  geom_text(data = df.highlight, aes(y=log(freq), x = polarity, label = words, color = group), fontface = "bold")+
  scale_color_manual(values = c("#0380b5", "#9e3173"), name = "")+
  labs(title = "Illustration des Skalierungsmodells",
       subtitle = "Basierend auf dem Latent Semantic Scaling Algorithmus (Watanabe 2021),\nallen Reden in der UN-Vollversammlung 1970-2020 (Baturo, Mikhaylov, und Dasandi, 2017)\nund einer Zufallsstichprobe von 6.000 Worten",
       y = "Häufigkeit der Worte\nüber alle UN-Reden (logarithmiert)",
       x = "Polarität des Wortes\nauf der Skala zwischen konfliktbetonter und kooperativer Sprache")+
  theme_bw()+
  theme(legend.position = "bottom",
        axis.text = element_text(color = "black"),
        text = element_text(family = "Dahrendorf"),
        plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(),
        panel.grid = element_blank(),
        axis.text.x = element_text(size = 10))

ggsave("./Plots/IllustrateScalingModel.png", width = 26, height = 16, units = "cm")

