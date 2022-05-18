

# Packages #####
library(tidyverse)
library(extrafont)
library(countrycode)
library(ggrepel)
library(quanteda)
library(LSX)

# Grün: #619933
# Pink: #9e3173
# Blau: #0380b5

# Data ####
master <- read_rds("./Data/UNGA_RussiaLanguage.RDS")


# How often do EU member speak about Russia ? #####

freq.members <- 
  master %>% 
  filter(eu == 1) %>% 
  filter(country != "DEU") %>% 
  group_by(country, year) %>% 
  summarise(freq = sum(russ.ref)) %>% 
  mutate(group = "Übrige EU Mitglieder")

freq.deu <- 
  master %>% 
  filter(country == "DEU") %>% 
  group_by(country, year) %>% 
  summarise(freq = sum(russ.ref)) %>% 
  mutate(group = "Deutschland")

freq.eu <- 
  master %>% 
  filter(eu == 1) %>% 
  group_by(year) %>% 
  summarise(freq = mean(russ.ref)) %>% 
  mutate(group = "EU Durchschnitt") %>% 
  mutate(country = "EU") %>% 
  relocate(country)

freq.us <- 
  master %>% 
  filter(country == "USA") %>% 
  group_by(country, year) %>% 
  summarise(freq = sum(russ.ref)) %>% 
  mutate(group = "USA")

freq <- rbind(freq.deu, freq.us, freq.eu, freq.members)


ggplot(data = freq %>% filter(group != "Übrige EU Mitglieder"), 
       aes(x = year, y = freq, group = country, color = group))+
  geom_vline(xintercept = 1985, linetype = "dashed")+
  annotate(geom = "text", x=1985, label="Perestroika\n", y=20, 
            colour="grey20", angle=90, vjust = .5, hjust = .5, size = 3)+
  geom_vline(xintercept = 1991, linetype = "dashed")+
  annotate(geom = "text", x=1991, label="Auflösung UDSSR\n", y=20, 
            colour="grey20", angle=90, vjust = .5, hjust = .5, size = 3)+
  geom_vline(xintercept = 2014, linetype = "dashed")+
  annotate(geom = "text", x=2014, label="Krim Annexion\n", y=20, 
            colour="grey20", angle=90, vjust = .5, hjust = .5, size = 3)+
  geom_line(size = 1.2)+
  scale_color_manual(values = c("#619933", "#0380b5", "#9e3173"), name = "Redner: ")+
  scale_y_continuous(breaks = seq(0,40, 5), limits = c(0,40))+
  scale_x_continuous(breaks = seq(1970,2020, 5), expand = c(0.01,0.01))+
  labs(title = "Verweise auf Russland in Reden vor der UN-Vollversammlung",
       y = "Anzahl direkter Verweise\nauf Russland bzw. die UDSSR\n",
       x = "")+
  theme_bw()+
  theme(legend.position = "bottom",
        text = element_text(family = "Dahrendorf"),
        axis.text = element_text(color = "black"),
        legend.text=element_text(size=11),
        panel.grid.minor.x = element_blank())


# ggplot(data = freq %>% filter(group != "Übrige EU Mitglieder"), 
#        aes(x = group, y = freq, group = country, color = group))+
#   stat_summary(geom = "pointrange", fun.data = mean_cl_boot)+
#   scale_color_manual(values = c("#619933", "#0380b5", "#9e3173"), name = "Redner: ")


# How do EU members speak about Russia ? #####

lang.members <- 
  master %>% 
  filter(year >= 2014) %>% 
  filter(eu == 1) %>% 
  filter(country != "DEU") %>% 
  group_by(country) %>% 
  summarise(coop = mean(coop.language, na.rm = T)) %>% 
  mutate(group = "EU Mitglieder")

lang.deu <- 
  master %>% 
  filter(year >= 2014) %>% 
  filter(country == "DEU") %>% 
  group_by(country) %>% 
  summarise(coop = mean(coop.language, na.rm = T)) %>% 
  mutate(group = "EU Mitglieder")

lang.eu <- 
  master %>% 
  filter(year >= 2014) %>% 
  filter(eu == 1) %>% 
  summarise(coop = mean(coop.language, na.rm = T)) %>% 
  mutate(group = "EU Mitglieder") %>% 
  mutate(country = "Durchschnitt EU") %>% 
  relocate(country)

lang.us <- 
  master %>% 
  filter(year >= 2014) %>% 
  filter(country == "USA") %>% 
  group_by(country) %>% 
  summarise(coop = mean(coop.language, na.rm = T)) %>% 
  mutate(group = "Vergleichswerte")

lang <- rbind(lang.deu, lang.us, lang.eu, lang.members,
              data.frame(country = "Durchschnitt UN", coop = mean(master$coop[master$year >= 2014], na.rm = T), group = "Vergleichswerte"),
              data.frame(country = "Durchschnitt NATO", coop = mean(master$coop[master$year >= 2014  & master$nato == 1], na.rm = T), group = "Vergleichswerte"))


# German country names

lang$country2 <- countrycode(lang$country, "iso3c", "country.name.de")
lang$country2[lang$country == "Durchschnitt EU"] <- "Durchschnitt EU"
lang$country2[lang$country == "Durchschnitt NATO"] <- "Durchschnitt NATO"
lang$country2[lang$country == "Durchschnitt UN"] <- "Durchschnitt UN"


# Order countries meaningfully
order <- lang %>% 
  arrange(desc(coop)) %>% 
  filter(!(group %in% c("Vergleichswerte"))) %>% 
  select(country2)
order <- c(order$country2, c("Durchschnitt UN", "Durchschnitt NATO", "Vereinigte Staaten"))

lang$country3 <- factor(lang$country2, levels = order)
lang <- lang %>% filter(!is.na(coop))

lang$vfill <- ifelse(lang$group == "Vergleichswerte", T, F)
lang$valpha <- ifelse(lang$country2 == "Deutschland" | lang$country2 == "Durchschnitt EU", T, F)

# Plot to prettify 
ggplot(lang, aes(x=fct_rev(country3), y = coop))+
  geom_col(aes(fill = vfill, alpha = valpha), width = .8)+
  scale_fill_manual(values = c("#0380b5", "#9e3173"))+
  scale_alpha_manual(values = c(.6, 1))+
  facet_grid(group~., scales = "free_y", space = "free")+
  labs(title = "Kooperative Sprache gegenüber Russland?",
       subtitle = "In Reden vor der Vollversammlung der Vereinten Nationen, 2014-2020",
       x = "",
       y = "Gewichtung der verwendeten Worte rund um Nennungen von Russland\nentlang einer automatisierten Skalierung zwischen Konflikt und Kooperation",
       caption = "Schätzwerte eines latent semantic scaling Modells (Watanabe, 2021).\nGemessen wird die semantische Ähnlichkeit von 40 Worten rund um Nennungen von Russland zu zwei konträren Begriffsgruppen.\nKooperative Sprache (= positive Werte) wird in der Begriffsgruppe \"friend\", \"partner\", \"ally\", \"peace\", \"peaceful\",  \"friendly\", \"cooperative\" verankert.\nKonfliktbetonte Sprache (= negative Werte) wird relativ zu entsprechenden Antonymen erfasst: \"foe\", \"opponent\", \"enemy\", \"war\", \"aggressive\", \"hostile\", \"uncooperative\".\n\nTextdaten aus dem UNGD Corpus von Baturo, Mikhaylov, und Dasandi (2017).\n\nAutor: @ChRauh")+
  coord_flip()+
  theme_bw()+
  theme(legend.position = "none",
        text = element_text(family = "Dahrendorf"),
        axis.text = element_text(color = "black"),
        plot.title = element_text(face = "bold"),
        legend.text=element_text(size=11),
        strip.background = element_blank(),
        strip.text.y = element_blank())

ggsave("./Plots/2_EU_Russia_Lang_PostCrimea.png", width = 28, height = 18, units = "cm")


# Before/after Crimea annexion ####

ba <- master %>% 
  filter(year >= 2004) %>% 
  filter(eu == 1) %>% 
  mutate(period = ifelse(year < 2014, 
                         "Vor Annexion\nder Krim (1991-2013)", 
                         "Nach Annexion\nder Krim (2014-2020)")) %>% 
  group_by(country, period) %>% 
  summarise(coop.language = mean(coop.language, na.rm = T))
  
baeu <- master %>% 
  filter(year >= 2004) %>% 
  filter(eu == 1) %>% 
  mutate(period = ifelse(year < 2014, 
                         "Vor Annexion\nder Krim (1991-2013)", 
                         "Nach Annexion\nder Krim (2014-2020)")) %>% 
  group_by(period) %>% 
  summarise(coop.language = mean(coop.language, na.rm = T)) %>% 
  mutate(country = "Durchschnitt EU") %>% 
  relocate(country)

ba <- rbind(ba, baeu)
ba$period2 <- factor(ba$period, levels =c("Vor Annexion\nder Krim (1991-2013)", 
                                          "Nach Annexion\nder Krim (2014-2020)"))
ba$colgroup <- ifelse(ba$country == "DEU", "Deutschland",
                      ifelse(ba$country == "Durchschnitt EU", "Durchschnitt EU","Andere EU Mitgliedsstaaten"))


ggplot(ba, aes(x=period2, y = coop.language, group = country, color = colgroup))+
  geom_point()+
  geom_line()+
  theme_minimal()


ba2 <- ba %>% 
  mutate(period = ifelse(period == "Vor Annexion\nder Krim (1991-2013)", "before", "after")) %>% 
  select(c(country, coop.language, period)) %>% 
  pivot_wider(id_cols = country, names_from = period, values_from = coop.language) %>% 
  mutate(x1 = 0,
         x2 = 1) %>% 
  filter(!is.na(after))
ba2$colgroup <- ifelse(ba2$country == "DEU", "Deutschland",
                      ifelse(ba2$country == "Durchschnitt EU", "Durchschnitt EU","Andere EU Mitgliedsstaaten")) %>% 
  factor(levels = c("Deutschland", "Durchschnitt EU", "Andere EU Mitgliedsstaaten"))

ba2$algroup <- ifelse(ba2$country == "DEU" | ba2$country == "Durchschnitt EU", T, F)

ba2$country2 <- countrycode(ba2$country, "iso3c", "country.name.de")
ba2$country2[is.na(ba2$country2)] <- "Durchschnitt EU"


ggplot(ba2, aes(x1, before, xend = x2, yend = after, label = country2, color = colgroup)) +
  geom_segment(size = 1, aes(alpha = algroup)) +
  scale_alpha_manual(values = c(.5,1))+
  guides(color = "none") +
  theme(axis.title.x = element_blank()) +
  geom_text_repel(
    nudge_x = -0.2, direction = "y", hjust = "right", fontface = "bold", family = "Dahrendorf"
  ) +
  geom_text_repel(
    aes(x2, after), nudge_x = 0.1, direction = "y", hjust = "left", fontface = "bold", family = "Dahrendorf"
  )+
  scale_x_continuous(
    breaks = 0:1, labels = c("Vor Annexion\nder Krim\n(2004-2013)", "Nach Annexion\nder Krim\n(2014-2020)"),
    # limits = c(-.3,1.3), 
    expand = expansion(mult = 0.5)
  )+
  geom_vline(xintercept = c(0,1), color ="black")+
  scale_color_manual(values=c(c("#9e3173", "#0380b5", "grey70")))+
  labs(title = "Kooperative Sprache gegenüber Russland?",
       subtitle = "In Reden von EU-Mitgliedsstaaten vor der Vollversammlung der Vereinten Nationen",
       x = "",
       y = "",
       # y = "Gewichtung der verwendeten Worte rund um Nennungen von Russland\nentlang einer automatisierten Skalierung zwischen Konflikt und Kooperation",
       caption = "Schätzwerte eines latent semantic scaling Modells (Watanabe, 2021).\nGemessen wird die semantische Ähnlichkeit von 40 Worten rund um Nennungen von Russland zu zwei konträren Begriffsgruppen.\nKooperative Sprache (= positive Werte) wird in der Begriffsgruppe \"friend\", \"partner\", \"ally\", \"peace\", \"peaceful\",  \"friendly\", \"cooperative\" verankert.\nKonfliktbetonte Sprache (= negative Werte) wird relativ zu entsprechenden Antonymen erfasst: \"foe\", \"opponent\", \"enemy\", \"war\", \"aggressive\", \"hostile\", \"uncooperative\".\n\nTextdaten aus dem UNGD Corpus von Baturo, Mikhaylov, und Dasandi (2017).\n\nAutor: @ChRauh")+
  theme_minimal()+
  theme(legend.position = "none",
        axis.text = element_text(color = "black"),
        text = element_text(family = "Dahrendorf"),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
        plot.subtitle = element_text(hjust = 0.5),
        panel.grid = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(face = "bold", size = 10))

ggsave("./Plots/1_EU_Russia_Lang_PrePostCrimea.png", width = 26, height = 20, units = "cm")





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
set.seed(12345678910)
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

ggsave("./Plots/3_IllustrateLanguageScaling.png", width = 26, height = 16, units = "cm")

