# ##################################################
# Exploring EU agency in SRL data
# Test whether there is a basis for this paper idea
# Christian Rauh (10.05.2022)
####################################################

# Packages #####
library(tidyverse)


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



# Identify EU-related sentences ####

# RegEx / Dictionary (to be run case sensitive)
# "(European Union)|(European Communit(y|ies))|(EU([^A-Za-z]|$))|(EC([^A-Za-z]|$))|(E\\.U\\.)|(E\\.C\\.)"
eu.dict <- paste("(European Union)", 
                 "(([^A-Za-z]|^)(EU)([^A-Za-z]|$))",
                 "(([^A-Za-z]|^)(E\\.U\\.)([^A-Za-z]|$))",
                 "(European Communit(y|ies))",
                 "(([^A-Za-z]|^)(EC)([^A-Za-z]|$))",
                 "(([^A-Za-z]|^)(E\\.C\\.)([^A-Za-z]|$))",
                 "(European Economic Communit(y|ies))",
                 "(([^A-Za-z]|^)(EEC)([^A-Za-z]|$))",
                 "(([^A-Za-z]|^)(E\\.E\\.C\\.)([^A-Za-z]|$))", 
                 sep = "|")

# EU as an agent
eu.ag <- srl %>% 
  filter(str_detect(agent, eu.dict))
 
# Filter obviously faulty cases
# To be done seriously if this should be a paper
eu.ag <- eu.ag %>% 
  filter(!(str_detect(agent, "(members|countries|states|partners) (of|in) the"))) %>% # about its MS only
  filter(!(str_detect(agent, "accession to|membership"))) %>% 
  filter(!(str_detect(agent, "(M|m)ember (S|s)tates")))



# Benchmark 1: P5 states ####

# USA 
us.dict <- paste("(United States)", 
                 "(([^A-Za-z]|^)(US)([^A-Za-z]|$))",
                 "(([^A-Za-z]|^)(U\\.S\\.)([^A-Za-z]|$))", 
                 sep = "|")
us.ag <- srl %>% 
  filter(str_detect(agent, us.dict))

# UK
uk.dict <- paste("(([^A-Za-z]|^)(United Kingdom)", 
                 "(([^A-Za-z]|^)(UK)([^A-Za-z]|$))",
                 "(Britain)", 
                 "(British (government|state))",
                 sep = "|")
uk.ag <- srl %>% 
  filter(str_detect(agent, uk.dict))

# France
fr.dict <- paste("(([^A-Za-z]|^)(France)",
                 "(French (government|state))",
                 sep = "|")
fr.ag <- srl %>% 
  filter(str_detect(agent, fr.dict))

# China
ch.dict <- paste("(([^A-Za-z]|^)(China)",
                 "(Chinese (government|state))",
                 sep = "|")
ch.ag <- srl %>% 
  filter(str_detect(agent, ch.dict))

# Russia
rus.dict <- paste("(([^A-Za-z]|^)(Russia)",
                 "(Russian (government|state))",
                 "(Soviet Union)",
                 "(Soviet (government|state))",
                 sep = "|")
rus.ag <- srl %>% 
  filter(str_detect(agent, rus.dict))


# Benchmark 2: Other IOs ####

# WTO, IMF, ASEAN, ECOWAS, African Union


# Add agency information to sentence level data ####
# Validation against mere presence of respective dictionary could be done here 

ungd$eu.agent <- ifelse(ungd$id %in% eu.ag$id, TRUE, FALSE)
ungd$us.agent <- ifelse(ungd$id %in% us.ag$id, TRUE, FALSE)
ungd$uk.agent <- ifelse(ungd$id %in% uk.ag$id, TRUE, FALSE)
ungd$fr.agent <- ifelse(ungd$id %in% fr.ag$id, TRUE, FALSE)
ungd$ch.agent <- ifelse(ungd$id %in% ch.ag$id, TRUE, FALSE)
ungd$rus.agent <- ifelse(ungd$id %in% rus.ag$id, TRUE, FALSE)


# Aggregate to speech level
speeches <- ungd %>% 
  group_by(country, state, year) %>% 
  summarise_at(vars(eu.agent:rus.agent), sum)


# Annual trends ####

annual <- speeches %>% 
  mutate_at(vars(eu.agent:rus.agent), as.logical) %>% 
  group_by(year) %>% 
  summarise_at(vars(eu.agent:rus.agent), mean) %>% 
  pivot_longer(2:7) %>% 
  mutate(name = str_remove(name, fixed(".agent")) %>% toupper())

annual$actor <- annual$name
annual$actor[annual$actor == "CH"] <- "China"
annual$actor[annual$actor == "RUS"] <- "Russia/SU"
annual$actor[annual$actor == "UK"] <- "United Kingdom"
annual$actor[annual$actor == "US"] <- "USA"
annual$actor[annual$actor == "FR"] <- "France"

annual$actor <- factor(annual$actor, levels = c("EU", "USA", "Russia/SU", "China", "France", "United Kingdom"))

annual$hl <- annual$actor == "EU"


ggplot(annual, aes(x = year, y = value, color = actor, linetype = hl)) + 
  # geom_point(size = .8, alpha = .6)+
  stat_smooth(method = "loess", se = F, span = .5)+
  scale_linetype_manual(values = c("dotted", "solid"))+
  guides(linetype = "none")+
  theme_bw()

# Mhhh . the China pattern seems to invalidate the method?
# What is going on?


ungd$ch.pres <- str_detect(ungd$sentence, ch.dict)
sum(ungd$ch.agent)
table(ungd$ch.agent, ungd$ch.pres, useNA = "ifany")

test <- ungd %>% 
  filter(ch.pres & !ch.agent)

i <- sample(1:nrow(test), 1)
test$sentence[i]

test2 <- srl[srl$id == test$id[i], ]

# Still encoding issues in there!
# The Peopleâ€ ™ s Republic of China
# And nominalizations
# With regard to \n Chinaâ€ ™ s disputes 
# IndoChina and Indo-China

china <- ungd %>% 
  select(year, ch.agent, ch.pres) %>% 
  group_by(year) %>% 
  summarise_at(vars(ch.agent, ch.pres), mean) %>% 
  pivot_longer(cols = c(2,3))

ggplot(china, aes(x = year, y = value, color = name))+
  geom_line()

# Not an artefact of the SRL!
# Agent share remains roughly constant


# Which countries speak recognize EU agency? ####

ctr <- speeches %>%
  filter(year >= 1992) %>% 
  group_by(country) %>% 
  summarise(eu = mean(as.logical(eu.agent))) %>% 
  arrange(desc(eu))

library(rworldmap)
wmap <- getMap(resolution = "low")
wmap_df <- fortify(wmap, region = "ISO3")
wmap_df <- left_join(wmap_df, ctr, by = c('id'='country')) 

ggplot(data=wmap_df) +
  coord_map(xlim = c(-180, 180), ylim = c(-60, 75))  +
  geom_polygon(aes(x = long, y = lat, fill=eu, group = group), color="white", size = .1) +
  # scale_fill_gradient(low = "white",
  #                      high = "darkblue",
  #                      na.value = "white",
  #                      limits = c(0,1))+
  scale_fill_gradient2(low = "grey90",
                       high = "darkblue",
                       mid = "blue",
                       midpoint = .5, 
                       na.value = "grey90",
                       limits = c(0,1))+
  theme_void()





