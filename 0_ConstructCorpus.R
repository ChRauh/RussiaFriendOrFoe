

# Packages #####
library(tidyverse)
library(readtext)
library(quanteda)
library(LSX)
library(igoR)
library(countrycode)


# Read txt ###

# File path to UNGDC unzipped data archive
DATA_DIR <- "D:/WZB-Nextcloud/WZB_CR/Datensaetze/UNGA/Original/UNGDC_1970-2020/TXT" 

# Read txt to data frame
# N should be 8481 as per documentation
ungd <- readtext(paste0(DATA_DIR, "/*"), 
                       docvarsfrom = "filenames", 
                       dvsep="_", 
                       docvarnames = c("country", "session", "year"), 
                 encoding = "UTF-8")

ungd <- ungd %>% as.data.frame()

# Clean text
ungd$text <- ungd$text %>% str_replace_all("\\s+", " ")


# IGO membership

# Based on COW (which technically only goes through 2014), through igoR package

igom <- state_year_format3 %>% 
  filter(year >= min(ungd$year)) %>% 
  select(ccode, year, state,
         eu, eec, nato, oecd, opec) %>%
  mutate(across(4:8, function(x){ifelse(x==1, 1, 0)})) %>% # Mutate IO membership so as toc apture full mebership only
  mutate(eu = eu + eec) %>% 
  mutate(country = countrycode(ccode, "cown", "iso3c"))

# table(igom$eu, igom$year)

# Correct some non-m,acthing ISOs
# unique(igom$state[is.na(igom$country)])
igom$country[igom$state == "egermany"] <- "DDR"
igom$country[igom$state == "wgermany"] <- "DEU"
igom$country[igom$state == "czechoslovakia"] <- "CSK"
igom$country[igom$state == "yugoslaviaserb"] <- "YUG"
igom$country[igom$state == "syemen"] <- "YMD"
igom$country[igom$state == "nyemen"] <- "YEM"
igom$country[igom$state == "svietnam"] <- "VNM"
igom$country[igom$state == "kosovo"] <- "XKX" # Temporary placeholder

# Extrapolate to 2020 (strong assumption)
max(igom$year)
igom <- rbind(igom,
              igom %>% filter(year == 2014) %>% mutate(year = 2015),
              igom %>% filter(year == 2014) %>% mutate(year = 2016),
              igom %>% filter(year == 2014) %>% mutate(year = 2017),
              igom %>% filter(year == 2014) %>% mutate(year = 2018),
              igom %>% filter(year == 2014) %>% mutate(year = 2019),
              igom %>% filter(year == 2014) %>% mutate(year = 2020))
igom$eu[igom$country == "GBR" & igom$year > 2016] <- 0 # Breyit

# Merge IO membership with speech data ####
ungd <- ungd %>% 
  left_join(igom, by = c("country", "year"))


# Export data
write_rds(ungd, "./Data/ungd.rds")





# # Russia references ####
# 
# ungd$russ.ref <- str_count(tolower(ungd$text), "(russia[a-z]*)|(soviet[a-z]*)")
# sum(ungd$russ.ref > 1)
# 
# # hist(ungd$russ.ref)
# # russ.ref.eu <- ungd %>% 
# #   filter(eu == 1) %>% 
# #   group_by(year, country) %>% 
# #   summarise(russ = sum(russ.ref))
# 
# # Russia text ####
# 
# # ID for merging
# ungd$id <- 1:nrow(ungd)
# 
# # Quanteda corpus
# ungd_corpus <- corpus(ungd$text, docvars = ungd[ ,c("id", "session", "country")])
# 
# # KWIC
# 
# russ.text <-
#   kwic(
#     ungd_corpus,
#     pattern = "(russia[a-z]*)|(soviet[a-z]*)",
#     window = 30,
#     valuetype = "regex",
#     separator = " ",
#     case_insensitive = TRUE
#   )


