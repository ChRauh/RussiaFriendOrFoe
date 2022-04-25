# Project:    Russia- Friend or Foe?
# Task:       Analyse results of SLR and LSS results
# Author:     Ch Rauh (April 6 2022)


# Packages
library(tidyverse)
library(Hmisc)
library(extrafont)
library(scales)
library(rworldmap)
library(ggrepel)
library(countrycode)


# Coded UN sentences with meta data ###
ungd <- read_rds("./Data/ungd_sentences_RussiaCodes.rds") 


# Do conclusions on conflictuality differ across semantic roles? ###

countrydf <- ungd %>%  filter(country == "AFG")

countrydf %>% 
  group_by(russia.pres, russia.agent, russia.patient) %>% 
  summarize(n=n())

test <- rbind(mean_cl_boot(countrydf$lss.fit[countrydf$russia.pres]),
              mean_cl_boot(countrydf$lss.fit[countrydf$russia.agent]),
              mean_cl_boot(countrydf$lss.fit[countrydf$russia.patient])) %>% 
  mutate(type = c("All Russia sentences", "Russia as agent", "Russia as patient"))

ggplot(test, aes(x = type, y = y, ymin = ymin, ymax=ymax, color = type))+
  geom_pointrange()

# For the US, they do - for the rest it's noise


# Add meta data from speech level ####
speeches <- read_rds("./Data/ungd.rds") %>% 
  select(-c(text, year, session, country, state))
ungd <- ungd %>% 
  left_join(speeches, by = "doc_id")
rm(speeches)


# Analysis over time ####

# Events:
# https://www.planet-schule.de/wissenspool/planspiel-atomkrieg/inhalt/hintergrund/zeitstrahl-kalter-krieg.html
# https://zdfheute-stories-scroll.zdf.de/putin-kriege-ukraine/index.html
# https://de.wikipedia.org/wiki/Liste_der_Milit%C3%A4roperationen_Russlands_und_der_Sowjetunion#20._Jahrhundert_Russische_Sowjetrepublik/Sowjetunion_(1917%E2%80%931991)

df <- ungd %>% filter(eu == 1 & russia.agent)

ggplot(df, aes(x = year, y = lss.fit, label = country)) +
  geom_hline(yintercept = 0, linetype = "dashed")+
  geom_vline(xintercept = c(1979.5, 1994.5, 1999.5, 2008.5, 2013.5, 2015.5), linetype = "dashed")+
  geom_vline(xintercept = 1991.5)+
  stat_smooth(method = "loess", span = .3, color = "black", fill = NA, size = 0.5, linetype = "dotted")+
  geom_jitter(width = .2, alpha = .8, shape = 16, size = 2, aes(color = lss.fit))+
  scale_colour_gradient2(low = "red",
                         high = "green",
                         mid = "grey70",
                         midpoint = 0, 
                         limits = c(-1.5,1.5),
                         oob=squish)+
  annotate(geom = "text", 
           label = c("Afghanistan", "Ende der\nSowjetunion", "Tschetschenien I", "Tschetschenien II", "Georgien", "Krim Annexion", "Syrien"), 
           x = c(1979, 1990.5, 1994, 1999, 2008, 2013, 2015), 
           y = -3.7, angle = 90, hjust = 0, vjust = 0.4, size = 3, family = "Dahrendorf")+
  scale_x_continuous(breaks = seq(1970, 2020, 5))+
  coord_cartesian(ylim = c(-3.5, 3.5)) +
  labs(title = "Wie EU-Staaten vor den Vereinten Nationen über Russland gesprochen haben",
       caption = "Die Datenpunkte repräsentieren die Sätze aus Reden von EU Staaten vor der UN Vollversammlung, die Russland bzw. die Sowjetunion als handelnden Akteur benennen.\nDer Wert auf der y-Achse zeigt an, ob die dabei verwendete Sprache eher konfliktbetont oder eher kooperativ ist.\nTextdaten von Mikhaylov et al. (2017). Role labelling, Skalierung und Plot: @ChRauh.",
       y = "Betonung von\nKonflikt vs. Kooperation\n",
       x = "")+
  theme_bw()+
  theme(legend.position = "none",
        plot.title = element_text(face = "bold"),
        text = element_text(family = "Dahrendorf"),
        axis.text = element_text(color = "black"),
        panel.grid = element_blank())

ggsave("./Plots/Analysis_SRL/OverTime.png", width = 26.5, height = 15, units = "cm")


# Post Crimea map ####

# Country data of signals on Russia
df2 <- df %>% 
  filter(year >= 2014) %>% 
  group_by(country) %>% 
  summarise(lss = mean(lss.fit, na.rm = T),
            freq = n())

# Get world map
wmap <- getMap(resolution = "low")

# Vector of EU 28 countries + EFTA
EU28 <- c("Austria","Belgium","Bulgaria","Croatia","Cyprus",
          "Czech Rep.","Denmark","Estonia","Finland","France",
          "Germany","Greece","Hungary","Ireland","Italy","Latvia",
          "Lithuania","Luxembourg","Malta","Netherlands","Poland",
          "Portugal","Romania","Slovakia","Slovenia","Spain",
          "Sweden","United Kingdom", "Norway", "Switzerland", "Liechtenstein")

# Select only the index of EU 28 in wmap
indEU <- which(wmap$NAME%in%EU28)

# Select polygons along EU28 index
eumap <- lapply(indEU, function(i){
  df <- data.frame(wmap@polygons[[i]]@Polygons[[1]]@coords)
  df$region <- as.character(wmap$NAME[i])
  df$id <- as.character(wmap$ISO_A3[i])
  colnames(df) <- list("long", "lat", "region", "id")
  return(df)})
eumap <- do.call("rbind", eumap)

eu28_iso <- unique(eumap$id)

# small edits
# wmap <- spTransform(wmap, CRS("+proj=robin")) # reproject
# wmap <-   subset(wmap, !(NAME %like% "Antar")) # Remove Antarctica

# Join country/time data with the world map
wmap_df <- fortify(wmap, region = "ISO3")
wmap_df <- wmap_df[wmap_df$id%in%eu28_iso, ]
wmap_df <- left_join(wmap_df, df2, by = c('id'='country')) 

# Plot the map
ggplot(data=wmap_df) +
  geom_polygon(aes(x = long, y = lat, fill=lss, alpha = freq, group = group), color="gray60") +
  scale_fill_gradient2(low = "red",
                         high = "green",
                         mid = "grey70",
                         midpoint = 0, 
                         na.value = "white",
                         limits = c(-1.5,1.5),
                         oob=squish)+
  scale_alpha_continuous(range = c(.7,1))+
  theme_void() +
  labs(title = "Which countries did European Commission\npress releases refer to between 1985 and 2018?") +
  labs(subtitle = "Year: {round(frame_time, 0)}\n")+
  labs(caption = "Coverage: EU28 + Norway & Switzerland (2019 borders)\nData: Full texts of all 43,760 English EC press releases 1985-2018  \nAuthor: Christian Rauh (@ChRauh; www.christian-rauh.eu)  \n") +
  coord_map(xlim = c(-20, 59),  ylim = c(32, 70.9))+
  theme( legend.position = "none", 
         legend.direction = "vertical", 
         legend.title.align = .5,
         legend.key.size = unit(1.3, "cm"),
         legend.title=element_text(size=17), 
         legend.text=element_text(size=17),
         text = element_text(family = "Dahrendorf"))

df2$country2 <- df2$country %>% 
  countrycode(origin = "iso3c", destination = "country.name.de")

ggplot(df2, aes(x=lss, y = reorder(country,lss), alpha = freq, fill = lss))+
  geom_col()+
  scale_fill_gradient2(low = "red",
                       high = "green",
                       mid = "grey70",
                       midpoint = 0, 
                       na.value = "white",
                       limits = c(-1.2,1.2),
                       oob=squish)+
  scale_alpha_continuous(range = c(.3,1))+
  geom_text(aes(label = country,
                hjust = ifelse(lss < 0, 1.25, -0.25)))+
  theme_void()+
  theme(legend.position = "none",
        plot.title = element_text(face = "bold"),
        text = element_text(family = "Dahrendorf"),
        axis.text = element_text(color = "black"),
        axis.text.y = element_blank(),
        panel.grid = element_blank())




# Plot EU Block since 2014

ggplot(df2, aes(y=lss, x = freq, alpha = freq, color = lss, label = country2))+
  geom_hline(yintercept = 0, linetype = "dashed")+
  geom_label_repel(family = "Dahrendorf", fontface = "bold")+
  scale_color_gradient2(low = "red",
                       high = "green",
                       mid = "grey70",
                       midpoint = 0, 
                       na.value = "white",
                       limits = c(-1.2,1.2),
                       oob=squish)+
  scale_alpha_continuous(range = c(.8,1))+
  labs(title = "Russland in UN Reden von EU-Staaten (2014-2020)",
       x = "\nHäufigkeit von Sätzen\ndie Russland als handelnden Akteur benennen\n",
       y = "Durchschnittliche Betonung von\nKonflikt vs. Kooperation\n",
       caption = "Textdaten von Mikhaylov et al. (2017). Role Labelling, Skalierung und Plot: @ChRauh.") +
  theme_bw()+
  theme(legend.position = "none",
        plot.title = element_text(face = "bold"),
        text = element_text(family = "Dahrendorf"),
        axis.text = element_text(color = "black"))
  
ggsave("./Plots/Analysis_SRL/PostCrimeaCountries.png", width = 26.5, height = 15, units = "cm")


# Example Sentences ####

example <- ungd %>% 
  filter(country == "SVK") %>% 
  filter(russia.agent) %>% 
  filter(year >= 2014)

example$sentence
example$year