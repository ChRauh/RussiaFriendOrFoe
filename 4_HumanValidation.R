####################################################################
# Project:  Russia - friend or foe?
# Task:     Compare LSS coding of conflictual/cooperative language
#           to human codes of 150 random UNGA sentences
# Author:   Christian Rauh 28.03.2022
####################################################################


# Packages ####
library(tidyverse)
library(ggdist)
library(extrafont)


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


# Intercoder reliability ####

# Percentage agreement, 5 categories
(sum(hc$coder1 == hc$coder2)/nrow(hc))*100

# Percentage agreement in tendency
# Removing the 'somewhat' categories
hc$coder1redux <- hc$coder1 %>% 
  str_remove("Somewhat ") %>% 
  tolower() %>% 
  str_trim()
hc$coder2redux <- hc$coder2 %>% 
  str_remove("Somewhat ") %>% 
  tolower() %>% 
  str_trim()
(sum(hc$coder1redux == hc$coder2redux)/nrow(hc))*100


# Plot

interrater <- hc %>% 
  select(coder1, coder2) %>% 
  group_by(coder1, coder2) %>% 
  summarise(freq = n())

interrater$coder1 <- interrater$coder1 %>% 
  factor(levels = c("Conflictual", "Somewhat conflictual", "Neutral", "Somewhat cooperative", "Cooperative"))

interrater$coder2 <- interrater$coder2 %>% 
  factor(levels = c("Conflictual", "Somewhat conflictual", "Neutral", "Somewhat cooperative", "Cooperative"))


ggplot(interrater, aes(x = coder1, y = coder2,  alpha = freq))+
  # geom_abline(intercept = 0, slope = 1)+
  geom_tile(fill = "#0380b5")+
  labs(title = "Intercoder reliability",
       subtitle = "150 random sentences from UN General Assembly speeches\nFull 5-point scale for conflictual/cooperative language",
       x = "\nChoices of Coder 1 (JS)",
       y = "Choices of Coder 2 (CR)\n",
       caption = paste0("Percentage agreement on full 5-point scale: ", (sum(hc$coder1 == hc$coder2)/nrow(hc))*100,"% \n",
                        "Percentage agreement on reduced 3-point scale: ", round((sum(hc$coder1redux == hc$coder2redux)/nrow(hc))*100, 0), "% "),
       alpha = "Frequency: ")+
  theme_minimal()+
  theme(legend.position = "bottom",
        axis.text = element_text(color = "black"),
        text = element_text(family = "Dahrendorf"),
        panel.grid = element_blank())

ggsave("./Plots/HumanValidation/IntercoderReliability.png", width = 20, height = 20, units = "cm")


# Human vs machine (LSS) ####

# Coder 1
hc$coder1fac <- hc$coder1 %>% 
  factor(levels = c("Conflictual", "Somewhat conflictual", "Neutral", "Somewhat cooperative", "Cooperative"))

ggplot(hc, aes(y = lss.fit, x = coder1fac))+
  geom_boxplot(fill = NA,outlier.shape = NA)+
  geom_jitter(width = .2, alpha = .2, shape = 16)+
  stat_summary(geom = "point", fun = mean, color = "red")+
  stat_summary(geom = "line", fun = mean, color = "red", aes(group =1))+
  labs(title = "Coder 1 vs. the machine",
       subtitle = "150 random sentences from UN General Assembly speeches",
       y= "\nEstimate of LSS model\nfor conflictual vs. cooperative language\n",
       x= "Choices\nof coder 1 (JS)\n",
       caption = "Red markers indicate LSS means by coder choice.")+
  coord_flip()+
  theme_minimal()+
  theme(legend.position = "bottom",
        axis.text = element_text(color = "black"),
        text = element_text(family = "Dahrendorf"))

ggsave("./Plots/HumanValidation/Coder1vsMachine.png", width = 20, height = 14, units = "cm")


# Coder 2
hc$coder2fac <- hc$coder2 %>% 
  factor(levels = c("Conflictual", "Somewhat conflictual", "Neutral", "Somewhat cooperative", "Cooperative"))

ggplot(hc, aes(y = lss.fit, x = coder2fac))+
  geom_boxplot(fill = NA,outlier.shape = NA)+
  geom_jitter(width = .2, alpha = .2, shape = 16)+
  stat_summary(geom = "point", fun = mean, color = "red")+
  stat_summary(geom = "line", fun = mean, color = "red", aes(group =1))+
  labs(title = "Coder 2 vs. the machine",
       subtitle = "150 random sentences from UN General Assembly speeches",
       y= "\nEstimate of LSS model\nfor conflictual vs. cooperative language\n",
       x= "Choices\nof coder 2 (CR)\n",
       caption = "Red markers indicate LSS means by coder choice.")+
  coord_flip()+
  theme_minimal()+
  theme(legend.position = "bottom",
        axis.text = element_text(color = "black"),
        text = element_text(family = "Dahrendorf"))

ggsave("./Plots/HumanValidation/Coder2vsMachine.png", width = 20, height = 14, units = "cm")  


# Average coder

hc$coder1num <- hc$coder1 %>% 
  recode("Conflictual" = -1,
        "Somewhat conflictual" =  -.5,
        "Neutral" = 0,
        "Somewhat cooperative" =  .5,
        "Cooperative" = 1)
hc$coder2num <- hc$coder2 %>% 
  recode("Conflictual" = -1,
         "Somewhat conflictual" =  -.5,
         "Neutral" = 0,
         "Somewhat cooperative" =  .5,
         "Cooperative" = 1)

hc$coderMean <- (hc$coder1num+hc$coder2num)/2
unique(hc$coderMean) %>% sort()

ggplot(hc, aes(y = lss.fit, x = factor(coderMean)))+
  geom_boxplot(fill = NA,outlier.shape = NA)+
  geom_jitter(width = .2, alpha = .2, shape = 16)+
  stat_summary(geom = "point", fun = mean, color = "red")+
  stat_summary(geom = "line", fun = mean, color = "red", aes(group =1))+
  labs(title = "The average coder vs. the machine",
       subtitle = "150 random sentences from UN General Assembly speeches",
       y= "\nEstimate of LSS model\nfor conflictual vs. cooperative language\n",
       x= "Mean choices of coders\n",
       caption = "Red markers indicate LSS means by coder choice.\nCoder choices are averaged such that 1 indicates agreement on cooperative, and -1 agreement on conflictual language.")+
  coord_flip()+
  theme_minimal()+
  theme(legend.position = "bottom",
        axis.text = element_text(color = "black"),
        text = element_text(family = "Dahrendorf"))

ggsave("./Plots/HumanValidation/AverageCodervsMachine.png", width = 20, height = 14, units = "cm")  


# Pretend binary classification


bin <- hc %>% 
  filter(coderMean != 0) %>% 
  mutate(lssTendency = ifelse(lss.fit >= 0, "Cooperative", "Conflictual"),
         coderTendency = ifelse(coderMean >= 0, "Cooperative", "Conflictual")) %>% 
  select(lssTendency, coderTendency) %>% 
  group_by(lssTendency, coderTendency) %>% 
  summarise(freq = n())

# Identification of conflictual language
acc <- hc %>% 
  filter(coderMean != 0) %>% 
  mutate(lssTendency = ifelse(lss.fit >= 0, "Cooperative", "Conflictual"),
         coderTendency = ifelse(coderMean >= 0, "Cooperative", "Conflictual"))

acc$tp <- acc$coderTendency == "Conflictual" & acc$lssTendency == "Conflictual" # True positives
acc$tn <- acc$coderTendency == "Cooperative" & acc$lssTendency == "Cooperative" # True negatives
acc$fp <- acc$coderTendency == "Cooperative" & acc$lssTendency == "Conflictual" # False positives
acc$fn <- acc$coderTendency == "Conflictual" & acc$lssTendency == "Cooperative" # False negatives

precision <- round(sum(acc$tp) / (sum(acc$tp)+sum(acc$fp)), 2)
recall <- round(sum(acc$tp) / (sum(acc$tp)+sum(acc$fn)), 2)
specificity <- round(sum(acc$tn) / (sum(acc$tn)+sum(acc$fp)), 2)
accuracy <- round((sum(acc$tp)+sum(acc$tn))/132, 2)

# Plot binary classification
ggplot(bin, aes(x = lssTendency, y = coderTendency,  alpha = freq))+
  # geom_abline(intercept = 0, slope = 1)+
  geom_tile(fill = "#0380b5")+
  geom_text(aes(label = freq))+
  labs(title = "Pretending a binary classification problem ...",
       subtitle = "Taking only those 132 sentences in which coders saw a tendency\ntowards either side and taking an LSS score of 0 as the cut-off.",
       x = "\nLSS model tends to ...",
       y = "Coders on average tend to ...\n",
       alpha = "Frequency: ",
       caption = paste0("\nMetrics for correctly classifying conflictual sentences:\n", 
                        "Precision: ", precision, "\n",
                        "Recall: ", recall, "\n",
                        "Specificity: ", specificity, "\n",
                        "Accuracy: ", accuracy, "\n"))+
  theme_minimal()+
  theme(legend.position = "bottom",
        axis.text = element_text(color = "black"),
        text = element_text(family = "Dahrendorf"),
        panel.grid = element_blank())

ggsave("./Plots/HumanValidation/BinaryClassification.png", width = 16, height = 16, units = "cm") 
