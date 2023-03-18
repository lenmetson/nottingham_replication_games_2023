##################################
# Hollyer et al. (2022)
# Replication Cross-National Analysis 
# Stata to R
# 18.03.23
##################################

rm(list=ls())

libs <- c("tidyverse", "haven")
lapply(libs, require, character.only = TRUE)

cnat.data <- read_dta("original_materials/main_paper/Data/cross-national-data.dta")


############
# Figure 3
############

#Personalism
ggplot(data = cnat.data, aes(ev_total, personal)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  xlab("Volatility Index") +
  theme_minimal()

#Programmaticness
ggplot(data = cnat.data, aes(ev_total, programmatic)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  xlab("Volatility Index") +
  theme_minimal()

############
# Figure 4
############

#split data
non.ethnic <- cnat.data %>%
  filter(ethnic == 0)

ethnic <- cnat.data %>%
  filter(ethnic == 1)

#Personalism Non-Ethnic
ggplot(data = non.ethnic, aes(ev_total, personal)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  xlab("Volatility Index") +
  theme_minimal()

#Personalism Ethnic
ggplot(data = ethnic, aes(ev_total, personal)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  xlab("Volatility Index") +
  theme_minimal()

#Programmaticism Non-Ethnic
ggplot(data = non.ethnic, aes(ev_total, programmatic)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  xlab("Volatility Index") +
  theme_minimal()

#Programmaticism Ethnic
ggplot(data = ethnic, aes(ev_total, programmatic)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  xlab("Volatility Index") +
  theme_minimal()


############
# Figure 5
############

#split data => HOW DID THEY SPLIT THE DATA???
moderate <- cnat.data %>%
  filter(extremist2 <= 3)

extreme <- cnat.data %>%
  filter(extremist2 >= 3)

#Personalism moderate
ggplot(data = moderate, aes(ev_total, personal)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  xlab("Volatility Index") +
  theme_minimal()

#Personalism extreme
ggplot(data = extreme, aes(ev_total, personal)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  xlab("Volatility Index") +
  theme_minimal()

#Programmaticism moderate
ggplot(data = moderate, aes(ev_total, programmatic)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  xlab("Volatility Index") +
  theme_minimal()

#Programmaticism Ethnic
ggplot(data = extreme, aes(ev_total, programmatic)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  xlab("Volatility Index") +
  theme_minimal()




