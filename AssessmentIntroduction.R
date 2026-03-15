## 1.1
## 
library(Lahman)
library(tidyverse)
library(dslabs)
ds_theme_set()

## Code: Scatterplot of the relationship Runs Per Game, and bats (AB) per game
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(Runs_per_game = R / G, AB_per_game = AB / G) %>%
  ggplot(aes(Runs_per_game, AB_per_game)) + 
  geom_point(alpha = 0.5)

## Code: Scatterplot of win_rate (number of wins per game) vs number of fielding errors per game
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(wins_per_game = W / G, errors_per_game = E / G) %>%
  ggplot(aes(errors_per_game, wins_per_game)) + 
  geom_point(alpha = 0.5)

## Code: Scatterplot of triples(X3B)  vs doubles(X2B) per game
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(triples_per_game = X3B /G, doubles_per_game = X2B / G) %>%
  ggplot(aes(triples_per_game, doubles_per_game)) + 
  geom_point(alpha = 0.5)

## Correlation between runs_per_game and number of bats per game
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(Runs_per_game = R / G, AB_per_game = AB / G) %>%
  summarize(cor(Runs_per_game, AB_per_game))

## Correlation between win_rate and errors per game
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(wins_per_game = W / G, errors_per_game = E / G) %>%
  summarize(cor(wins_per_game , errors_per_game))

## Correlation between triples(X3B)  vs doubles(X2B) per game
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(triples_per_game = X3B /G, doubles_per_game = X2B / G) %>%
  summarize(cor(doubles_per_game, triples_per_game))

## Assessment 2 : Stratification and Variance explained.
set.seed(1989) #if you are using R 3.5 or earlier
set.seed(1989, sample.kind="Rounding") #if you are using R 3.6 or later
library(HistData)
data("GaltonFamilies")

female_heights <- GaltonFamilies%>%     
  filter(gender == "female") %>%     
  group_by(family) %>%     
  sample_n(1) %>%     
  ungroup() %>%     
  select(mother, childHeight) %>%     
  rename(daughter = childHeight)

# 1. Mean and standard deviations
mu_x <- mean(female_heights$mother)
mu_y <- mean(female_heights$daughter)
s_x <- sd(female_heights$mother)
s_y <- sd(female_heights$daughter)
r <- cor(female_heights$mother, female_heights$daughter)

# compute a regression line to predict the son's height from the father's height
m <-  r * s_y / s_x
b <- mu_y - m*mu_x

r * s_y / s_x

r^2 * 100

# Expected value of daughters height given mothers height
x<- 60
E_y_given_x <- mu_y + r * (60 - mu_x) * (s_y / s_x)
