library(dplyr)
library(tidyverse)
library(broom)
library(Lahman)
data("Teams")

Teams_small <- Teams %>% 
  filter(yearID %in% 1961:2001) %>% 
  mutate(avg_attendance = attendance/G)

#Q1
Teams_small %>% 
      mutate(Runs_per_game = R/G) %>%
      lm(avg_attendance ~ Runs_per_game, data = .)

Teams_small %>% 
  mutate(HR_per_game = HR/G) %>%
  lm(avg_attendance ~ HR_per_game, data = .)

Teams_small %>% 
  lm(avg_attendance ~ W, data = .)

Teams_small %>% 
  lm(avg_attendance ~ yearID, data = .)

#Q2
Teams_small %>% 
  mutate(Runs_per_game = R/G) %>%
    summarize(cor(Runs_per_game,W))

Teams_small %>% 
  mutate(HR_per_game = HR/G) %>%
  summarize(cor(HR_per_game,W))

#Q3
# Stratify Team_Small with Wins to nearest 10, filter out strata with few points
Teams_Strata <- Teams_small %>%
      mutate(Wins_strata = round(W/10)) %>%
      filter(Wins_strata >= 5 & Wins_strata <= 10)

table(Teams_Strata$Wins_strata)

# calculate slope of regression line (average attendance ~ Runs_per_game) after stratifying
Teams_Strata %>%
  mutate(Runs_per_game = R/G) %>%
  group_by(Wins_strata) %>%
  summarize(slope = cor(avg_attendance, Runs_per_game)*sd(Runs_per_game)/sd(avg_attendance))

# calculate slope of regression line (average attendance ~ Home Runs_per_game) after stratifying
Teams_Strata %>%
  mutate(HR_per_game = HR/G) %>%
  group_by(Wins_strata) %>%
  summarize(slope = cor(avg_attendance, HR_per_game)*sd(HR_per_game)/sd(avg_attendance))

Teams_small %>%
  mutate(HR_per_game = HR/G) %>%
  summarize(slope = cor(avg_attendance, HR_per_game)*sd(HR_per_game)/sd(avg_attendance))

#Q4
## multivariate regression determining the effects of runs per game, home runs per game, wins, and year on average attendance
fit <- Teams_small %>% 
  mutate(Runs_per_game = R/G, HR_per_game = HR/G) %>%
  lm(avg_attendance ~ Runs_per_game + HR_per_game + yearID + W ,data = .)

tidy(fit)

sample1 <- data.frame(Runs_per_game = 5, HR_per_game =1.2, yearID =2002, W =80)
predict(fit,sample1)

sample2 <- data.frame(Runs_per_game = 5, HR_per_game =1.2, yearID =1960, W =80)
predict(fit,sample2)

Teams %>% 
  filter(yearID %in% 2002) %>% 
  mutate(avg_attendance = attendance/G , Runs_per_game = R/G, HR_per_game = HR/G)  %>% 
  mutate(avg_attendance_hat = predict(fit, newdata = .)) %>%
  summarize(cor(avg_attendance_hat,avg_attendance))

