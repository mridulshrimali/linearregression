# find regression line for predicting runs from BBs (not shown in video)
library(tidyverse)
library(HistData)
data("GaltonFamilies")

galton_heights <- GaltonFamilies %>%
  filter(gender == "male") %>%
  group_by(family) %>%
  sample_n(1) %>%
  ungroup() %>%
  select(father, childHeight) %>%
  rename(son = childHeight)

lm(son ~ father, data = galton_heights)

galton_heights <- galton_heights %>%
  mutate(father_centered=father - mean(father))

lm(son ~ father_centered, data = galton_heights)

###
beta1 = seq(0, 1, len=nrow(galton_heights))
results <- data.frame(beta1 = beta1,
                      rss = sapply(beta1, rss, beta0 = 36))
results %>% ggplot(aes(beta1, rss)) + geom_line() + 
  geom_line(aes(beta1, rss), col=2)

results

####
library(tidyverse)
library(Lahman)

modeldata <- Teams %>% 
  filter(yearID %in% 1961:2001 ) %>% 
  mutate(BB_per_game = BB/G, runs_per_game = R/G, homeruns_per_game = HR/G) 
  
model <-lm(runs_per_game ~ BB_per_game + homeruns_per_game, data = modeldata)
  
# Get the model summary and extract R-squared
model_summary <- summary(model)

####
B <- 1000
N <- 100
lse <- replicate(B, {
  sample_n(galton_heights, N, replace = TRUE) %>% 
    lm(son ~ father, data = .) %>% .$coef 
})

lse <- data.frame(beta_0 = lse[1,], beta_1 = lse[2,]) 

galton_heights %>% ggplot(aes(father, son)) +
  geom_point() +
  geom_smooth()

galton_heights %>% ggplot(aes(father, son)) +
  geom_point() +
  geom_smooth(method = "lm")

model <- lm(son ~ father, data = galton_heights)
predictions <- predict(model, interval = c("confidence"), level = 0.95)
data <- as_tibble(predictions) %>% bind_cols(father = galton_heights$father)

ggplot(data, aes(x = father, y = fit)) +
  geom_line(color = "blue", size = 1) + 
  geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=0.2) + 
  geom_point(data = galton_heights, aes(x = father, y = son))

model <- lm(son ~ father, data = galton_heights)
predictions <- predict(model)
data <- as_tibble(predictions) %>% bind_cols(father = galton_heights$father)

ggplot(data, aes(x = father, y = fit)) +
  geom_line(color = "blue", size = 1) + 
  geom_point(data = galton_heights, aes(x = father, y = son))

### Assessment 2
set.seed(1989) #if you are using R 3.5 or earlier
set.seed(1989, sample.kind="Rounding") #if you are using R 3.6 or later
library(HistData)
data("GaltonFamilies")
options(digits = 3)    # report 3 significant digits

female_heights <- GaltonFamilies %>%     
  filter(gender == "female") %>%     
  group_by(family) %>%     
  sample_n(1) %>%     
  ungroup() %>%     
  select(mother, childHeight) %>%     
  rename(daughter = childHeight)

model <- lm(mother ~ daughter, data = female_heights)
model

first_daughter <- female_heights[1,"daughter"]

first_prediction <- predict(model, first_daughter)
first_mother <- female_heights[1,"mother"]

### Q 7 Onwards
library(tidyverse)
library(Lahman)
data("Batting")

bat_02 <- Batting %>% filter(yearID == 2002) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>%
  select(playerID, stint, singles, bb)

bat_02_summary <- bat_02 %>%  group_by(playerID) %>%
  summarize(mean_singles = mean(singles), mean_bb = mean(bb))

bat_01 <- Batting %>% filter(yearID %in% 1999:2001 ) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>%
  select(playerID, stint, singles, bb)

bat_01_summary <- bat_01 %>%  group_by(playerID) %>%
  summarize(mean_singles = mean(singles), mean_bb = mean(bb))
  
nrow(bat_01_summary)
nrow(bat_01_summary %>% filter(mean_singles > 0.2))
nrow(bat_01_summary %>% filter(mean_bb > 0.2))

merged_bat <- inner_join(bat_02_summary, bat_01_summary, by = "playerID") %>%
  rename(mean_singles_2002 = mean_singles.x , mean_bb_2002 = mean_bb.x, mean_singles_2001 = mean_singles.y , mean_bb_2001 = mean_bb.y)

cor(merged_bat$mean_singles_2002, merged_bat$mean_singles_2001)
cor(merged_bat$mean_bb_2002, merged_bat$mean_bb_2001)

merged_bat %>% 
  ggplot(aes(mean_singles_2002, mean_singles_2001)) +  
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm")

merged_bat %>% 
  ggplot(aes(mean_bb_2002, mean_bb_2001)) +  
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm")

## lm function
# fit regression line to predict mean_singles_2002 from mean_singles_2001
fit <- lm(mean_singles_2002 ~ mean_singles_2001, data = merged_bat)
fit

# summary statistics
summary(fit)

## lm function
# fit regression line to predict mean_singles_2002 from mean_singles_2001
fit <- lm(mean_bb_2002 ~ mean_bb_2001, data = merged_bat)
fit

# summary statistics
summary(fit)

##### advanced dplyr ###
dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR = round(HR/G, 1), 
         BB = BB/G,
         R = R/G) %>%
  select(HR, BB, R) %>%
  filter(HR >= 0.4 & HR<=1.2)

get_slope <- function(data) {
  fit <- lm(R ~ BB, data = data)
  sum.fit <- summary(fit)
  
  data.frame(slope = sum.fit$coefficients[2, "Estimate"], 
             se = sum.fit$coefficients[2, "Std. Error"],
             pvalue = sum.fit$coefficients[2, "Pr(>|t|)"])
}

dat %>% 
  group_by(HR) %>% 
  summarize(get_slope(across()))

dat %>% 
  group_by(HR) %>% 
  summarize(slope = get_slope(across()))


dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR = HR/G,
         R = R/G) %>%
  select(lgID, HR, BB, R)

dat %>% 
  group_by(lgID) %>% 
  summarize(tidy(lm(R ~ HR, data = across()), conf.int = T)) %>% 
  filter(term == "HR") 

dat %>% 
  summarize(tidy(lm(R ~ HR, data = across()), conf.int = T)) %>% 
  filter(term == "HR")

dat %>%
  group_by(lgID) %>%
  summarize(tidy(lm(R ~ HR, data = .), conf.int = T)) %>%
  filter(term == "HR")


## Q 8 - 10
library(tidyverse)
library(HistData)
data("GaltonFamilies")
# set.seed(1) # if you are using R 3.5 or earlier
set.seed(1, sample.kind = "Rounding") # if you are using R 3.6 or later
galton <- GaltonFamilies %>%
  group_by(family, gender) %>%
  sample_n(1) %>%
  ungroup() %>% 
  gather(parent, parentHeight, father:mother) %>%
  mutate(child = ifelse(gender == "female", "daughter", "son")) %>%
  unite(pair, c("parent", "child"))

galton

galton %>% group_by(pair) %>%
        summarize(n())


galton %>%  
  group_by(pair) %>%
  summarize(cor(childHeight,parentHeight))

galton %>% 
  group_by(pair) %>% 
  summarize(tidy(lm(childHeight ~ parentHeight, data = across()), conf.int = T))

# visualize the table with ggplot
galton %>%  
  group_by(pair) %>%
  summarize(tidy(lm(childHeight ~ parentHeight), conf.int = TRUE))

galton %>%  
  group_by(pair) %>%
  summarize(tidy(lm(childHeight ~ parentHeight), conf.int = TRUE)) %>%
  filter(term == "parentHeight") %>%
  select(pair, estimate, p.value, conf.low, conf.high) %>%
  ggplot(aes(pair, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_errorbar() +
  geom_point()
  

## 2.4 Assessment 
# Q3
BetaBB <- 0.371
BetaS <- 0.519
BetaD <- 0.771
BetaT <- 1.24
BetaHR <- 1.44

TeamA_Score <- BetaBB*2 + BetaS*4 + BetaD*1 + BetaT*0 +BetaHR*1
TeamA_Score

TeamB_Score <- BetaBB*1 + BetaS*6 + BetaD*2 + BetaT*1 +BetaHR*0
TeamB_Score

#Q7
library(tidyverse)
library(Lahman)
data("Teams")

model <- Teams %>% 
  filter(yearID == 1971) %>% 
  lm(R ~ BB + HR, data = .)
tidy(model, conf.int = TRUE)

res <- Teams %>% 
  filter(yearID %in% 1961:2018) %>%
  group_by(yearID) %>% 
  summarize(tidy(lm(R ~ BB + HR, data = across())))

res %>% 
  filter(term == "BB") %>%
  ggplot(aes(yearID, estimate)) +  
  geom_point() +
  geom_smooth(method = "lm")

fit <- res %>% 
  filter(term == "BB") %>%
  lm(estimate ~ yearID, data = . )

summary(fit)

