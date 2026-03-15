library(dslabs)
data("research_funding_rates")
research_funding_rates

# Compute totals for men and women, across all disciplines
totals <- research_funding_rates %>%
              select(-discipline) %>%
              summarize ( applications_men_total = sum(applications_men),
                          awards_men_total = sum(awards_men),
                          applications_women_total = sum(applications_women),
                          awards_women_total = sum(awards_women)
                          )

# construct two-by-two table for observed data
two_by_two <- tibble(awarded = c("awarded", "not"),
                     men = c(totals$awards_men_total, (totals$applications_men_total - totals$awards_men_total) ),
                     women = c(totals$awards_women_total, (totals$applications_women_total - totals$awards_women_total))
              )
two_by_two

totals$awards_men_total/totals$applications_men_total

totals$awards_women_total/totals$applications_women_total

# chi-squared test
chisq_test <- two_by_two %>%
  select(-awarded) %>%
  chisq.test()
chisq_test$p.value

tidy(chisq_test)


#### 
dat <- research_funding_rates %>% 
  mutate(discipline = reorder(discipline, success_rates_total)) %>%
  rename(success_total = success_rates_total,
         success_men = success_rates_men,
         success_women = success_rates_women) %>%
  pivot_longer(-discipline) %>%
  separate(name, c("type", "gender")) %>%
  pivot_wider(names_from = type, values_from = value) %>%
  filter(gender != "total")
dat

#
dat %>%
ggplot(aes(discipline, y = applications, fill = gender)) +
  geom_bar(stat = "identity", position = "stack") +
  coord_flip()

dat %>%
  ggplot(aes(discipline, success, fill = gender, linewidth = applications)) +
  geom_bar(stat = "identity", position = "stack") +
  coord_flip()

dat %>%
  ggplot(aes(discipline, y = applications, fill = gender)) +
  geom_bar(stat = "identity", position = "stack") +
  coord_flip()
