# Data visualization 
# Anna Moeller 
# 3/19/2021

library(tidyverse)

dat <- read_csv("data/Data PCA herb comp values trans fires.csv")

datl <- dat %>% 
  pivot_longer(2:9, names_to = "functional_grp",
               values_to = "pct_cover")
  # filter(functional_grp %in% c("c4m_bunch", "c4m_rhiz")) %>% 
datm <- datl %>%
  group_by(functional_grp, treatment, year) %>% 
  summarize(m = mean(pct_cover))

# Look at correlation between the columns
covs <- dat[, 2:9]
cor(covs)
corrplot::corrplot(cor(covs))

ggplot(datm, aes(x = year, y = m, color = treatment)) + 
  geom_point() +
  # geom_jitter() +
  # geom_smooth(method = "lm", se = F) +
  facet_wrap(~functional_grp) + 
  theme_classic() 

# Calculate % change between each year 
datp <- datm %>% 
  group_by(functional_grp, treatment) %>% 
  mutate(
    d = m - lag(m),
    # pd = (m - lag(m))/lag(m) # As a percentage of the previous year
    ) %>% 
  filter(year %in% c(2001, 2003))
  
ggplot(datp, aes(x = year, y = d, color = treatment)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = F) +
  facet_wrap(~functional_grp) + 
  theme_classic()
