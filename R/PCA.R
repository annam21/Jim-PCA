# PCA
# Anna Moeller
# 4/19/21

# Recreate PCA in R
library(ggbiplot) # For plotting
library(tidyverse) # Run this last 

# QAQC - see if means and reps align
# mns <- read_csv("data/01 Data PCA herb comp values trans fires - means only.csv")%>% 
  # filter_all(any_vars(!is.na(.)))
veg <- read_csv("data/02 Data PCA herb comp values trans fires - reps only.csv") %>%
  select(-X12) %>%
  filter_all(any_vars(!is.na(.))) %>%
  rename(year = Year,
         treatment = Treat) 
#   
# tst <- veg %>% 
#   group_by(year, treatment) %>% 
#   summarise(across(c3a:bare, mean)) %>%
#   ungroup %>% 
#   select(-year,-treatment)
# mns2 <- mns %>% 
#   select(-Year, - Trt)
# tst < mns2 + 0.1  & tst > mns2 - 0.1

# Data
vegm <- veg %>% 
  group_by(treatment, year) %>% 
  summarise(across(c3a:bare, mean)) %>% 
  as.data.frame
rownames(vegm) <- paste(vegm$treatment, vegm$year)
# vegm <- vegm %>% 
#   select(-treatment, -year)

# PCA on means data
vegm_pca <- prcomp(
  vegm %>% select(c3a:bare), 
  center = TRUE,
  scale. = TRUE
)

str(vegm_pca)
summary(vegm_pca)

# Plot
ggbiplot(vegm_pca)
ggbiplot(vegm_pca, labels = rownames(vegm))
ggbiplot(vegm_pca,
         ellipse = TRUE,
         labels = rownames(vegm),
         groups = vegm$treatment) +
  theme_classic() + 
  labs(x = "PC1", 
       y = "PC2",
       color = "Treatment") 
ggsave("results/pca_means_pretty.png")

# Can also check out different PCA axes
ggbiplot(vegm_pca,
         ellipse = TRUE,
         choices = c(3,4),   
         labels = rownames(vegm), 
         groups = vegm$treatment)

# Alternatively... PCA on all data
vegd <- as.data.frame(veg)
rownames(vegd) <- paste(veg$treatment, veg$year, veg$plot)
vegd_pca <- prcomp(
  vegd %>% select(c3a:bare), 
  center = TRUE,
  scale. = TRUE
)
ggbiplot(vegd_pca)
ggbiplot(vegd_pca, labels = rownames(vegd))
ggbiplot(vegd_pca,
         ellipse = TRUE,
         # labels = rownames(vegd),
         groups = vegd$treatment) +
         # groups = vegd$year)
  theme_classic() + 
  labs(x = "PC1", 
       y = "PC2",
       color = "Treatment")
ggsave("results/pca_all replicates_pretty.png")

# Alternatively 2... Do a PCA for each year 
pca_98 <- prcomp(
  vegm %>% filter(year == 1998) %>% select(c3a:bare), 
  center = TRUE,
  scale. = TRUE
)
pca_01 <- prcomp(
  vegm %>% filter(year == 2001) %>% select(c3a:bare), 
  center = TRUE,
  scale. = TRUE
)
pca_03 <- prcomp(
  vegm %>% filter(year == 2003) %>% select(c3a:bare), 
  center = TRUE,
  scale. = TRUE
)
pca_05 <- prcomp(
  vegm %>% filter(year == 2005) %>% select(c3a:bare), 
  center = TRUE,
  scale. = TRUE
)
a <- ggbiplot(pca_98, 
         labels = vegm %>% filter(year == 1998) %>% .$treatment) + 
  labs(title = "1998",
       x = "PC1",
       y = "PC2")
b <- ggbiplot(pca_01, 
         labels = vegm %>% filter(year == 2001) %>% .$treatment) + 
  labs(title = 2001,
       x = "PC1",
       y = "PC2")
c <- ggbiplot(pca_03, 
         labels = vegm %>% filter(year == 2003) %>% .$treatment) + 
  labs(title = 2003,
       x = "PC1",
       y = "PC2")
d <- ggbiplot(pca_05, 
         labels = vegm %>% filter(year == 2005) %>% .$treatment) + 
  labs(title = 2005,
       x = "PC1",
       y = "PC2")
ggpubr::ggarrange(a,b,c,d)

# Alternatively 3... Do a PCA for each treatment 
pca_2SF <- prcomp(
  vegm %>% filter(treatment == "2SF") %>% select(c3a:bare), 
  center = TRUE,
  scale. = TRUE
)
pca_3AF <- prcomp(
  vegm %>% filter(treatment == "3AF") %>% select(c3a:bare), 
  center = TRUE,
  scale. = TRUE
)
pca_3WF <- prcomp(
  vegm %>% filter(treatment == "3WF") %>% select(c3a:bare), 
  center = TRUE,
  scale. = TRUE
)
pca_ctl <- prcomp(
  vegm %>% filter(treatment == "control") %>% select(c3a:bare), 
  center = TRUE,
  scale. = TRUE
)
a <- ggbiplot(pca_2SF, 
              labels = vegm %>% filter(treatment == "2SF") %>% .$year) + 
  labs(title = "2SF",
       x = "PC1",
       y = "PC2")
b <- ggbiplot(pca_3AF, 
              labels = vegm %>% filter(treatment == "3AF") %>% .$year) + 
  labs(title = "3AF",
       x = "PC1",
       y = "PC2")
c <- ggbiplot(pca_3WF, 
              labels = vegm %>% filter(treatment == "3WF") %>% .$year) + 
  labs(title = "3WF",
       x = "PC1",
       y = "PC2")
d <- ggbiplot(pca_ctl, 
              labels = vegm %>% filter(treatment == "control") %>% .$year) + 
  labs(title = "control",
       x = "PC1",
       y = "PC2")
ggpubr::ggarrange(a,b,c,d)

# Try to re-create Jim's plot
# He did analysis by year, then joined them all into the same plot
raw <- purrr::map_dfr(
  list(pca_98$x, pca_01$x, pca_03$x, pca_05$x), 
  as.data.frame
) %>%
  mutate(trtyr = rownames(.)) %>% 
  separate(trtyr, into = c("treatment", "year")) %>% 
  # flip upside down
  mutate(PC2 = PC2 * -1)
ggplot(raw, aes(x = PC1, y = PC2, color = treatment, label = year)) + 
  # geom_point() + 
  geom_text() +
  # geom_line(arrow = arrow()) + # This doesn't do them in order
  geom_path(arrow = arrow(length = unit(0.3, "cm"))) +
  theme_classic() +
  scale_color_manual(values = c( "red", "limegreen","cyan", "black")) 
  



# Other notes: 
# PCA here is kind of backward from the demo - we have a bunch of response 
#   variables from one set of treatments and one set of years 
# I wonder if this assumes independence at all? As in, independence 
#   between years within a single treatment? 
# PCA seems better than CCA here because ... 


# Need to find out if their visualization is better for interpretation than
#  just using the raw PCA values...