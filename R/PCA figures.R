# PCA plots 
# Anna Moeller 
# 5/13/2021

# devtools::install_github("vqv/ggbiplot")
library(ggbiplot) # For plotting
library(tidyverse)

# Data
# Old
# veg <- read_csv("data/old/Data PCA herb comp values trans fires.csv") %>%
#   rename(Treat = treatment) 
veg <- read_csv("data/02 Data PCA herb comp values trans fires - reps only.csv") %>% 
  select(-X12) %>% 
  filter_all(any_vars(!is.na(.))) %>% 
  rename(year = Year,
         treatment = Treat) %>% 
  mutate(
    treatment = case_when(
      treatment == "3-2SF" ~ "2SF",
      treatment == "4-3AF" ~ "3AF",
      treatment == "2-3WF" ~ "3WF",
      treatment == "1-CTL" ~ "Control"
    ),
    treatment = factor(treatment, levels = c("Control", "3WF", "2SF", "3AF"))    
  )

# PCA on means ####
vegm <- veg %>% 
  group_by(treatment, year) %>% 
  summarize_at(vars(c3a:bare), mean) %>% 
  as.data.frame
# rownames(vegm) <- paste(vegm$treatment, vegm$year)

vegm_pca <- prcomp(
  vegm %>% select(c3a:bare), 
  center = TRUE,
  scale. = TRUE
)

# PCA on all replicates ####
vegd <- as.data.frame(veg)
rownames(vegd) <- paste(veg$treatment, veg$year, veg$plot)
vegd_pca <- prcomp(
  vegd %>% select(c3a:bare), 
  center = TRUE,
  scale. = TRUE
)

# Plot features decided 5/17/21 ####
ggbiplot(vegm_pca,
         ellipse = TRUE,
         groups = vegm$treatment,
         # labels = vegm$year
         ) +
  theme_classic() + 
  scale_color_manual(name = "Treatment",
                     values = c("black", "blue", "red", "darkgreen")) +
  # For changing shape 
  # labs(color = "Treatment") +
  scale_shape_manual(name = "Treatment", 
                     values = c(16, 24, 25, 22)) + 
  scale_fill_manual(name = "Treatment", 
                    values = c("black", "blue", "red", "darkgreen")) +
  geom_point(aes(color = vegm$treatment, 
                 shape = vegm$treatment, 
                 fill = vegm$treatment),
             size = 2) +

  # geom_text(aes(color = vegm$treatment, label = vegm$year),
  #           size = 3,
  #           hjust = -0.2,
  #           # It was putting a's in the legend
  #           show.legend = FALSE) 
  # geom_label gives background
  geom_label(aes(color = vegm$treatment, label = vegm$year),
            size = 3,
            hjust = -0.2,
            label.size = 0,
            label.padding = unit(0, "lines"),
            fill = alpha("white", 0.8),
            # It was putting a's in the legend
            show.legend = FALSE)
  # Alternate option to repel labels from other text
  # ggrepel::geom_label_repel(
  #   aes(color = vegm$treatment, label = vegm$year),
  #   size = 3,
  #   label.size = NA,
  #   label.padding = unit(0, "lines"),
  #   fill = alpha("white", 0.8),
  #   show.legend = FALSE
  # )
ggsave("results/pca_means.png")

# PCA all replicates
ggbiplot(vegd_pca,
         ellipse = TRUE,
         # labels = rownames(vegd),
         groups = vegd$treatment) +
  theme_classic() + 
  # labs(x = "PC1", 
  #      y = "PC2",
  #      color = "Treatment")
  scale_color_manual(name = "Treatment",
                     values = c("black", "blue", "red", "darkgreen")) +
  # For changing shape 
  # labs(color = "Treatment") +
  scale_shape_manual(name = "Treatment", 
                     values = c(16, 24, 25, 22)) + 
  scale_fill_manual(name = "Treatment", 
                    values = c("black", "blue", "red", "darkgreen")) +
  geom_point(aes(color = vegd$treatment, 
                 shape = vegd$treatment, 
                 fill = vegd$treatment),
             size = 2)
ggsave("results/pca_all replicates.png")

# Old plot options ######
ggbiplot(vegm_pca,
         ellipse = TRUE,
         labels = vegm$year,
         groups = vegm$treatment) +
  theme_classic() + 
  labs(x = "PC1", 
       y = "PC2",
       color = "Treatment") 
ggsave("results/pca_means_default.png")

ggbiplot(vegm_pca,
         ellipse = TRUE,
         labels = NULL,
         groups = vegm$treatment) +
  theme_classic() + 
  labs(x = "PC1", 
       y = "PC2",
       color = "Treatment") 
ggsave("results/pca_means_nolabels.png")

cbp1 <- c("#000000", "#E69F00","#0072B2","#009E73",
          "#F0E442", "#D55E00", "#CC79A7",  "#56B4E9")
ggbiplot(vegm_pca,
         ellipse = TRUE,
         labels = rownames(vegm),
         groups = vegm$treatment) +
  theme_classic() + 
  labs(x = "PC1", 
       y = "PC2",
       color = "Treatment") + 
  scale_color_manual(values = cbp1)
ggsave("results/pca_means_colorblind.png")

