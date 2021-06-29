# PCA plots 
# Anna Moeller 
# 5/13/2021

# devtools::install_github("vqv/ggbiplot")
library(ggbiplot) # For plotting
library(tidyverse)
library(gridExtra)
library(gtable)

# Data
# Old
# veg <- read_csv("data/old/Data PCA herb comp values trans fires.csv") %>%
#   rename(Treat = treatment) 
veg <- read_csv("data/02 Data PCA herb cover 5 trts - reps only.csv") %>% 
  select(-X12) %>% 
  filter_all(any_vars(!is.na(.))) %>% 
  rename(year = Year,
         treatment = Treat) %>% 
  mutate(
    treatment = case_when(
      treatment == "5-3AFX" ~ "3AFX", # Not for 4 treatment groups
      treatment == "4-3AF" ~ "3AF",
      treatment == "3-2SF" ~ "2SF",
      treatment == "2-3WF" ~ "3WF",
      treatment == "1-CTL" ~ "Control"
    ),
    treatment = factor(treatment, levels = c("Control", "3WF", "2SF", "3AF", "3AFX")),
  )

# PCA on means ####
vegm <- veg %>% 
  group_by(treatment, year) %>% 
  summarize_at(vars(c3a:bare), mean) %>% 
  # mutate(year = str_sub(year, 3, 4)) %>% ## 2-digit year
  as.data.frame
# rownames(vegm) <- paste(vegm$treatment, vegm$year)

vegm_pca <- prcomp(
  vegm %>% select(c3a:bare), 
  center = TRUE,
  scale. = TRUE
)

# PCA on all replicates ####
# vegd <- as.data.frame(veg)
# # rownames(vegd) <- paste(veg$treatment, veg$year, veg$Rep)
# vegd_pca <- prcomp(
#   vegd %>% select(c3a:bare), 
#   center = TRUE,
#   scale. = TRUE
# )

# Plot features decided 5/17/21 ####
source("R/ggbiplot2.R")
g1 <- ggbiplot2(vegm_pca,
         ellipse = TRUE,
         groups = vegm$treatment,
         varname.size = 4,
         varname.adjust = 1.5,
         # labels = vegm$year
         alpha = 0 # Don't show the points
         ) +
  theme_classic() + 
  scale_color_manual(name = "Treatment",
                     values = c("black", "blue", "red", "darkgreen", "darkgreen")) +
  # For changing shape 
  # labs(color = "Treatment") +
  scale_shape_manual(name = "Treatment", 
                     values = c(16, 24, 25, 22, 22)) + 
  scale_fill_manual(name = "Treatment", 
                    values = c("black", "blue", "red", "darkgreen", "white")) +
  geom_point(aes(color = vegm$treatment, 
                 shape = vegm$treatment, 
                 fill = vegm$treatment),
             size = 2) +
  # For one dashed line
  scale_linetype_manual(
    name = "Treatment",
    values = c(rep("solid", 4), "dotted")) +

  # geom_text(aes(color = vegm$treatment, label = vegm$year),
  #           size = 3,
  #           hjust = -0.2,
  #           # It was putting a's in the legend
  #           show.legend = FALSE) 
  # geom_label gives background
  # geom_label(aes(color = vegm$treatment, label = vegm$year),
  #           size = 3,
  #           hjust = -0.2,
  #           label.size = 0,
  #           label.padding = unit(0, "lines"),
  #           fill = alpha("white", 0.8),
  #           # It was putting a's in the legend
  #           show.legend = FALSE)
  # Alternate option to repel labels from other text
  ggrepel::geom_label_repel(
    aes(color = vegm$treatment, label = vegm$year),
    size = 4,
    label.size = NA,
    nudge_y = 0.08,
    label.padding = unit(0, "lines"),
    fill = alpha("white", 0.8),
    show.legend = FALSE
  )
g1 

# t2 <- t
# 1: arrows, 2: points original, 3: ellipses, 4: arrow labels, 
# 5: points new, 6: points labels
# t2$layers <- c(t2$layers[1:3], t2$layers[5:6], t2$layers[[4]])

# ggsave("results/pca_means_5trt3.0.png")


# Just points, no functional groups
# Totally by hand
pcobj <- vegm_pca
choices <- 1:2
obs.scale <- 0
var.scale <- 1
circle.prob <- 0.69
groups <- vegm$treatment
varname.adjust = 1.5

nobs.factor <- sqrt(nrow(pcobj$x) - 1)
d <- pcobj$sdev
u <- sweep(pcobj$x, 2, 1/(d * nobs.factor), FUN = "*")
# v <- pcobj$rotation

choices <- pmin(choices, ncol(u))
df.u <- as.data.frame(sweep(u[, choices], 2, d[choices]^obs.scale, 
                            FUN = "*"))
# v <- sweep(v, 2, d^var.scale, FUN = "*")
# df.v <- as.data.frame(v[, choices])
names(df.u) <- c("xvar", "yvar")
# names(df.v) <- names(df.u)

df.u <- df.u * nobs.factor

# r <- sqrt(qchisq(circle.prob, df = 2)) * prod(colMeans(df.u^2))^(1/4)
# v.scale <- rowSums(v^2)
# df.v <- r * df.v/sqrt(max(v.scale))
u.axis.labs <- paste("standardized PC", choices, 
                     sep = "")
u.axis.labs <- paste(u.axis.labs, sprintf("(%0.1f%% explained var.)", 
                                          100 * pcobj$sdev[choices]^2/sum(pcobj$sdev^2)))
df.u$groups <- groups
# df.v$varname <- rownames(v)
# df.v$angle <- with(df.v, (180/pi) * atan(yvar/xvar))
# df.v$hjust = with(df.v, (1 - varname.adjust * sign(xvar))/2)

# For my segment arrows
nx <- lead(df.u$xvar)
nx[c(4,8,12,16)] <- NA
ny <- lead(df.u$yvar)
ny[c(4,8,12,16)] <- NA

g2 <- ggplot(data = df.u, aes(x = xvar, y = yvar)) +
  xlab(u.axis.labs[1]) + 
  ylab(u.axis.labs[2]) + 
  coord_equal() +
  theme_classic() + 
  scale_color_manual(name = "Treatment",
                     values = c("black", "blue", "red", "darkgreen", "darkgreen")) +
  # For changing shape 
  # labs(color = "Treatment") +
  scale_shape_manual(name = "Treatment", 
                     values = c(16, 24, 25, 22, 22)) + 
  scale_fill_manual(name = "Treatment", 
                    values = c("black", "blue", "red", "darkgreen", "white")) +
  geom_point(aes(color = vegm$treatment, 
                 shape = vegm$treatment, 
                 fill = vegm$treatment),
             size = 2) +
  geom_text(aes(color = vegm$treatment, label = vegm$year),
            size = 3,
            hjust = -0.2,
            # It was putting a's in the legend
            show.legend = FALSE) + 
  # theme(legend.position = "none") + 
  xlim(-2.2,1.4) + 
  geom_segment(aes(xend = nx,
                   yend = ny,
                   color = vegm$treatment),
               arrow = arrow(type = "open",
                             length = unit(0.3,"cm"),
                             angle = 20),
               show.legend = FALSE,
               linetype = c(rep("solid", 16), rep("dotted", 4)))
g2

# # https://cran.r-project.org/web/packages/egg/vignettes/Ecosystem.html
# grid_arrange_shared_legend <-
#   function(...,
#            ncol = length(list(...)),
#            nrow = 1,
#            position = c("bottom", "right")) {
#     
#     plots <- list(...)
#     position <- match.arg(position)
#     g <-
#       ggplotGrob(plots[[1]] + theme(legend.position = position))$grobs
#     legend <- g[[which(sapply(g, function(x)
#       x$name) == "guide-box")]]
#     lheight <- sum(legend$height)
#     lwidth <- sum(legend$width)
#     gl <- lapply(plots, function(x)
#       x + theme(legend.position = "none"))
#     gl <- c(gl, ncol = ncol, nrow = nrow)
#     
#     combined <- switch(
#       position,
#       "bottom" = arrangeGrob(
#         do.call(arrangeGrob, gl),
#         legend,
#         ncol = 1,
#         heights = unit.c(unit(1, "npc") - lheight, lheight)
#       ),
#       "right" = arrangeGrob(
#         do.call(arrangeGrob, gl),
#         legend,
#         ncol = 2,
#         widths = unit.c(unit(1, "npc") - lwidth, lwidth)
#       )
#     )
#     
#     grid.newpage()
#     grid.draw(combined)
#     
#     # return gtable invisibly
#     invisible(combined)
#     
#   }
# 
# grid_arrange_shared_legend(
#   g1, g2, 
#   position = "right",
#   nrow = 2, ncol = 1)

legend <- gtable_filter(ggplotGrob(g1), "guide-box") 
# grid.draw(legend)    # Make sure the legend has been extracted

label <- gtable_filter(ggplotGrob(g1), "ylab-l")
# grid.draw(label)

grid.arrange(
  label, 
  arrangeGrob(
    g1 + 
      theme(legend.position = "none") + 
      xlab(element_blank()) + 
      ylab(element_blank()),
    g2 + 
      ylab(element_blank()) +
      theme(legend.position = "none"),
    nrow = 2),
  legend,
  nrow = 1,
  widths = unit.c(unit(1, "lines"), 
                  unit(0.4, "npc") - unit(2, "lines") - legend$width, 
                  legend$width)
)




# PCA all replicates ####
ggbiplot2(vegd_pca,
         ellipse = TRUE,
         # labels = rownames(vegd),
         groups = vegd$treatment,
         alpha = 0) +
  theme_classic() + 
  # labs(x = "PC1", 
  #      y = "PC2",
  #      color = "Treatment")
  scale_color_manual(
    name = "Treatment",
    values = c("black", "blue", "red", "darkgreen", "darkgreen")
  ) +
  # For changing shape 
  # labs(color = "Treatment") +
  scale_shape_manual(name = "Treatment", 
                     values = c(16, 24, 25, 22, 22)) + 
  scale_fill_manual(name = "Treatment", 
                    values = c("black", "blue", "red", "darkgreen", NA)) +
  geom_point(aes(color = vegd$treatment, 
                 shape = vegd$treatment, 
                 fill = vegd$treatment),
             size = 2) + 
  # For one dashed line
  scale_linetype_manual(
    name = "Treatment",
    values = c(rep("solid", 4), "dotted"))
ggsave("results/pca_all replicates_5trt.png")

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

