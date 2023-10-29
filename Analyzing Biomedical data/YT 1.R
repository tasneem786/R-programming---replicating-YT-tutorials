#loading the libraries

library(tidyverse)
library(ggplot2)
library(ggpubr)

# load the dataset into a variable
luminescence <- read_csv(file = "Luminescence Quick Read 2021.04.12 14_13_35.csv")

# dataframe - layout of the plate
plate_layout <- data.frame(Row = (c("A", "B", "C")), Genotype = c("tau_AGG", "EGFP_AGG_1", "EGFP_AGG_2"))

# Perform pairwise t-tests to compare each EGFP control to the tau_AGG group. Add comparisons with significance to the graph
luminescence %>%
  mutate(Row = substr(WellPosition, 1, 1)) %>%
  mutate(Column = substr(WellPosition, 3, 3)) %>%
  select(Luminescence = "RLU", Row, Column) %>%
  left_join(plate_layout, by = "Row") %>%
  group_by(Genotype) %>%
  mutate(Luminescence_Mean = mean(Luminescence)) %>%
  mutate(Luminescence_Stdev = sd(Luminescence)) %>%
  mutate(Luminescence_SEM = Luminescence_Stdev/sqrt(n())) %>%
  ggplot(mapping = aes(x = factor(Genotype, levels = c("tau_AGG", "EGFP_AGG_1", "EGFP_AGG_2")), y = Luminescence, fill = Genotype)) +
  geom_bar(stat = "summary", fun = "mean") +
  geom_errorbar(aes(ymin = Luminescence_Mean - Luminescence_SEM, ymax = Luminescence_Mean + Luminescence_SEM)) +
  labs(
    x = "Genotype",
    y = "Luminescence",
    title = "Comparison of tau aggregation to EGFP controls") +
  theme_classic() +
  theme(
    plot.title = element_text(size = 15, hjust = 0.5),
    text = element_text(size = 14),
    legend.position = "none") +
  stat_compare_means(method = "anova", label.y = 3700) +
  stat_compare_means(method = "t.test", comparisons = list(c("tau_AGG", "EGFP_AGG_1"), c("tau_AGG", "EGFP_AGG_2")), label = "p.signif") +
  ggsave("resultplot.png", height = 7, width = 10, dpi = 300)
