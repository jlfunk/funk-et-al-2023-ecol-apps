library(tidyverse)
library(brms)
library(bayesplot)
library(glue)
library(ggthemes)
library(purrr)

args <- commandArgs(trailingOnly = T)
index <- as.integer(args[1])
stopifnot(index >= 1)

traits.cwm <- c(
  ## -- community weighted means
  "photo_cwm",
  "wue_cwm",
  "sla_cwm",
  "seed_cwm",
  "rdiam_cwm",
  "rmf_cwm",
  "srl_cwm",
  "N_cwm",
  "rtd_cwm"
  ## "pc1_cwm",
  ## "pc2_cwm",
)

## -- functional dispersion
traits.fd <- c(
  "photo_fd",
  "wue_fd",
  "sla_fd",
  "seed_fd",
  "rdiam_fd",
  "rmf_fd",
  "srl_fd",
  "N_fd",
  "rtd_fd"
  ## "pc1_fd",
  ## "pc2_fd",
)

## -- native community performance
traits.perf <- c(
  "native_species_richness",
  "total_native_cover",
  "multivariate_FDis"
)


lmap.cwm <- list(
  photo_cwm = "CWM~A[area]",
  wue_cwm = "CWM~WUE",
  N_cwm = "CWM~Leaf~N",
  ##
  sla_cwm = "CWM~SLA",
  seed_cwm = "CWM~Seed~mass",
  rdiam_cwm = "CWM~R[diam]",
  ##
  rmf_cwm = "CWM~RMF",
  rtd_cwm = "CWM~RTD",
  srl_cwm = "CWM~SRL"
)

lmap.fd <- list(
  photo_fd = "FDis~A[area]",
  wue_fd = "FDis~WUE",
  N_fd = "FDis~Leaf~N",
  ##
  sla_fd = "FDis~SLA",
  seed_fd = "FDis~Seed~mass",
  rdiam_fd = "FDis~R[diam]",
  ##
  rmf_fd = "FDis~RMF",
  rtd_fd = "FDis~RTD",
  srl_fd = "FDis~SRL"
)

lmap.perf <- list(
  total_native_cover = "Total~native~cover",
  native_species_richness = "Native~species~richness",
  multivariate_FDis = "Multivariate~FDis"
)

if (index == 1) {
  class <- "cwm"
  traits <- traits.cwm
  lmap <- lmap.cwm
  formula.num <- 2048
  asp <- 1.0
} else if (index == 2) {
  class <- "fd"
  traits <- traits.fd
  lmap <- lmap.fd
  formula.num <- 2048
  asp <- 1.0
} else {
  class <- "perf"
  traits <- traits.perf
  lmap <- lmap.perf
  formula.num <- 2048
  asp <- 0.40
}

entity.order <- c(
  "community",
  "slope",
  "year",
  "community:year",
  "community:slope:year"
)


fits <- list()
for (trait in traits) {
  fits[[trait]] <- readRDS(glue("results/anova/{trait}-formula{formula.num}-anova.rds"))
}


df <- fits %>%
  purrr::map_dfr(mcmc_intervals_data, .id = "trait") %>%
  filter(str_detect(parameter, "^sd_")) %>%
  extract(parameter, c("entity"), "sd_([A-Za-z:]+)__Intercept") %>%
  ## mutate(entity = factor(entity, levels = rev(entity.order), ordered = T)) %>%
  mutate(entity = fct_rev(recode_factor(entity,
    community = "Seed mix",
    slope = "Aspect",
    Herb = "Herb",
    year = "Year",
    `community:slope` = "Seed mix:Aspect",
    `community:Herb` = "Seed mix:Herb",
    `community:year` = "Seed mix:Year",
    `slope:Herb` = "Aspect:Herb",
    `slope:year` = "Aspect:Year",
    `year:Herb` = "Herb:Year",
    `community:slope:Herb` = "Seed mix:Aspect:Herb",
    `community:slope:year` = "Seed mix:Aspect:Year",
    `community:year:Herb` = "Seed mix:Herb:Year",
    `slope:year:Herb` = "Aspect:Herb:Year",
    `community:slope:year:Herb` = "Seed mix:Aspect:Herb:Year",
    .ordered = T
  ))) %>%
  mutate(trait = recode_factor(trait,
    !!!lmap,
    .ordered = T
  ))



ggplot(df, aes(x = m, y = entity)) +
  facet_wrap(~trait, ncol = 3, label = "label_parsed") +
  geom_point(shape = 3, size = 3) +
  geom_linerange(aes(xmin = ll, xmax = hh)) +
  geom_linerange(aes(xmin = l, xmax = h), size = 2.0) +
  theme_fivethirtyeight() +
  theme(
    panel.spacing = unit(2, "lines"),
    plot.background = element_rect(fill = "white"),
    panel.background = element_rect(fill = "white"),
    strip.background = element_rect(fill = "white"),
    axis.title = element_text(),
    axis.title.y = element_blank()
  ) +
  xlab("Estimated standard deviation of effects")

ggsave(glue("results/anova/figure-variance-partitioning-{class}.pdf"),
  width = 8.5,
  height = 8.5 * asp
)
