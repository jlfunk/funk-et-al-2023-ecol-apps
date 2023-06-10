library(tidyverse)
library(brms)
library(bayesplot)
library(glue)
library(ggthemes)

args <- commandArgs(trailingOnly = T)
index <- as.integer(args[1])

stopifnot(index >= 1)
formula.index <- as.integer(args[2])
stopifnot(formula.index >= 1)

data.file <- "data/master-all-data.csv"
traits <- c(
  ## -- community weighted means
  "photo_cwm",
  "wue_cwm",
  "sla_cwm",
  "seed_cwm",
  "rdiam_cwm",
  "rmf_cwm",
  "srl_cwm",
  "N_cwm",
  "rtd_cwm",
  ## -- functional dispersion
  "photo_fd",
  "wue_fd",
  "sla_fd",
  "seed_fd",
  "rdiam_fd",
  "rmf_fd",
  "srl_fd",
  "N_fd",
  "rtd_fd",
  ## -- native community performance
  "native_species_richness",
  "total_native_cover",
  "multivariate_FDis"
)
##
extra_columns <- c(
  "Mix"
)
stopifnot(index <= length(traits))
trait <- traits[index]

df <- read_csv(data.file) %>%
  dplyr::select(community, slope, year, Herb, one_of(c(traits, extra_columns))) %>%
  rename(community_raw = community) %>%
  unite("community", c("community_raw", "Mix")) %>%
  filter(year != "Seed Mix") %>%
  mutate(
    community = factor(
      community,
      levels = c(
        "CSS_All", "CSS_Forb", "CSS_Grass", "CSS_Shrub", "GL_All", "GL_Forb", "GL_Grass"
      ),
      ordered = T
    ),
    year = factor(year, levels = c("2012", "2013", "2014", "2015"), ordered = T),
    slope = factor(slope, levels = c("North", "South"), ordered = T)
  ) %>%
  ## -- standardize each trait globally
  mutate(across(traits, ~ (. - mean(., na.rm = T)) / sd(., na.rm = T)))


interaction.terms <- c(
  "community:slope",
  "community:year",
  "community:Herb",
  "slope:year",
  "slope:Herb",
  "year:Herb",
  "community:slope:year",
  "community:slope:Herb",
  "community:year:Herb",
  "slope:year:Herb",
  "community:slope:year:Herb"
)

n <- length(interaction.terms)
id <- unlist(
  lapply(
    1:n,
    function(i) combn(1:n, i, simplify = FALSE)
  ),
  recursive = FALSE
)


formulas <- c("{trait} ~ (1 | community + slope + year + Herb)", sapply(id, function(i) {
  paste("{trait} ~ (1 | community + slope + year + Herb + ", paste(interaction.terms[i], collapse = " + "), ")")
}))

message(length(formulas))


message(glue("Building anova model [{formula.index}] for [{trait}]"))
frm <- as.formula(glue(formulas[formula.index]))
bfit <- brm(
  data = df,
  family = gaussian,
  formula = frm,
  iter = 4000,
  warmup = 1000,
  chains = 4,
  cores = 4,
  control = list(adapt_delta = 0.99, max_treedepth = 15),
  seed = 99,
  file = glue("results/anova/{trait}-formula{formula.index}-anova")
)

mcmc_plot(bfit, type = "neff") +
  theme_fivethirtyeight()
ggsave(glue("results/anova/{trait}-formula{formula.index}-neff-anova.pdf"),
  width = 8,
  height = 10
)

mcmc_plot(bfit, type = "intervals", pars = c("^sd_")) +
  theme(axis.text.y = element_text(hjust = 0)) +
  theme_fivethirtyeight() +
  ggtitle(glue("Variance Partitioning for [{trait}]"))
ggsave(glue("results/anova/{trait}-formula{formula.index}-anova.png"),
  width = 8,
  height = 4
)
