library(ggeffects)
library(glue)
library(patchwork)
library(splines)
library(tidyverse)


data.file <- "data/master-all-data.csv"


traits.cwm1 <- c(
  ## -- community weighted means
  "photo_cwm",
  "wue_cwm",
  "N_cwm",
  "sla_cwm",
  "seed_cwm"
)

traits.cwm2 <- c(
  ## -- community weighted means
  "rdiam_cwm",
  "rmf_cwm",
  "rtd_cwm",
  "srl_cwm"
)

## -- functional dispersion
traits.fd1 <- c(
  "photo_fd",
  "wue_fd",
  "N_fd",
  "sla_fd",
  "seed_fd"
)

traits.fd2 <- c(
  "rdiam_fd",
  "rmf_fd",
  "rtd_fd",
  "srl_fd"
)

traits.perf <- c(
  "total_native_cover",
  "native_species_richness",
  "multivariate_FDis"
)

traits <- c(traits.cwm1, traits.cwm2, traits.fd1, traits.fd2, traits.perf)

extra_columns <- c(
  "Mix"
)

df <- read_csv(data.file) %>%
  dplyr::select(community, slope, year, Herb, one_of(c(traits, extra_columns))) %>%
  rename(community_raw = community) %>%
  unite("community", c("community_raw", "Mix")) %>%
  ## mutate(year = str_replace(year, "Seed Mix", "2011")) %>%
  filter(year != "Seed Mix") %>%
  mutate(
    community = factor(
      community,
      levels = c(
        "CSS_All", "CSS_Forb", "CSS_Grass", "CSS_Shrub", "GL_All", "GL_Forb",
        "GL_Grass"
      ),
      ordered = T
    ),
    ## year = factor(year, levels = c("Seed Mix", "2012", "2013", "2014", "2015"), ordered = T),
    year = as.numeric(year),
    slope = factor(slope, levels = c("North", "South"), ordered = T)
  )


seedmix_df <- read_csv(data.file) %>%
  dplyr::select(community, slope, year, one_of(c(traits, extra_columns))) %>%
  rename(community_raw = community) %>%
  unite("community", c("community_raw", "Mix")) %>%
  ## mutate(year = str_replace(year, "Seed Mix", "2011")) %>%
  filter(year == "Seed Mix") %>%
  ## North and South are redundant for seed mix, wlog restrict to North.
  filter(slope == "North") %>%
  mutate(
    community = factor(
      community,
      levels = c(
        "CSS_All", "CSS_Forb", "CSS_Grass", "CSS_Shrub", "GL_All", "GL_Forb",
        "GL_Grass"
      ),
      ordered = T
    )
    ## slope = factor(slope, levels = c("North", "South"), ordered = T)
  ) %>%
  mutate(group_col = community)

fits <- list()
for (trait in traits) {
  message(glue("Building trait model for [{trait}]"))
  ## fits[[trait]] <- lm(as.formula(glue("{trait} ~ 1 + bs(year, 2) * community * slope")), data = df)
  fits[[trait]] <- lm(as.formula(glue("{trait} ~ 1 + year * community * slope * Herb")), data = df)
}


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
  native_species_richness = "Native~Species~Richness",
  total_native_cover = "Total~Native~Cover",
  multivariate_FDis = "Multivariate~FDis"
)

community.colors <- c(
  CSS_All = "#FF4136", # a warm, medium-bright red
  CSS_Forb = "#FF9F00", # a warm, medium-bright orange
  CSS_Grass = "#FFCE44", # a warm, medium-bright yellow
  CSS_Shrub = "#FFC0CB", # a warm, medium-bright pink
  ## CSS_Shrub = "#FFEC8B", # a warm, light yellow
  GL_All = "#0074D9", # a cool, medium-bright blue
  GL_Forb = "#2ECC40", # a cool, medium-bright green
  GL_Grass = "#3D9970" # a cool, dark green
)

slope.colors <- c(
  ## North = "#FF4136", # a warm, medium-bright red
  ## South = "#0074D9" # a cool, medium-bright blue
  North = "#0074D9", # a warm, medium-bright red
  South = "#FF4136" # a cool, medium-bright blue
)

colors.map <- list(
  `year:community` = community.colors,
  `year:slope` = slope.colors
)


asp <- 1.8
root <- "results/trait-models"

eighteen_cm <- 18 / 2.54
twentyfour_cm <- 24 / 2.54

for (class in c("cwm1", "cwm2", "fd1", "fd2", "perf")) {
  if (class == "cwm1") {
    traits <- traits.cwm1
    lmap <- lmap.cwm
  } else if (class == "cwm2") {
    traits <- traits.cwm2
    lmap <- lmap.cwm
  } else if (class == "fd1") {
    traits <- traits.fd1
    lmap <- lmap.fd
  } else if (class == "fd2") {
    traits <- traits.fd2
    lmap <- lmap.fd
  } else if (class == "perf") {
    traits <- traits.perf
    lmap <- lmap.perf
  }
  ##
  plots <- list()
  for (trait in traits) {
    fit <- fits[[trait]]
    terms <- c("year:community", "year:slope")
    for (term in terms) {
      if (grepl("community", term)) {
        label <- "Seed mix"
      } else {
        label <- "Slope"
      }
      colors <- colors.map[[term]]
      terms.exp <- strsplit(term, ":")[[1]]
      message(glue("[{class}] [{trait}] [{terms.exp}] [{term}]"))
      predict_df <- ggpredict(fits[[trait]], terms = terms.exp)
      p <- (
        plot(predict_df, add.data = F) +
          ylab(parse(text = lmap[[trait]])) +
          xlab(NULL) +
          ggtitle(NULL) +
          scale_colour_manual(values = colors, aesthetics = c("color", "fill")) +
          guides(color = guide_legend(title = label), fill = guide_legend(title = label))
      )
      if (term == "year:community") {
        p <- p + geom_point(aes(y = .data[[trait]]), x = 2011.9, size = 3, alpha = 0.8, data = seedmix_df)
      }
      plots[[glue("{trait}::{term}")]] <- p
    }
  }
  wrap_plots(plots, ncol = 2, guides = "collect")
  height <- min(length(traits) * asp, twentyfour_cm)
  message(height)
  ggsave(glue("{root}/combined-trait-models-{class}.pdf"),
    width = eighteen_cm,
    height = height
  )
}
