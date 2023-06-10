options(
  digits = 4, # Number of digits to print. Default is 7, max is 15
  ## prompt = "R> ",
  options = c("https://cran.rstudio.com", "https://cloud.r-project.org"),
  scipen = 2, # Penalty applied to inhibit the use of scientific notation
  show.signif.stars = FALSE # Do not show stars indicating statistical significance in model outputs
)

local({
  ## n <- max(parallel::detectCores() - 2L, 1L) # Detect the number of cores available for use in parallelisation
  ## options(Ncpus = n) # Parallel package installation in install.packages()
  ## options(mc.cores = n) # Parallel apply-type functions via 'parallel' package
})

## Don't save my state, and don't ask me about it!
utils::assignInNamespace(
  "q",
  function(save = "no", status = 0, runLast = TRUE) {
    .Internal(quit(save, status, runLast))
  },
  "base"
)

source("renv/activate.R")
