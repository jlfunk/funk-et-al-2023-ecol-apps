This repository contains the data and code accompanying the paper:

> Interacting ecological filters influence success and functional composition in restored plant communities over time
> Jennifer L. Funk, Sarah Kimball, Monica A. Nguyen, Megan Lulow, Gregory E. Vose
> Published in Ecological Applications

## Contents

```plaintext
.
├── anova.R
├── data/
├── DESCRIPTION
├── generate_variance_partitioning_figure.R
├── Makefile
├── renv/
├── renv.lock
├── results/
└── trait-model.R
```

### Description of the contents

    anova.R: Fits Bayesian ANOVA models described in paper
    data/: This directory contains main dataset for paper
    generate_variance_partitioning_figure.R: Generates plots of ANOVA models
    Makefile: Controls the workflow of the project. See "Workflow" section below  
    results/: This directory contains fitted models and plots
    trait-model.R: Fits trait models and builds associated plots
    renv/, renv.lock, DESCRIPTION: Specify the R environment for the project to ensure reproducibility

## Workflow

The workflow of this project is controlled by a Makefile. Here's a brief description of the targets:

    anova-fits: This target runs the `anova.R` script against each trait.
    anova-plots: This target generates variance-partitioning figures for the ANOVA fits.
    trait-effect-plots: This target runs the `trait-model.R` script.

You can execute the entire pipeline by running `make figures` in the root directory of the project.

## Environment setup

The R environment required to run the code in this repository is managed with `renv`. To set up the environment, you can run the following commands in the R console:

```r
install.packages("renv")
renv::restore()
```

## License

Uses the "Creative Commons Zero v1.0 Universal" license. See `LICENSE`.


## Contact

For any additional questions, please contact `funk@ucdavis.edu`.

