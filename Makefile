figures: anova-plots trait-effect-plots

anova-fits: anova.R
	mkdir -p results/anova
    # Traits 1..21 are the 21 traits in the dataset.
    # Model 2048 is the full model, containing all terms/interactions.
	parallel -j11 Rscript ./anova.R {1} {2} ::: $(shell seq 1 21) ::: 2048

anova-plots: anova-fits generate_variance_partitioning_figure.R
	parallel Rscript generate_variance_partitioning_figure.R {} ::: 1 2 3

trait-effect-plots: trait-model.R
	mkdir -p results/trait-models
	Rscript ./trait-model.R {}

.PHONY: anova-fits anova-plots trait-effect-plots
