# Prepare workspace --------------------------------------------------------

## Load libraries
library(correlation)

## Load dataset
load(file = "data/sleep.RData")

## Auxiliary functions
######################

# Generate correlatinos ---------------------------------------------------

cor_spearman <- correlation(sleep, p_adjust = "none", method = "spearman")
cor_pearson <- correlation(sleep, p_adjust = "none", method = "pearson")

corr_results <- list(spearman = cor_spearman, pearson = cor_pearson)

save(corr_results, file = "R/correlations.RData")
