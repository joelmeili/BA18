# ---- allocate_assets.R 
# @title: Source code for asset allocation
# @author: joel.meili

# - load packages
library(ghyp)
library(dplyr)
library(tidyr)

# - create multivariate skewed Student's-t distribution
path <- "~/Dropbox/BA18/data/"
skew <- c(-0.1, 0, 0.05, -0.05)
mv <- student.t(nu = 3.5, mu = -2*skew + 0.025, sigma = diag(4), gamma = skew)

# - asset allocation
skew <- c(-0.1, 0, 0.05, -0.05)
mv <- student.t(nu = 3.5, mu = (skew - 0.025)/2, gamma = skew)
horizons <- c(1, 5, 20, 250, 1000)
opt.weights <- sapply(horizons, FUN = function(x) portfolio.optimize(mv, risk.measure = "expected.shortfall", level = 1 - x/26000, distr = "return", silent = TRUE)$opt.weights)
opt.weights <- as.data.frame(t(opt.weights))
colnames(opt.weights) <- c("Equities", "Bonds", "FX", "Commodities")
opt.weights$Days <- horizons
opt.weights <- opt.weights %>% gather(Asset, Weight, 1:4)
opt.weights$Weight[opt.weights$Asset == "Equities" & opt.weights$Days == 1] <- 0
save(opt.weights, file = paste0(path, "opt.weights.Rdata"))