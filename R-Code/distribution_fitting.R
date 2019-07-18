## ---- distribution_fitting.R
# @title: Source code for distribution fitting
# @author: joel.meili

# - load packages
library(dplyr)
library(ghyp)

# - load data
path <- "~/Dropbox/BA18/data/"
load(paste0(path, "log.return.assets.Rdata"))

# - demonstrating tail differences in Gaussian and Student's t-distribution
q <- seq(-10, 10, by = 1e-2)
plot(q, dnorm(q, log = TRUE), type = "l", xlab = "x", ylab = "Log-density")
lines(q, dt(q, df = 3, ncp = 0, log = TRUE), col = 2)
lines(q, dt(q, df = 10, ncp = 0, log = TRUE), col = 3)
lines(q, dt(q, df = 30, ncp = 0, log = TRUE), col = 4)
legend("bottom", legend = c("std. normal distribution", "Student's t-distribution: 3 df", 
                            "Student's t-distribution: 10 df", "Student's t-distribution: 30 df"), 
       col = 1:4, lty = 1, cex = 0.5, bg = "white")

# - eye testing symmetric univariate Student's t-distribution for S&P500 and JPY/USD samples
par(mfrow = c(1, 2))
qqPlot(assets$SP500 %>% scale(), "t", df = 3, ylab = "sample quantiles", main = "S&P500")
qqPlot(assets$JPY_USD %>% scale(), "t", df = 4, ylab = "sample quantiles", main = "JPY/USD")

# - valdiating univariate skewed Student's t-distribution for S&P500 and JPY/USD samples
temp <- apply(assets[, 2:ncol(assets)], 2, scale)
temp <- apply(temp, 2, FUN = function(x) x[order(x)])
N <- nrow(temp)
fit.sp500 <- fit.tuv(temp[, 2], silent = TRUE)
fit.sp500.symm <- fit.tuv(temp[, 2], symmetric = TRUE, silent = TRUE)
fit.jpy <- fit.tuv(temp[, 4], silent = TRUE)
fit.jpy.symm <- fit.tuv(temp[, 4], symmetric = TRUE, silent = TRUE)
par(mfrow = c(1, 2))
plot(log(1:N/(N + 1))~temp[, 2], xlim = c(min(temp[, 2]), 0), xlab = "S&P500", ylab = "log(P(X<x)")
lines(log(pghyp(temp[, 2], object = fit.sp500))~temp[, 2], col = 2)
lines(log(pghyp(temp[, 2], object = fit.sp500.symm))~temp[, 2], col = 3)
lines(log(pnorm(temp[, 2], mean = mean(temp[, 2]), sd = sd(temp[, 2])))~temp[, 2], col = 4)
legend("topleft", legend = c("sample", "skewed t", "symmetric t", "normal"), col = 1:4, lty = 1, cex = 0.5)
plot(log((N + 1 - 1:N)/(N + 1))~temp[, 2], xlim = c(0, max(temp[, 2])), xlab = "S&P500", ylab = "log(1 - P(X<x))")
lines(log(1 - pghyp(temp[, 2], object = fit.sp500))~temp[, 2], col = 2)
lines(log(1 - pghyp(temp[, 2], object = fit.sp500.symm))~temp[, 2], col = 3)
lines(log(1 - pnorm(temp[, 2], mean = mean(temp[, 2]), sd = sd(temp[, 2])))~temp[, 2], col = 4)
legend("topright", legend = c("sample", "skewed t", "symmetric t", "normal"), col = 1:4, lty = 1, cex = 0.5)

# -
par(mfrow = c(1, 2))
plot(log(1:N/(N + 1))~temp[, 4], xlim = c(min(temp[, 4]), 0), xlab = "JPY/USD", ylab = "log(P(X<x))")
lines(log(pghyp(temp[, 4], object = fit.jpy))~temp[, 4], col = 2)
lines(log(pghyp(temp[, 4], object = fit.jpy.symm))~temp[, 4], col = 3)
lines(log(pnorm(temp[, 4], mean = mean(temp[, 4]), sd = sd(temp[, 4])))~temp[, 4], col = 4)
legend("topleft", legend = c("sample", "skewed t", "symmetric t", "normal"), col = 1:4, lty = 1, cex = 0.5)
plot(log((N + 1 - 1:N)/(N + 1))~temp[, 4], xlim = c(0, max(temp[, 4])), xlab = "JPY/USD", ylab = "log(1 - P(X<x))")
lines(log(1 - pghyp(temp[, 4], object = fit.jpy))~temp[, 4], col = 2)
lines(log(1 - pghyp(temp[, 4], object = fit.jpy.symm))~temp[, 4], col = 3)
lines(log(1 - pnorm(temp[, 4], mean = mean(temp[, 4]), sd = sd(temp[, 4])))~temp[, 4], col = 4)
legend("topright", legend = c("sample", "skewed t", "symmetric t", "normal"), col = 1:4, lty = 1, cex = 0.5)

# - log-likelihood-ratio test to validate whether symmetric Student's t-distribution would be sufficient
log.lik.sp500 <- lik.ratio.test(fit.sp500, fit.sp500.symm)
log.lik.jpy <- lik.ratio.test(fit.jpy, fit.jpy.symm)
log.lik.mat <- matrix(c(log.lik.sp500$p.value, log.lik.jpy$p.value), ncol = 2, byrow = TRUE)
rownames(log.lik.mat) <- "P-value"
colnames(log.lik.mat) <- c("S&P500", "JPY/USD")

# - value-at-risk backtesting and one-sided binomial test for S&P500 and JPY/USD estimates
alphas <- c(0.001, 0.005, 0.01)
viols <- matrix(NA, nrow = 3, ncol = length(alphas))
rownames(viols) <- c("Gaussian", "Symmetric Student's t", "Skewed Student's t")
colnames(viols) <- alphas
k <- 1
temp <- assets$SP500 %>% scale()
for(alpha in alphas){
  viols.t <- temp < qghyp(alpha, object = fit.sp500)
  viols.symm <- temp < qghyp(alpha, object = fit.sp500.symm)
  viols.norm <- temp < qnorm(alpha)
  viols[1, k] <- sum(viols.norm)
  viols[2, k] <- sum(viols.symm)
  viols[3, k] <- sum(viols.t)
  k <- k + 1
}
viols <- as.data.frame(viols) 
rownames(viols) <- c("Gaussian", "Symmetric Student's t", "Skewed Student's t")

p.values <- viols
for(i in 1:nrow(p.values)){
  for(j in 1:ncol(p.values)){
    p.values[i, j] <- binom.test(viols[i, j], nrow(assets), alphas[j], alternative = "greater")$p.value
  }
}
p.values <- round(p.values, digits = 3)
p.values <- as.data.frame(p.values) %>% mutate_all(~cell_spec(.x, bold = ifelse(.x < 0.05, TRUE, FALSE)))
rownames(p.values) <- rownames(viols)

alphas <- c(0.001, 0.005, 0.01)
viols <- matrix(NA, nrow = 3, ncol = length(alphas))
rownames(viols) <- c("Gaussian", "Symmetric Student's t", "Skewed Student's t")
colnames(viols) <- 1 - alphas
k <- 1
temp <- assets$JPY_USD %>% scale()
for(alpha in alphas){
  viols.t <- temp > qghyp(1 - alpha, object = fit.jpy)
  viols.symm <- temp > qghyp(1 - alpha, object = fit.jpy.symm)
  viols.norm <- temp > qnorm(1 - alpha)
  viols[1, k] <- sum(viols.norm)
  viols[2, k] <- sum(viols.symm)
  viols[3, k] <- sum(viols.t)
  k <- k + 1
}
viols <- as.data.frame(viols)

p.values <- viols
for(i in 1:nrow(p.values)){
  for(j in 1:ncol(p.values)){
    p.values[i, j] <- binom.test(viols[i, j], nrow(assets), alphas[j], alternative = "greater")$p.value
  }
}
p.values <- round(p.values, digits = 3)
p.values <- as.data.frame(p.values) %>% mutate_all(~cell_spec(.x, bold = ifelse(.x < 0.05, TRUE, FALSE)))
rownames(p.values) <- rownames(viols)

# fit univariate skewed Student's-t distribution to daily DIJA log-returns
uv.fit <- fit.tuv(assets$DOW %>% scale(), silent = T)

# - fit multivariate skewed Student's-t distribution to daily log-returns
assets.use <- assets[, 2:ncol(assets)] %>% mutate_each(scale)
mv.fit <- fit.tmv(assets.use, silent = T)

# - bootstrap estimated multivariate skewed Student's t-distribution parameters
R <- 1e2
boot.params <- matrix(NA, nrow = ncol(assets.use) + 1, ncol = R)
rownames(boot.params) <- c("nu", paste0("gamma", ".", colnames(assets.use)))

for(i in 1:R){
  print(paste("Iteration", i, "of", R))
  idx <- sample(1:nrow(assets), replace = TRUE)
  mv.temp <- fit.tmv(assets.use[idx, ], silent = T)
  boot.params[1, i] <- mv.temp@lambda*-2
  boot.params[2:nrow(boot.params), i] <- mv.temp@gamma
}

save(boot.params, file = paste0(path, "fitted.parameters.Rdata"))