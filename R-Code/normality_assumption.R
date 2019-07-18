## ---- normality_assumption.R
# @title: Source code for assessment of the normality assumption
# @author: joel.meili

# - load packages
library(ghyp)

# - contour plots of copulas in review section
x <- seq(-10, 10, by = 0.1)
y <- x
n <- gauss(mu = rep(0, 2), sigma = matrix(c(1, 0.5, 0.5, 1), ncol = 2, byrow = TRUE))
t <- student.t(nu = 2.5, mu = rep(0, 2), sigma = matrix(c(1, 0.5, 0.5, 1), ncol = 2, byrow = TRUE), gamma = rep(0, 2))  
norm <- function(x, y){dghyp(cbind(x, y), object = n, logvalue = TRUE) - dghyp(x, object = n[1], logvalue = TRUE) - dghyp(y, object = n[2], logvalue = TRUE)}
tn <- function(x, y){dghyp(cbind(x, y), object = t, logvalue = TRUE) - dghyp(x, object = t[1], logvalue = TRUE) - dghyp(y, object = t[2], logvalue = TRUE)}
z.norm <- outer(x, y, norm)
z.t <- outer(x, y, tn)
par(mfrow = c(1, 2))
contour(x, y, z.norm, main = "Gaussian copula")
contour(x, y, z.t, main = "Student's t-copula")

# - quantile-quantile plots for daily S&P500 and JPY/USD log-returns
par(mfrow = c(1, 2))
qqPlot(assets$SP500 %>% scale(), ylab = "sample quantiles", main = "S&P500")
qqPlot(assets$JPY_USD %>% scale(), ylab = "sample quantiles", main = "JPY/USD")

# - correlation of log-returns
z <- round(cor(assets[, 2:ncol(assets)]), digits = 2)
colnames(z) <- c("DIJA", "S&P500", "10Y US Bond Futures", "JPY/USD", "Gold")
rownames(z) <- colnames(z)

# - volatility clustering
par(mfrow = c(1, 2))
acf(assets$SP500 %>% scale() %>% abs(), main = "")
z <- zoo(assets$SP500 %>% scale(), order.by = assets$Date)
plot(z, xlab = "Date", ylab = "S&P500")
abline(v = as.Date("2007-01-01"), col = 2)
abline(v = as.Date("2010-01-01"), col = 2)

# - GARCH modelling for volatiliy clustering
set.seed(100)
par(mfrow = c(1, 2))
garch.norm <- garchFit(data = rnorm(1e4) %>% scale())
garch.sp500 <- garchFit(data = assets$SP500 %>% scale())
plot(zoo(garch.norm@sigma.t), ylab = "Estimated volatility")
plot(zoo(garch.sp500@sigma.t, order.by = assets$Date), xlab = "Time", ylab = "Estimated volatility")
abline(v = as.Date(c("2007-01-01", "2010-01-01")), col = 2)

# - tail correlation in daily log-returns
set.seed(100)
par(mfrow = c(1, 2))
temp <- assets[, c(3, 4)] %>% scale()
plot(temp, xlim = c(-10, 10), ylim = c(-10, 10), ylab = "10Y US Bonds Futures")
abline(lm(temp[, 2]~temp[, 1]), col = 2)
lines(lowess(temp[, 2]~temp[, 1]), col = 3)
abline(h = c(3, -3), v = c(3, -3))
legend("bottomright", legend = c("linear regression", "local regression"),
       col = 2:3, lty = 1, cex = 0.5, bg = "white")
temp <- rghyp(1e4, gauss(mu = rep(0, 2))) %>% scale()
plot(temp, xlim = c(-5, 5), ylim = c(-5, 5), xlab = "sample 1", ylab = "sample 2")
abline(lm(temp[, 2]~temp[, 1]), col = 2)
abline(h = c(3, -3), v = c(3, -3))
lines(lowess(temp[, 2]~temp[, 1]), col = 3)
legend("bottomright", legend = c("linear regression", "local regression"), 
       col = 2:3, lty = 1, cex = 0.5, bg = "white")

# - quantile-quantile plots for yearly log-returns
assets.yearly <- assets %>% group_by(Year = year(Date)) %>% 
  summarise(SP500 = sum(SP500), JPY_USD = sum(JPY_USD))
par(mfrow = c(1, 2))
qqPlot(assets.yearly$SP500 %>% scale(), ylab = "sample quantiles", main = "S&P500")
qqPlot(assets.yearly$JPY_USD %>% scale(), ylab = "sample quantiles", main = "JPY/USD")

# - tail correlation in yearly log-returns
temp <- assets.yearly[, c(2, 3)] %>% scale()
plot(temp, ylab = "10Y US Bonds Futures")
abline(lm(temp[, 2]~temp[, 1]), col = 2)
lines(lowess(temp[, 2]~temp[, 1]), col = 3)
legend("bottomright", legend = c("linear regression", "local regression"), 
       col = 2:3, lty = 1, cex = 0.5, bg = "white")
