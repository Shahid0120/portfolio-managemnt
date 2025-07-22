# Load Data
df <- read.csv("~/Github/portfolio-management/modern-portfolio-theory-and-investment-analysis/week-7-the-correlation0-structure-of-secruity-returns-SIM/question_one_4_stocks_dataset.csv")
head(df)

# Exploratory Analysis - Time Series Plot
library(ggplot2)
ggplot(df, aes(x = Month)) +
  geom_line(aes(y = A, color = "A")) +
  geom_line(aes(y = B, color = "B")) +
  geom_line(aes(y = C, color = "C")) +
  geom_line(aes(y = `S.P`, color = "S&P")) +
  labs(title = "Monthly Returns for Securities A, B, C, and S&P",
       x = "Month", y = "Returns (%)", color = "Security") +
  theme_minimal()

# Calculating Expected Returns (Mean)
a.expectedreturn.mean <- mean(df$A)
b.expectedreturn.mean <- mean(df$B)
c.expectedreturn.mean <- mean(df$C)
s.p.expectedreturn.mean <- mean(df$S.P)

alpha <- list(
  alpha.a = a.expectedreturn.mean,
  alpha.b = b.expectedreturn.mean,
  alpha.c = c.expectedreturn.mean,
  alpha.s.p = s.p.expectedreturn.mean
)
print(alpha)

# Scatter Plots for Each Stock vs Market
library(patchwork)

pa <- ggplot(df) + geom_point(aes(x = S.P, y = A)) + theme_light()
pb <- ggplot(df) + geom_point(aes(x = S.P, y = B)) + theme_light()
pc <- ggplot(df) + geom_point(aes(x = S.P, y = C)) + theme_light()

p <- pa + pb + pc + plot_layout(ncol = 2) + 
  plot_annotation("Scatter plots comparing a given stock's return to stock market index return")
print(p)

# Linear Regression Models
R_m <- df$S.P
R_a <- df$A
R_b <- df$B
R_c <- df$C

model.a.s.p <- lm(R_a ~ R_m)
model.b.s.p <- lm(R_b ~ R_m)
model.c.s.p <- lm(R_c ~ R_m)

models.linear.regression <- list(
  model_a = model.a.s.p,
  model_b = model.b.s.p,
  model_c = model.c.s.p
)

summary(models.linear.regression$model_a)

# Regression Line Plot for Stock A
x.s.p <- seq(from = -10 , to = 20, by = 0.25)
a.coef <- models.linear.regression$model_a$coefficients
y.a <- a.coef[1] + a.coef[2]*x.s.p
line_df <- data.frame(x = x.s.p, y = y.a)

pa.and.lm <- ggplot(df, aes(x = S.P, y = A)) +
  geom_point(color = "steelblue") +
  geom_line(data = line_df, aes(x = x, y = y), color = "darkred", size = 1.2) +
  annotate("text", x = 2, y = 8,
           label = paste0("R_i = ", round(a.coef[2], 3), "R_m + ", round(a.coef[1], 3)),
           parse = FALSE)
print(pa.and.lm)

# Coefficients and Residuals
b.coef <- models.linear.regression$model_b$coefficients
c.coef <- models.linear.regression$model_c$coefficients

y.b <- b.coef[1] + b.coef[2]*x.s.p
y.c <- c.coef[1] + c.coef[2]*x.s.p

regression_df <- data.frame(x = x.s.p, y.a = y.a, y.b = y.b, y.c = y.c)

pb.and.lm <- ggplot(df, aes(x = S.P, y = B)) +
  geom_point(color = "steelblue") +
  geom_line(data = regression_df, aes(x = x, y = y.b), color = "darkred", size = 1.2)

pc.and.lm <- ggplot(df, aes(x = S.P, y = C)) +
  geom_point(color = "steelblue") +
  geom_line(data = regression_df, aes(x = x, y = y.c), color = "darkred", size = 1.2)

p <- pa.and.lm + pb.and.lm + pc.and.lm + plot_layout(ncol = 2) +
  plot_annotation("Scatter plots comparing stock returns to market index with regression lines")
print(p)

# Beta and Intercept Lists
beta.list <- list(
  beta.a = a.coef[2],
  beta.b = b.coef[2],
  beta.c = c.coef[2]
)
print(beta.list)

intercept.list <- list(
  intercept.a = a.coef[1],
  intercept.b = b.coef[1],
  intercept.c = c.coef[1]
)

# Sigma (Standard Error) of Residuals
sigma.linear.regression.models <- list(
  sigma.a = summary(model.a.s.p)$sigma,
  sigma.b = summary(model.b.s.p)$sigma,
  sigma.c = summary(model.c.s.p)$sigma
)
print(sigma.linear.regression.models)

# Correlation with Market
corr.coefficient.stocks.market.index <- list(
  corr.A = cor(df$A, df$S.P),
  corr.B = cor(df$B, df$S.P),
  corr.C = cor(df$C, df$S.P)
)
print(corr.coefficient.stocks.market.index)

# Intercepts
intercept.list <- list(
  intercept.a = a.coef[1],
  intercept.b = b.coef[1],
  intercept.c = c.coef[1]
)

# Plot for A
pa.and.lm <- ggplot(df, aes(x = S.P, y = A)) +
  geom_point(color = "steelblue") +
  geom_line(data = line_df, aes(x = x, y = y.a), color = "darkred", size = 1.2) +
  annotate("text",
           x = 2, y = 20,
           label = paste0("R_i = ", round(beta.list$beta.a, 2), "R_m + ", round(intercept.list$intercept.a, 2)),
           parse = FALSE, size = 3)

# Plot for B
pb.and.lm <- ggplot(df, aes(x = S.P, y = B)) +
  geom_point(color = "steelblue") +
  geom_line(data = line_df, aes(x = x, y = y.b), color = "darkred", size = 1.2) +
  annotate("text",
           x = 2, y = 20,
           label = paste0("R_i = ", round(beta.list$beta.b, 2), "R_m + ", round(intercept.list$intercept.b, 2)),
           parse = FALSE, size = 3)

# Plot for C
pc.and.lm <- ggplot(df, aes(x = S.P, y = C)) +
  geom_point(color = "steelblue") +
  geom_line(data = line_df, aes(x = x, y = y.c), color = "darkred", size = 1.2) +
  annotate("text",
           x = 2, y = 50,
           label = paste0("R_i = ", round(beta.list$beta.c, 2), "R_m + ", round(intercept.list$intercept.c, 2)),
           parse = FALSE, size = 3)

# Combine plots
graph.scatter.lm.line <- pa.and.lm + pb.and.lm + pc.and.lm + plot_layout(ncol = 2) +
  plot_annotation("Scatter plots comparing a given stock's return to stock market index return, and regression line")

graph.scatter.lm.line

# Market return average
average.return.market <- mean(df$S.P)

# Display with percentage
paste0(average.return.market, "%")

# Sample variance of market returns
variance.market.return <- sum((df$S.P - average.return.market)^2) / (length(df$S.P) - 1)
variance.market.return

# Question 2 
# (1) Calculate the mean return and mean variance using Single Index Model 
# Expected Return of Market R_m
expected.R_m <- mean(R_m)
expected.R_m
# based
expected.return.sim <- list(
  expectedR_a = 1.18 * expected.R_m - 0.61,
  expectedR_b = 0.98 * expected.R_m + 3.25,
  expectedR_c = 2.4  * expected.R_m - 0.13
)

# (2)
expected.return.historial <- list(
  expectedR_a = mean(R_a),
  expectedR_b = mean(R_b),
  expectedR_c = mean(R_c)
)

# B Covariance
# (1) SIM 
variance.R_m <- mean((R_m - mean(R_m))^2)
covariance.sim <- list(
  cov.a.b <- 1.18 * 0.98 * variance.R_m,
  cov.a.c <- 1.18 * 2.4 * variance.R_m,
  cov.b.c <- 0.98 * 2.4 * variance.R_m
)
covariance.sim

# (2) 
cov(df[, -which(names(df) == "Month")])

# C wi = 1/3 for i = 1,2,3. Calculating return on stocks and sigma
# SIM 
## Return on port = sum(1/3 * Expected return on earch stock)
expected.return.portfolio.sim <- 
  (1/3) * expected.return.sim$expectedR_a + 
  (1/3) * expected.return.sim$expectedR_b +
  (1/3) * expected.return.sim$expectedR_c

## Historial Return Portfolio 
expected.return.historial.portfolio <- 
  (1/3) * expected.return.historial$expectedR_a +
  (1/3) * expected.return.historial$expectedR_b +
  (1/3) * expected.return.historial$expectedR_c

expected.return.historial.portfolio

expected.return.sim
