# Question One
library(ggplot2)
df <- read.csv("./Documents/finance-stocks-unsw/portfolio-management/modern-portfolio-theory-and-investment-analysis/week-7-the-correlation0-structure-of-secruity-returns-SIM/question_one_4_stocks_dataset.csv")
head(df)

# Exploratory Analysis
# Plot on Time series 
ggplot(data = df)
ggplot(df, aes(x = Month)) + 
  geom_line(aes(y = A, color = "A")) + 
  geom_line(aes(y = B, color = "B")) + 
  geom_line(aes(y = C, color = "C")) + 
  geom_line(aes(y = `S.P`, color = "S&P")) + 
  labs(title = "Monthly Returns for Securities A, B, C, and S&P", 
       x = "Month", y = "Returns (%)", color = "Security") +
  theme_minimal()
# Single Index Models
