install.packages("forecast")
library(tidyquant)
library(ggplot2)
library(forecast)
library(dplyr)
library(tidyr)

library(readxl)

file_path <- "D:/User/Desktop/Necessary/Financial Models and Content/Education Funding Work.xlsx"

df <- read_excel(file_path, sheet = 2)

head(df)

ggplot(df, aes(x = Year, y = `Dollars Outstanding (in billions)`)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "red") +
  geom_vline(xintercept = c(2009, 2017, 2021), linetype = "dashed", color = "black") +
  labs(title = "Federal Student Loan Debt (2007-2021)",
       x = "Year",
       y = "Debt Outstanding (in billions)") +
  theme_minimal()

df <- df %>%
  arrange(Year) %>%
  mutate(Growth_Rate = (`Dollars Outstanding (in billions)` / lag(`Dollars Outstanding (in billions)`) - 1) * 100)

print(df)

library(forecast)

ts_data <- ts(df$`Dollars Outstanding (in billions)`, start = 2007, frequency = 1)

model <- auto.arima(ts_data)

forecast_data <- forecast(model, h = 10)

autoplot(forecast_data) +
  labs(title = "Projected Student Loan Growth (2022-2031)",
       x = "Year",
       y = "Debt Outstanding (in billions)")
