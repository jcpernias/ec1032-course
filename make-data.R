if (!dir.exists("data"))
  dir.create("data")

library(wooldridge)
library(dplyr)
library(writexl)

## Para ejemplo de datos de panel
b <- crime2 |>
  mutate(city = rep(1:(NROW(crime2) / 2), each = 2)) |>
  select(city, year, crmrte, unem, d87) |>
  rename(crime_rate = crmrte) |>
  write_xlsx("data/crime2.xlsx")

