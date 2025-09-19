library(wooldridge)
library(plm)
library(tidyverse)

crime2$city <- rep(1:(NROW(crime2) / 2), each = 2)

db <- crime2 |>
  mutate(city = rep(1:(NROW(crime2) / 2), each = 2)) |>
  select(city, year, crmrte, unem, d87) |>
  pdata.frame(index = c("city", "year"),
              drop.index = FALSE, row.names = FALSE)

head(db, n = 10)

mod_fd <- plm(crmrte ~ unem, data = db, model = "fd")
summary(mod_fd)

v_fd <- vcovHC(mod_fd, method = "arellano", type = "HC1")
summary(mod_fd, vcov = v_fd)

mod_fe <- plm(crmrte ~ d87 + unem,
              data = db, model = "within")
summary(mod_fe)

v_fe <- vcovHC(mod_fe, method = "arellano", type = "HC1")
summary(mod_fe, vcov = v_fe)



