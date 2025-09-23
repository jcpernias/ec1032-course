library(wooldridge)
library(plm)
library(tidyverse)

pvcov <- \(x) plm::vcovHC(x, method = "arellano", type = "HC1")

crime2$city <- rep(1:(NROW(crime2) / 2), each = 2)

db <- crime2 |>
  mutate(city = rep(1:(NROW(crime2) / 2), each = 2)) |>
  select(city, year, crmrte, unem, d87) |>
  pdata.frame(index = c("city", "year"),
              drop.index = FALSE, row.names = FALSE)

head(db, n = 10)

mod_t87 <- lm(crmrte ~ unem, data = db,
              subset = year == 87)
summary(mod_t87)


mod_pool <- lm(crmrte ~ d87 + unem, data = db)
summary(mod_pool)



mod_fd <- plm(crmrte ~ unem, data = db, model = "fd")
summary(mod_fd)
summary(mod_fd, vcov = pvcov)

mod_fe <- plm(crmrte ~ d87 + unem,
              data = db, model = "within")
summary(mod_fe)
summary(mod_fe, vcov = pvcov)



