library(wooldridge)
library(tidyverse)
library(sandwich)
library(lmtest)
library(plm)
library(broom)

pvcov <- \(x) plm::vcovHC(x, method = "arellano", type = "HC1")


mod_pool <- lm(crmrte ~ d87 + unem, data = db)
summary(mod_pool)

# OLS
par <- mod_pool |>
  lmtest::coeftest() |>
  broom::tidy()

# OLS with robust SEs
par_robust <- mod_pool |>
  lmtest::coeftest(vcov = vcovHC, type = "HC1") |>
  broom::tidy()

## Bondad de ajuste y contraste F
gof <- mod_pool |>
  lmtest::coeftest(save = TRUE) |>
  broom::glance()

## Contraste F no funciona con errores típicos robustos.
gof_robust <- mod_pool |>
  lmtest::coeftest(vcov = vcovHC, type = "HC1", save = TRUE) |>
  broom::glance()


db <- crime2 |>
  mutate(city = rep(1:(NROW(crime2) / 2), each = 2)) |>
  select(city, year, crmrte, unem, d87) |>
  pdata.frame(index = c("city", "year"),
              drop.index = FALSE, row.names = FALSE)

head(db, n = 10)

mod_t87 <- lm(crmrte ~ unem, data = db,
              subset = year == 87)
summary(mod_t87)

tidy(mod_t87)
glance(mod_t87)




mod_fd <- plm(crmrte ~ unem, data = db, model = "fd")
summary(mod_fd)
summary(mod_fd, vcov = pvcov)

tidy(mod_fd)


mod_fe <- plm(crmrte ~ d87 + unem,
              data = db, model = "within")
summary(mod_fe)
summary(mod_fe, vcov = pvcov)



