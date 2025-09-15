library(wooldridge)
library(lmtest)
library(sandwich)
library(car)

source("regr_table.r")

mod1 <- lm(lwage ~ educ + exper + female, data = cps78_85)
regr_table(mod1)


regr_table(mod1, vcov = "HC2")

coeftest(mod1, vcov = vcovHC, type = "HC2")

mod2 <- update(mod1, . ~ . + y85)
coeftest(mod2)

mod3 <- update(mod1, . ~ . + y85 * (female + educ))
coeftest(mod3, vcov = vcovHC, type = "HC2")

modelsummary(list(mod1, mod2, mod3),
             stars = c('*' = 0.1, '**' = 0.05, '***' = 0.01),
             gof_omit = "AIC|BIC|Log.Lik.|RMSE",
             vcov = "HC2")

mod4 <- update(mod1, . ~ . * y85)
lht(mod4, matchCoefs(mod4, ":y85"), vcov = vcovHC, type = "HC2")

lht(mod4, matchCoefs(mod4, "y85"), vcov = vcovHC, type = "HC2")
