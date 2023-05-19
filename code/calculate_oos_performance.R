# ------------------------------------------------------------------------------
# OOB Performance
#
# Calculates the out-of-sample performance (MSE) of the fitted feature selection
# vector using a linear regression.
#
# Author: Johann Pfitzinger
# Date: 2023-05-11
# ------------------------------------------------------------------------------

calculate_oos_performance <- function(data_train, data_test, vars) {

  mod <- data_train %>%
    select(all_of(c("target", as.character(vars)))) %>%
    lm(target ~ ., data = .)

  pred <- predict(mod, data_test)
  MSE <- mean((pred - data_test$target)^2)

  return(MSE)

}
