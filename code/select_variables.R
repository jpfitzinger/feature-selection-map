# ------------------------------------------------------------------------------
# Variable selection
#
# Function to select 'nvars' features by ranking features according to a score
# given in 'selection_column' for each method.
#
# Author: Johann Pfitzinger
# Date: 2023-05-11
# ------------------------------------------------------------------------------

selection_column <- list(
  subset = "statistic",
  cor  = "estimate",
  mrmr = "estimate",
  relief = "estimate",
  bma = "pip",
  blasso = "estimate",
  pcr = "statistic",
  plsr = "statistic",
  rf = "%IncMSE",
  boost = "estimate",
  gets = "statistic",
  spikeslab = "inc.prob"
)

select_variables <- function(model, nvars) {

  method <- model$model_object[[1]]$method
  coef_df <- coef(model) %>% filter(term != "(Intercept)") %>% unnest(model_info)
  vars <- top_by(coef_df, nvars, selection_column[[method]])
  return(vars)

}
