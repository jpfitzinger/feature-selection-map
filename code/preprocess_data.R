# ------------------------------------------------------------------------------
# Preprocess data
#
# Create target variable, standardize and create bootstrap samples
#
# Author: Johann Pfitzinger
# Date: 2023-05-11
# ------------------------------------------------------------------------------

preprocess_data <- function(data, target_var, nlead = 1, ngroups = 10, nboot = 10) {

  data_path <- glue::glue("results/{EXPERIMENT_ID}/data/model_data_{target_var}_l{nlead}.rds")

  if (file.exists(data_path)) {
    saved_obj <- readRDS(data_path)
    if (saved_obj$match_call == match.call()) return(saved_obj$data)
  }

  data <- data %>%
    arrange(date) %>%
    mutate(target = !! rlang::sym(target_var)) %>%
    mutate(target = lead(target, nlead)) %>%
    relocate(target, .after = date) %>%
    drop_na %>%
    arrange(date) %>%
    mutate(groups = ntile(1:n(), ngroups)) %>%
    mutate_at(vars(-date, -target, -groups), function(x) BBmisc::normalize(x))

  set.seed(123)
  data <- data %>%
    mutate(yr = lubridate::year(date)) %>%
    rsample::group_bootstraps(times = nboot, group = yr)

  saveRDS(list(data = data, match_call = match.call()), data_path)

  return(data)
}
