# ------------------------------------------------------------------------------
# Model Estimation
#
# Perform feature selection using an initialized tidyfit::m() model
# as well as additional settings.
#
# Author: Johann Pfitzinger
# Date: 2023-05-11
# ------------------------------------------------------------------------------

estimate_model <- function(
    name,
    model,
    target_name,
    nvars,
    nleads,
    nbootstraps
  ) {

  mod_path <- glue::glue("results/{EXPERIMENT_ID}/estimates/{name}_{target_name}_n{nvars}_l{nleads}.rds")

  if (file.exists(mod_path)) {
    saved_obj <- readRDS(mod_path)
    if (saved_obj$match_call == match.call()) return(saved_obj$mod)
  }

  df_raw <- raw_data(INPUTPATH)
  df_mod <- df_raw %>%
    preprocess_data(target_var = target_name, nlead = nleads, nboot = nbootstraps)

  variable_names <- colnames(select(df_raw, -any_of("date")))

  out <- df_mod %>%
    pmap_dfr(function(splits, id) {
      df <- rsample::training(splits)
      print(glue::glue("{name} - {id}"))

      cv <- "group_vfold_cv"
      # Custom rules
      if (name == "InfoGain") {
        df <- df %>%
          mutate(target = as.factor(ntile(target, 10)))
      }
      if (name %in% c("PCR", "PLSR", "RF")) cv <- "none"

      if (name == "LaPlacian") {
        vars <- get_lscore(df, nvars)
      } else if (name == "FOS-MOD") {
        vars <- get_fosmod(df, nvars)
      } else if (name == "GA") {
        vars <- get_ga(df, nvars)
      } else {
        mod <- regress(df, target ~ ., model, .mask = c("date", "groups", "yr"), .cv = cv, .cv_args = list(group = "groups"))
        vars <- select_variables(mod, nvars)
        vars <- post_process(mod, vars)
      }

      performance_scores <- calculate_oos_performance(rsample::training(splits), rsample::testing(splits), vars)
      factor_entropy_scores <- calculate_factor_entropy(rsample::training(splits), vars)

      min_clusters <- 10
      max_clusters <- ncol(df_raw) - 1

      out <- map_dfr(min_clusters:max_clusters, function(ncl) {
        selection_vector <- cluster_variables(vars, df, ncl)
        tibble(model = name,
               target_name = target_name,
               nleads = nleads,
               nvars = nvars,
               boot_id = id,
               nclusters = ncl,
               vars = list(selection_vector),
               oob_performance = performance_scores,
               factor_entropy = factor_entropy_scores)
      })

      return(out)

    })

  saveRDS(list(mod = out, match_call = match.call()), mod_path)

  return(out)

}
