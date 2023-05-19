# Creates the empty directory structure for a new experiment

create_experiment <- function(EXPERIMENT_ID) {

  dir.create(glue::glue("results/{EXPERIMENT_ID}/data"), recursive = TRUE, showWarnings = FALSE)
  dir.create(glue::glue("results/{EXPERIMENT_ID}/estimates"), recursive = TRUE, showWarnings = FALSE)
  dir.create(glue::glue("results/{EXPERIMENT_ID}/plots"), recursive = TRUE, showWarnings = FALSE)

}
