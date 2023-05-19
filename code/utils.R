# ------------------------------------------------------------------------------
# Various utility functions
#
# Author: Johann Pfitzinger
# Date: 2023-05-11
# ------------------------------------------------------------------------------

str_attr <- function(str, name) {
  attr_index <- c("model", "target_name", "nleads", "nvars", "boot_id")
  str_split(str, "__", simplify = TRUE)[, which(attr_index == name)]
}

top_by <- function(data, nvars, col = NULL) {
  if (!is.null(col)) {
    data <- data %>%
      rename(filter_col = !!col) %>%
      filter(rank(-abs(filter_col))<=nvars)
  }
  return(data$term)
}

get_lscore <- function(df, nvars) {
  dat <- df %>%
    arrange(date) %>%
    select(-date, -target, -groups, -yr)

  mod <- Rdimtools::do.lscore(as.matrix(dat))

  colnames(dat)[rank(abs(mod$lscore)) <= nvars]

}

get_fosmod <- function(df, nvars) {
  dat <- df %>%
    arrange(date) %>%
    select(-date, -target, -groups, -yr)

  mod <- Rdimtools::do.fosmod(as.matrix(dat), ndim = nvars)

  colnames(dat)[mod$featidx]

}

entr <- function(vec) {
  vec <- abs(vec) / sum(abs(vec))
  vec <- vec + 1e-8
  n <- length(vec)
  return(exp(-sum(vec * log(vec))) / n)
}

cluster_variables <- function(variables, data, nclusters) {

  data_mat <- data %>%
    select(-any_of(c("date", "target", "groups", "yr"))) %>%
    data.matrix

  variable_names <- colnames(data_mat)

  selection_vector <- rep(F, length(variable_names))
  names(selection_vector) <- variable_names
  selection_vector[variables] <- T

  clust <- data_mat %>%
    cor %>%
    dist %>%
    hclust %>%
    cutree(nclusters)

  clustered_vector <- rep(0, length(selection_vector))

  for (cl in 1:nclusters) {
    clustered_vector[clust == cl] <- any(selection_vector[clust == cl])
  }

  return(clustered_vector)

}
