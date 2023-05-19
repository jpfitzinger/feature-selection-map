# ------------------------------------------------------------------------------
# Factor entropy
#
# Calculates the effective number of principal components contained in a model
# selection vector using an entropy approach.
#
# Author: Johann Pfitzinger
# Date: 2023-05-11
# ------------------------------------------------------------------------------

calculate_factor_entropy <- function(data, vars) {

  df <- data %>%
    select(-any_of(c("date", "target", "groups", "yr")))

  variable_names <- colnames(df)

  selection_vector <- rep(F, length(variable_names))
  names(selection_vector) <- variable_names
  selection_vector[vars] <- T

  df_mat <- df %>%
    data.matrix %>%
    scale
  pca_rot <- df_mat %>%
    cov %>%
    whitening::whiteningMatrix("PCA")

  return(entr(selection_vector %*% t(pca_rot)))

}
