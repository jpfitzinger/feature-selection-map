# Post processing functions for the feature selection vector

post_process <- function(model, vars) {

  # Ensure glmnet methods contain at least one variable
  if ((model$model %in% c("lasso", "enet", "adalasso")) & (length(vars)==0)) {
    beta_mat <- as.matrix(model$model_object[[1]]$object$beta)
    beta_mat <- beta_mat[, apply(beta_mat, 2, function(x) sum(x!=0) >= 1)]
    vec <- beta_mat[,1]
    vars <- names(vec)[vec!=0]
    vars <- str_replace_all(vars, "`", "")
  }

  return(vars)

}
