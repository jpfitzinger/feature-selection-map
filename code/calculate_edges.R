# ------------------------------------------------------------------------------
# Calculate Jaccard-AUC metric for graph edges
#
# Author: Johann Pfitzinger
# Date: 2023-05-11
# ------------------------------------------------------------------------------

calculate_edges_by_clusters <- function(results_df) {
  intra_model_edges <- results_df %>%
    rename(vars.x = vars) %>%
    full_join(results_df %>% rename(vars.y = vars), by = c("model", "target_name", "nleads", "nvars", "nclusters"), multiple = "all") %>%
    filter(boot_id.x != boot_id.y) %>%
    mutate(distance = as.numeric(map2(vars.x, vars.y, function(vars.x, vars.y) {sum(vars.x * vars.y) / sqrt(sum(vars.x) * sum(vars.y))}))) %>%
    mutate(name.x = paste(model, target_name, nleads, nvars, boot_id.x, sep = "__")) %>%
    mutate(name.y = paste(model, target_name, nleads, nvars, boot_id.y, sep = "__"))

  inter_model_edges <- results_df %>%
    rename(vars.x = vars) %>%
    full_join(results_df %>% rename(vars.y = vars), by = c("target_name", "nleads", "nvars", "boot_id", "nclusters"), multiple = "all") %>%
    filter(model.x != model.y) %>%
    mutate(distance = as.numeric(map2(vars.x, vars.y, function(vars.x, vars.y) {sum(vars.x * vars.y) / sqrt(sum(vars.x) * sum(vars.y))}))) %>%
    mutate(name.x = paste(model.x, target_name, nleads, nvars, boot_id, sep="__")) %>%
    mutate(name.y = paste(model.y, target_name, nleads, nvars, boot_id, sep="__"))

  edge_df <- bind_rows(
    intra_model_edges %>% select(name.x, name.y, distance, nclusters),
    inter_model_edges %>% select(name.x, name.y, distance, nclusters)
  )

  return(edge_df)

}

calculate_edges <- function(results_df) {

  edge_df <- calculate_edges_by_clusters(results_df)

  edge_df <- edge_df %>%
    group_by(name.x, name.y) %>%
    mutate(distance = mean(distance)) %>%
    select(-nclusters) %>%
    distinct %>%
    ungroup

  edge_df <- edge_df %>%
    rename(weight = distance)

  return(edge_df)

}
