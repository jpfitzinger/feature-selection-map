# ------------------------------------------------------------------------------
# Plotting
#
# Author: Johann Pfitzinger
# Date: 2023-05-11
# ------------------------------------------------------------------------------

composite_similarity_plot <- function(edge_df, node_df, threshold = 0, seed = 123) {

  set.seed(seed)

  data <- edge_df %>%
    drop_na %>%
    mutate(weight = weight + 1e-6) %>%
    mutate(joined_name = as.character(map2(name.x, name.y, function(a, b) {paste(sort(c(a,b)), collapse = "___")}))) %>%
    distinct(joined_name, .keep_all = TRUE) %>%
    select(-joined_name) %>%
    mutate(weight_type = ifelse(str_attr(name.x, "model") == str_attr(name.y, "model"), "within", "between")) %>%
    group_by(weight_type) %>% mutate(qtile = quantile(weight, threshold)) %>% ungroup %>%
    filter((weight > qtile) | (weight_type == "within")) %>%
    mutate(weight = weight)

  network <- graph_from_data_frame(data, directed = F)
  l <- layout.fruchterman.reingold(network)

  network_df <- ggnetwork(network, layout = l) %>%
    as_tibble %>%
    drop_na

  node_df <- node_df %>%
    select(-nclusters, -vars) %>%
    distinct %>%
    group_by(boot_id, target_name, nleads, nvars) %>%
    mutate(oob_performance = rank(-oob_performance)/n())

  network_mean_df <- network_df %>%
    mutate(name = as.character(map(str_split(name, "__"), function(x) paste(x[-5], collapse = "__")))) %>%
    group_by(name) %>%
    summarize(across(c(x, y), mean)) %>%
    mutate(name = str_attr(name, "model")) %>%
    drop_na

  network_df <- network_df %>%
    mutate(name = str_attr(name, "model"))

  color_map <- methods_df$color
  names(color_map) <- methods_df$method

  p_main <- ggplot(network_df) +
    aes(x=x, y=y, color=name) +
    geom_point(alpha = 0.5, size = .05) +
    stat_ellipse(aes(fill=name), size = .1, alpha=.05, geom="polygon", type = "norm") +
    geom_point(size=2, data=network_mean_df) +
    ggrepel::geom_label_repel(aes(label=name, color=name), size = 3, data=network_mean_df)

  p_main <- ggpar(p_main, ggtheme = theme_bw(), legend = "none", xlab=F, ylab=F,
             title = "Feature Selection Map", subtitle = "Graph of feature selection behavior. Adjacent clusters select similar information.",
             font.title = c(10, "bold"), font.subtitle = 8)

  p_performance <- node_df %>%
    drop_na %>%
    group_by(model) %>%
    mutate(mean_perf = mean(oob_performance)) %>%
    ungroup %>%
    arrange(desc(mean_perf)) %>%
    mutate(model = factor(model, levels = unique(model))) %>%
    ggerrorplot("model", "oob_performance", desc_stat = "mean_ci", error.plot = "errorbar") %>%
    ggpar(ggtheme = theme_bw(), xlab = F, ylab = F,
          title = "Out-of-sample performance", subtitle="Inverse % rank of MSE",
          font.title=c(10, "bold"), font.subtitle = 8) +
    rotate_x_text(45)

  p_entropy <- node_df %>%
    drop_na %>%
    group_by(model) %>%
    mutate(mean_perf = mean(factor_entropy)) %>%
    ungroup %>%
    arrange(desc(mean_perf)) %>%
    mutate(model = factor(model, levels = unique(model))) %>%
    ggerrorplot("model", "factor_entropy", desc_stat = "mean_ci", error.plot = "errorbar") %>%
    ggpar(ggtheme = theme_bw(), xlab = F, ylab = F,
          title = "Diversity of information selected", subtitle="Effective % of orthogonal components selected",
          font.title = c(10, "bold"), font.subtitle = 8) +
    coord_flip() +
    scale_x_discrete(position = "top")

  grid1 <- cowplot::plot_grid(p_main, p_performance, ncol = 1, rel_heights = c(2.25, 1))
  grid2 <- cowplot::plot_grid(p_entropy, ggplot() + theme_void(), ncol = 1, rel_heights = c(2.25, 1))

  cowplot::plot_grid(grid1, grid2, ncol = 2, rel_widths = c(2.6, 1))

}
