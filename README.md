
<!-- README.md is generated from README.Rmd. Please edit that file -->

This repository provides code to replicate the feature selection
workflow published [here](). The analysis was created using `R 4.2.1`.

``` r
# Data handling
library(tidyverse)   # 1.3.2 

# Modeling
library(tidyfit)     # 0.6.3

# Plotting
library(ggpubr)      # 0.5.0
library(igraph)      # 1.3.5
library(ggnetwork)   # 0.5.12

progressr::handlers(global = TRUE)
list.files("code", full.names = TRUE) %>% walk(source)
```

# Set up a new experiment

Running all models takes a long time. It is advisable when replicating
to use only one `TARGET_FEATURE_NAME` and/or `MODEL_SIZE`. Estimated
feature selection results are stored in `results/{EXPERIMENT_ID}/` and
interrupting the calculation does not require rerunning all models.

``` r
# Name of directory to store results
EXPERIMENT_ID <- "feat_sel"

TARGET_FEATURE_NAMES <- c("INDPRO", "UNRATE", "S&P 500", "CPIAUCSL")
MODEL_SIZES <- c(10, 20, 30)
BOOTSTRAPS <- 100
```

``` r
create_experiment(EXPERIMENT_ID)
```

# Initalize algorithms

The algorithms are initialized in `init_algorithms`. The analysis makes
use the `tidyfit` package to fit models. Algorithms are initialized
using `m()`. Here is a snippet:

``` r
model_list[["Correlation"]] <- m("cor")
model_list[["MRMR"]] <- m("mrmr", feature_count = nvars)
model_list[["FS (AIC)"]] <- m("subset", method = "forward", nvmax = nvars, IC = "AIC")
model_list[["Lasso"]] <- m("lasso", dfmax = nvars - 1)
```

The algorithms need to be fitted for each `MODEL_SIZE`,
`TARGET_FEATURE_NAME` and `BOOTSTRAP`. The number of leading periods
(months) can also be configured here.

``` r
grid_df <- expand_grid(
  models = unlist(map(MODEL_SIZES, init_algorithms), recursive = FALSE),
  target_name = TARGET_FEATURE_NAMES,
  nbootstraps = BOOTSTRAPS,
  nleads = 1
) 

grid_df <- unnest(grid_df, models)
```

# Fit models

This is the core of the analysis. Have a look at `estimate_model` to
understand the workflow under the hood. The core is the
`tidyfit::regress` function, which fits models to data:

``` r
regress(df, target ~ ., model, .mask = c("date", "groups", "yr"), 
        .cv = cv, .cv_args = list(group = "groups"))
```

Use `pmap` to fit models for each combination specified above:

``` r
node_df <- pmap_dfr(grid_df, estimate_model)
```

# Calculate Jaccard-AUC score for graph edges

This operation needs to iterate over combinations of many fitted feature
selection vectors and is consequently quite slow.

``` r
edge_df <- calculate_edges(node_df)
```

# Plot results

The below plot shows the similarity of the algorithms, as well as
out-of-bootstrap relative performance and diversity of selected
information. Refer to the [article]() for a description. The plot uses
`ggnetwork` and `igraph` to generate a graph layout.

``` r
edge_df %>% 
  # Select one setting to plot
  filter(str_attr(name.x, "nvars") == 10) %>%
  filter(str_attr(name.x, "target_name") == "INDPRO") %>%
  # Generate the plot
  composite_similarity_plot(node_df, 0.75, seed = 123)
```
