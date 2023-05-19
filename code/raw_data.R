# ------------------------------------------------------------------------------
# Load raw data
#
# FRED-MD data is loaded using the fbi package
#
# Author: Johann Pfitzinger
# Date: 2023-05-11
# ------------------------------------------------------------------------------

INPUTPATH <- "https://files.stlouisfed.org/files/htdocs/fred-md/monthly/2022-09.csv"

raw_data <- function(INPUTPATH) {

  data_path <- glue::glue("results/{EXPERIMENT_ID}/data/raw_data.rds")

  if (file.exists(data_path)) {
    saved_obj <- readRDS(data_path)
    if (saved_obj$match_call == match.call()) return(saved_obj$data)
  }

  data <- as_tibble(fbi::fredmd(INPUTPATH,
                                date_start = NULL,
                                date_end = NULL,
                                transform = TRUE))

  # Note that PMI variables are not included in the public data set and need to be added manually

  # data_pmi <- readxl::read_excel("data/pmis_vals.xlsx") %>%
  #   rename(date = ...1) %>%
  #   mutate(date = lubridate::floor_date(as.Date(date), unit = "month"))
  # renam_pmi <- c(NAPMPROD = "NAPMPI", NAPMEMPL = "NAPMEI", NAPMPMI = "NAPM", NAPMNEWO = "NAPMNOI",
  #                NAPMSUPL = "NAPMSDI", NAPMINV = "NAPMII", NAPMPRIC = "NAPMPRI")
  # colnames(data_pmi) <- c("date", renam_pmi[colnames(data_pmi)[-1]])
  # data <- data %>% left_join(data_pmi)

  saveRDS(list(data = data, match_call = match.call()), data_path)

  return(data)

}
