library(tidyverse)

log_folder <- here::here("data/team_logs")

test_file <- list.files(log_folder)[[1]]

file_path <- paste0(log_folder, "/", test_file)

first_tbl <- readr::read_csv(file_path, skip = 1)

table_update <- first_tbl %>%
    dplyr::select(
        !c("...1", "...3")
    ) %>%
    dplyr::filter(
        !(
            Name %in% c(
                "Totals",
                "Player Note",
                "O",
                "No new player Notes",
                "IR-LT",
                "IR-NR",
                "NA",
                "Name",
                "New Player Note"
            )
        )
    ) %>%
    tidyr::drop_na(Name)
