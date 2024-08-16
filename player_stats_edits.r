pkgload::load_all()

stats_folder <- "data/player_stats"

list_stats_files <- list.files(stats_folder)

list_clean_stats <- lapply(
    list_stats_files,
    load_and_clean_player_stats,
    folder = stats_folder
)

all_stats_clean <- dplyr::bind_rows(list_clean_stats)

readr::write_csv(all_stats_clean, "output/player_stats_full.csv")
