pkgload::load_all()

position_log_folder <- 'data/player_elligibility'
log_folder <- "data/team_logs"

position_file_list <- list.files(position_log_folder)
file_list <- list.files(log_folder)

list_clean_log <- lapply(
    file_list,
    create_list_of_clean_logs,
    folder = log_folder
)

all_players <- dplyr::bind_rows(list_clean_log) %>%
    dplyr::mutate(
        year = as.double(year)
    )

#add positionality to all players by year.

clean_player_position <- lapply(
    position_file_list,
    create_clean_positions,
    position_log_folder
)

all_players_positions <- dplyr::bind_rows(clean_player_position) 

all_players_w_pos <- dplyr::rows_upsert(
    all_players,
    all_players_positions,
    by = c("Name", "year")
)
