pkgload::load_all()

position_log_folder <- 'data/player_elligibility'
log_folder <- "data/team_logs"

position_file_list <- list.files(position_log_folder)
file_list <- list.files(log_folder)


#add positionality to all players by year.

clean_player_position <- lapply(
    position_file_list,
    create_clean_positions,
    position_log_folder
)

test <- list_clean_log[[5]]
test2 <- list_clean_log[[6]]
