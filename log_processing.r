pkgload::load_all()

log_folder <- "data/team_logs"

file_list <- list.files(log_folder)

list_clean_log <- lapply(
    file_list,
    create_list_of_clean_logs,
    folder = log_folder
)


list_forwards_tables <- lapply(list_clean_log, create_forwards_table)
list_goalie_tables <- lapply(list_clean_log, create_goalie_table)


all_fwd_and_def <- dplyr::bind_rows(list_forwards_tables)
