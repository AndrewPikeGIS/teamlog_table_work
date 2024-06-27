pkgload::load_all()

log_folder <- "data/team_logs"

file_list <- list.files(log_folder)



table_update <- create_clean_log_table(first_tbl)

forwards_table <- create_forwards_table(table_update)

goalie_table <- create_goalie_table(table_update)

list_clean_log <- lapply(
    file_list,
    create_list_of_clean_logs,
    folder = log_folder
)

View(list_clean_log[[1]])

list_forwards_tables <- lapply(list_clean_log, create_forwards_table)
list_goalie_tables <- lapply(list_clean_log, create_goalie_table)


all_fwd_and_def <- dplyr::bind_rows(list_forwards_tables)
