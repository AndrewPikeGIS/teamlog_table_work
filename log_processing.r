pkgload::load_all()

log_folder <- "data/team_logs"

test_file <- list.files(log_folder)[[1]]

file_path <- paste0(log_folder, "/", test_file)

first_tbl <- readr::read_csv(file_path, skip = 1)



table_update <- create_clean_log_table(first_tbl)

forwards_table <- create_forwards_table(table_update)

goalie_table <- create_goalie_table(table_update)
