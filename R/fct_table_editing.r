create_clean_log_table <- function(table_in) {
    table_out <- table_in %>%
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
        tidyr::drop_na(Name) %>%
        tidyr::separate_wider_delim(
            .,
            Name,
            delim = " - ",
            names = c("Name", "Position")
        ) %>%
        dplyr::mutate(
            center = dplyr::case_when(
                stringr::str_detect(Position, "C") ~ 1,
                TRUE ~ 0
            ),
            lw = dplyr::case_when(
                stringr::str_detect(Position, "LW") ~ 1,
                TRUE ~ 0
            ),
            rw = dplyr::case_when(
                stringr::str_detect(Position, "RW") ~ 1,
                TRUE ~ 0
            ),
            defence = dplyr::case_when(
                stringr::str_detect(Position, "D") ~ 1,
                TRUE ~ 0
            ),
            Position = stringr::str_replace_all(Position, "[^[:alnum:]]", ""),
            multi_position = dplyr::case_when(
                stringr::str_length(Position) >= 3 ~ 1,
                TRUE ~ 0
            )
        )
    return(table_out)
}

create_goalie_table <- function(table_in) {
    goalies_table <- table_in %>%
        dplyr::filter(
            Position == "G"
        ) %>%
        dplyr::rename(
            gp = `GP*`,
            wins = G,
            GA = A,
            GAA = `+/-`,
            saves = PIM,
            shots_against = PPP,
            sv_percent = SOG,
            shutout = HIT
        ) %>%
        dplyr::select(
            !c(BLK, center, lw, rw, defence, defence, multi_position)
        )
    return(goalies_table)
}


create_forwards_table <- function(table_in) {
    forward_table <- table_in %>%
        dplyr::filter(
            Position != "G"
        )
    return(forward_table)
}


create_list_of_clean_logs <- function(file, folder) {
    file_path <- paste0(folder, "/", file)
}
