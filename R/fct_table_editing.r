create_clean_log_table_2023 <- function(table_in, file_name) {
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
            ),
            manager = stringr::str_split_1(file_name, "_")[1],
            year = get_year_from_file(file_name),
            Name = stringr::str_sub(Name, 1, stringr::str_length(Name)-3)
        )
    return(table_out)
}

get_year_from_file <- function(file_name){
    year_w_ext = stringr::str_split_1(file_name, "_")[2]
    year_str = stringr::str_remove_all(year_w_ext, ".csv")
    return(year_str)
}

create_clean_log_table_pre2023 <- function(table_in, file_name) {
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
        dplyr::mutate(
            manager = stringr::str_split_1(file_name, "_")[1],
            year = get_year_from_file(file_name)
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
            !c(
                BLK,
                center,
                lw,
                rw,
                defence,
                GWG,
                multi_position
            )
        ) %>%
        dplyr::mutate(
            saves = as.numeric(saves),
            shots_against = as.numeric(shots_against),
            sv_percent = as.numeric(sv_percent),
            GAA = as.numeric(GAA),
            gp = as.numeric(gp),
            wins = as.numeric(wins)
        ) %>%
        dplyr::mutate(
            sv_percent = saves/shots_against
        ) %>%
        dplyr::arrange(
            year,
            manager
        )
    return(goalies_table)
}


create_forwards_table <- function(table_in) {
    forward_table <- table_in %>%
        dplyr::filter(
            Position != "G"
        ) %>%
        dplyr::arrange(year, manager)
    return(forward_table)
}


create_list_of_clean_logs <- function(file, folder) {
    file_path <- paste0(folder, "/", file)
    tbl_in <- readr::read_csv(file_path, skip = 1)
    print(file_path)
    if (stringr::str_detect(file_path, "2023" )[1]) {
        clean_log <- create_clean_log_table_2023(tbl_in, file)
    } else {
        clean_log <- create_clean_log_table_pre2023(tbl_in, file)
    }
    return(clean_log)
}


create_clean_positions <- function(file, folder){
    file_path <- paste0(folder, "/", file)
    tbl_in <- readr::read_csv(file_path)
    clean_positions <- clean_position_table(tbl_in)
    return(clean_positions)
}

clean_position_table<- function(table_in) {

    if (c("PLAYER (TEAM)") %in% colnames(table_in)){
        table_in <- table_in %>%
            dplyr::rename(
                "Name" = "PLAYER (TEAM)"
            )
    }

    if (c("PLAYER") %in% colnames(table_in)){
        table_in <- table_in %>%
            dplyr::rename(
                "Name" = "PLAYER"
            )
    }

    if (c("Player") %in% colnames(table_in)){
        table_in <- table_in %>%
            dplyr::rename(
                "Name" = "Player"
            )
    }

    if (c("POS.") %in% colnames(table_in)){
        table_in <- table_in %>%
            dplyr::rename(
                "Position" = "POS."
            )
    }

    if (!any(c("Position") %in% colnames(table_in))){
        table_out <- table_in %>%
            tidyr::separate_wider_delim(
                .,
                Name,
                "(",
                names = c("Name", "Team")
            ) %>%
            tidyr::separate_wider_delim(
                .,
                Team,
                "-",
                names = c("Team", "Position")
            ) %>%
            dplyr::mutate(
                Team = stringr::str_trim(Team),
                Position = stringr::str_trim(Position),
                Name = iconv(Name, "UTF-8", "UTF-8", sub = "")
            ) %>%
            dplyr::mutate(
                Position = stringr::str_remove(Position, "[)]")
            ) %>%
            dplyr::mutate(
                Position = stringr::str_remove_all(Position, ",F")
            ) %>%
            dplyr::select(
                Name,
                Position,
                year
            )
        return(table_out)
    } else {
        table_out <- table_in %>%
            dplyr::mutate(
                Position = stringr::str_replace_all(Position, "[/]", ",")
            ) %>%
            dplyr::select(
                Name,
                Position,
                year
            )
    }
    return(table_out)
}

calc_position_fields <- function(table_in){
    table_out <- table_in %>%
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
            multi_position = dplyr::case_when(
                stringr::str_length(Position) >= 3 ~ 1,
                TRUE ~ 0
            )
        )

    return(table_out)
}