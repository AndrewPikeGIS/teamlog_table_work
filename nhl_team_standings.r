pkgload::load_all()

folder_path <- "data/nhl_standings"

nhl_standins <- load_and_clean_nhl_standings(folder_path)

readr::write_csv(nhl_standins, "output/nhl_standings.csv")
