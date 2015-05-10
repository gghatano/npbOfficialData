#' Get player stats data from NPB official page
#' @param batter profile page URL of NPB
#'
#' @return player data(data.frame)
#'
#' @export

getPlayerSeasonData <- function(profile_page_url = "http://bis.npb.or.jp/players/51953886.html"){

  profile_page_html = rvest::html(profile_page_url)

  player_name =
    profile_page_html %>%
    html_node(xpath='//*[@id="registerdivtitle"]/table/tr[2]') %>%
    html_text()

  year_stats_table = profile_page_html %>%
    html_node('#registermaintbl > table') %>%
    html_table

  year_stats_table %>%
    data.table::setnames(c("YEAR", "TEAM", "GAME",
                           "PA", "AB", "RUN",
                           "H", "DOUBLE", "TRIPLE", "HR", "BASE", "RBI", "STL",
                           "STLB", "SB", "SF", "BB", "DEAD", "SO", "DP", "AVG", "ISO", "OBP"))

  year_stats_df =
    year_stats_table %>%
    dplyr::mutate(SINGLE = H - DOUBLE - TRIPLE - HR) %>%
    mutate(NAME = player_name) %>%
    filter(TEAM != "通　算") %>%
    dplyr::select(YEAR, TEAM, NAME, PA, AB,
                  H, SINGLE, DOUBLE, TRIPLE, HR, BASE,
                  RUN, RBI, SO,
                  BB, DEAD, AVG)

  return(year_stats_df)
}
