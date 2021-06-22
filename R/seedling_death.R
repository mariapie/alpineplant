
#' @title random_death
#'
#' create a function that randomly selct the seedling who has to die
#'
#' @description kills plt for the seedling info data frame
#' @param df dataframe
#' @param num_deaths
#'
#' @return
#' @export
#'
#' @examples
#' info_transform_seedling()
random_death <- function(df, num_deaths) {

  deaths <- df %>%
    dplyr::filter(alive == 1) %>%
    dplyr::slice(sample(nrow(.), num_deaths)) %>%
    purrr::pluck("plt")
  df_tmp <- df[df$plt %in% deaths,] %>%
    dplyr::mutate(alive = 0)
  df <- dplyr::bind_rows(df %>% filter(!plt %in% deaths),
                  df_tmp) %>%
    dplyr::arrange(site, year, plt)

  return(df)

}
