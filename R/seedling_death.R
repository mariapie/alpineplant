# random_death
# kills plt for the seedling info data frame
random_death <- function(df, num_deaths) {

  deaths <- df %>%
    filter(alive == 1) %>%
    slice(sample(nrow(.), num_deaths)) %>%
    pluck("plt")
  df_tmp <- df[df$plt %in% deaths,] %>%
    mutate(alive = 0)
  df <- bind_rows(df %>% filter(!plt %in% deaths),
                  df_tmp) %>%
    arrange(site, year, plt)

  return(df)

}
