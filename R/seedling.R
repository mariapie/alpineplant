#' @title create seedling cleaned database
#'
#' create seedling cleaned database
#'
#' @description transforms the seedling info data frame into the data cleaned format
#' @param df_info initial database
#' @param year_dependency allows for plants generation taking into account previous year
#'
#' @return cleaned database that i will use .. ???
#' @export

info_transform_seedling <- function(df_info, year_dependency = FALSE) {


  df_info <- df_info %>%
   dplyr::select("site", "year", "SSsd", "NSsd", "NDsd") %>%
   dplyr::arrange(site, year) %>% #arrange rows by variables
   dplyr::mutate_at(.vars = c("SSsd", "NSsd", "NDsd"), .funs = as.numeric) #.vars is a list of columns

  sites <- df_info$site %>% unique() #duplicate elements/rows removed

  df_results <- vector("list", length(sites)) %>%
    purrr::set_names(sites)

  for (s in sites) {

    cat(paste("\nTrasforming info for site", s, "...\n\n"))

    df_tmp <- df_info %>%
      dplyr::filter(site == s) %>%
      dplyr::slice(-n())
    years <- df_tmp$year %>% unique()
    ss_old <- ns_old <- nd_old <- max_id_old <- NULL

    for (y in seq_along(years)) {

      values_tmp <- df_tmp %>%
        dplyr::filter(year == years[y]) %>%
        dplyr::select(-site, -year)
      ss_tmp <- values_tmp$SSsd
      ns_tmp <- values_tmp$NSsd
      nd_tmp <- values_tmp$NDsd

      if (ss_tmp != sum(ns_tmp, nd_tmp)) {
        stop(paste("NS + ND not equal to SS for site", s, "and year", years[y], ". Please check the database."), call. = FALSE)
      }

      if (years[y] == years[1]) { # se primo anno

        res_tmp <- tibble::tibble(
          "site" = rep(s, ss_tmp),
          "plt" = paste0("id_", 1:ss_tmp),
          "year" = rep(years[y], ss_tmp),
          "alive" = c(rep(1, ns_tmp), rep(0, nd_tmp))
        )

        res <- res_tmp

      } else {# dal secondo anno in poi


        if (year_dependency) { # dipendenza tra un anno e l'altro nella generazione delle piante

          res_tmp <- res %>%
            dplyr::filter(year == years[y - 1] & alive == 1) %>%
            dplyr::mutate(year = years[y])

          if (nd_tmp > ns_old) {
            message(paste("Site", s, ": ND of year", years[y], "is greater than NS of year", years[y - 1],
                          ".\nSkipping ND for this year. Please check the database and correct the ND value."))
          } else if (nd_tmp > 0) { # ammazzo piante dell'anno prima in modo random
            set.seed(123)
            res_tmp <- random_death(df = res_tmp, num_deaths = nd_tmp)
          }

          # aggiungo la differenza delle piante sopravvissute
          ns_diff <- ns_tmp - sum(res_tmp$alive)
          if (ns_diff > 0) {
            res_tmp <- dplyr::bind_rows(
              res_tmp,
              tibble::tibble(
                "site" = rep(s, ns_diff),
                "plt" = paste0("id_", (max_id_old + 1):(max_id_old + ns_diff)),
                "year" = rep(years[y], ns_diff),
                "alive" = rep(1, ns_diff)
              )
            )
          }

          res <- dplyr::bind_rows(res, res_tmp)

        } else {# indipendenza tra gli anni nella generazione delle piante

          res_tmp <- tibble::tibble(
            "site" = rep(s, ss_tmp),
            "plt" = paste0("id_", (max_id_old + 1):(max_id_old + ss_tmp)),
            "year" = rep(years[y], ss_tmp),
            "alive" = c(rep(1, ns_tmp), rep(0, nd_tmp))
          )

          res <- dplyr::bind_rows(res, res_tmp)

        }


      }

      ss_old <- ss_tmp
      ns_old <- ns_tmp
      nd_old <- nd_tmp
      max_id_old <- res$plt %>% unique() %>% stringr::str_extract("\\d+") %>% as.numeric() %>% max()

    }

    df_results[[s]] <- res

  }

  df_results <- dplyr::bind_rows(df_results) %>%
    dyplr::mutate(trans = paste0(site, "_", plt)) %>%
    dyplr::select(all_of(c("site", "trans", "plt", "year", "alive"))) %>%
    dyplr::mutate_all(as.character)

  return(df_results)

}
