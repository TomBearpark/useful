
#' Add leads and lags to a datafranme, with panel ID variables
#' id and date
#'
#' @param df
#' @param vars
#' @param max.p
#' @param lags
#' @param add_leads
#' @param sort_df
#'
#' @return
#' @export
#'
#' @examples
add_lags <- function(df, vars, max.p = 3, lags = 4, add_leads = FALSE,
                     sort_df = TRUE){

  if(sort_df) {
    df <- df %>% dplyr::arrange(id, date) %>% dplyr::group_by(id)
  }else{
    print("you better have pre-sorted and arranged these data!")
  }

  print(paste0("grouped by: ", paste(dplyr::group_vars(df),
                                     collapse = " and ")))

  for(pp in 1:max.p) for (var in vars) for (lag in 0:lags) {
    df <- df %>%
      dplyr::mutate(!!paste0("l", lag, "_", var, pp) :=
               dplyr::lag(.data[[paste0(var, pp)]], lag))

    if(add_leads){
      df <- df %>%
        dplyr::mutate(!!paste0("f", lag, "_", var, pp) :=
                 dplyr::lead(.data[[paste0(var, pp)]], lag))
    }

  }
  df <- dplyr::ungroup(df)
  df
}
