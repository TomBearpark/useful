
#' Title: Extract lagged resposne function values separately for each lag
#'
#' @param m
#' @param leads number of leads in the model
#' @param lags number of lags in the model
#' @param var
#' @param min
#' @param max
#' @param ref
#' @param ci_level
#' @param step.length
#' @param id.col
#' @param xvar_name
#'
#' @return
#' @export
#'
#' @examples
#'
predict_lags_separately <- function(m, var, min, max, ref,
                                    leads, lags,
                                    ci_level = 95,
                                    step.length = 1, id.col = NULL,
                                    xvar_name = "temp"){
  purrr::map_dfr(
    (-leads):lags,
    function(ll){

      ltag <- dplyr::if_else(ll >= 0, "l", "f")
      var  <- paste0(ltag, abs(ll), "_", var)

      useful::predict_poly(m.poly.lags, var,
                           min = min, max = max, ref = ref,
                           ci_level = ci_level, step.length = step.length,
                           id.col = id.col, xvar_name = xvar_name) %>%
        dplyr::mutate(lag = !!ll)
    }
  )
}


