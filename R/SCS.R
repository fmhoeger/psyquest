#' SCS
#'
#' This function defines a SCS module for incorporation into a
#' psychTestR timeline.
#' Use this function if you want to include the SCS in a
#' battery of other tests, or if you want to add custom psychTestR
#' pages to your test timeline.
#' For a standalone implementation of the SCS,
#' consider using \code{\link{SCS_standalone}()}.
#' @param label (Character scalar) Three uppercase letter acronym of the questionnaire.
#' This is also the label given to the results in the output file.
#' @param dict The psyquest dictionary used for internationalisation.
#' @param subscales (Character vector) The subscales to be included in the questionnaire.
#' When no subscales are provided all subscales are selected.
#' @param short_version (Boolean) For the short version of the questionnaire set to TRUE.
#' @param ... Further arguments to be passed to \code{\link{SCS}()}.
#' @export
SCS <- function(label = "SCS",
                dict = psyquest::psyquest_dict,
                subscales = c(),
                short_version = FALSE,
                ...) {
  stopifnot(purrr::is_scalar_character(label))

  main_test(
    label = label,
    items = get_items(label, subscales, short_version),
    short_version = short_version,
    offset = 1,
    arrange_vertically = TRUE,
    style = "min-width: 220px"
  )
}

postprocess_scs_short <- function(scores) {
  coefficients <-
    tibble::tribble(~a, ~b1, ~b2, ~b3,
                    2.094,-2.75,-1.178,0.811,
                    2.317,-2.431,-1.487,1.115,
                    2.355,-2.728,-1.771,0.607,
                    2.483,-2.92,-1.743,1.247,
                    2.128,-2.51,-1.093,0.862,
                    2.285,-2.475,-1.256,0.852,
                    2.159,-2.216,-1.211,0.841,
                    2.261,-2.797,-1.247,0.821)

  coefficients <- as.data.frame(coefficients)
  scores_map <- psyquest::scoring_maps[["SCS_short"]]
  theta <- catR::thetaEst(it = coefficients, x = scores[1:8] - 1, model = "GRM", method = "WL")
  scores_map[which.min(abs(scores_map$raw - theta)),]$score
}
