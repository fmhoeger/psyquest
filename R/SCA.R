#' SCA
#'
#' This function defines a SCA module for incorporation into a
#' psychTestR timeline.
#' Use this function if you want to include the SCA in a
#' battery of other tests, or if you want to add custom psychTestR
#' pages to your test timeline.
#' For a standalone implementation of the SCA,
#' consider using \code{\link{SCA_standalone}()}.
#' @param label (Character scalar) Three uppercase letter acronym of the questionnaire.
#' This is also the label given to the results in the output file.
#' @param dict The psyquest dictionary used for internationalisation.
#' @param subscales (Character vector) The subscales to be included in the questionnaire.
#' When no subscales are provided all subscales are selected.
#' @param ... Further arguments to be passed to \code{\link{SCA}()}.
#' @export
SCA <- function(label = "SCA",
                dict = psyquest::psyquest_dict,
                subscales = c(),
                ...) {
  stopifnot(purrr::is_scalar_character(label))

  main_test(
    label = label,
    items = get_items(label, subscales),
    offset = 1,
    arrange_vertically = TRUE
  )
}
