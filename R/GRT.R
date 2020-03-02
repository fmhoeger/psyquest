#' GRT
#'
#' This function defines a GRT module for incorporation into a
#' psychTestR timeline.
#' Use this function if you want to include the GRT in a
#' battery of other tests, or if you want to add custom psychTestR
#' pages to your test timeline.
#' For a standalone implementation of the GRT,
#' consider using \code{\link{GRT_standalone}()}.
#' @param label (Character scalar) Label to give the GRT results in the output file.
#' @param dict The psyquest dictionary used for internationalisation.
#' @param items (Data frame) The items to be included in the questionnaire.
#' @param ... Further arguments to be passed to \code{\link{GRT}()}.
#' @export
GRT <- function(label = "GRT",
                dict = psyquest::psyquest_dict,
                items = items,
                ...) {
  stopifnot(purrr::is_scalar_character(label))

  main_test(
    questionnaire = label,
    label = label,
    items = items,
    offset = 1,
    arrange_vertically = TRUE
  )
}
