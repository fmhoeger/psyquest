#' SDQ
#'
#' This function defines a SDQ module for incorporation into a
#' psychTestR timeline.
#' Use this function if you want to include the SDQ in a
#' battery of other tests, or if you want to add custom psychTestR
#' pages to your test timeline.
#' For a standalone implementation of the SDQ,
#' consider using \code{\link{SDQ_standalone}()}.
#' @param label (Character scalar) Label to give the SDQ results in the output file.
#' @param dict The psyquest dictionary used for internationalisation.
#' @param ... Further arguments to be passed to \code{\link{SDQ}()}.
#' @export
SDQ <- function(label = "SDQ",
                dict = psyquest::psyquest_dict,
                ...) {
  stopifnot(purrr::is_scalar_character(label))

  main_test(
    questionnaire = label,
    label = label,
    num_items = 25,
    offset = 1,
    arrange_vertically = TRUE
  )
}
