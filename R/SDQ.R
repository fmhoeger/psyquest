#' SDQ
#'
#' This function defines a SDQ module for incorporation into a
#' psychTestR timeline.
#' Use this function if you want to include the SDQ in a
#' battery of other tests, or if you want to add custom psychTestR
#' pages to your test timeline.
#' For demoing the SDQ, consider using \code{\link{SDQ_demo}()}.
#' For a standalone implementation of the SDQ,
#' consider using \code{\link{SDQ_standalone}()}.
#' @param label (Character scalar) Label to give the SDQ results in the output file.
#' @param dict The psychTestR dictionary used for internationalisation.
#' @export
SDQ <- function(label = "SDQ",
                dict = psyquest::psyquest_dict) {
  stopifnot(purrr::is_scalar_character(label))

  main_test(
    questionnaire = label,
    label = label,
    num_items = 25,
    offset = 1
  )
}
