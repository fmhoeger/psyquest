#' TPI
#'
#' This function defines a TPI module for incorporation into a
#' psychTestR timeline.
#' Use this function if you want to include the TPI in a
#' battery of other tests, or if you want to add custom psychTestR
#' pages to your test timeline.
#' For a standalone implementation of the TPI,
#' consider using \code{\link{TPI_standalone}()}.
#' @param label (Character scalar) Label to give the TPI results in the output file.
#' @param dict The psyquest dictionary used for internationalisation.
#' @param ... Further arguments to be passed to \code{\link{TPI}()}.
#' @export
TPI <- function(label = "TPI",
                dict = psyquest::psyquest_dict,
                ...) {
  stopifnot(purrr::is_scalar_character(label))

  main_test(
    questionnaire = label,
    label = label,
    num_items = 10,
    offset = 1,
    arrange_vertically = TRUE
  )
}
