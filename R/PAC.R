#' PAC
#'
#' This function defines a PAC module for incorporation into a
#' psychTestR timeline.
#' Use this function if you want to include the PAC in a
#' battery of other tests, or if you want to add custom psychTestR
#' pages to your test timeline.
#' For a standalone implementation of the PAC,
#' consider using \code{\link{PAC_standalone}()}.
#' @param label (Character scalar) Label to give the PAC results in the output file.
#' @param dict The psyquest dictionary used for internationalisation.
#' @export
PAC <- function(label = "PAC",
                dict = psyquest::psyquest_dict) {
  stopifnot(purrr::is_scalar_character(label))

  main_test(
    questionnaire = label,
    label = label,
    num_items = 5,
    offset = 1,
    arrange_vertically = FALSE
  )
}
