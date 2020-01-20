#' DAC
#'
#' This function defines a DAC module for incorporation into a
#' psychTestR timeline.
#' Use this function if you want to include the DAC in a
#' battery of other tests, or if you want to add custom psychTestR
#' pages to your test timeline.
#' For demoing the DAC, consider using \code{\link{DAC_demo}()}.
#' For a standalone implementation of the DAC,
#' consider using \code{\link{DAC_standalone}()}.
#' @param label (Character scalar) Label to give the DAC results in the output file.
#' @param dict The psyquest dictionary used for internationalisation.
#' @export
DAC <- function(label = "DAC",
                dict = psyquest::psyquest_dict) {
  stopifnot(purrr::is_scalar_character(label))

  main_test(
    questionnaire = label,
    label = label,
    num_items = 4,
    offset = 1,
    arrange_vertically = FALSE
  )
}
