#' TOI
#'
#' This function defines a TOI module for incorporation into a
#' psychTestR timeline.
#' Use this function if you want to include the TOI in a
#' battery of other tests, or if you want to add custom psychTestR
#' pages to your test timeline.
#' For demoing the TOI, consider using \code{\link{TOI_demo}()}.
#' For a standalone implementation of the TOI,
#' consider using \code{\link{TOI_standalone}()}.
#' @param label (Character scalar) Label to give the TOI results in the output file.
#' @param dict The psyquest dictionary used for internationalisation.
#' @export
TOI <- function(label = "TOI",
                dict = psyquest::psyquest_dict) {
  stopifnot(purrr::is_scalar_character(label))

  main_test(
    questionnaire = label,
    label = label,
    num_items = 7,
    offset = 1
  )
}
