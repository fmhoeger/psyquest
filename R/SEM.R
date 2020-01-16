#' SEM
#'
#' This function defines a SEM module for incorporation into a
#' psychTestR timeline.
#' Use this function if you want to include the SEM in a
#' battery of other tests, or if you want to add custom psychTestR
#' pages to your test timeline.
#' For demoing the SEM, consider using \code{\link{SEM_demo}()}.
#' For a standalone implementation of the SEM,
#' consider using \code{\link{SEM_standalone}()}.
#' @param label (Character scalar) Label to give the SEM results in the output file.
#' @param dict The psyquest dictionary used for internationalisation.
#' @export
SEM <- function(label = "SEM",
                dict = psyquest::psyquest_dict) {
  stopifnot(purrr::is_scalar_character(label))

  main_test(
    questionnaire = label,
    label = label,
    num_items = 23,
    offset = 1
  )
}
