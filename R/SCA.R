#' SCA
#'
#' This function defines a SCA module for incorporation into a
#' psychTestR timeline.
#' Use this function if you want to include the SCA in a
#' battery of other tests, or if you want to add custom psychTestR
#' pages to your test timeline.
#' For a standalone implementation of the SCA,
#' consider using \code{\link{SCA_standalone}()}.
#' @param label (Character scalar) Label to give the SCA results in the output file.
#' @param dict The psyquest dictionary used for internationalisation.
#' @param items (Data frame) The items to be included in the questionnaire.
#' @param ... Further arguments to be passed to \code{\link{SCA}()}.
#' @export
SCA <- function(label = "SCA",
                dict = psyquest::psyquest_dict,
                items = items,
                ...) {
  stopifnot(purrr::is_scalar_character(label))

  main_test(
    questionnaire = label,
    label = label,
    items = items,
    num_items = 25,
    offset = 1,
    arrange_vertically = TRUE
  )
}
