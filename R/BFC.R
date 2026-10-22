#' BFC
#'
#' This function defines a Big Five Inventory (10-items, BFI-10, BFC) module for incorporation into a
#' psychTestR timeline.
#' Use this function if you want to include the BFC in a
#' battery of other tests, or if you want to add custom psychTestR
#' pages to your test timeline.
#' For a standalone implementation of the BFC,
#' consider using \code{\link{BFC_standalone}()}.
#' @param label (Character scalar) Three uppercase letter acronym of the questionnaire.
#' This is also the label given to the results in the output file.
#' @param dict (i18n_dict) The psyquest dictionary used for internationalisation.
#' @param ... Further arguments to be passed to \code{\link{BFC}()}.
#' @export
BFC <- function(label = "BFC",
                dict = psyquest::psyquest_dict,
                ...) {
  stopifnot(purrr::is_scalar_character(label))

  questionnaire_id <- "BFC"

  main_test(
    questionnaire_id = questionnaire_id,
    label = label,
    items = get_items(questionnaire_id),
    offset = 1,
    arrange_vertically = TRUE,
    button_style = "min-width: 290px",
    style_params = list(intro_style = "margin-left:20%;margin-right:20%;text-align:center;margin-bottom:2em;"),
    dict = dict
  )
}
