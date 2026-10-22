#' PNS
#'
#' This function defines a Positive and Negative Affect Schedule (PANAS) module for incorporation into a
#' psychTestR timeline.
#' Use this function if you want to include the PNS in a
#' battery of other tests, or if you want to add custom psychTestR
#' pages to your test timeline.
#' For a standalone implementation of the PNS,
#' consider using \code{\link{PNS_standalone}()}.
#' @param label (Character scalar) Three uppercase letter acronym of the questionnaire.
#' This is also the label given to the results in the output file.
#' @param dict (i18n_dict) The psyquest dictionary used for internationalisation.
#' @param ... Further arguments to be passed to \code{\link{PNS}()}.
#' @export
PNS <- function(label = "PNS",
                dict = psyquest::psyquest_dict,
                ...) {
  stopifnot(purrr::is_scalar_character(label))

  questionnaire_id <- "PNS"

  main_test(
    questionnaire_id = questionnaire_id,
    label = label,
    items = get_items(questionnaire_id),
    offset = 1,
    arrange_vertically = TRUE,
    style_params = list(prompt_style = "color:green;font-size:20px;font-weight:bold", with_counter = FALSE),
    button_style = "min-width: 290px"
  )
}
