#' CMS
#'
#' This function defines a CMS module for incorporation into a
#' psychTestR timeline.
#' Use this function if you want to include the CMS in a
#' battery of other tests, or if you want to add custom psychTestR
#' pages to your test timeline.
#' For a standalone implementation of the MHE,
#' consider using \code{\link{CMS_standalone}()}.
#' @param label (Character scalar) Three uppercase letter acronym of the questionnaire.
#' This is also the label given to the results in the output file.
#' @param dict (i18n_dict) The psyquest dictionary used for internationalisation.
#' @param ... Further arguments to be passed to \code{\link{CMS}()}.
#' @export
CMS <- function(label = "CMS",
                subscales = c(),
                with_extra_scales = F,
                dict = psyquest::psyquest_dict,
                alt_intro = F,
                ...) {
  stopifnot(purrr::is_scalar_character(label))
  extra_scales <- c("Instrumental Lessons",
                    "Instrument",
                    "Number Instruments",
                    "Practice Hours",
                    "Music Group Duration")

  questionnaire_id <- "CMS"
  if(is.null(subscales) || length(subscales) == 0){
    subscales <- get_subscales("CMS")
    if(!with_extra_scales){
      subscales <- setdiff(subscales, extra_scales)
    }
  }
  if(!is.null(alt_intro) && alt_intro){
    alt_intro <- "TCMS_0016_PROMPT"
  }
  main_test(
    questionnaire_id = questionnaire_id,
    label = label,
    items = get_items(questionnaire_id, subscales = subscales),
    offset = 1,
    alt_intro = alt_intro,
    arrange_vertically = TRUE,
    button_style = "min-width: 290px",
    dict = dict
  )

}
get_plain_text_cms <- function(results, label, item_id){
  plain_text <- map_chr(results[[label]][[sprintf("q%s", item_id)]], function(x){
    sprintf("%s",
            psyquest::psyquest_dict$translate(sprintf("TCMS_00%02d_CHOICE%s",
                                                      as.integer(item_id) + 1, parse_number(x)),
                                              language = "en"))
  })
  paste(plain_text, collapse = ",")
}

postprocess_cms <- function(label, subscale, results, scores) {
  #print(subscale)
  extra_scales <- c("Instrumental Lessons",
                    "Instrument",
                    "Number Instruments",
                    "Practice Hours",
                    "Music Group Duration")
  extra_idx <- which(extra_scales == subscale)
  if(length(extra_idx) > 0){
    get_plain_text_cms(results, label, 9 + extra_idx)
  }
  else {
    mean(scores)
  }
}

