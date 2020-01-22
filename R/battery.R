#' battery
#'
#' This function defines a battery, a series of questionnaires.
#' Use this function if you want to create a battery of tests.
#' @param label (Character scalar) Label to give the DAC results in the output file.
#' @param dict The psyquest dictionary used for internationalisation.
#' @export
battery <- function(title = "LongGold Test Umbrella Test",
                    documentation = "LGTUT",
                    admin_password = "conifer",
                    researcher_email = "musicsophistication@gmail.com",
                    languages = c("EN", "DE"),
                    validate_id = "auto",
                    dict = psyquest::psyquest_dict,
                    questionnaires = questionnaires,
                    ...) {
  elts <- c(register_participant(validate_id, dict))
  elts <- append(elts, c(questionnaires))
  elts <- append(elts, c(psychTestR::elt_save_results_to_disk(complete = TRUE)))
  elts <- append(elts,
    c(psychTestR::new_timeline(
      psychTestR::final_page(shiny::p(
        psychTestR::i18n("RESULTS_SAVED"),
        psychTestR::i18n("CLOSE_BROWSER"))
      ), dict = dict
    ))
  )

  psychTestR::make_test(
    elts,
    opt = psychTestR::test_options(title = title,
                                   admin_password = admin_password,
                                   researcher_email = researcher_email,
                                   demo = FALSE,
                                   languages = languages))
}

register_participant <- function(validate_id, dict) {
  psychTestR::new_timeline(
    psychTestR::get_p_id(
      prompt = psychTestR::i18n("ENTER_ID"),
      button_text = psychTestR::i18n("CONTINUE"),
      validate = validate_id
    ),
    dict = dict
  )
}
