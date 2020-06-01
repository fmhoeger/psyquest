#' battery
#'
#' This function defines a battery, a series of questionnaires.
#' Use this function if you want to create a battery of questionnaires.
#' @param title (Character scalar) Title of the umbrella test battery.
#' @param questionnaires (Character vector)
#' @param languages (Character vector)
#' Determines the languages available to participants.
#' Possible languages include English (\code{"en"}), and German (\code{"de"}).
#' The first language is selected by default.
#' @param dict The psyquest dictionary used for internationalisation.
#' @param admin_password Password to access the admin panel.
#' @param researcher_email Researcher's email; used in participant help message.
#' @param validate_id (Character scalar or closure) Function for validating IDs or string "auto"
#' for default validation which means ID should consist only of alphanumeric characters.
#' @param ... Further arguments to be passed to \code{\link{battery}()}.
#' @export
battery <- function(title = "LongGold Test Umbrella Test",
                    questionnaires,
                    languages = c("en", "de"),
                    dict = psyquest::psyquest_dict,
                    admin_password = "conifer",
                    researcher_email = "musicsophistication@gmail.com",
                    validate_id = "auto",
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
      placeholder = paste(psychTestR::i18n("E.G."), "10492817"),
      button_text = psychTestR::i18n("CONTINUE"),
      validate = validate_id
    ),
    dict = dict
  )
}
