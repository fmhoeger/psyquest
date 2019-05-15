
options(shiny.error = browser)
debug_locally <- !grepl("shiny-server", getwd())



#' Standalone DAC
#'
#' This function launches a standalone testing session for the DAC
#' This can be used for data collection, either in the laboDACory or online.
#' @param title (Scalar character) Title to display during testing.
#' @param num_items (Scalar integer) Number of items to be adminstered.
#' @param with_feedback (Scalar boolean) Indicates if performance feedback will be given at the end of the test. Defaults to  FALSE
#' @param take_training (Boolean scalar) Defines whether instructions and training are included.
#' Defaults to TRUE.
#' @param admin_password (Scalar character) Password for accessing the admin panel.
#' @param researcher_email (Scalar character)
#' If not \code{NULL}, this researcher's email address is displayed
#' at the bottom of the screen so that online participants can ask for help.
#' @param languages (Character vector)
#' Determines the languages available to participants.
#' Possible languages include English (\code{"EN"}),
#' and German (\code{"DE"}).
#' The first language is selected by default
#' @param dict The psychTestR dictionary used for internationalisation.
#' @param validate_id (Character scalar or closure) Function for validating IDs or string "auto" for default validation
#' which means ID should consist only of  alphanumeric characters.
#' @param ... Further arguments to be passed to \code{\link{DAC}()}.
#' @export
DAC_standalone  <- function(title = "Drama Activity Questionnaire",
                           admin_password = "conifer",
                           researcher_email = "musicsophistication@gmail.com",
                           languages = c("EN", "DE"),
                           dict = DAC::DAC_dict,
                           validate_id = "auto",
                           ...) {
  elts <- c(
    psychTestR::new_timeline(
      psychTestR::get_p_id(prompt = psychTestR::i18n("ENTER_ID"),
                           button_text = psychTestR::i18n("CONTINUE"),
                           validate = validate_id),
      dict = dict
    ),
    #register_participant(),
    psychTestR::one_button_page(body = psychTestR::i18n("TDAC_0001_PROMPT"),
                                button_text = psychTestR::i18n("CONTINUE")),
    DAC(...),
    #psychTestRCAT::cat.feedback.graph("DAC"),
    psychTestR::elt_save_results_to_disk(complete = TRUE),
    #upload_results(F),
    psychTestR::new_timeline(
      psychTestR::final_page(shiny::p(
        psychTestR::i18n("RESULTS_SAVED"),
        psychTestR::i18n("CLOSE_BROWSER"))
      ), dict = dict)
  )

  psychTestR::make_test(
    elts,
    opt = psychTestR::test_options(title = title,
                                   admin_password = admin_password,
                                   researcher_email = researcher_email,
                                   demo = FALSE,
                                   languages = languages))
}
