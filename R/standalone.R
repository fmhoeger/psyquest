options(shiny.error = browser)
debug_locally <- !grepl("shiny-server", getwd())


#' Standalone
#'
#' This function launches a standalone testing session for DAC/PAC
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


standalone <- function(questionnaire = questionnaire,
                       admin_password = "conifer",
                       researcher_email = "musicsophistication@gmail.com",
                       languages = languages,
                       dict = psyquest::psyquest_dict,
                       validate_id = "auto",
                       ...) {
  elts <- c(
    psychTestR::new_timeline(
      psychTestR::get_p_id(
        prompt = psychTestR::i18n("ENTER_ID"),
        button_text = psychTestR::i18n("CONTINUE"),
        validate = validate_id
      ),
      dict = dict
    ),
    psychTestR::new_timeline(
      psychTestR::one_button_page(
        body = psychTestR::i18n(str_interp("T${questionnaire}_0001_PROMPT")),
        button_text = psychTestR::i18n("CONTINUE")
      ),
      dict = dict
    ),
    get(questionnaire)(...), # call DAC, PAC,...
    psychTestR::code_block(function(state, ...) {
      results <- psychTestR::get_results(state = state, complete = FALSE)
      scores <-
        as.numeric(gsub("[^0-9]", "", unlist(as.list(results))))
      psychTestR::save_result(place = state,
                              label = "mean",
                              value = mean(scores))
    }),
    psychTestR::elt_save_results_to_disk(complete = TRUE),
    psychTestR::new_timeline(psychTestR::final_page(
      shiny::p(
        psychTestR::i18n("RESULTS_SAVED"),
        psychTestR::i18n("CLOSE_BROWSER")
      )
    ), dict = dict)
  )

  psychTestR::make_test(
    elts,
    opt = psychTestR::test_options(
      title = dict$translate(str_interp("T${questionnaire}_0000_PROMPT"), languages),
      admin_password = admin_password,
      researcher_email = researcher_email,
      demo = FALSE,
      languages = languages
    )
  )
}

#' @export
DAC_standalone <- function(languages = c("DE", "EN"), ...) standalone(questionnaire = "DAC", languages = languages)
#' @export
PAC_standalone <- function(languages = c("DE", "EN"), ...) standalone(questionnaire = "PAC", languages = languages)
#' @export
TPI_standalone <- function(languages = c("DE", "EN"), ...) standalone(questionnaire = "TPI", languages = languages)
