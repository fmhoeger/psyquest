library(tidyverse)
library(stringr)

options(shiny.error = browser)
debug_locally <- !grepl("shiny-server", getwd())


#' Standalone
#'
#' This function launches a standalone testing session for a given questionnaire.
#' This can be used for data collection, either in the laboDACory or online.
#' @param questionnaire (Scalar character) The questionnaire acronym.
#' @param with_feedback (Scalar boolean) Indicates if performance feedback will be given at the end
#' of the test. Defaults to FALSE.
#' @param take_training (Boolean scalar) Defines whether instructions and training are included.
#' Defaults to TRUE.
#' @param admin_password (Scalar character) Password for accessing the admin panel.
#' @param researcher_email (Scalar character)
#' If not \code{NULL}, this researcher's email address is displayed
#' at the bottom of the screen so that online participants can ask for help.
#' @param languages (Character vector)
#' Determines the languages available to participants.
#' Possible languages include English (\code{"EN"}), and German (\code{"DE"}).
#' The first language is selected by default.
#' @param dict The psyquest dictionary used for internationalisation.
#' @param validate_id (Character scalar or closure) Function for validating IDs or string "auto"
#' for default validation which means ID should consist only of alphanumeric characters.
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
        body = psychTestR::i18n(stringr::str_interp("T${questionnaire}_0001_PROMPT")),
        button_text = psychTestR::i18n("CONTINUE")
      ),
      dict = dict
    ),
    get(questionnaire)(...), # call questionnaires (DAC, PAC, TPI, ...)
    psychTestR::code_block(function(state, ...) {
      results <- psychTestR::get_results(state = state, complete = FALSE)
      score_funcs <-
        psyquest::psyquest_item_bank %>%
        filter(stringr::str_detect(prompt_id, stringr::str_interp("T${questionnaire}"))) %>%
        pull(score_func)
      subscales <-
        psyquest::psyquest_item_bank %>%
        filter(stringr::str_detect(prompt_id, stringr::str_interp("T${questionnaire}"))) %>%
        pull(subscales)
      scores_raw <- map(results, function(result) {
        result <- as.numeric(gsub("[^0-9]", "", result))
        result
      })[[1]]
      scores <- map_dbl(1:length(scores_raw), function(i){ eval(parse(text = score_funcs[i]))(scores_raw[i])})

      subscale_list = list()
      for (i in 1:length(scores)) {
        for (subscale in strsplit(subscales[i], ";")[[1]]) {
          subscale_list[[subscale]] = c(subscale_list[[subscale]], scores[i])
        }
      }

      postprocess(questionnaire, subscale_list, state)
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
      title = dict$translate(stringr::str_interp("T${questionnaire}_0000_PROMPT"), languages[1]),
      admin_password = admin_password,
      researcher_email = researcher_email,
      demo = FALSE,
      languages = languages
    )
  )
}


postprocess <- function(questionnaire = questionnaire, subscale_list = subscale_list, state = state) {
  for (subscale in names(subscale_list)) {
    scores <- subscale_list[[subscale]]

    if(questionnaire == 'SCA' | questionnaire == 'SCS') {
      score_mapping <- read.csv(file = stringr::str_interp("data_raw/${questionnaire}_scores.csv"),
                                header = FALSE,
                                sep = ";")
      row <- score_mapping %>% filter(stringr::str_detect(V3, toString(sum(scores))))
      value <- row[1,2]
    } else {
      value = mean(scores)
    }

    psychTestR::save_result(place = state,
                            label = subscale,
                            value = value)
  }
}


#' @export
DAC_standalone <- function(languages = c("EN", "DE"), ...) standalone(questionnaire = "DAC", languages = languages, ...)
#' @export
PAC_standalone <- function(languages = c("EN", "DE"), ...) standalone(questionnaire = "PAC", languages = languages, ...)
#' @export
SCA_standalone <- function(languages = c("EN", "DE"), ...) standalone(questionnaire = "SCA", languages = languages, ...)
#' @export
SCS_standalone <- function(languages = c("EN", "DE"), ...) standalone(questionnaire = "SCS", languages = languages, ...)
#' @export
SDQ_standalone <- function(languages = c("EN", "DE"), ...) standalone(questionnaire = "SDQ", languages = languages, ...)
#' @export
SEM_standalone <- function(languages = c("EN", "DE"), ...) standalone(questionnaire = "SEM", languages = languages, ...)
#' @export
SOS_standalone <- function(languages = c("EN", "DE"), ...) standalone(questionnaire = "SOS", languages = languages, ...)
#' @export
TOI_standalone <- function(languages = c("EN", "DE"), ...) standalone(questionnaire = "TOI", languages = languages, ...)
#' @export
TOM_standalone <- function(languages = c("EN", "DE"), ...) standalone(questionnaire = "TOM", languages = languages, ...)
#' @export
TPI_standalone <- function(languages = c("EN", "DE"), ...) standalone(questionnaire = "TPI", languages = languages, ...)
