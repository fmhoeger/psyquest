library(shiny)
library(stringr)
library(tidyverse)

options(shiny.error = browser)
debug_locally <- !grepl("shiny-server", getwd())


#' Standalone
#'
#' This function launches a standalone testing session for a given questionnaire.
#' This can be used for data collection, either in the laboratory or online.
#' @param label (Scalar character) Three uppercase letter acronym of the questionnaire.
#' @param languages (Character vector)
#' Determines the languages available to participants.
#' Possible languages include English (\code{"EN"}), and German (\code{"DE"}).
#' The first language is selected by default.
#' @param subscales (Character vector) The subscales to be included in the questionnaire.
#' When no subscales are provided all subscales are selected.
#' @param dict The psyquest dictionary used for internationalisation.
#' @param admin_password (Scalar character) Password for accessing the admin panel.
#' @param researcher_email (Scalar character)
#' If not \code{NULL}, this researcher's email address is displayed
#' at the bottom of the screen so that online participants can ask for help.
#' @param validate_id (Character scalar or closure) Function for validating IDs or string "auto"
#' for default validation which means ID should consist only of alphanumeric characters.
#' @param with_feedback (Scalar boolean) Indicates if performance feedback will be given at the end
#' of the test. Defaults to FALSE.
#' @param take_training (Boolean scalar) Defines whether instructions and training are included.
#' Defaults to TRUE.
#' @param ... Further arguments to be passed to \code{\link{standalone}()}.
#' @export
standalone <- function(label,
                       languages = c("EN", "DE"),
                       subscales = NULL,
                       dict = psyquest::psyquest_dict,
                       admin_password = "conifer",
                       researcher_email = "musicsophistication@gmail.com",
                       validate_id = "auto",
                       with_feedback = FALSE,
                       take_training = TRUE,
                       ...) {
  subscales <- sort(subscales)
  items <- get_items(label, subscales)

  elts <- c(
    psychTestR::new_timeline(
      psychTestR::get_p_id(
        prompt = psychTestR::i18n("ENTER_ID"),
        placeholder = paste(psychTestR::i18n("E.G."), "10492817"),
        button_text = psychTestR::i18n("CONTINUE"),
        validate = validate_id
      ),
      dict = dict
    ),
    # call the questionnaire
    get(label)(language = languages, items = items, subscales = subscales, ...),
    psychTestR::elt_save_results_to_disk(complete = TRUE),
    psychTestR::new_timeline(psychTestR::final_page(
      shiny::p(
        psychTestR::i18n("RESULTS_SAVED"),
        psychTestR::i18n("CLOSE_BROWSER")
      )
    ), dict = dict)
  )

  if (file.exists(file.path("../..", "VERSION"))) {
    # workaround for testthat
    version_info <- read.delim(file.path("../..", "VERSION"), header = FALSE)
  } else {
    version_info <- read.delim("inst/VERSION", header = FALSE)
  }

  psychTestR::make_test(
    elts,
    opt = psychTestR::test_options(
      title = dict$translate(
        stringr::str_interp("T${label}_0000_PROMPT"),
        languages[1]
      ),
      admin_password = admin_password,
      researcher_email = researcher_email,
      demo = FALSE,
      languages = languages,
      app_info = paste0("psyquest v", toString(version_info$V1[1]))
    )
  )
}

#' CCM Standalone
#' This function launches a standalone testing session for the CCM questionnaire.
#' @param languages (Character vector)
#' Determines the languages available to participants.
#' Possible languages include English (\code{"EN"}), and German (\code{"DE"}).
#' The first language is selected by default.
#' @param subscales (Character vector)
#' Determines the subscales to be included.
#' If no subscales are provided all subscales for the questionnaire are selected.
#' @param ... Further arguments to be passed to \code{\link{CCM_standalone}()}.
#' @export
CCM_standalone <-
  function(languages = CCM_languages(), subscales = NULL, ...)
    standalone(label = "CCM", languages = languages, subscales = subscales, ...)

#' DAC Standalone
#' This function launches a standalone testing session for the DAC questionnaire.
#' @param languages (Character vector)
#' Determines the languages available to participants.
#' Possible languages include English (\code{"EN"}), and German (\code{"DE"}).
#' The first language is selected by default.
#' @param ... Further arguments to be passed to \code{\link{DAC_standalone}()}.
#' @export
DAC_standalone <-
  function(languages = DAC_languages(), ...)
    standalone(label = "DAC", languages = languages, ...)

#' DEG Standalone
#' This function launches a standalone testing session for the DEG questionnaire.
#' @param languages (Character vector)
#' Determines the languages available to participants.
#' Possible languages include English (\code{"EN"}), and German (\code{"DE"}).
#' The first language is selected by default.
#' @param subscales (Character vector)
#' Determines the subscales to be included.
#' If no subscales are provided all subscales for the questionnaire are selected.
#' @param ... Further arguments to be passed to \code{\link{DEG_standalone}()}.
#' @export
DEG_standalone <-
  function(languages = DEG_languages(), subscales = NULL, ...)
    standalone(label = "DEG", languages = languages, subscales = subscales, ...)

#' GMS Standalone
#' This function launches a standalone testing session for the GMS questionnaire.
#' @param languages (Character vector)
#' Determines the languages available to participants.
#' Possible languages include English (\code{"EN"}), and German (\code{"DE"}).
#' The first language is selected by default.
#' @param subscales (Character vector)
#' Determines the subscales to be included.
#' If no subscales are provided all subscales for the questionnaire are selected.
#' @param ... Further arguments to be passed to \code{\link{GMS_standalone}()}.
#' @export
GMS_standalone <-
  function(languages = GMS_languages(), subscales = NULL, ...)
    standalone(label = "GMS", languages = languages, subscales = subscales, ...)

#' GRT Standalone
#' This function launches a standalone testing session for the GRT questionnaire.
#' @param languages (Character vector)
#' Determines the languages available to participants.
#' Possible languages include English (\code{"EN"}), and German (\code{"DE"}).
#' The first language is selected by default.
#' @param ... Further arguments to be passed to \code{\link{GRT_standalone}()}.
#' @export
GRT_standalone <-
  function(languages = GRT_languages(), ...)
    standalone(label = "GRT", languages = languages, ...)

#' HOP Standalone
#' This function launches a standalone testing session for the HOP questionnaire.
#' @param languages (Character vector)
#' Determines the languages available to participants.
#' Possible languages include English (\code{"EN"}), and German (\code{"DE"}).
#' The first language is selected by default.
#' @param ... Further arguments to be passed to \code{\link{HOP_standalone}()}.
#' @export
HOP_standalone <-
  function(languages = HOP_languages(), ...)
    standalone(label = "HOP", languages = languages, ...)

#' MHE Standalone
#' This function launches a standalone testing session for the MHE questionnaire.
#' @param languages (Character vector)
#' Determines the languages available to participants.
#' Possible languages include English (\code{"EN"}), and German (\code{"DE"}).
#' The first language is selected by default.
#' @param ... Further arguments to be passed to \code{\link{MHE_standalone}()}.
#' @export
MHE_standalone <-
  function(languages = MHE_languages(), ...)
    standalone(label = "MHE", languages = languages, ...)

#' PAC Standalone
#' This function launches a standalone testing session for the PAC questionnaire.
#' @param languages (Character vector)
#' Determines the languages available to participants.
#' Possible languages include English (\code{"EN"}), and German (\code{"DE"}).
#' The first language is selected by default.
#' @param subscales (Character vector)
#' Determines the subscales to be included.
#' If no subscales are provided all subscales for the questionnaire are selected.
#' @param ... Further arguments to be passed to \code{\link{PAC_standalone}()}.
#' @export
PAC_standalone <-
  function(languages = PAC_languages(), subscales = NULL, ...)
    standalone(label = "PAC", languages = languages, subscales = subscales, ...)

#' SCA Standalone
#' This function launches a standalone testing session for the SCA questionnaire.
#' @param languages (Character vector)
#' Determines the languages available to participants.
#' Possible languages include English (\code{"EN"}), and German (\code{"DE"}).
#' The first language is selected by default.
#' @param subscales (Character vector)
#' Determines the subscales to be included.
#' If no subscales are provided all subscales for the questionnaire are selected.
#' @param ... Further arguments to be passed to \code{\link{SCA_standalone}()}.
#' @export
SCA_standalone <-
  function(languages = SCA_languages(), subscales = NULL, ...)
    standalone(label = "SCA", languages = languages, subscales = subscales, ...)

#' SCS Standalone
#' This function launches a standalone testing session for the SCS questionnaire.
#' @param languages (Character vector)
#' Determines the languages available to participants.
#' Possible languages include English (\code{"EN"}), and German (\code{"DE"}).
#' The first language is selected by default.
#' @param subscales (Character vector)
#' Determines the subscales to be included.
#' If no subscales are provided all subscales for the questionnaire are selected.
#' @param ... Further arguments to be passed to \code{\link{SCS_standalone}()}.
#' @export
SCS_standalone <-
  function(languages = SCS_languages(), subscales = NULL, ...)
    standalone(label = "SCS", languages = languages, subscales = subscales, ...)

#' SDQ Standalone
#' This function launches a standalone testing session for the SDQ questionnaire.
#' @param languages (Character vector)
#' Determines the languages available to participants.
#' Possible languages include English (\code{"EN"}), and German (\code{"DE"}).
#' The first language is selected by default.
#' @param subscales (Character vector)
#' Determines the subscales to be included.
#' If no subscales are provided all subscales for the questionnaire are selected.
#' @param ... Further arguments to be passed to \code{\link{SDQ_standalone}()}.
#' @export
SDQ_standalone <-
  function(languages = SDQ_languages(), subscales = NULL, ...)
    standalone(label = "SDQ", languages = languages, subscales = subscales, ...)

#' SEM Standalone
#' This function launches a standalone testing session for the SEM questionnaire.
#' @param languages (Character vector)
#' Determines the languages available to participants.
#' Possible languages include English (\code{"EN"}), and German (\code{"DE"}).
#' The first language is selected by default.
#' @param subscales (Character vector)
#' Determines the subscales to be included.
#' If no subscales are provided all subscales for the questionnaire are selected.
#' @param ... Further arguments to be passed to \code{\link{SEM_standalone}()}.
#' @export
SEM_standalone <-
  function(languages = SEM_languages(), subscales = NULL, ...)
    standalone(label = "SEM", languages = languages, subscales = subscales, ...)

#' SES Standalone
#' This function launches a standalone testing session for the SES questionnaire.
#' @param languages (Character vector)
#' Determines the languages available to participants.
#' Possible languages include English (\code{"EN"}), and German (\code{"DE"}).
#' The first language is selected by default.
#' @param subscales (Character vector)
#' Determines the subscales to be included.
#' If no subscales are provided all subscales for the questionnaire are selected.
#' @param ... Further arguments to be passed to \code{\link{SES_standalone}()}.
#' @export
SES_standalone <-
  function(languages = SES_languages(), subscales = NULL, ...)
    standalone(label = "SES", languages = languages, subscales = subscales, ...)

#' SOS Standalone
#' This function launches a standalone testing session for the SOS questionnaire.
#' @param languages (Character vector)
#' Determines the languages available to participants.
#' Possible languages include English (\code{"EN"}), and German (\code{"DE"}).
#' The first language is selected by default.
#' @param subscales (Character vector)
#' Determines the subscales to be included.
#' If no subscales are provided all subscales for the questionnaire are selected.
#' @param ... Further arguments to be passed to \code{\link{SOS_standalone}()}.
#' @export
SOS_standalone <-
  function(languages = SOS_languages(), subscales = NULL, ...)
    standalone(label = "SOS", languages = languages, subscales = subscales, ...)

#' TOI Standalone
#' This function launches a standalone testing session for the TOI questionnaire.
#' @param languages (Character vector)
#' Determines the languages available to participants.
#' Possible languages include English (\code{"EN"}), and German (\code{"DE"}).
#' The first language is selected by default.
#' @param subscales (Character vector)
#' Determines the subscales to be included.
#' If no subscales are provided all subscales for the questionnaire are selected.
#' @param ... Further arguments to be passed to \code{\link{TOI_standalone}()}.
#' @export
TOI_standalone <-
  function(languages = TOI_languages(), subscales = NULL, ...)
    standalone(label = "TOI", languages = languages, subscales = subscales, ...)

#' TOM Standalone
#' This function launches a standalone testing session for the TOM questionnaire.
#' @param languages (Character vector)
#' Determines the languages available to participants.
#' Possible languages include English (\code{"EN"}), and German (\code{"DE"}).
#' The first language is selected by default.
#' @param subscales (Character vector)
#' Determines the subscales to be included.
#' If no subscales are provided all subscales for the questionnaire are selected.
#' @param ... Further arguments to be passed to \code{\link{TOM_standalone}()}.
#' @export
TOM_standalone <-
  function(languages = TOM_languages(), subscales = NULL, ...)
    standalone(label = "TOM", languages = languages, subscales = subscales, ...)

#' TPI Standalone
#' This function launches a standalone testing session for the TPI questionnaire.
#' @param languages (Character vector)
#' Determines the languages available to participants.
#' Possible languages include English (\code{"EN"}), and German (\code{"DE"}).
#' The first language is selected by default.
#' @param subscales (Character vector)
#' Determines the subscales to be included.
#' If no subscales are provided all subscales for the questionnaire are selected.
#' @param ... Further arguments to be passed to \code{\link{TPI_standalone}()}.
#' @export
TPI_standalone <-
  function(languages = TPI_languages(), subscales = NULL, ...)
    standalone(label = "TPI", languages = languages, subscales = subscales, ...)
