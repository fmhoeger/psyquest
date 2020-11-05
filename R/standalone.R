#' Standalone
#'
#' This function launches a standalone testing session for a questionnaire with the specified label.
#' Valid labels are 'CCM', 'DAC', 'DEG', 'GDS', 'GMS', 'GRT', 'HOP', 'MHE', 'PAC',
#' 'SDQ', 'SEM', 'SES','SMP', 'SOS', 'TOI', 'TOM', and 'TPI'.
#' This can be used for data collection, either in the laboratory or online.
#'
#' @param label (Character scalar) Three uppercase letter acronym of the questionnaire.
#'
#' @param languages (Character vector)
#' Determines the languages available to participants.
#' Possible languages include \code{"en"} (English), and \code{"de"} (German).
#' The first language is selected by default.
#'
#' @param subscales (Character vector) The subscales to be included in the questionnaire.
#' If no subscales are provided all subscales are selected.
#'
#' @param short_version (Boolean scalar) For the short version of the questionnaire
#' set this to TRUE. Defaults to FALSE.
#'
#' @param configuration_filepath (Character scalar) Optional path to a configuration file
#' exported from the GMSI-Configurator at https://shiny.gold-msi.org/gmsiconfigurator (GMS only).
#'
#' @param dict (i18n_dict) The psyquest dictionary used for internationalisation.
#'
#' @param admin_password (Character scalar) Password for accessing the admin panel.
#'
#' @param researcher_email (Character scalar)
#' If not \code{NULL}, this researcher's email address is displayed at the
#' bottom of the screen so that online participants can ask for help.
#'
#' @param validate_id (Character scalar or closure) Function for validating IDs or string "auto"
#' for default validation which means ID should consist only of alphanumeric characters.
#'
#' @param ... Further arguments to be passed to \code{\link{standalone}()}.
#'
#' @export
standalone <- function(label,
                       languages = psyquest::languages(),
                       subscales = NULL,
                       short_version = FALSE,
                       configuration_filepath = NULL,
                       dict = psyquest::psyquest_dict,
                       admin_password = "conifer",
                       researcher_email = NULL,
                       validate_id = "auto",
                       ...) {
  subscales <- sort(subscales)
  items <-
    get_items(label, subscales, short_version, configuration_filepath)

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
    # Call the questionnaire
    get(label)(
      language = languages,
      items = items,
      subscales = subscales,
      short_version = short_version,
      configuration_filepath = configuration_filepath,
      ...
    ),
    psychTestR::new_timeline(psychTestR::final_page(
      shiny::p(
        psychTestR::i18n("RESULTS_SAVED"),
        psychTestR::i18n("CLOSE_BROWSER")
      )
    ), dict = dict)
  )

  title <-
    unlist(setNames(
      map(psyquest::languages(), function(x)
        psyquest::psyquest_dict$translate(stringr::str_interp("T${label}_0000_PROMPT"), x)),
      psyquest::languages()
    ))

  shiny::addResourcePath("www_psyquest", system.file("www", package = "psyquest"))
  psychTestR::make_test(
    elts,
    opt = psychTestR::test_options(
      title = title,
      admin_password = admin_password,
      problems_info = problems_info(researcher_email),
      researcher_email = researcher_email,
      demo = FALSE,
      languages = languages,
      logo = "www_psyquest/images/longgold_logo_transparent.png",
      logo_width = "100px",
      logo_height = "auto"
    )
  )
}

#' CCM Standalone
#'
#' This function launches a standalone testing session for the CCM questionnaire.
#' CCM stands for 'Concurrent Musical Activities'.
#'
#' @param languages (Character vector)
#' Determines the languages available to participants.
#' Possible languages include \code{"en"} (English), and \code{"de"} (German).
#' The first language is selected by default.
#'
#' @param subscales (Character vector) The subscales to be included in the questionnaire.
#' Possible subscales are \code{"General"}, and \code{"Extra"}.
#' If no subscales are provided all subscales for the questionnaire are selected.
#'
#' @param ... Further arguments to be passed to \code{\link{CCM_standalone}()}.
#'
#' @export
CCM_standalone <-
  function(languages = psyquest::languages(),
           subscales = NULL,
           ...)
    standalone(label = "CCM",
               languages = languages,
               subscales = subscales,
               ...)

#' CMT Standalone
#'
#' This function launches a standalone testing session for the CMT questionnaire.
#' CMT stands for 'Competence focus in Music Teaching'.
#'
#' @param languages (Character vector)
#' Determines the languages available to participants.
#' Possible languages include \code{"en"} (English), and \code{"de"} (German).
#' The first language is selected by default.
#'
#' @param ... Further arguments to be passed to \code{\link{CMT_standalone}()}.
#'
#' @export
CMT_standalone <-
  function(languages = psyquest::languages(), ...)
    standalone(label = "CMT", languages = languages, ...)

#' DAC Standalone
#'
#' This function launches a standalone testing session for the DAC questionnaire.
#' DAC stands for 'Drama Activity'.
#'
#' @param languages (Character vector)
#' Determines the languages available to participants.
#' Possible languages include \code{"en"} (English), and \code{"de"} (German).
#' The first language is selected by default.
#'
#' @param ... Further arguments to be passed to \code{\link{DAC_standalone}()}.
#'
#' @export
DAC_standalone <-
  function(languages = psyquest::languages(), ...)
    standalone(label = "DAC", languages = languages, ...)

#' DEG Standalone
#'
#' This function launches a standalone testing session for the DEG questionnaire.
#' DEG stands for 'Demographics'.
#'
#' @param languages (Character vector)
#' Determines the languages available to participants.
#' Possible languages include \code{"en"} (English), and \code{"de"} (German).
#' The first language is selected by default.
#'
#' @param subscales (Character vector) The subscales to be included in the questionnaire.
#' Possible subscales are "Best Shot", "Hearing Impairment", "Type of Hearing Impairment", "Gender", "Age", "Nationality", "Country Formative Years", "First Language", "Second Language", and "Handedness".S4methods
#' If no subscales are provided all subscales for the questionnaire are selected.
#'
#' @param ... Further arguments to be passed to \code{\link{DEG_standalone}()}.
#'
#' @export
DEG_standalone <-
  function(languages = psyquest::languages(),
           subscales = NULL,
           ...)
    standalone(label = "DEG",
               languages = languages,
               subscales = subscales,
               ...)
#' GDS Standalone
#'
#' This function launches a standalone testing session for the GDS questionnaire.
#' GDS stands for 'Goldsmiths Dance Sophistication Index'.
#'
#' @param languages (Character vector)
#' Determines the languages available to participants.
#' Possible languages only includes \code{"en"} (English).
#'
#' @param subscales (Character vector) There are 4 subscales, in general subscale and 6 items of 'dance experience observations' to be included in the questionnaire.
#' Possible subscales are \code{"Body Awareness"}, \code{"Social Dancing"}, \code{"Urge to Dance"}, \code{"Dance Training"}, \code{"General"}, and \code{"Observational Dance Experience"}.
#' If no subscales are provided all subscales for the questionnaire are selected.
#'
#' @param ... Further arguments to be passed to \code{\link{GDS_standalone}()}.
#'
#' @export
GDS_standalone <-
  function(languages = psyquest::languages(),
           subscales = NULL,
           ...)
    standalone(
      label = "GDS",
      languages = languages,
      subscales = subscales,
      ...
    )

#' GMS Standalone
#'
#' This function launches a standalone testing session for the GMS questionnaire.
#' GMS stands for 'Goldsmiths Musical Sophistication Index'.
#'
#' @param languages (Character vector)
#' Determines the languages available to participants.
#' Possible languages include \code{"en"} (English), and \code{"de"} (German).
#' The first language is selected by default.
#'
#' @param subscales (Character vector) The subscales to be included in the questionnaire.
#' Possible subscales are \code{"Abilities"}, \code{"Absolute Pitch"}, \code{"Active Engagement"}, \code{"Emotions"}, \code{"General"}, \code{"Instrument"}, \code{"Musical Training"}, \code{"Perceptual Abilities"}, \code{"Singing Abilities"}, and \code{"Start Age"}.
#' If no subscales are provided all subscales for the questionnaire are selected.
#' Overrides the \code{"short_version"} argument.
#' Overridden by the \code{configuration_filepath} argument.
#'
#' @param short_version (Scalar boolean) For the short version of the questionnaire set this to TRUE.
#' Defaults to FALSE.
#' Overridden by the \code{configuration_filepath} and \code{"subscales"} arguments.
#'
#' @param configuration_filepath (Character scalar) Optional path to a configuration file exported from the \href{https://shiny.gold-msi.org/gmsiconfigurator}{GMSI-Configurator}.
#' Overrides the \code{short_version} and \code{subscales} arguments.
#'
#' @param ... Further arguments to be passed to \code{\link{GMS_standalone}()}.
#'
#' @export
GMS_standalone <-
  function(languages = psyquest::languages(),
           subscales = NULL,
           short_version = FALSE,
           configuration_filepath = NULL,
           ...)
    standalone(
      label = "GMS",
      languages = languages,
      subscales = subscales,
      short_version = short_version,
      configuration_filepath = configuration_filepath,
      ...
    )

#' GRT Standalone
#'
#' This function launches a standalone testing session for the GRT questionnaire.
#' GRT stands for 'Short Grit Scale'.
#'
#' @param languages (Character vector)
#' Determines the languages available to participants.
#' Possible languages include \code{"en"} (English), and \code{"de"} (German).
#' The first language is selected by default.
#'
#' @param ... Further arguments to be passed to \code{\link{GRT_standalone}()}.
#'
#' @export
GRT_standalone <- function(languages = psyquest::languages(), ...)
  standalone(label = "GRT", languages = languages, ...)

#' HOP Standalone
#'
#' This function launches a standalone testing session for the HOP questionnaire.
#' HOP stands for 'Children’s Hope Scale'.
#'
#' @param languages (Character vector)
#' Determines the languages available to participants.
#' Possible languages include \code{"en"} (English), and \code{"de"} (German).
#' The first language is selected by default.
#'
#' @param ... Further arguments to be passed to \code{\link{HOP_standalone}()}.
#'
#' @export
HOP_standalone <-
  function(languages = psyquest::languages(), ...)
    standalone(label = "HOP", languages = languages, ...)

#' MHE Standalone
#'
#' This function launches a standalone testing session for the MHE questionnaire.
#' MHE stands for 'Musical Home Environment'.
#'
#' @param languages (Character vector)
#' Determines the languages available to participants.
#' Possible languages include \code{"en"} (English), and \code{"de"} (German).
#' The first language is selected by default.
#'
#' @param ... Further arguments to be passed to \code{\link{MHE_standalone}()}.
#'
#' @export
MHE_standalone <-
  function(languages = psyquest::languages(), ...)
    standalone(label = "MHE", languages = languages, ...)

#' PAC Standalone
#'
#' This function launches a standalone testing session for the PAC questionnaire.
#' PAC stands for 'Physical Activity'.
#'
#' @param languages (Character vector)
#' Determines the languages available to participants.
#' Possible languages include \code{"en"} (English), and \code{"de"} (German).
#' The first language is selected by default.
#'
#' @param ... Further arguments to be passed to \code{\link{PAC_standalone}()}.
#'
#' @export
PAC_standalone <-
  function(languages = psyquest::languages(), ...)
    standalone(label = "PAC", languages = languages, ...)

#' SCA Standalone
#'
#' This function launches a standalone testing session for the SCA questionnaire.
#' SCA stands for 'Academic Self-Concept'.
#'
#' @param languages (Character vector)
#' Determines the languages available to participants.
#' Possible languages include \code{"en"} (English), and \code{"de"} (German).
#' The first language is selected by default.
#'
#' @param short_version (Scalar boolean) For the short version of the questionnaire set this to TRUE.
#' Defaults to FALSE.
#'
#' @param ... Further arguments to be passed to \code{\link{SCA_standalone}()}.
#'
#' @export
SCA_standalone <-
  function(languages = psyquest::languages(),
           short_version = FALSE,
           ...)
    standalone(label = "SCA",
               languages = languages,
               short_version = short_version,
               ...)

#' SCS Standalone
#'
#' This function launches a standalone testing session for the SCS questionnaire.
#' SCS stands for 'Social Self-Concept'.
#'
#' @param languages (Character vector)
#' Determines the languages available to participants.
#' Possible languages include \code{"en"} (English), and \code{"de"} (German).
#' The first language is selected by default.
#'
#' @param short_version (Scalar boolean) For the short version of the questionnaire set this to TRUE.
#' Defaults to FALSE.
#'
#' @param ... Further arguments to be passed to \code{\link{SCS_standalone}()}.
#'
#' @export
SCS_standalone <-
  function(languages = psyquest::languages(),
           short_version = FALSE,
           ...)
    standalone(label = "SCS",
               languages = languages,
               short_version = short_version,
               ...)

#' SDQ Standalone
#'
#' This function launches a standalone testing session for the SDQ questionnaire.
#' SDQ stands for 'Strengths and Difficulties Questionnaire (mental health)'.
#'
#' @param languages (Character vector)
#' Determines the languages available to participants.
#' Possible languages include \code{"en"} (English), and \code{"de"} (German).
#' The first language is selected by default.
#'
#' @param subscales (Character vector) The subscales to be included in the questionnaire.
#' Possible subscales are \code{"Conduct problems"}, \code{"Difficulties"}, \code{"Emotional problems"}, \code{"Externalising"}, \code{"Hyperactivity"}, \code{"Internalising"}, \code{"Peer problems"}, and \code{"Prosocial"}.
#' If no subscales are provided all subscales for the questionnaire are selected.
#'
#' @param ... Further arguments to be passed to \code{\link{SDQ_standalone}()}.
#'
#' @export
SDQ_standalone <-
  function(languages = psyquest::languages(),
           subscales = NULL,
           ...)
    standalone(label = "SDQ",
               languages = languages,
               subscales = subscales,
               ...)

#' SEM Standalone
#'
#' This function launches a standalone testing session for the SEM questionnaire.
#' SEM stands for 'School Engagement Measurement'.
#'
#' @param languages (Character vector)
#' Determines the languages available to participants.
#' Possible languages include \code{"en"} (English), and \code{"de"} (German).
#' The first language is selected by default.
#'
#' @param subscales (Character vector) The subscales to be included in the questionnaire.
#' Possible subscales are \code{"Attentiveness"}, \code{"Behavioral Engagement"}, \code{"Cognitive Strategy Use"}, \code{"Education"}, \code{"Emotional Engagement"}, \code{"School belonging"}, \code{"School Compliance"}, \code{"Self-regulated Learning"}, and \code{"Valuing of School Education"}.
#' If no subscales are provided all subscales for the questionnaire are selected.
#'
#' @param ... Further arguments to be passed to \code{\link{SEM_standalone}()}.
#'
#' @export
SEM_standalone <-
  function(languages = psyquest::languages(),
           subscales = NULL,
           ...)
    standalone(label = "SEM",
               languages = languages,
               subscales = subscales,
               ...)

#' SES Standalone
#'
#' This function launches a standalone testing session for the SES questionnaire.
#' SES stands for 'Socio-economic Status'.
#'
#' @param languages (Character vector)
#' Determines the languages available to participants.
#' Possible languages include \code{"en"} (English), and \code{"de"} (German).
#' The first language is selected by default.
#'
#' @param subscales (Character vector) The subscales to be included in the questionnaire.
#' Possible subscales are \code{"Educational Degree"}, and \code{"ESeC"}.
#' If no subscales are provided all subscales for the questionnaire are selected.
#'
#' @param ... Further arguments to be passed to \code{\link{SES_standalone}()}.
#'
#' @export
SES_standalone <-
  function(languages = psyquest::languages(),
           subscales = NULL,
           ...)
    standalone(label = "SES",
               languages = languages,
               subscales = subscales,
               ...)

#' SMP Standalone
#'
#' This function launches a standalone testing session for the STOMP questionnaire.
#' SMP stands for 'Short Test of Musical Preferences (STOMP)'.
#'
#' @param languages (Character vector)
#' Determines the languages available to participants.
#' Possible languages include \code{"en"} (English), and \code{"de"} (German).
#' The first language is selected by default.
#'
#' @param subscales (Character vector) The subscales to be included in the questionnaire.
#' There are two possible sets subscales: 1. From the Do-re-mi paper: \code{"Reflective & Complex"},
#' \code{"Intense & Rebellious"}, \code{"Upbeat & Conventional"}, and \code{"Energetic & Rhythmic"};
#' 2. from the STOMP-R paper (MUSIC): \code{"Mellow"}, \code{"Unpretentious"}, \code{"Sophisticated"},
#' \code{"Intense"}, and \code{"Contemporary"}.
#'
#' If no subscales are provided all subscales for the questionnaire are selected.
#'
#'@param short_version (Scalar boolean) For the short version (original STOMP)
#'of the questionnaire set this to TRUE. Defaults to FALSE.
#'
#' @param ... Further arguments to be passed to \code{\link{SMP_standalone}()}.
#'
#' @export
SMP_standalone <-
  function(languages = psyquest::languages(),
           subscales = NULL,
           short_version = FALSE,
           ...)
    standalone(label = "SMP",
               languages = languages,
               subscales = subscales,
               short_version = FALSE,
               ...)

#' SOS Standalone
#'
#' This function launches a standalone testing session for the SOS questionnaire.
#' SOS stands for 'Student Opinion Scale'.
#'
#' @param languages (Character vector)
#' Determines the languages available to participants.
#' Possible languages include \code{"en"} (English), and \code{"de"} (German).
#' The first language is selected by default.
#'
#' @param subscales (Character vector) The subscales to be included in the questionnaire.
#' Possible subscales are \code{"Effort"}, and \code{"Importance"}.
#' If no subscales are provided all subscales for the questionnaire are selected.
#'
#' @param ... Further arguments to be passed to \code{\link{SOS_standalone}()}.
#'
#' @export
SOS_standalone <-
  function(languages = psyquest::languages(),
           subscales = NULL,
           ...)
    standalone(label = "SOS",
               languages = languages,
               subscales = subscales,
               ...)

#' TOI Standalone
#'
#' This function launches a standalone testing session for the TOI questionnaire.
#' TOI stands for 'Theory of Intelligence'.
#'
#' @param languages (Character vector)
#' Determines the languages available to participants.
#' Possible languages include \code{"en"} (English), and \code{"de"} (German).
#' The first language is selected by default.
#'
#' @param subscales (Character vector) The subscales to be included in the questionnaire.
#' Possible subscales are \code{"Goals Choice"}, and \code{"Theory of Inteligence"}.
#' If no subscales are provided all subscales for the questionnaire are selected.
#'
#' @param ... Further arguments to be passed to \code{\link{TOI_standalone}()}.
#'
#' @export
TOI_standalone <-
  function(languages = psyquest::languages(),
           subscales = NULL,
           ...)
    standalone(label = "TOI",
               languages = languages,
               subscales = subscales,
               ...)

#' TOM Standalone
#'
#' This function launches a standalone testing session for the TOM questionnaire.
#' TOM stands for 'Theory of Musicality'.
#'
#' @param languages (Character vector)
#' Determines the languages available to participants.
#' Possible languages include \code{"en"} (English), and \code{"de"} (German).
#' The first language is selected by default.
#'
#' @param subscales (Character vector) The subscales to be included in the questionnaire.
#' Possible subscales are \code{"Entity"}, \code{"Gift"}, \code{"Improvement"}, \code{"Incremental"}, \code{"Learning"}, and \code{"Stable"}.
#' If no subscales are provided all subscales for the questionnaire are selected.
#'
#' @param ... Further arguments to be passed to \code{\link{TOM_standalone}()}.
#'
#' @export
TOM_standalone <-
  function(languages = psyquest::languages(),
           subscales = NULL,
           ...)
    standalone(label = "TOM",
               languages = languages,
               subscales = subscales,
               ...)

#' TPI Standalone
#'
#' This function launches a standalone testing session for the TPI questionnaire.
#' TPI stands for 'Ten Item Personality Inventory'.
#'
#' @param languages (Character vector)
#' Determines the languages available to participants.
#' Possible languages include \code{"en"} (English), and \code{"de"} (German).
#' The first language is selected by default.
#'
#' @param subscales (Character vector) The subscales to be included in the questionnaire.
#' Possible subscales are \code{"Agreeableness"}, \code{"Conscientiousness"}, \code{"Emotional Stability"}, \code{"Extraversion"}, and \code{"Openness to Experiences"}.
#' If no subscales are provided all subscales for the questionnaire are selected.
#'
#' @param ... Further arguments to be passed to \code{\link{TPI_standalone}()}.
#'
#' @export
TPI_standalone <-
  function(languages = psyquest::languages(),
           subscales = NULL,
           ...)
    standalone(label = "TPI",
               languages = languages,
               subscales = subscales,
               ...)
