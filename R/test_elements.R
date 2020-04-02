#' New NAFC radiobuttons page
#'
#' Creates an n-option multiple choice page.
#'
#' @param label Label for the current page (character scalar).
#'
#' @param prompt Prompt to be displayed above the response choices.
#' Can be either a character scalar (e.g. "What is 2 + 2?")
#' or an object of class "shiny.tag", e.g. \code{shiny::tags$p("What is 2 + 2?")}.
#'
#' @param sublabel Additional text appearing above the radiobuttons (character scalar).
#'
#' @param choiceNames Character vector of choice names used as labels for the radiobuttons
#'
#' @param choiceValues Character vector of choice names for the radiobuttons
#'
#' @param labels Optional vector of labels for the NAFC radiobutton choices.
#' If not \code{NULL}, will overwrite the names of \code{choices}.
#' This vector of labels can either be a character vector
#' or a list of Shiny tag objects, e.g. as created by \code{shiny::HTML()}.
#'
#' @param save_answer Whether or not to save the answer.
#'
#' @param arrange_vertically Whether to arrange the checkboxes vertically
#' (the default) as opposed to horizontally.
#'
#' @param hide_response_ui Whether to begin with the response interface hidden
#' (it can be subsequently made visible through Javascript,
#' using the element ID as set in \code{response_ui_id}.
#' See \code{audio_NAFC_page} for an example.).
#'
#' @param response_ui_id HTML ID for the response user interface.
#'
#' @param on_complete Optional function to execute on leaving the page
#' (after successful validation).
#' The argument list should include \code{...},
#' and any of:
#' \code{state}, the participant's state object;
#' \code{answer}, the participant's most recent answer;
#' \code{input}, the current page's Shiny input object;
#' \code{session}, the current Shiny session object;
#' \code{opt}, the test's option list as created by \code{test_options()}.
#'
#' @param admin_ui Optional UI component for the admin panel.
#'
#' @param failed_validation_message Text to display when validation fails (character scalar).
#'
#' @export
NAFC_radiobuttons_page <-
  function(label,
           prompt,
           sublabel,
           choiceNames,
           choiceValues,
           labels = NULL,
           save_answer = TRUE,
           arrange_vertically = length(choiceNames) > 2L,
           hide_response_ui = FALSE,
           response_ui_id = "response_ui",
           on_complete = NULL,
           admin_ui = NULL,
           failed_validation_message = "Answer missing!") {
    stopifnot(
      is.scalar.character(label),
      length(choiceNames) > 0L,
      is.scalar.logical(arrange_vertically)
    )
    ui <- shiny::div(
      tagify(prompt),
      make_ui_NAFC_radiobuttons(
        label,
        choiceNames,
        choiceValues,
        sublabel = sublabel,
        labels = labels,
        hide = hide_response_ui,
        arrange_vertically = arrange_vertically,
        id = response_ui_id
      )
    )
    get_answer <- function(input, ...)
      input[[label]]
    validate <- function(answer, ...)
      if (!is.null(answer)) {
        TRUE
      } else {
        failed_validation_message
      }
    page(
      ui = ui,
      label = label,
      get_answer = get_answer,
      save_answer = save_answer,
      validate = validate,
      on_complete = on_complete,
      final = FALSE,
      admin_ui = admin_ui
    )
  }

#' Make NAFC radiobuttons
#'
#' Creates HTML code for n-option multiple choice response options.
#'
#' @param label Label for the current page (character scalar).
#'
#' @param choiceNames Character vector of choice names used as labels for the radiobuttons
#'
#' @param choiceValues Character vector of choice names for the radiobuttons
#'
#' @param sublabel Additional text appearing above the radiobuttons (character scalar).
#'
#' @param labels Optional vector of labels for the NAFC radiobutton choices.
#' If not \code{NULL}, will overwrite the names of \code{choices}.
#' This vector of labels can either be a character vector
#' or a list of Shiny tag objects, e.g. as created by \code{shiny::HTML()}.
#'
#' @param hide Whether the checkboxes should be hidden
#' (possibly to be shown later).
#'
#' @param arrange_vertically Whether to arrange the checkboxes vertically
#' (the default) as opposed to horizontally.
#'
#' @param id HTML ID for the div containing the checkboxes.
#'
#' @export
make_ui_NAFC_radiobuttons <-
  function(label,
           choiceNames,
           choiceValues,
           sublabel,
           labels = NULL,
           hide = FALSE,
           arrange_vertically = length(choiceNames) > 2L,
           id = "response_ui") {
    stopifnot(
      length(choiceNames) > 0L,
      is.scalar.logical(hide),
      is.null(labels) ||
        ((is.character(labels) || is.list(labels)) &&
           length(labels) == length(choiceNames)
        )
    )
    if (is.null(labels)) {
      labels <- if (is.null(names(choiceNames)))
        choiceNames
      else
        names(choiceNames)
    }
    outer_div <-
      shiny::tags$div(id = id, style = "text-align: left;", shiny::tags$strong(sublabel))
    radiobuttons <- shiny::tags$div(style = "text-align: left;", outer_div,
                                    shiny::radioButtons(label, "",
                                                        choiceNames = choiceNames,
                                                        choiceValues = choiceValues,
                                                        selected = 0))

    shiny::tags$div(id = "rb", style = "display: inline-block", radiobuttons, psychTestR::trigger_button("next", psychTestR::i18n("CONTINUE")))
  }


#' New NOMC page
#'
#' Creates an n-option multiple choice page.
#'
#' @param label Label for the current page (character scalar).
#'
#' @param prompt Prompt to be displayed above the response choices.
#' Can be either a character scalar (e.g. "What is 2 + 2?")
#' or an object of class "shiny.tag", e.g. \code{shiny::tags$p("What is 2 + 2?")}.
#'
#' @param choiceNames Character vector of choice names used as labels for the checkboxes.
#'
#' @param choiceValues Character vector of choice names for the checkboxes
#'
#' @param sublabel Additional text appearing above the checkboxes (character scalar).
#'
#' @param labels Optional vector of labels for the NOMC choices.
#' If not \code{NULL}, will overwrite the names of \code{choices}.
#' This vector of labels can either be a character vector
#' or a list of Shiny tag objects, e.g. as created by \code{shiny::HTML()}.
#'
#' @param save_answer Whether or not to save the answer.
#'
#' @param arrange_vertically Whether to arrange the checkboxes vertically
#' (the default) as opposed to horizontally.
#'
#' @param hide_response_ui Whether to begin with the response interface hidden
#' (it can be subsequently made visible through Javascript,
#' using the element ID as set in \code{response_ui_id}.
#' See \code{audio_NAFC_page} for an example.).
#'
#' @param response_ui_id HTML ID for the response user interface.
#'
#' @param on_complete Optional function to execute on leaving the page
#' (after successful validation).
#' The argument list should include \code{...},
#' and any of:
#' \code{state}, the participant's state object;
#' \code{answer}, the participant's most recent answer;
#' \code{input}, the current page's Shiny input object;
#' \code{session}, the current Shiny session object;
#' \code{opt}, the test's option list as created by \code{test_options()}.
#'
#' @param admin_ui Optional UI component for the admin panel.
#'
#' @param force_answer Require at least one checkbox to be ticked.
#'
#' @param failed_validation_message Text to display when validation fails (character scalar).
#'
#' @export
NOMC_page <-
  function(label,
           prompt,
           sublabel,
           choiceNames,
           choiceValues,
           labels = NULL,
           save_answer = TRUE,
           arrange_vertically = length(choiceNames) > 2L,
           hide_response_ui = FALSE,
           response_ui_id = "response_ui",
           on_complete = NULL,
           admin_ui = NULL,
           force_answer = FALSE,
           failed_validation_message = "Please select at least one answer.") {
    stopifnot(
      is.scalar.character(label),
      length(choiceNames) > 0L,
      is.scalar.logical(arrange_vertically)
    )
    ui <- shiny::div(
      tagify(prompt),
      make_ui_NOMC(
        label,
        choiceNames,
        choiceValues,
        sublabel = sublabel,
        labels = labels,
        hide = hide_response_ui,
        arrange_vertically = arrange_vertically,
        id = response_ui_id
      )
    )
    get_answer <- function(input, ...) {
      ret <- if (is.null(input[[label]])) {
        ""
      } else {
        paste(input[[label]], collapse = ',')
      }
      ret
    }
    validate <- function(answer, ...)
      if (answer == "" && force_answer) {
        failed_validation_message
      } else {
        TRUE
      }
    page(
      ui = ui,
      label = label,
      get_answer = get_answer,
      save_answer = save_answer,
      validate = validate,
      on_complete = on_complete,
      final = FALSE,
      admin_ui = admin_ui
    )
  }

#' Make NOMC checkboxes
#'
#' Creates HTML code for n-option multiple choice response options.
#'
#' @param label Label for the current page (character scalar).

#' @param choiceNames Character vector of choice names used as labels for the checkboxes.
#'
#' @param choiceValues Character vector of choice names for the checkboxes
#'
#' @param sublabel Additional text appearing above the checkboxes (character scalar).
#'
#' @param labels Optional vector of labels for the NOMC choices.
#' If not \code{NULL}, will overwrite the names of \code{choices}.
#' This vector of labels can either be a character vector
#' or a list of Shiny tag objects, e.g. as created by \code{shiny::HTML()}.
#'
#' @param hide Whether the checkboxes should be hidden
#' (possibly to be shown later).
#'
#' @param arrange_vertically Whether to arrange the checkboxes vertically
#' (the default) as opposed to horizontally.
#'
#' @param id HTML ID for the div containing the checkboxes.
#'
#' @export
make_ui_NOMC <-
  function(label,
           choiceNames,
           choiceValues,
           sublabel,
           labels = NULL,
           hide = FALSE,
           arrange_vertically = length(choiceNames) > 2L,
           id = "response_ui") {
    stopifnot(
      length(choiceNames) > 0L,
      is.scalar.logical(hide),
      is.null(labels) ||
        ((is.character(labels) || is.list(labels)) &&
           length(labels) == length(choiceNames)
        )
    )
    if (is.null(labels)) {
      labels <- if (is.null(names(choiceNames)))
        choiceNames
      else
        names(choiceNames)
    }
    outer_div <-
      shiny::tags$div(id = id, style = "text-align: left;", shiny::tags$strong(sublabel))
    checkboxes <- shiny::tags$div(style = "text-align: left;", outer_div,
      shiny::checkboxGroupInput(label, "",
        choiceNames = choiceNames, choiceValues = choiceValues))

    shiny::tags$div(id = "rb", style = "display: inline-block", checkboxes, psychTestR::trigger_button("next", psychTestR::i18n("CONTINUE")))
  }

#' New month and year select page
#'
#' Creates month and year select page.
#'
#' @param label Label for the current page (character scalar).
#'
#' @param prompt Prompt to be displayed above the response choices.
#' Can be either a character scalar (e.g. "What is 2 + 2?")
#' or an object of class "shiny.tag", e.g. \code{shiny::tags$p("What is 2 + 2?")}.
#'
#' @param save_answer Whether or not to save the answer.
#'
#' @param hide_response_ui Whether to begin with the response interface hidden
#' (it can be subsequently made visible through Javascript,
#' using the element ID as set in \code{response_ui_id}.
#' See \code{audio_NAFC_page} for an example.).
#'
#' @param response_ui_id HTML ID for the response user interface.
#'
#' @param on_complete Optional function to execute on leaving the page
#' (after successful validation).
#' The argument list should include \code{...},
#' and any of:
#' \code{state}, the participant's state object;
#' \code{answer}, the participant's most recent answer;
#' \code{input}, the current page's Shiny input object;
#' \code{session}, the current Shiny session object;
#' \code{opt}, the test's option list as created by \code{test_options()}.
#'
#' @param admin_ui Optional UI component for the admin panel.
#'
#' @export
month_and_year_select_page <-
  function(label,
           prompt,
           save_answer = TRUE,
           hide_response_ui = FALSE,
           response_ui_id = "response_ui",
           on_complete = NULL,
           admin_ui = NULL) {
    stopifnot(
      is.scalar.character(label)
    )
    ui <- shiny::div(
      tagify(prompt),
      make_ui_month_and_year_select(
        label,
        hide = hide_response_ui,
        id = response_ui_id
      )
    )
    get_answer <- function(input, ...) {
      ret <- if (is.null(input[["month"]])) {
        ""
      } else {
        c(input[["month"]], input[["year"]])
      }
      ret
    }
    validate <- function(answer, ...) TRUE
    page(
      ui = ui,
      label = label,
      get_answer = get_answer,
      save_answer = save_answer,
      validate = validate,
      on_complete = on_complete,
      final = FALSE,
      admin_ui = admin_ui
    )
  }

#' Make month and year selectboxes
#'
#' Creates HTML code for month and year selectboxes.
#'
#' @param label Label for the current page (character scalar).
#'
#' @param hide Whether the checkboxes should be hidden
#' (possibly to be shown later).
#'
#' @param id HTML ID for the div containing the checkboxes.
#'
#' @export
make_ui_month_and_year_select <-
  function(label,
           hide = FALSE,
           id = "response_ui") {
    stopifnot(
      is.scalar.logical(hide)
    )

    months <- c("SELECT_MONTH", "JANUARY", "FEBRUARY", "MARCH", "APRIL", "MAY", "JUNE", "JULY", "AUGUST", "SEPTEMBER", "OCTOBER", "NOVEMBER", "DECEMBER")
    month_numbers <- c(NA, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
    months <- setNames(month_numbers, purrr::map(months, psychTestR::i18n))
    years <- c(psychTestR::i18n("SELECT_YEAR"), c(1995:2013))

    outer_div <-
      shiny::tags$div(id = id)
    selectboxes <-
      shiny::tags$div(outer_div,
      shiny::selectizeInput("month", label = psychTestR::i18n("MONTH"),
                                        choices = months, multiple = FALSE),
      shiny::selectizeInput("year", label = psychTestR::i18n("YEAR"),
                                        choices = years, multiple = FALSE))

    shiny::tags$div(id = "rb", style = "width: 300px", selectboxes, psychTestR::trigger_button("next", psychTestR::i18n("CONTINUE")))
  }
