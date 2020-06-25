#' New radiobutton NAFC page
#'
#' Creates a radiobutton n-alternative forced choice page.
#'
#' @param label (Character scalar) Label for the current page.
#'
#' @param prompt Prompt to be displayed above the response choices.
#' Can be either a character scalar (e.g. "What is 2 + 2?")
#' or an object of class "shiny.tag", e.g. \code{shiny::tags$p("What is 2 + 2?")}.
#'
#' @param subprompt (Character scalar) Additional text in bold letters below the prompt.
#'
#' @param choiceNames Character vector of choice names used as labels for the radiobuttons
#'
#' @param choiceValues Character vector of choice names for the radiobuttons
#'
#' @param labels Optional vector of labels for the NAFC radiobutton choices.
#' If not \code{NULL}, will overwrite the names of \code{choiceNames}.
#' This vector of labels can either be a character vector
#' or a list of Shiny tag objects, e.g. as created by \code{shiny::HTML()}.
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
#' @param failed_validation_message (Character scalar) Text to be displayed when validation fails.
#'
#' @inheritParams psychTestR::page
#' @inheritParams make_ui_radiobutton_NAFC
#'
#' @export
radiobutton_NAFC_page <-
  function(label,
           prompt,
           subprompt,
           choiceNames,
           choiceValues,
           labels = NULL,
           save_answer = TRUE,
           hide_response_ui = FALSE,
           response_ui_id = "response_ui",
           on_complete = NULL,
           admin_ui = NULL,
           failed_validation_message = "Answer missing!") {
    stopifnot(
      is.scalar.character(label),
      length(choiceNames) > 0L
    )
    ui <- shiny::div(
      tagify(prompt),
      make_ui_radiobutton_NAFC(
        label,
        choiceNames,
        choiceValues,
        subprompt = subprompt,
        labels = labels,
        hide = hide_response_ui,
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
#' Creates HTML code for n-alternative forced-choice response radiobutton options.
#'
#' @param label (Character scalar) Label for the current page.
#'
#' @param choiceNames Character vector of choice names used as labels for the radiobuttons.
#'
#' @param choiceValues Character vector of choice names for the radiobuttons.
#'
#' @param subprompt Additional text appearing above the radiobuttons (character scalar).
#'
#' @param labels Optional vector of labels for the NAFC radiobutton choices.
#' If not \code{NULL}, will overwrite the names of \code{choices}.
#' This vector of labels can either be a character vector
#' or a list of Shiny tag objects, e.g. as created by \code{shiny::HTML()}.
#'
#' @param hide Whether the radiobuttons should be hidden (possibly to be shown later).
#'
#' @param id HTML ID for the div containing the radiobuttons.
#'
#' @export
make_ui_radiobutton_NAFC <-
  function(label,
           choiceNames,
           choiceValues,
           subprompt,
           labels = NULL,
           hide = FALSE,
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
      shiny::tags$div(style = "text-align: center;", shiny::tags$strong(subprompt))
    radiobuttons <- shiny::tags$div(style = "text-align: left;", outer_div,
                                    shiny::tags$div(style = "display: table; margin: 0 auto;", shiny::tags$div(style = "display: inline-block; width:100%", shiny::radioButtons(label, "",
                                                        choiceNames = choiceNames,
                                                        choiceValues = choiceValues,
                                                        selected = 0))))

    shiny::tags$div(id = id, style = "display: inline-block", radiobuttons, psychTestR::trigger_button("next", psychTestR::i18n("CONTINUE")))
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
#' @param javascript JavaScript code to be added to the div enclosing the checkboxes.
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
           javascript = "",
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
        id = response_ui_id,
        javascript = javascript
      )
    )
    get_answer <- function(input, ...) {
      ret <- if (is.null(input[[label]])) {
        ""
      } else {
        paste(input[[label]], collapse = ",")
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
#' @param javascript JavaScript code to be added to the div enclosing the checkboxes.
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
           id = "response_ui",
           javascript = "") {
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
      shiny::tags$div(id = id, style = "text-align: center;", shiny::tags$strong(sublabel), shiny::tags$script(shiny::HTML(javascript)))
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
#' @param validate Validation function to execute.
#'
#' @param failed_validation_message Message to be displayed when validation fails.
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
           validate = month_and_year_select_page.validate(),
           hide_response_ui = FALSE,
           response_ui_id = "response_ui",
           on_complete = NULL,
           admin_ui = NULL,
           failed_validation_message = psychTestR::i18n("SELECT_MONTH_AND_YEAR")) {
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
      c(input$month, input$year)
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

#' Validate month and year from the month and year select page
#'
month_and_year_select_page.validate <- function() {
  function(state, input, ...) {
    if (input$month != "NA" && input$year != "NA") {
     TRUE
    } else {
      psychTestR::i18n("SELECT_MONTH_AND_YEAR")
    }
  }
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
    years <- c(psychTestR::i18n("SELECT_YEAR"), rev(c(1930:2013)))
    years_numbers <- c(NA, rev(c(1930:2013)))
    years <- setNames(years_numbers, years)

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
