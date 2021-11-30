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
           min_year = 1930,
           max_year = 2013,
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
        id = response_ui_id,
        min_year = min_year,
        max_year = max_year
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
           id = "response_ui",
           min_year = 1930,
           max_year = 2013) {
    stopifnot(
      is.scalar.logical(hide),
      max_year >= min_year
    )
    months <- c("SELECT_MONTH", "JANUARY", "FEBRUARY", "MARCH", "APRIL", "MAY", "JUNE", "JULY", "AUGUST", "SEPTEMBER", "OCTOBER", "NOVEMBER", "DECEMBER")
    month_numbers <- c(NA, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
    months <- setNames(month_numbers, map(months, psychTestR::i18n))
    years <- c(psychTestR::i18n("SELECT_YEAR"), rev(c(min_year:max_year)))
    years_numbers <- c(NA, rev(c(min_year:max_year)))
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
