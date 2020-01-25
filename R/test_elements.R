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
#' @param choices Character vector of choices for the participant.
#' If unnamed, then these values will be used both for checkbox IDs
#' and for checkbox labels.
#' If named, then values will be used for checkbox IDs and names
#' will be used for checkbox labels.
#'
#' @param labels Optional vector of labels for the NAFC radiobutton choices.
#' If not \code{NULL}, will overwrite the names of \code{choices}.
#' This vector of labels can either be a character vector
#' or a list of Shiny tag objects, e.g. as created by \code{shiny::HTML()}.
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
#' @inheritParams page
#' @inheritParams make_ui_NOMC
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
           admin_ui = NULL) {
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
      ! is.null(answer)
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
#' @param choices Character vector of choices for the participant.
#' If unnamed, then these values will be used both for checkboxes IDs
#' and for checkboxes labels.
#' If named, then values will be used for checkboxes IDs and names
#' will be used for checkboxes labels.
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
      radioButtons(label, "",
        choiceNames = choiceNames, choiceValues = choiceValues, selected = 0))

    shiny::tags$div(id = "rb", radiobuttons, psychTestR::trigger_button("next", psychTestR::i18n("CONTINUE")))
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
#' @param choices Character vector of choices for the participant.
#' If unnamed, then these values will be used both for checkbox IDs
#' and for checkbox labels.
#' If named, then values will be used for checkbox IDs and names
#' will be used for checkbox labels.
#'
#' @param labels Optional vector of labels for the NOMC choices.
#' If not \code{NULL}, will overwrite the names of \code{choices}.
#' This vector of labels can either be a character vector
#' or a list of Shiny tag objects, e.g. as created by \code{shiny::HTML()}.
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
#' @inheritParams page
#' @inheritParams make_ui_NOMC
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
           admin_ui = NULL) {
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
      print(input[[label]])
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

#' Make NOMC checkboxes
#'
#' Creates HTML code for n-option multiple choice response options.
#'
#' @param choices Character vector of choices for the participant.
#' If unnamed, then these values will be used both for checkboxes IDs
#' and for checkboxes labels.
#' If named, then values will be used for checkboxes IDs and names
#' will be used for checkboxes labels.
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
      checkboxGroupInput(label, "",
        choiceNames = choiceNames, choiceValues = choiceValues))

    shiny::tags$div(id = "rb", checkboxes, psychTestR::trigger_button("next", psychTestR::i18n("CONTINUE")))
  }
