#' GMS
#'
#' This function defines a GMS module for incorporation into a
#' psychTestR timeline.
#' Use this function if you want to include the GMS in a
#' battery of other tests, or if you want to add custom psychTestR
#' pages to your test timeline.
#' For a standalone implementation of the GMS,
#' consider using \code{\link{GMS_standalone}()}.
#' @param label (Character scalar) Three uppercase letter acronym of the questionnaire.
#' This is also the label given to the results in the output file.
#' @param dict The psyquest dictionary used for internationalisation.
#' @param subscales (Character vector) The subscales to be included in the questionnaire.
#' When no subscales are provided all subscales are selected.
#' @param ... Further arguments to be passed to \code{\link{GMS}()}.
#' @export
GMS <- function(label = "GMS",
                dict = psyquest::psyquest_dict,
                subscales = c(),
                ...) {
  stopifnot(purrr::is_scalar_character(label))

  main_test_gms(
    label = label,
    items = get_items(label, subscales),
    subscales = subscales
  )
}

main_test_gms <- function(label, items, subscales) {
  elts <- c()
  prompt_id <- NULL
  prompt_ids <- items %>% pull(prompt_id)

  question_numbers <- as.numeric(gsub("[^0-9]", "", prompt_ids))

  for (counter in seq_along(numeric(length(question_numbers)))) {
    question_label <- sprintf("q%d", question_numbers[counter])
    item_bank_row <-
      items %>%
      filter(stringr::str_detect(prompt_id, sprintf("T%s_%04d", label, question_numbers[counter])))
    num_of_options <- strsplit(item_bank_row$option_type, "-")[[1]][1]
    choices <- sprintf("btn%d_text", 1:num_of_options)
    choice_ids <- sprintf("T%s_%04d_CHOICE%d", label, question_numbers[counter], 1:num_of_options)

    arrange_vertically <- TRUE
    if (question_numbers[counter] %in% c(2, 12, 17, 18, 21, 22, 31, 32, 40, 41)) {
      arrange_vertically <- FALSE
    }

    style <- "margin-bottom: 4px"
    if (!question_numbers[counter] %in% c(2, 12, 17, 18, 21, 22, 31, 32, 40, 41)) {
      style <- paste(style, "min-width: 214px", sep="; ")
    }

    item_page <- psychTestR::new_timeline(
      psychTestR::NAFC_page(
        label = question_label,
        prompt = get_prompt(
          counter,
          length(question_numbers),
          sprintf("T%s_%04d_PROMPT", label,  question_numbers[counter])
        ),
        choices = choices,
        arrange_vertically = arrange_vertically,
        style = style,
        labels = purrr::map(choice_ids, psychTestR::i18n)
      ),
      dict = psyquest::psyquest_dict
    )
    elts <- c(elts, item_page)
  }

  psychTestR::join(psychTestR::begin_module(label),
                   elts,
                   scoring(label, items, subscales),
                   psychTestR::end_module())

}
