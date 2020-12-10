#' MUS(MUSIC)
#'
#' This function defines a SMP module for incorporation into a
#' psychTestR timeline.
#' Use this function if you want to include the Musical Preferences: Five factor
#' model in a battery of other tests, or if you want to add custom psychTestR
#' pages to your test timeline.
#' For a standalone implementation of the MUS,
#' consider using \code{\link{MUS_standalone}()}.
#'
#' @param label (Character scalar) Three uppercase letter acronym of the questionnaire.
#' This is also the label given to the results in the output file.
#'
#' @param dict (i18n_dict) The psyquest dictionary used for internationalisation.
#'
#' @param subscales (Character vector) The subscales to be included in the questionnaire.
#' There is a set of posible subscales (MUSIC): \code{"Mellow"}, \code{"Unpretentious"}, \code{"Sophisticated"},
#' \code{"Intense"}, and \code{"Contemporary"}.
#' If no subscales are provided all subscales for the questionnaire are selected.
#'
#' @param ... Further arguments to be passed to \code{\link{MUS}()}.
#'
#' @export

MUS <- function(label = "MUS",
                dict = psyquest_dict,
                subscales = c(),
                ...) {
  audio_dir <- "https://media.gold-msi.org/test_materials/MUSIC/"
  audio_dir <- gsub("/$", "", audio_dir)
  stopifnot(purrr::is_scalar_character(label))

  main_test_mus(
    label = label,
    items = get_items(label,
                      subscales = subscales),
    audio_dir = audio_dir,
    subscales = subscales
  )
}

main_test_mus <- function(label, items, subscales, audio_dir) {
  elts <- c()
  links <- c("15635_C1.mp3", "17884_U1.mp3", "38370_I4.mp3", "38843_S1.mp3",
             "41925_C2.mp3", "51061_C3.mp3", "53334_C4.mp3", "54767_U2.mp3",
             "56045_S2.mp3", "58264_U3.mp3", "58469_M5.mp3", "62187_S3.mp3",
             "73851_S4.mp3", "74532_S5.mp3", "76683_S6.mp3", "77316_C5.mp3",
             "90042_I5.mp3", "94357_U4.mp3", "110353_M1.mp3", "112632_I1.mp3",
             "116480_M2.mp3", "128093_M3.mp3", "147989_I2.mp3", "147991_I3.mp3",
             "159346_M4.mp3")
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

    button_style <- "margin-bottom: 4px"

    item_page <- psychTestR::new_timeline(
      psychTestR::audio_NAFC_page(
        label = question_label,
        prompt = get_prompt(
          counter,
          length(question_numbers),
          sprintf("T%s_%04d_PROMPT", label, question_numbers[counter])
        ),
        url = file.path(audio_dir, links[counter]),
        choices = choices,
        #arrange_vertically = TRUE,
        #button_style = button_style,
        labels = map(choice_ids, psychTestR::i18n)
      ),
      dict = psyquest::psyquest_dict
    )
    elts <- psychTestR::join(elts, item_page)
  }

  psychTestR::join(psychTestR::begin_module(label),
                   elts,
                   scoring(label, items, subscales),
                   psychTestR::end_module())
}
