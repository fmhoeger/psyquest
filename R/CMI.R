#' CMI
#'
#' This function defines a CMI module for incorporation into a
#' psychTestR timeline.
#' Use this function if you want to include the CMI in a
#' battery of other tests, or if you want to add custom psychTestR
#' pages to your test timeline.
#' For a standalone implementation of the Child Musicality Index,
#' consider using \code{\link{CMI_standalone}()}.
#' @param label (Character scalar) Three uppercase letter acronym of the questionnaire.
#' This is also the label given to the results in the output file.
#' @param dict (i18n_dict) The psyquest dictionary used for internationalisation.
#' @param ... Further arguments to be passed to \code{\link{CMI}()}.
#' @export
CMI <- function(label = "CMI",
                silent = T,
                use_thumbscale = F,
                dict = psyquest::psyquest_dict,
                ...) {
  stopifnot(purrr::is_scalar_character(label))

  questionnaire_id <- "CMI"
  dots <- list(...)
  if("button_style" %in% names(dots)){
    button_style <- dots$button_style
  }
  else{
    button_style <- "min-width: 250px;"
  }
  main_test_cmi(
    questionnaire_id = questionnaire_id,
    label = label,
    items = get_items(questionnaire_id),
    offset = 0,
    silent = silent,
    use_thumbscale = use_thumbscale,
    button_style = button_style,
    dict = dict
  )

}


main_test_cmi <- function(questionnaire_id,
                      label,
                      items,
                      subscales = c(),
                      offset = 0,
                      button_style = "",
                      silent = T,
                      use_thumbscale = F,
                      dict = psyquest::psyquest_dict,
                      style_params = NULL) {
  elts <- c()
  target_ext <- style_params$target
  prompt_id <- NULL
  prompt_ids <- items %>% pull(prompt_id)
  question_numbers <- as.numeric(gsub("[^0-9]", "", prompt_ids))
  item_pages <- list()

  for (counter in 1:length(question_numbers)) {
    question_label <- sprintf("q%d", question_numbers[counter] - offset)
    item_bank_row <-
      items %>%
      filter(stringr::str_detect(prompt_id, sprintf("T%s_%04d", questionnaire_id, question_numbers[counter])))
    num_of_options <- strsplit(item_bank_row$option_type, "-")[[1]][1] %>% as.integer()
    choices <- sprintf("btn%d_text", 1:num_of_options)
    choice_ids <- sprintf("T%s_%04d_CHOICE%d",
                          questionnaire_id,
                          question_numbers[counter],
                          1:num_of_options)

    bs <- button_style[1]
    #item_bank_row$layout <- NA
    #arrange_vertically = FALSE
    #bs <- "max_width:100px"
    if(!is.na(item_bank_row$layout)){
      arrange_vertically <- tolower(item_bank_row$layout[1]) == "vertical"
      if(!arrange_vertically & length(button_style) >  0){
        bs <- button_style[2]
      }
    }
    #choice_labels <- map(choice_ids, psychTestR::i18n)
    thumb_labels <- thumb_scale(num_of_options)
    if(silent){
      if(use_thumbscale){
        item_page <- psychTestR::new_timeline(
          psychTestR::NAFC_page(
            label = question_label,
            prompt = get_prompt(
              counter,
              length(question_numbers),
              sprintf("T%s_%04d_PROMPT", questionnaire_id, question_numbers[counter]),
              with_prompt_head = F,
              style_params
            ),

            choices = choices,
            arrange_vertically = FALSE,
            button_style = bs,
            labels = thumb_labels
          ),
          dict = dict
        )

      }
      else{

        item_page <- psychTestR::new_timeline(

          psychTestR::NAFC_page(
            label = question_label,

            prompt = get_prompt(
              counter,
              length(question_numbers),
              sprintf("T%s_%04d_PROMPT", questionnaire_id, question_numbers[counter]),
              with_prompt_head = F,
              style_params
            ),

            choices = choices,
            arrange_vertically = TRUE,
            button_style = bs,
            labels = map(choice_ids, psychTestR::i18n)
          ),
          dict = dict
        )

      }
    }
    else{
      if(use_thumbscale){
        audio_url <- sprintf("https://s3.eu-west-1.amazonaws.com/media.gold-msi.org/test_materials/CMI/TCMI_%04d_PROMPT.mp3", question_numbers[counter] )
        item_page <- psychTestR::new_timeline(
          psychTestR::audio_NAFC_page(
            url = audio_url,
            label = question_label,
            prompt = get_prompt(
              counter,
              length(question_numbers),
              sprintf("T%s_%04d_PROMPT", questionnaire_id, question_numbers[counter]),
              with_prompt_head = F,
              style_params
            ),
            btn_play_prompt = shiny::span("▶", style = "font-size:xx-large"),

            choices = choices,
            arrange_choices_vertically = FALSE,
            button_style = bs,
            labels = thumb_labels
          ),
          dict = dict
        )

      }
      else{
        audio_url <- sprintf("https://s3.eu-west-1.amazonaws.com/media.gold-msi.org/test_materials/CMI/TCMI_%04d_PROMPT.mp3", question_numbers[counter] )
        item_page <- psychTestR::new_timeline(
          psychTestR::audio_NAFC_page(
            url = audio_url,
            label = question_label,
            prompt = get_prompt(
              counter,
              length(question_numbers),
              sprintf("T%s_%04d_PROMPT", questionnaire_id, question_numbers[counter]),
              with_prompt_head = F,
              style_params
            ),
            btn_play_prompt = shiny::span("▶", style = "font-size:xx-large"),

            choices = choices,
            arrange_choices_vertically = TRUE,
            button_style = bs,
            labels =  map(choice_ids, psychTestR::i18n)
          ),
          dict = dict
        )

      }

    }
    #elts <- psychTestR::join(elts, item_page)
    item_pages <- c(item_pages, item_page)
  }
  elts <- psychTestR::join(elts, item_pages)

  do.call(psychTestR::join, elts)
  psychTestR::join(psychTestR::begin_module(label = label),
                   elts,
                   scoring(questionnaire_id, label, items, subscales, short_version),
                   psychTestR::elt_save_results_to_disk(complete = TRUE),
                   psychTestR::code_block(function(state, ...){
                     res <- psychTestR::get_results(state, complete = TRUE) %>% as.list()
                     #browser()
                   }),
                   psychTestR::end_module())
}
