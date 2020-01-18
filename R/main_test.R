main_test <- function(questionnaire, label, num_items, offset = 1) {
  elts <- c()
  for (item_id in (offset + 1):(offset + num_items)) {
    label <- sprintf("q%d", item_id - offset)
    item_bank_row  <-
      psyquest::psyquest_item_bank %>%
      filter(stringr::str_detect(prompt_id,
                        sprintf("T%s_%04d", questionnaire, item_id)))
    num_of_options <- strsplit(item_bank_row$option_type, '-')[[1]][1]
    choices <- sprintf("btn%d_text", 1:num_of_options)
    choice_ids <- sprintf("T%s_%04d_CHOICE%d", questionnaire, item_id, 1:num_of_options)

    item_page <- psychTestR::new_timeline(
      psychTestR::NAFC_page(
        label = label,
        prompt = get_prompt(
          item_id - offset,
          num_items,
          sprintf("T%s_%04d_PROMPT", questionnaire,  item_id)
        ),
        choices = choices,
        labels = purrr::map(choice_ids, psychTestR::i18n)
      ),
      dict = psyquest::psyquest_dict
    )
    elts <- c(elts, item_page)
  }
  psychTestR::join(elts)
}

get_prompt <- function(item_number,
                       num_items_in_test,
                       prompt_id) {
  shiny::div(
    shiny::h4(
      psychTestR::i18n(
        "PAGE_HEADER",
        sub = list(num_question = item_number,
                   test_length = if (is.null(num_items_in_test))
                    "?" else
                    num_items_in_test)),
      style  = "text_align:center"
    ),
    shiny::p(
      psychTestR::i18n(prompt_id),
      style = "margin-left:20%;margin-right:20%;text-align:justify")
  )
}
