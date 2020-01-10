
DAC_main_test <- function(label, num_items, offset = 1) {
  elts <- c()
  num_options <- 5
  for(item_id in (offset + 1):(offset + num_items)){
    label <- sprintf("q%d", item_id - offset)
    choices <- sprintf("btn%d_text", 1:num_options)
    choice_ids <- sprintf("TDAC_000%d_CHOICE%d", item_id, 1:num_options)
    item_page <- psychTestR::new_timeline(
      psychTestR::NAFC_page(label = label,
                            prompt = get_prompt(item_id - offset, num_items, sprintf("TDAC_000%d_PROMPT", item_id)),
                            choices = choices,
                            labels = map(choice_ids, psychTestR::i18n)),
      dict = psyquest::psyquest_dict)
    elts <- c(elts, item_page)
  }
  psychTestR::join(elts)
}

get_prompt <- function(item_number, num_items_in_test, prompt_id) {
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
