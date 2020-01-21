main_test <- function(questionnaire, label, num_items, offset = 1, arrange_vertically = TRUE) {
  elts <- c()
  elts <- c(elts, psychTestR::new_timeline(
      psychTestR::one_button_page(
        body = psychTestR::i18n(stringr::str_interp("T${questionnaire}_0001_PROMPT")),
        button_text = psychTestR::i18n("CONTINUE")
      ),
      dict = psyquest::psyquest_dict
    )
  )
  for (item_id in (offset + 1):(offset + num_items)) {
    label <- sprintf("q%d", item_id - offset)
    item_bank_row  <-
      psyquest::psyquest_item_bank %>%
      filter(stringr::str_detect(prompt_id, sprintf("T%s_%04d", questionnaire, item_id)))
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
        arrange_vertically = arrange_vertically,
        labels = purrr::map(choice_ids, psychTestR::i18n)
      ),
      dict = psyquest::psyquest_dict
    )
    elts <- c(elts, item_page)
  }
  elts <- c(elts, psychTestR::code_block(function(state, ...) {
    results <- psychTestR::get_results(state = state, complete = FALSE)
    score_funcs <-
      psyquest::psyquest_item_bank %>%
      filter(stringr::str_detect(prompt_id, stringr::str_interp("T${questionnaire}"))) %>%
      pull(score_func)
    subscales <-
      psyquest::psyquest_item_bank %>%
      filter(stringr::str_detect(prompt_id, stringr::str_interp("T${questionnaire}"))) %>%
      pull(subscales)
    scores_raw <- purrr::map(results, function(result) {
      result <- get(questionnaire, results)
      result <- as.numeric(gsub("[^0-9]", "", result))
      result
    })[[1]]
    scores <-
      purrr::map_dbl(1:length(scores_raw), function(i) {
        eval(parse(text = score_funcs[i]))(scores_raw[i])
      })

    subscale_list = list()
    for (i in 1:length(scores)) {
      for (subscale in strsplit(subscales[i], ";")[[1]]) {
        subscale_list[[subscale]] = c(subscale_list[[subscale]], scores[i])
      }
    }
    postprocess(questionnaire, subscale_list, state)
  }))

  psychTestR::join(psychTestR::begin_module(questionnaire),
                   elts,
                   psychTestR::end_module())
}

postprocess <- function(questionnaire = questionnaire, subscale_list = subscale_list, state = state) {
  for (subscale in names(subscale_list)) {
    scores <- subscale_list[[subscale]]

    if(questionnaire == 'SCA' | questionnaire == 'SCS') {
      score_mapping <- read.csv(file = stringr::str_interp("data_raw/${questionnaire}_scores.csv"),
                                header = FALSE,
                                sep = ";")
      row <- score_mapping %>% filter(stringr::str_detect(V3, toString(sum(scores))))
      value <- row[1,2]
    } else {
      value = mean(scores)
    }

    psychTestR::save_result(place = state,
                            label = subscale,
                            value = value)
  }
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
