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
      style = "text_align: center;"
    ),
    shiny::p(
      psychTestR::i18n(prompt_id),
      style = "margin-left: 10%; margin-right: 10%;")
  )
}

scoring <- function(label, items, subscales = c(), short_version = FALSE) {
  result_subscales <- items %>% pull(subscales)
  score_func <- NULL
  score_funcs <- items %>% pull(score_func)

  psychTestR::code_block(function(state, ...) {
    results <- psychTestR::get_results(state = state, complete = FALSE)
    scores_raw <- purrr::map(results, function(result) {
      result <- get(label, results)
      result <- as.numeric(gsub("[^0-9]", "", result))
      result
    })[[1]]

    scores <- purrr::map_dbl(1:length(scores_raw), function(i) {
      eval(parse(text = score_funcs[i]))(scores_raw[i])
    })

    # hack for conditional in DEG
    if (label == "DEG" && length(scores) == 10) {
      scores <- insert(scores, ats = 3, values = NA)
    }

    subscale_list <- list()
    for (i in 1:length(scores)) {
      for (subscale in strsplit(result_subscales[i], ";")[[1]]) {
        if (length(subscales) == 0 || subscale %in% subscales) {
          subscale_list[[subscale]] <- c(subscale_list[[subscale]], scores[i])
        }
      }
    }

    postprocess(label, subscale_list, short_version, state, results)
  })
}

postprocess <- function(label, subscale_list, short_version, state, results = results) {
  for (subscale in names(subscale_list)) {
    scores <- subscale_list[[subscale]]
    value <- if (label == "CCM") {
      postprocess_ccm(subscale, results, scores)
    } else if (label == "DEG") {
      postprocess_deg(subscale, results, scores)
    } else if (label == "GMS") {
      if (subscale == "Start Age" && scores == 19) { NA } else { mean(scores) }
    } else if (label == "MHE") {
      postprocess_mhe(subscale_list[["General"]])
    } else if (label == "SCA") {
      if (short_version) {
        postprocess_sca_short(scores)
      } else {
        postprocess_sca(scores)
      }
    } else if (label == "SCS") {
      if (short_version) {
        postprocess_scs_short(scores)
      } else {
        postprocess_scs(scores)
      }
    } else if (label == "SES") {
      subscale <- tolower(gsub(" ", "_", subscale))
      if (subscale == "esec") {
        subscale <- "class"
      }
      postprocess_ses(subscale, results, scores)
    } else {
      mean(scores)
    }

    psychTestR::save_result(place = state,
                            label = subscale,
                            value = value)
  }
}

main_test <- function(label, items, short_version = FALSE, subscales = c(), offset = 1, arrange_vertically = TRUE, style = "") {
  elts <- c()
  if (label != "GMS") {
    elts <- c(elts, psychTestR::new_timeline(
      psychTestR::one_button_page(
        body = psychTestR::i18n(stringr::str_interp("T${label}_0001_PROMPT")),
        button_text = psychTestR::i18n("CONTINUE")
      ),
      dict = psyquest::psyquest_dict
    ))
  }

  prompt_id <- NULL
  prompt_ids <- items %>% pull(prompt_id)
  question_numbers <- as.numeric(gsub("[^0-9]", "", prompt_ids))

  for (counter in seq_along(numeric(length(question_numbers)))) {
    question_label <- sprintf("q%d", question_numbers[counter] - offset)
    item_bank_row <-
      items %>%
      filter(stringr::str_detect(prompt_id, sprintf("T%s_%04d", label, question_numbers[counter])))
    num_of_options <- strsplit(item_bank_row$option_type, "-")[[1]][1]
    choices <- sprintf("btn%d_text", 1:num_of_options)
    choice_ids <- sprintf("T%s_%04d_CHOICE%d", label, question_numbers[counter], 1:num_of_options)

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
    elts <- psychTestR::join(elts, item_page)
  }

  psychTestR::join(psychTestR::begin_module(label = label),
                   elts,
                   scoring(label, items, subscales, short_version),
                   psychTestR::elt_save_results_to_disk(complete = TRUE),
                   psychTestR::end_module())
}
