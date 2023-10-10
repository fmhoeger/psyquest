get_prompt <- function(item_number,
                       num_items_in_test,
                       prompt_id,
                       with_prompt_head = FALSE,
                       style_params = NULL) {
  prompt_style <- ifelse(is.null(style_params$prompt_style), "margin-left: 20%; margin-right: 20%;width:60%;margin-bottom:1em", style_params$prompt_style)
  with_counter <- ifelse(is.null(style_params$with_counter), TRUE, style_params$with_counter)
  div_style <- ifelse(is.null(style_params$div_style), "inherit", style_params$div_style)

  prompt <- psychTestR::i18n(prompt_id)
  if (with_prompt_head) {
    prompt <- shiny::p(psychTestR::i18n("PROMPT_HEAD"), shiny::br(), shiny::span(prompt, style = "font-weight: bold"))
  }
  shiny::div(
    if(with_counter)
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
      prompt,
      style = prompt_style),
    style = div_style
  )
}

scoring <- function(questionnaire_id, label, items, subscales = c(), short_version = FALSE) {
  result_subscales <- items %>% pull(subscales)
  score_func <- NULL
  score_funcs <- items %>% pull(score_func)

  psychTestR::code_block(function(state, ...) {
    results <- psychTestR::get_results(state = state, complete = FALSE)
    scores_raw <- map(results, function(result) {
      result <- get(label, results)
      as.numeric(gsub("[^0-9]", "", result))
    })[[1]]
    if(questionnaire_id == "MDS"){
      scores_raw <- scores_raw[!is.na(scores_raw)]
    }
    scores <- map_dbl(1:length(scores_raw), function(i) {
      eval(parse(text = score_funcs[i]))(scores_raw[i])
    })

    # hack for conditional in DEG
    if (questionnaire_id == "DEG" && length(scores) == 10) {
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
    #hack for Target in MDS
    # if (questionnaire_id == "MDS") {
    #   browser()
    #   subscale_list <- insert(subscale_list,
    #                           ats = 1,
    #                           values = c("Target" = paste(results$MDS[["target"]], collapse = ";")),
    #                           useNames = T)
    # }

    postprocess(questionnaire_id, label, subscale_list, short_version, state, results)
  })
}

postprocess <- function(questionnaire_id, label, subscale_list, short_version, state, results = results) {
  for (subscale in names(subscale_list)) {
    scores <- subscale_list[[subscale]]
    value <- if (questionnaire_id == "CCM") {
      postprocess_ccm(questionnaire_id, label, subscale, results, scores)
    } else if (questionnaire_id == "DEG") {
      postprocess_deg(label, subscale, results, scores)
    } else if (questionnaire_id == "EWE") {
      postprocess_ewe(label, subscale, results, scores)
    } else if (questionnaire_id == "CRT") {
      postprocess_crt(label, subscale, results, scores)
    } else if (questionnaire_id == "GMS") {
      if (subscale == "Start Age" && scores == 19) {
        NA
      } else {
        mean(scores)
      }
    } else if (questionnaire_id == "MHE") {
      postprocess_mhe(questionnaire_id, subscale_list[["General"]])
    } else if (questionnaire_id == "QHC") {
      sum(scores)
    } else if (questionnaire_id == "SCA") {
      if (short_version) {
        postprocess_sca_short(scores)
      } else {
        postprocess_sca(scores)
      }
    } else if (questionnaire_id == "SCS") {
      if (short_version) {
        postprocess_scs_short(scores)
      } else {
        postprocess_scs(scores)
      }
    } else if (questionnaire_id == "SES") {
      subscale <- tolower(gsub(" ", "_", subscale))
      if (subscale == "esec") {
        subscale <- "class"
      }
      postprocess_ses(subscale, results, scores)
    } else if (questionnaire_id == "MDS" && subscale == "Target") {
      scores
    } else {
      mean(scores)
    }
    message(sprintf("Subscale: %s, value: %s", subscale, ifelse(length(value) == 1,
                                                                value,
                                                                paste(value, collapse = ";"))))
    psychTestR::save_result(place = state,
                            label = subscale,
                            value = value)
  }
}

main_test <- function(questionnaire_id,
                      label,
                      items,
                      with_prompt_head = FALSE,
                      short_version = FALSE,
                      subscales = c(),
                      offset = 1,
                      arrange_vertically = TRUE,
                      button_style = "",
                      dict = psyquest::psyquest_dict,
                      style_params = NULL) {
  elts <- c()
  #browser()
  #hack, needed for MDS
  target_ext <- style_params$target
  if (questionnaire_id != "GMS" && offset != 0) {
    if(questionnaire_id == "MDS"){
      elts <- c(elts, psychTestR::new_timeline(
        psychTestR::reactive_page(function(state, ...){
          target <- psychTestR::get_global("target", state)
          if (is.null(target)){
            if(is.null(target_ext)){
              target <- "{{}}"
            }
            else{
              target <- target_ext
            }
          }
          message(sprintf("Target: %s", target))
          target <- psychTestR::set_global("target", target, state)
          psychTestR::one_button_page(
            body = shiny::div(
              psychTestR::i18n("TMDS_0001_PROMPT", sub = list(target = target)),
              style = "margin-left:20%;margin-right:20%;text-align:justify;margin-bottom:2em"),
            button_text = psychTestR::i18n("CONTINUE")
          )
        }),
        dict = dict
      ),
      psychTestR::code_block(function(state, ...){
        target <- psychTestR::get_global("target", state)
        psychTestR::save_result(state, "target", target)

      })
      )
    }
    else{
      elts <- c(elts, psychTestR::new_timeline(
        psychTestR::one_button_page(
          body = shiny::p(
            psychTestR::i18n(stringr::str_interp("T${questionnaire_id}_0001_PROMPT"), ),
            style = "margin-left:20%;margin-right:20%;text-align:justify;margin-bottom:2em"
          ),
          button_text = psychTestR::i18n("CONTINUE")
        ),
        dict = dict
      ))
    }
  }
  prompt_id <- NULL
  prompt_ids <- items %>% pull(prompt_id)
  question_numbers <- as.numeric(gsub("[^0-9]", "", prompt_ids))
  for (counter in 1:length(question_numbers)) {
    question_label <- sprintf("q%d", question_numbers[counter] - offset)
    item_bank_row <-
      items %>%
      filter(stringr::str_detect(prompt_id, sprintf("T%s_%04d", questionnaire_id, question_numbers[counter])))
    num_of_options <- strsplit(item_bank_row$option_type, "-")[[1]][1]
    choices <- sprintf("btn%d_text", 1:num_of_options)
    choice_ids <- sprintf("T%s_%04d_CHOICE%d", questionnaire_id, question_numbers[counter], 1:num_of_options)
    bs <- button_style[1]
    #item_bank_row$layout <- NA
    #arrange_vertically = FALSE
    #bs <- "max_width:100px"
    if(!is.na(item_bank_row$layout)){
      arrange_vertically <- item_bank_row$layout[1] == "vertical"
      if(!arrange_vertically & length(button_style) >  0){
        bs <- button_style[2]
      }
    }
    item_page <- psychTestR::new_timeline(
      psychTestR::NAFC_page(
        label = question_label,
        prompt = get_prompt(
          counter,
          length(question_numbers),
          sprintf("T%s_%04d_PROMPT", questionnaire_id, question_numbers[counter]),
          with_prompt_head,
          style_params
        ),
        choices = choices,
        arrange_vertically = arrange_vertically,
        button_style = bs,
        labels = map(choice_ids, psychTestR::i18n)
      ),
      dict = dict
    )
    elts <- psychTestR::join(elts, item_page)
  }
  psychTestR::join(psychTestR::begin_module(label = label),
                   elts,
                   scoring(questionnaire_id, label, items, subscales, short_version),
                   psychTestR::elt_save_results_to_disk(complete = TRUE),
                   psychTestR::end_module())
}
