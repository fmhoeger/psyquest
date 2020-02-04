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
      style  = "text_align:center;"
    ),
    shiny::p(
      psychTestR::i18n(prompt_id),
      style = "margin-left:20%; margin-right:20%; text-align:justify;")
  )
}

scoring <- function(questionnaire){
  psychTestR::code_block(function(state, ...) {
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
    scores <- purrr::map_dbl(1:length(scores_raw), function(i) { eval(parse(text = score_funcs[i]))(scores_raw[i]) })

    subscale_list = list()
    for (i in 1:length(scores)) {
      for (subscale in strsplit(subscales[i], ";")[[1]]) {
        subscale_list[[subscale]] = c(subscale_list[[subscale]], scores[i])
      }
    }

    postprocess(questionnaire, subscale_list, state, results)
  })
}

postprocess <- function(questionnaire, subscale_list, state, results = results) {
  for (subscale in names(subscale_list)) {
    scores <- subscale_list[[subscale]]
    if(questionnaire == "DEG") {
      if (subscale == "Type of Hearing Impairment") {
        value = results[["DEG"]][["q3"]]
      } else if (subscale == "Age") {
        min_year <- 2005
        max_year <- 2013
        month <- get_month_as_int(results[["DEG"]][["q9"]][1]) - 1
        year <- as.numeric(results[["DEG"]][["q9"]][2]) - min_year
        cur_date <- Sys.Date()
        cur_year <- get_year(cur_date) - min_year
        cur_month <- get_month(cur_date) - 1
        value = (cur_year - year) * 12 + cur_month - month
      } else if (subscale == "Nationality") {
        value = get_country_language_code(results[["DEG"]][["q5"]])
      } else if (subscale == "Country Formative Years") {
        value = get_country_language_code(results[["DEG"]][["q6"]])
      } else if (subscale == "First Language") {
        value = tolower(get_country_language_code(results[["DEG"]][["q7"]]))
      } else if (subscale == "Second Language") {
        value = tolower(get_country_language_code(results[["DEG"]][["q8"]]))
      } else if (subscale == "Handedness") {
        value = c(as.numeric(gsub("[^0-9]", "", results[[questionnaire]][["q10"]])), as.numeric(gsub("[^0-9]", "", results[[questionnaire]][["q11"]])))
      } else {
        value = mean(scores)
      }
    } else if(questionnaire == "SCA" | questionnaire == "SCS") {
      tmp <- psyquest::scoring_maps[[questionnaire]]
      value <- tmp[tmp$raw == sum(scores),]$score
    } else if(questionnaire == 'MHE') {
      View(subscale_list[["General"]])
      sum_parents = nchar(toString(subscale_list[["General"]][1])) + nchar(toString(subscale_list[["General"]][2]))
      print(sum_parents)
      tmp <- psyquest::scoring_maps[[questionnaire]]
      score_stats<-data.frame(id     = c("ability", "encourage", "support"),
                              mean   = c(0.1143363, 3.156951, 2.769058),
                              sd     = c(0.5637453, 1.116791, 1.258868),
                              weight = c(0.57,      0.87,     0.88))

      # get_MHE_general_score<-function(mother_activities, father_activities, encourage, support){
      #   sum_parents <-  sum(mother_activities) + sum(father_activities)
      #   print(sum_parents)
      #   raws <- list()
      #   raws[["ability"]] <- MHE_scores[MHE_scores$raw_scores == sum_parents,]$ability
      #   raws[["encourage"]] <- encourage
      #   raws[["support"]] <- support
      #   total_score <- 0
      #   for(var in names(raws)){
      #     weight <- score_stats[score_stats$id == var, "weight"][1]
      #     mean <- score_stats[score_stats$id == var, "mean"][1]
      #     sd <- score_stats[score_stats$id == var, "sd"][1]
      #     score <- weight*(raws[[var]]-mean)/sd
      #     print(sprintf("var: %s, raw: %f, weight: %f, m=%f, sd=%f, score=%f", var, raws[[var]], weight, mean, sd, score))
      #     total_score <- total_score + score
      #   }
      #   print(sprintf("Total score %f", total_score))
      #   return(total_score)
      value = 0
    } else {
      value = mean(scores)
    }

    psychTestR::save_result(place = state,
                            label = subscale,
                            value = value)
  }
}

main_test <- function(questionnaire, label, num_items, offset = 1, arrange_vertically = TRUE) {
  elts <- c()
  if (questionnaire != "GMS") {
    elts <- c(elts, psychTestR::new_timeline(
      psychTestR::one_button_page(
        body = psychTestR::i18n(stringr::str_interp("T${questionnaire}_0001_PROMPT")),
        button_text = psychTestR::i18n("CONTINUE")
      ),
      dict = psyquest::psyquest_dict
    ))
  }
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

  psychTestR::join(psychTestR::begin_module(label = questionnaire),
                   elts,
                   scoring(questionnaire),
                   psychTestR::end_module())
}
