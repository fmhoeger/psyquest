#' MUS(MUSIC)
#'
#' This function defines a MUS module for incorporation into a
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
                dict = psyquest::psyquest_dict,
                subscales = c(),
                ...) {
  audio_dir <- "https://media.gold-msi.org/test_materials/MUSIC/"
  audio_dir <- gsub("/$", "", audio_dir)
  stopifnot(purrr::is_scalar_character(label))


  main_test_mus(
    label = label,
    items = get_items("MUS",
                      subscales = subscales),
    audio_dir = audio_dir,
    subscales = subscales,
    dict = dict
  )
}

main_test_mus <- function(
                          label,
                          items,
                          subscales,
                          audio_dir,
                          dict) {


  prompt_id <- NULL
  prompt_ids <- items %>% pull(prompt_id)

  question_numbers <- as.numeric(gsub("[^0-9]", "", prompt_ids))

  num_items <- length(question_numbers)

  elts <- list()

  for (j in seq_along(question_numbers)) {

    item_bank_row <-
      items %>%
      filter(stringr::str_detect(prompt_id, sprintf("TMUS_%04d",
                                                    question_numbers[j])))

    num_of_options <- strsplit(item_bank_row$option_type, "-")[[1]][1]
    choices <- sprintf("btn%d_text", 1:num_of_options)

    get_audio_url <- function(item_id){
      item_bank <- psyquest::psyquest_item_bank %>%
        filter(q_id == "MUS",
               item_id == as.integer(!!item_id) - 1)
      if(nrow(item_bank) > 1){
        browser()
      }
      url <- file.path(audio_dir, item_bank[1,]$audio_file)
      #messagef("Currently playing item %d (%s): %s", item_id, item_bank[1,]$subscales, url)
      url
    }
    #messagef("Adding page %d, item %d", j, question_numbers[j])

    item_page <- psychTestR::new_timeline(
      psychTestR::reactive_page(
        function(state, ...){
          #browser()
          counter <- psychTestR::get_local(key = "counter", state = state)
          #messagef("Current counter: %d (%d)", counter, j)
          res <- psychTestR::get_results(state, T, F) %>% as.list()

          item_sequence <- res[[label]][["item_sequence"]]
          #messagef("Item Sequence: %s", paste(question_numbers[item_sequence], collapse = ", "))

          cur_item_id <- question_numbers[item_sequence[counter]]
          if(is.na(cur_item_id)){
            browser()
            return(psychTestR::one_button_page(body = "BADBADBADNOTGOOD", button_text = "WEITER"))
          }
          question_label <- sprintf("q%d", cur_item_id - 1)

          choice_ids <- sprintf("TMUS_0002_CHOICE%d", 1:num_of_options)

          psychTestR::audio_NAFC_page(
            label = question_label,
            prompt = get_prompt(
              counter,
              length(question_numbers),
              "TMUS_0002_PROMPT",
              #sprintf("TMUS_%04d_PROMPT", cur_item_id)
            ),
            url = get_audio_url(cur_item_id),
            choices = choices,
            button_style = "min-width:250px",
            labels = map(choice_ids, psychTestR::i18n)
          )
        }
      ),
      dict = dict
    )
    #item_page <- psychTestR::one_button_page(body = sprintf("FRAGE %d", j), button_text = "CONT")
    counter_page <- psychTestR::code_block(function(state, ...){
      counter <- psychTestR::get_local(key = "counter", state = state)
      psychTestR::set_local("counter", counter + 1, state)

    })
    elts <- c(elts, psychTestR::join(item_page, counter_page))
  }
  psychTestR::join(psychTestR::begin_module(label),

                   psychTestR::new_timeline(psychTestR::code_block(function(state, ...) {
                     psychTestR::set_local("counter", 1, state)
                   }), dict = dict),
                   psychTestR::randomise_at_run_time(label = "item_sequence", logic = elts),
#
#                    psychTestR::order_at_run_time(label = "item_sequence",
#                                                  logic = elts,
#                                                  get_order = function(...){
#                                                    order <- rep(0, 2 * length(num_items))
#                                                    item_pages <- 2* (sample(1:num_items) - 1) + 1
#                                                    dummy_pages <- item_pages + 1
#                                                    order[2 * (1:num_items - 1) + 1] <- item_pages
#                                                    order[2 * (1:num_items - 1) + 2] <- dummy_pages
#                                                    print(order)
#                                                    order
#
#                                                  }),
                   scoring("MUS", label, items, subscales),
                   psychTestR::end_module())
}
