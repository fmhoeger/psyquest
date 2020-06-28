is.scalar.character <- function(x) {
  is.character(x) && is.scalar(x)
}

is.scalar.logical <- function(x) {
  is.logical(x) && is.scalar(x)
}

is.scalar <- function(x) {
  identical(length(x), 1L)
}

tagify <- function(x) {
  stopifnot(is.character(x) || is(x, "shiny.tag"))
  if (is.character(x)) {
    stopifnot(is.scalar(x))
    shiny::p(x)
  } else x
}

get_year <- function(date) {
  as.numeric(strsplit(as.character(date), "-")[[1]][1])
}

get_month <- function(date) {
  as.numeric(strsplit(as.character(date), "-")[[1]][2])
}

get_items <- function(label, subscales = c(), short_version = FALSE, configuration_filepath = NULL) {
  prompt_id <- NULL
  items <- psyquest::psyquest_item_bank %>%
    filter(stringr::str_detect(prompt_id, stringr::str_interp("T${label}")))

  if (!is.null(subscales)) {
    filtered_items <- as.data.frame(items[purrr::map(subscales, function(x) grep(gsub("(", "\\(", gsub(")", "\\)", x, fixed = TRUE), fixed = TRUE), items$subscales)) %>% unlist() %>% unique(), ])
    return(filtered_items[order(filtered_items$prompt_id), ])
  }

  question_ids <- c()
  if (label == "SCA") {
    if (short_version) {
      question_ids <- c(3, 13, 26, 30)
    } else {
      question_ids <- c(2, 4:12, 14:25, 27:29)
    }
    filtered_items <- as.data.frame(items[purrr::map(question_ids, function(x) grep(sprintf("TSCA_%04d", x), items$prompt_id)) %>% unlist() %>% unique(), ])

    return(filtered_items[order(filtered_items$prompt_id), ])
  } else if (label == "SCS") {
    if (short_version) {
      question_ids <- c(3, 5, 6, 10, 11, 17, 18, 22)
    } else {
      question_ids <- c(2:26)
    }
    filtered_items <- as.data.frame(items[purrr::map(question_ids, function(x) grep(sprintf("TSCS_%04d", x), items$prompt_id)) %>% unlist() %>% unique(), ])

    return(filtered_items[order(filtered_items$prompt_id), ])
  } else if (label == "GMS") {
    if (!is.null(configuration_filepath)) {
      subscale_ids <-
        (
          read.csv(
            file = configuration_filepath,
            header = TRUE,
            stringsAsFactors = FALSE,
          ) %>% tibble::as_tibble()
        )$MID

      subscale_id <- NULL
      question_ids <- unlist(purrr::map((
        read.csv(
          file = system.file("extdata", "GMS_items_to_subscales.csv", package = "psyquest"),
          header = TRUE,
          stringsAsFactors = FALSE
        ) %>% tibble::as_tibble() %>% filter(subscale_id %in% subscale_ids)
      )$item_id, function(item_id)
        as.integer(unlist(strsplit(item_id, "_"))[2])))

      filtered_items <- as.data.frame(items[purrr::map(question_ids, function(x) grep(sprintf("TGMS_%04d", x), items$prompt_id)) %>% unlist() %>% unique(), ])

      return(filtered_items[order(filtered_items$prompt_id), ])
    } else if (short_version) {
      # list("Active Engagement" =  c(1, 2, 5, 6, 7, 9),  # -> 1, 4, 19, 27, 30, 36
      #      "Emotions" = c(1, 2, 3, 5, 6),               # -> 3, 10, 20, 26, 39
      #      "Musical Training" = c(1, 2, 3, 6, 7),       # -> 2, 12, 16, 22, 35
      #      "Perceptual Abilities"= c(1, 3, 4, 6, 7, 8), # -> 6, 13, 14, 24, 28, 29
      #      "Instrument" = 1,                            # -> 32
      #      "Singing Abilities" = c(2, 3, 4, 5),         # -> 8, 11, 23, 33
      #      "Start Age" = 1,                             # -> 40
      #      "Absolute Pitch" = 1)                        # -> 41
      question_ids <- c(1, 4, 19, 27, 30, 36, 3, 10, 20, 26, 39, 2, 12, 16, 22, 35, 6, 13, 14, 24, 28, 29, 32, 8, 11, 23, 33, 40, 41)

      filtered_items <- as.data.frame(items[purrr::map(question_ids, function(x) grep(sprintf("TGMS_%04d", x), items$prompt_id)) %>% unlist() %>% unique(), ])

      return(filtered_items[order(filtered_items$prompt_id), ])
    }
  }

  items[order(items$prompt_id), ]
}
