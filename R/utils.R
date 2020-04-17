messagef <- function(...) message(sprintf(...))
printf <- function(...) print(sprintf(...))

is.scalar.character <- function(x) {
  is.character(x) && is.scalar(x)
}

is.scalar.numeric <- function(x) {
  is.numeric(x) && is.scalar(x)
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

is.null.or <- function(x, f) {
  is.null(x) || f(x)
}

get_year <- function(date) {
  as.numeric(strsplit(as.character(date), "-")[[1]][1])
}

get_month <- function(date) {
  as.numeric(strsplit(as.character(date), "-")[[1]][2])
}

get_items <- function(label, subscales, short_version = FALSE) {
  prompt_id <- NULL
  items <- psyquest::psyquest_item_bank %>%
    filter(stringr::str_detect(prompt_id, stringr::str_interp("T${label}")))

  if (!is.null(subscales)) {
    filtered_items <- as.data.frame(items[purrr::map(subscales, function(x) grep(x, items$subscales)) %>% unlist() %>% unique(), ])
    return(filtered_items[order(filtered_items$prompt_id), ])
  }

  question_ids = c()
  if (label == "SCA") {
    if (short_version) {
      question_ids <- c(27:30)
    } else {
      question_ids <- c(2:26)
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
  } else if (label == "GMS" && short_version) {
    short_scale <- list("Active Engagement" =  c(1, 2, 5, 6, 7, 9),
                    "Emotions" = c(1, 2, 3, 5, 6),
                    "Musical Training" = c(1, 2, 3, 6, 7),
                    "Perceptual Abilities"= c(1, 3, 4, 6, 7, 8),
                    "Singing Abilities" = c(2, 3, 4, 5),
                    "Instrument" = 1,
                    "Start Age" = 1,
                    "Absolute Pitch" = 1)
    short_scale_items <- purrr::map(names(short_scale), function(x){sprintf("%s_%s", x, short_scale[[x]])}) %>% unlist()

    question_ids <- c(1, 4, 19, 27, 30, 36, 3, 10, 20, 26, 39, 2, 12, 16, 22, 35, 6, 13, 14, 24, 28, 29, 32, 8, 11, 23, 33, 40, 41)

    filtered_items <- as.data.frame(items[purrr::map(question_ids, function(x) grep(sprintf("TGMS_%04d", x), items$prompt_id)) %>% unlist() %>% unique(), ])

    return(filtered_items[order(filtered_items$prompt_id), ])
  }

  items[order(items$prompt_id), ]
}
