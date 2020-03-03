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

get_year <- function(date){
  as.numeric(strsplit(as.character(date), "-")[[1]][1])
}

get_month <- function(date){
  as.numeric(strsplit(as.character(date), "-")[[1]][2])
}

get_items <- function(label, subscales) {
  items <- psyquest::psyquest_item_bank %>%
    filter(stringr::str_detect(prompt_id, stringr::str_interp("T${label}")))

  if (!is.null(subscales)) {
    filtered_items <- as.data.frame(items[purrr::map(subscales, function(x) grep(x, items$subscales)) %>% unlist() %>% unique(),])
    return(filtered_items[order(filtered_items$prompt_id),])
  }

  items[order(items$prompt_id),]
}
