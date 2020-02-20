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

get_month_as_int <- function(key = key) {
  month_map <- list()
  month_map[[ "Januar" ]] <- 1
  month_map[[ "Februar" ]] <- 2
  month_map[[ "März" ]] <- 3
  month_map[[ "April" ]] <- 4
  month_map[[ "Mai" ]] <- 5
  month_map[[ "Juni" ]] <- 6
  month_map[[ "Juli" ]] <- 7
  month_map[[ "August" ]] <- 8
  month_map[[ "September" ]] <- 9
  month_map[[ "Oktober" ]] <- 10
  month_map[[ "November" ]] <- 11
  month_map[[ "Dezember" ]] <- 12

  month_map[[ "January" ]] <- 1
  month_map[[ "February" ]] <- 2
  month_map[[ "March" ]] <- 3
  month_map[[ "April" ]] <- 4
  month_map[[ "May" ]] <- 5
  month_map[[ "June" ]] <- 6
  month_map[[ "July" ]] <- 7
  month_map[[ "August" ]] <- 8
  month_map[[ "September" ]] <- 9
  month_map[[ "October" ]] <- 10
  month_map[[ "November" ]] <- 11
  month_map[[ "December" ]] <- 12

  month_map[[key]]
}

get_year <- function(date){
  as.numeric(strsplit(as.character(date), "-")[[1]][1])
}

get_month <- function(date){
  as.numeric(strsplit(as.character(date), "-")[[1]][2])
}
