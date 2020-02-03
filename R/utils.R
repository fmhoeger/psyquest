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

get_country_language_code <- function(key = key) {
  country_language_map <- list()
  country_language_map[[ "UK" ]] <- "UK"
  country_language_map[[ "Deutschland" ]] <- "DE"
  country_language_map[[ "Afghanistan" ]] <- "AF"
  country_language_map[[ "Bulgarien" ]] <- "BG"
  country_language_map[[ "Bulgaria" ]] <- "BG"
  country_language_map[[ "China" ]] <- "CN"
  country_language_map[[ "Griechenland" ]] <- "GR"
  country_language_map[[ "Großbritannien" ]] <- "UK"
  country_language_map[[ "Greece" ]] <- "GR"
  country_language_map[[ "Irak" ]] <- "IQ"
  country_language_map[[ "Iraq" ]] <- "IQ"
  country_language_map[[ "Iran" ]] <- "IR"
  country_language_map[[ "Italien" ]] <- "IT"
  country_language_map[[ "Italy" ]] <- "IT"
  country_language_map[[ "Kosovo" ]] <- "XK"
  country_language_map[[ "Polen" ]] <- "PL"
  country_language_map[[ "Poland" ]] <- "PL"
  country_language_map[[ "Rumänien" ]] <- "RO"
  country_language_map[[ "Romania" ]] <- "RO"
  country_language_map[[ "Russland" ]] <- "RU"
  country_language_map[[ "Russia" ]] <- "RU"
  country_language_map[[ "Serbien" ]] <- "RS"
  country_language_map[[ "Serbia" ]] <- "RS"
  country_language_map[[ "Syrien" ]] <- "SY"
  country_language_map[[ "Syria" ]] <- "SY"
  country_language_map[[ "Türkei" ]] <- "TR"
  country_language_map[[ "Turkey" ]] <- "TR"
  country_language_map[[ "sonstiges" ]] <- "OTHER"

  country_language_map[[ "Albanisch" ]] <- "SQ"
  country_language_map[[ "Arabisch" ]] <- "AR"
  country_language_map[[ "British" ]] <- "UK"
  country_language_map[[ "Britisch" ]] <- "UK"
  country_language_map[[ "Bulgarisch" ]] <- "BG"
  country_language_map[[ "Chinesisch" ]] <- "ZH"
  country_language_map[[ "Deutsch" ]] <- "DE"
  country_language_map[[ "Englisch" ]] <- "EN"
  country_language_map[[ "Griechisch" ]] <- "EL"
  country_language_map[[ "Italienisch" ]] <- "IT"
  country_language_map[[ "Kurdisch" ]] <- "KU"
  country_language_map[[ "Paschto" ]] <- "PS"
  country_language_map[[ "Paschtunisch" ]] <- "PS"
  country_language_map[[ "Persisch" ]] <- "FS"
  country_language_map[[ "Polnisch" ]] <- "PL"
  country_language_map[[ "Rumänisch" ]] <- "RO"
  country_language_map[[ "Russisch" ]] <- "RU"
  country_language_map[[ "Serbisch" ]] <- "SR"
  country_language_map[[ "Türkisch" ]] <- "TR"
  country_language_map[[ "andere" ]] <- "OTHER"
  country_language_map[[ "Keine" ]] <- "NONE"

  country_language_map[[ "Albanian" ]] <- "SQ"
  country_language_map[[ "Arabic" ]] <- "AR"
  country_language_map[[ "Bulgarian" ]] <- "BG"
  country_language_map[[ "Chinese" ]] <- "ZH"
  country_language_map[[ "German" ]] <- "DE"
  country_language_map[[ "Englisch" ]] <- "EN"
  country_language_map[[ "Farsi/Dari" ]] <- "FS"
  country_language_map[[ "Greek" ]] <- "EL"
  country_language_map[[ "Italian" ]] <- "IT"
  country_language_map[[ "Kurdish" ]] <- "KU"
  country_language_map[[ "Pashto" ]] <- "PS"
  country_language_map[[ "Polish" ]] <- "PL"
  country_language_map[[ "Romanian" ]] <- "RO"
  country_language_map[[ "Russian" ]] <- "RU"
  country_language_map[[ "Serbian" ]] <- "SR"
  country_language_map[[ "Turkish" ]] <- "TR"
  country_language_map[[ "None" ]] <- "NONE"
  country_language_map[[ "other" ]] <- "OTHER"

  country_language_map[[key]]
}
