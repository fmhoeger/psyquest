# Generate csv -> RDS -> rda 

countries_languages_nationalities.csv <- read.csv("data_raw/countries_languages_nationalities.csv", sep = ";", stringsAsFactors=FALSE, header = TRUE)
saveRDS(countries_languages_nationalities, "data_raw/dicts/COUNTRIES_LANGUAGES_NATIONALITIES_dict.RDS")
