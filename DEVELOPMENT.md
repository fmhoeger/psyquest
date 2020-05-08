# Generate csv -> RDS -> rda 

data <- read.csv("data_raw/countries_languages_nationalities.csv", sep = ";", stringsAsFactors=FALSE, header = TRUE)
saveRDS(data, "data_raw/dicts/zz-COUNTRIES_LANGUAGES_NATIONALITIES_dict.RDS")
