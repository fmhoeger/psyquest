# Generate csv -> RDS -> rda 

countries <- read.csv("data_raw/countries.csv", sep = ";", 
stringsAsFactors=FALSE, header = TRUE)
saveRDS(countries, "data_raw/dicts/COUNTRIES_dict.RDS")
