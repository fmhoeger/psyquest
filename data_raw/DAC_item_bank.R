item_bank <- readRDS("data_raw/RAT_item_bank.RDS")
item_bank$item_id <- sprintf("%s-%s", item_bank$pattern, item_bank$bit_flips)
RAT_lures_bank <- item_bank %>% select(item_id, lures)
item_bank$lures <- NULL
item_bank <- item_bank %>% distinct(item_id, .keep_all = T)
item_bank$answer <- sample(1:4, nrow(item_bank), replace = T)
item_bank$length.cat <- NULL
item_bank$item_no <- NULL
item_bank$training <- NULL
item_bank$sample <- NULL
item_bank$K9.rescaled <- NULL
RAT_item_bank <- as.data.frame(item_bank)


#stopifnot(is.numeric(item_bank$answer))
#usethis::use_data(RAT_item_bank, overwrite = TRUE)
#usethis::use_data(RAT_lures_bank, overwrite = TRUE)
