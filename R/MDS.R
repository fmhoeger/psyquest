#' MDS
#'
#' This function defines a Music Dislikes Scale (MDS) module for incorporation into a
#' psychTestR timeline.
#' Use this function if you want to include the MDS in a
#' battery of other tests, or if you want to add custom psychTestR
#' pages to your test timeline.
#' For a standalone implementation of the MDS,
#' consider using \code{\link{MDS_standalone}()}.
#' @param label (Character scalar) Three uppercase letter acronym of the questionnaire.
#' This is also the label given to the results in the output file.
#' @param target (Character scalar or named character vector) The main target (style, artist...) for the questionnaire, if different values for different lanugages are needed then it should be a named vector with the names according to language columns in the dict.
#' @param dict (i18n_dict) The psyquest dictionary used for internationalisation.
#' @param ... Further arguments to be passed to \code{\link{MDS}()}.
#' @export
MDS <- function(label = "MDS",
                target = c("en" = "a music", "de" = "einer Musik", "de_f" = "einer Musik"),
                dict = psyquest::psyquest_dict,
                ...) {
  stopifnot(purrr::is_scalar_character(label))
  questionnaire_id <- "MDS"
  #browser()
  message("Check point 1")
  dict_raw <- dict %>% as.data.frame()
  if(length(target) > 1){
    message("Check point 2")
    if(is.null(names(target))){
      stop("Target vector must be named if it has more than one element")
    }
    common_names <- intersect(names(dict_raw), names(target))
    if(length(common_names) != length(target)){
      warning("Target does not contain all dictionary languages.")
    }
    target <- target[common_names]
    if(length(target) == 0){
      stop("All target names invalid.")
    }
  }
  else{
    message("Check point 3")
    target <- rep(target, length(dict_raw)-1)
    names(target) <- setdiff(names(dict_raw), "key")
  }
  message("Check point 4")

  prompt <- dict_raw[dict_raw$key  == "TMDS_0001_PROMPT", names(target)] %>% as.character()
  message("Check point 5")
  fixed_prompt <- purrr::map_chr(seq_along(names(target)),
                          function(i){
                            stringr::str_replace(prompt[[i]], stringr::fixed("{{target}}"), target[i])
                            })
  message("Check point 6")
  dict_raw[dict_raw$key  == "TMDS_0001_PROMPT", names(target)] <- as.list(fixed_prompt)
  message("Check point 7")
  patch_dict <-  psychTestR::i18n_dict$new(dict_raw)
  message("Check point 8")

  main_test(
    questionnaire_id = questionnaire_id,
    label = label,
    items = get_items(questionnaire_id),
    offset = 1,
    arrange_vertically = TRUE,
    button_style = "min-width: 290px;width: 290px",
    dict = patch_dict
  )
}
