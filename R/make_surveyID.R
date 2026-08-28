make_surveyID <- function(x) {
  id <- sub("_[0-9]+$", "", x)            # drop the segment number
  id <- tolower(gsub("[_-]", "", id))     # 2014_12_13 -> 20141213; 201704Hidro -> 201704hidro
  recoded <- unname(hidro_lkp[id])
  ifelse(is.na(recoded), id, recoded)
}
