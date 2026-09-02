#' Build a canonical survey identifier from a Sample.Label
#'
#' \code{Sample.Label} is not written the same way in every data file
#' (\code{"20060512_15"}, \code{"201704Hidro_10"}, \code{"2014_12_13_18"},
#' \code{"2017_hidro_oto_2"}, ...). This collapses those variants to a single
#' canonical \code{surveyID} so that segment/observation/distance tables can be
#' joined on it.
#'
#' @details
#' The trailing segment number is dropped first (\code{sub("_[0-9]+$", "", x)});
#' naively taking everything before the first underscore instead would
#' truncate labels like \code{"2014_12_13_18"} or \code{"2017_hidro_pri_2"} to
#' just the year. Separators (\code{_}, \code{-}) are then stripped and the
#' result lowercased, e.g. \code{"2014_12_13"} -> \code{"20141213"} and
#' \code{"201704Hidro"} -> \code{"201704hidro"}.
#'
#' The four hydroacoustic surveys are named by season in \code{distdata} but
#' by year-month in \code{segdata}, so after the steps above they still don't
#' match (e.g. \code{"2017hidrooto"} vs. \code{"201704hidro"}) and require an
#' explicit recode. This is done via \code{hidro_lkp}, a named character
#' vector (season-form name -> year-month-form name) that must exist in the
#' calling environment; ids with no entry in \code{hidro_lkp} are returned
#' unchanged.
#'
#' @param x Character vector of raw \code{Sample.Label} values.
#'
#' @return A character vector of canonical survey IDs, the same length as
#'   \code{x}.
#'
#' @examples
#' hidro_lkp <- c("2017hidrooto" = "201704hidro")
#' make_surveyID(c("20060512_15", "2014_12_13_18", "2017_hidro_oto_2"))
#'
#' @export
make_surveyID <- function(x) {
  id <- sub("_[0-9]+$", "", x)            # drop the segment number
  id <- tolower(gsub("[_-]", "", id))     # 2014_12_13 -> 20141213; 201704Hidro -> 201704hidro
  recoded <- unname(hidro_lkp[id])
  ifelse(is.na(recoded), id, recoded)
}
