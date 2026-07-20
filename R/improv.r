#' Percentage improvement (reduction) between two values
#'
#' Expresses the change from \code{old} to \code{new} as a percentage
#' \emph{reduction}, so that a smaller \code{new} value counts as a positive
#' improvement:
#' \deqn{\mathrm{improv} = 100 \times \frac{\mathrm{old} - \mathrm{new}}{\mathrm{old}}
#'       = -100 \times \frac{\mathrm{new} - \mathrm{old}}{\mathrm{old}}.}
#' Intended for quantities where smaller is better (e.g. a coefficient of
#' variation, AIC, or unexplained deviance): a positive result means \code{new}
#' improved on \code{old}, a negative result means it got worse.
#'
#' @details
#' The result is rounded to one decimal place. The function is vectorised over
#' \code{old} and \code{new} (recycled following the usual R rules). \code{old}
#' is the denominator, so \code{old = 0} yields \code{Inf}, \code{-Inf}, or
#' \code{NaN}; \code{NA} in either argument propagates to \code{NA}.
#'
#' @param old Numeric vector of baseline (reference) value(s).
#' @param new Numeric vector of new value(s) to compare against \code{old}.
#'
#' @return Numeric vector of percentage reductions (rounded to one decimal
#'   place): positive when \code{new < old}, zero when unchanged, negative when
#'   \code{new > old}.
#'
#' @examples
#' # a CV that dropped from 0.42 to 0.30 -> 28.6% improvement
#' improv(0.42, 0.30)
#'
#' # a worsening returns a negative value
#' improv(0.30, 0.42)
#'
#' # vectorised
#' improv(c(0.42, 1200), c(0.30, 950))
#'
#' @export
improv <- function(old, new){
  imp <- round(-100 * ((new - old)/old), 1)
  return(imp)
}
