#' Pipe operator
#'
#' See \code{dplyr::\link[dplyr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom dplyr %>%
#' @usage lhs \%>\% rhs
#' @param lhs A value or the dplyr placeholder.
#' @param rhs A function call using the dplyr semantics.
#' @return The result of calling `rhs(lhs)`.
NULL
