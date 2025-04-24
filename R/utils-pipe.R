#' Pipe operator
#'
#' See \code{\link[dplyr]{\%>\%}} for details.
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

#' Null-coalescing operator
#'
#' See \code{\link[rlang]{\%||\%}} for details.
#'
#' @name %||%
#' @rdname null_coalesce
#' @keywords internal
#' @export
#' @importFrom rlang %||%
#' @usage lhs \%||\% rhs
#' @param lhs A value that may be NULL.
#' @param rhs A default value to return if lhs is NULL.
#' @return The value of `lhs` if it is not NULL, otherwise `rhs`.
NULL