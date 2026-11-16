#' @title Check if Input is a LAS Object
#' @description
#' Internal utility function that throws an error if the input is not a `LAS` object (as defined by the `{lidR}` package). Primarily used for early validation in point cloud processing workflows.
#' @param las Object to be tested. Expected to be a `LAS` object.
#' @return Invisibly returns the input if valid; otherwise, throws an informative error.
#' @noRd
.stopifnotlas <- function(las) {
  name <- deparse(substitute(las))
  if (!inherits(las, "LAS")) {
    stop(
      sprintf("`%s` must be a LAS object, but has class: %s",
              name, paste(class(las), collapse = ", ")),
      call. = FALSE
    )
  }
  invisible(las)
}