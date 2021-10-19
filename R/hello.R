
#' @title Say hello
#'
#' @description This function says hello.
#' @param world a logical indicating if we are saying hello
#' to the world. (Default TRUE)
#' @param verbose should the return be printed? (Default FALSE)
#' @return nothing
#' @examples
#' hello(FALSE)
#' @export
hello <- function(world = TRUE, verbose = FALSE) {
  if (world) {
    ret <- "Hello, world!"
  } else {
    ret <- "Hello!"
  }
  if (verbose) {
    print(ret)
  }
  invisible(ret)
}
