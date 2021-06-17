#' Dummy function
#'
#' This is a dummy function to be used as a template - with imports
#'
#' @param x Text to be displayed
#'
#' @param return The function has no return value
#' @export
#' @examples
#' dummy("I am a dummy function call!")
dummy <- function(x){
	cat(x, "\n")
	utils::flush.console()
}

