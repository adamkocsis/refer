#Accessing file within the package
#' @export
pkg_file <- function(...) {
	system.file(..., package = "refer")
}
