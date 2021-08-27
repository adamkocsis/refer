#Accessing file within the package
#' @export
pkg_file <- function(...) {
	system.file(..., package = "refer")
}

#Break url so that it fits in report
#' @export
wrap_url <- function(x, width=50, by=" "){
	
	n1 <- which(nchar(x)>width)
	n2 <- grep("^http", x)
	
	n <- intersect(n1,n2)
	
	for(i in 1:length(n)){
		
		temp <- x[n][i]
		
		ind <- unique(c(seq(1, nchar(temp), width), nchar(temp)))
		ind <- data.frame(sta=ind[1:(length(ind)-1)],
						  end=ind[2:length(ind)])
		x[n][i] <- paste(apply(ind, 1, function(x) substr(temp, x[1], x[2]-1)), collapse =by)
		
	}
	
	return(x)
}
