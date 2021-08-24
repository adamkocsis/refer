#' Subset references according to an index
#'
#' @param fname The name of the file containing the references. Should be a bib file.
#' @param index Vector of indices to be subset
#' @param output the name of the output file
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' index <- c(61274, 30858, 65677)
#' subset_bib("pbdb_references.bib", index, "references.bib")
#' }
subset_bib <- function(fname, index, output){
	
	bib <- readLines(fname, encoding = "UTF-8")
	
	#find start and end point
	start <- grep("^@.+\\{", bib)
	end <- c(start[-1]-2, length(bib))
	
	bibindex <- data.frame(start,end)
	
	#pbdb style referencing
	index <- paste0("ref", ":", index, ",")
	
	#create temporary files to subset file
	tempf1 <- tempfile(fileext="txt")
	tempf2 <- tempfile(fileext="txt")
	
	write(index, tempf1)
	write(bib, tempf2)
	
	i <- system(sprintf('grep -n -f %s %s', tempf1, tempf2), intern=T)
	i <- gsub(":.*$", "", i)
	
	refs <- bibindex[which(bibindex$start %in% i),]
	
	if(nrow(refs)==1){
		refs <- matrix(refs[,1]:refs[,2], ncol=1)
	} else{
		refs <- apply(refs, 1, function(x) x[1]:x[2])
	}
	
	if(is.matrix(refs)){
		refs2 <- list()
		
		for(j in 1:ncol(refs)){
			temp <- bib[refs[,j]]
			temp <- temp[-grep("KEYWORDS", temp)]
			
			temp[length(temp)-1] <- paste0(temp[length(temp)-1], ",")
			temp <- gsub(",,", ",", temp)
			
			temp[length(temp):(length(temp)+1)] <- c("KEYWORDS={data}", "}") # add keyword for references
			
			refs2[[j]] <- temp
		}
	} else {
		refs2 <- list()
		
		for(j in 1:length(refs)){
			temp <- bib[refs[[j]]]
			temp <- temp[-grep("KEYWORDS", temp)]
			
			temp[length(temp)-1] <- paste0(temp[length(temp)-1], ",")
			temp <- gsub(",,", ",", temp)
			
			temp[length(temp):(length(temp)+1)] <- c("KEYWORDS={data}", "}") # add keyword for references
			
			refs2[[j]] <- temp
		}
	}
	
	refs <- unlist(refs2)
	
	if(file.exists(output)) file.remove(output)
	refs <- refs[refs != ""]
	xfun::write_utf8(refs, con=output)
	
	message("References were saved in ", output)
}
