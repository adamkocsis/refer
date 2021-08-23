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
  
  bib <- readLines(fname)
  
  #find start and end point
  start <- grep("^@.+\\{", bib)
  length(start)
  end <- grep("\\}$", bib)
  length(end)
  
  bibindex <- data.frame(start,end)
  
  #pbdb style referencing
  index <- paste0("ref", "\\:", index, ",$")
  
  #subsetting starts here
  i <- grep(paste0(index, collapse = "|"), bib)
  
  refs <- bibindex[which(bibindex$start %in% i),]
  refs <- apply(refs, 1, function(x) x[1]:x[2])
  
  for(i in 1:length(refs)){
    temp <- bib[refs[[i]]]
    
    temp[length(temp):(length(temp)+1)] <- c("keyword=data", "}") # add keyword for references
    
    refs[[i]] <- temp
  }
  
  refs <- unlist(refs)
  xfun::write_utf8(refs, con=output)
  
  message("References were saved in ", output)
}
