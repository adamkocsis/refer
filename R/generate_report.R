#' Generate data report
#'
#' Generates a report for data documentation from databases and for meta analyses.
#' 
#' @param inputFile (\code{csv}) path to metadata file. The template can be generated using the \code{\link{create_metadata}} function.
#' @param data_refs (\code{data.frame}) data references. See \code{\link{generate_bib}} for more info.
#' @param combine (\code{logical}) Should the generated reference file be combined? See \code{\link{generate_bib}} for more info.
#' @param enterer_names (\code{character}) names of data enterers to be printed at the end of the report
#' @param output_path folder in which to save report
#' @param output_file file name of generated report. Must contain ".pdf" extension
#' @param create_dir (\code{logical}) should a new directory be created in which the report and related files are stored?
#' @param draft (\code{logical}) draft mode allows the editing of the markdown file before the report is generated. Set to \code{FALSE} to generate the file without editing. 
#' @param template (\code{tex}) LaTex file for the formatting of the report. Uses the in-built template by default.
#' @param skeletonFile (\code{Rmd}) Rmd file for the formatting of the report. Uses the in-built template by default. 
#' @param in_header custom additions to the header, before the \code{\\begin{document}} statement
#' @param quiet An option to suppress printing of the pandoc command line.
#'
#' @examples
#'\dontrun{
#'
#'#Create generic metafile
#'inputFile <- create_metadata(path=".", edit=FALSE)
#'
#'# Download references from the Paleobiology Database
#'refs <- read.csv("https://paleobiodb.org/data1.2/occs/refs.csv?base_name=Scleractinia&occs_created_after=2020-01-01&select=occs&all_records",
#'  # encoding required to allow reading of special characters such as accented characters
#'  encoding="UTF-8")[1:2,]
#'
#'# Generate report
#'report(inputFile=inputFile,
#'  data_refs = refs,
#'  output_path = file.path("."),
#'  output_file = "report.pdf",
#'  enterer_names=c("Enterer 1", "Enterer 2"))
#'}
#' 
#' @export
#'
report <- function(inputFile,
                   data_refs, combine=TRUE,
                   enterer_names=NULL,
                   output_path=".", 
                   output_file="report.pdf",
                   create_dir =TRUE,
                   draft=FALSE,
                   template=pkg_file("rmarkdown", "templates", "template.tex"),
                   skeletonFile=NULL,
                   in_header = NULL, 
                   quiet = FALSE){
  # checking parameters -----------------------------------------------------
  if(length(grep("\\.pdf$", output_file)) == 0) {
    stop('Please enter a valid output file name, e.g. \"report.pdf\"')
  }
  
  # create new directory if specified ---------------------------------------
  if(create_dir) {
    output_path <- file.path(output_path, "report")
    dir.create(path=output_path, showWarnings = FALSE)
  }
  
  #read metadata from file
  input <- read.csv(inputFile, header=FALSE, encoding="UTF-8")
  input <- input[input$V1 != "",]
  
  #metadata 
  n <- grep("specs", input[,1])
  
  if(length(n) == 0) {
    warning("No data specifications provided")
    n=nrow(input)+1 #setting end of field
  }
  
  metadata <- input[2:(n-1),]
  
  #contains title and authors?
  pars <- c("title", "authors")
  
  for(p in pars){
    if(length(grep(p, metadata$V1)) == 0| metadata$V2[grep(p, metadata$V1)]=="") warning(sprintf("No %s found", p))
    
  }
  
  metadata <- setNames(split(metadata$V2, seq(nrow(metadata))), metadata$V1)
  
  #authors & affiliations
  metadata[["authors"]] <- strsplit(metadata[["authors"]], ";")[[1]]
  metadata[["affiliation"]] <- strsplit(metadata[["affiliation"]], ";")[[1]]
  
  #add attributes for corresponding author
  metadata$corresauth <- as.numeric(metadata$corresauth)
  
  metadata$authors[metadata$corresauth] <- paste0(metadata$authors[metadata$corresauth], "*")
  metadata$corresauth <- paste("*Corresponding author, email:", metadata$corresemail)
  
  authors <- metadata$authors
  affiliation <- metadata$aff
  
  #multiple authors
  multi <- list()
  
  for(i in 1:length(authors)){
    multi[[i]] <- list(name=authors[i], 
                       affiliation=affiliation[i])
  }
  
  metadata$author <- multi
  
  metadata$affiliation <- metadata$authors <- NULL
  metadata$corresemail <- NULL
  
  specs <- input[(n+1):nrow(input),]
  specs <- setNames(split(specs$V2, seq(nrow(specs))), specs$V1)
  
  metadata$params <- specs
  
  if(!is.null(enterer_names)){
    xfun::write_utf8(paste(enterer_names, collapse = ", "), con=file.path(output_path, "enterernames.txt"))
    metadata$enterer_names <- "enterernames.txt"
  }
  
  
  # Format references -------------------------------------------------------
  if(is.character(data_refs)){
    if(length(grep("\\.bib$", data_refs[1]))> 0){
      metadata$bibliography <- data_refs
      skeletonFile <- pkg_file("rmarkdown", "templates", "skeleton", "skeletonbib.Rmd")
      
    } else{
      xfun::write_utf8(data_refs, con=file.path(output_path, "bib.txt"))
      skeletonFile <- pkg_file("rmarkdown", "templates", "skeleton", "skeletontxt.Rmd")
      metadata$params$bibliography <- "bib.txt"
      # create file for package reference
      metadata$bibliography <- "packageref.bib"
      file.create(file.path(output_path, metadata$bibliography))
    }
    
  } else {
    stop("Please provide a valid reference list or file")
  }
  
  #save reference for refer
  write(knitr::write_bib("refer", prefix = "R-pkg-")[[1]],
        file=file.path(output_path, metadata$bibliography[1]), append=TRUE)
  
  output_file <- file.path(output_path, output_file)
  
  template_pandoc(metadata=metadata,
                  output_file = output_file, 
                  output_path = output_path,
                  template=template,
                  skeletonFile = skeletonFile,
                  draft=draft,
                  quiet=quiet)
}

#' Create metadata file to generate report
#'
#' @param path path where to create file
#' @param edit open file after creation for edits
#' @param overwrite (\code{logical}) Should the file be overwritten if already exists?
#' @param return_path (\code{logical}) return the file path of the create file
#'
#' @examples 
#' \dontrun{
#' create_metadata(path=".", edit=FALSE)
#' }
#' @export
#'
create_metadata <- function(path, edit=TRUE, overwrite=FALSE, return_path=TRUE){
  file.copy(pkg_file("rmarkdown", "templates", "sample.csv"), to=file.path(path, "metadata.csv"))
  
  if (edit) file.show(file.path(path, "metadata.csv"), overwrite=overwrite)
  
  if(return_path) return(file.path(path, "metadata.csv"))
}

#' Capitalise string
#'
#' @export 
capitalize <- function(string) {
  paste0(toupper(substr(string, 1, 1)),
         tolower(substr(string, 2, nchar(string))))
}

template_pandoc <- function(metadata, 
                            template,
                            skeletonFile,
                            output_file,
                            output_path,
                            draft,
                            quiet) {
  #create yaml document
  tmp <- tempfile(fileext = ".yaml")
  on.exit(unlink(tmp), add = TRUE)
  
  xfun::write_utf8(yaml::as.yaml(metadata), tmp)
  
  append_yaml(gsub("pdf", "Rmd", output_file),
              skeletonFile = skeletonFile,
              yaml=tmp)
  
  #copy template as well
  file.copy(from = template, to=file.path(output_path, basename(template)))
  
  if(draft){
    output_file <- gsub("pdf", "Rmd", output_file)
  } else {
    rmarkdown::render(
      input=gsub("pdf", "Rmd", output_file), quiet=quiet)
    
  }
  
  invisible(output_file)
  file.show(output_file)
}

append_yaml <- function (inputFile, skeletonFile, encoding, yaml) {   
  # read in the YAML + src file
  yaml <- readLines(yaml)
  rmd <- readLines(skeletonFile)
  
  # insert the YAML in after the first ---
  # I'm assuming all my RMDs have properly-formed YAML and that the first
  # occurence of --- starts the YAML. You could do proper validation if you wanted.
  yamlHeader <- grep('^---$', rmd)[1]
  # put the yaml in
  rmd <- append(rmd, yaml, after=yamlHeader)
  
  # write out to a temp file
  ofile <- file.path(tempdir(), basename(inputFile))
  writeLines(rmd, ofile)
  
  # copy back to the current directory.
  file.copy(ofile, file.path(dirname(inputFile), 
                             basename(ofile)), overwrite=T)
}
