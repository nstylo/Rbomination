#' Search trough the entire regrtest code base for a keyword
#'
#' This function searches all the roxygen doc entries for
#' the given search pattern. It then displays the documentation.
#' The search is case insensitive, and grep patterns can be used.
#'
#' @param searchpattern The searchpattern to look for.
#' @return Displays the documentation of all matching functions.
#' @examples
#' search("predictor centered")
#' @export
search <- function(searchpattern) {
	files <- list.files(path="man", pattern="\\.Rd$")
	aliases <- c()
	#matches substring between "\alias{" & "}"
	alias_regex <- "\\\\alias\\{\\s*(.*?)\\s*\\}"

	#Search all .rd files in man/ directory
	for(i in 1:length(files)){
		rd_file <- file(paste("man/", files[i], sep=""))
	    rd_lines <- readLines(rd_file, n=-1L)
	    close(rd_file)
	    #Find the search pattern
	    matches_search <- grepl(searchpattern, rd_lines, ignore.case = TRUE)
	    if(sum(matches_search) > 0){
	    	#Find the "\alias{" line
	    	alias_line <- grep(alias_regex, rd_lines, ignore.case = TRUE, value=TRUE)
	    	alias <- regmatches(alias_line, regexec(alias_regex, alias_line))[[1]][2]
	    	aliases <- c(aliases, alias)
	    }
	}
	
	if(length(aliases) == 0){
		cat("No results found.\n")
	} else if(length(aliases) == 1){
		#immediately display only option
		help_result <- help(aliases[1])
		print(help_result)
	} else {
		cat("Found multiple functions which contain the pattern. Which one do you wish to view?\n")
		for(i in 1:length(aliases)){
			cat(sprintf("%-6s%s\n", sprintf("- [%d]", i), aliases[i]))
		}
		cat("Enter a number:")
		f <- file("stdin")
	    answer <- readLines(f, n = 1L)
	    file_idx <- strtoi(answer)
	    close(f)
	    if(file_idx < 1 || file_idx > length(aliases)){
	    	cat("Please enter a valid range.\n")
    	} else {
    		help_result <- help(aliases[file_idx])
			print(help_result)
    	}
	}
}