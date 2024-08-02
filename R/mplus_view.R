# Functions from results.R

##########################################################################
#' Mplus View Results
#'
#' Loads the file and lists all available functions
#'
#' @param file the quoted name of an existing H5 file
#' @param quiet logical
#'
#' @return lists the functions available for the output
#' @export
#'
#' @examples
#'
#' ex3_1 <- system.file("extdata", "ex3_1.h5", package = "mplush5")
#' mplus.view.results(ex3_1)
#'
#'
mplus.view.results <- function(file,quiet=FALSE) {
  if (!(file.exists(file))) {
    cstr <- paste("- file does not exist:",file,"\n")
    cat(cstr)
  }

  #H5close()
  gh5 <- rhdf5::h5dump(file, load=TRUE)

  # sections <- read.csv(results_sections_file)

  nsections <- dim(sections)[1]

  if (!quiet) {
    cat(c("\nUse the following functions to view the available results.\n\n"))
  }

  functionlist <- vector()
  descriptions <- vector()
  for (i in c(1:nsections)) {
    if (sections$section_name[i] %in% names(gh5)) {
      if (sections$function_name[i] != "") {
        if (exists(sections$function_name[i],mode="function"))
        {
          if (!quiet) {
            cat(" - ",sections$section_name[i],"\n",sep="")
            cat("\t - ",sections$function_name[i],"\n",sep="")
            if (sections$arguments[i] != "") {
              cat("\t - use '",sections$arguments[i],"'\n",sep="")
            }
          }
          descriptions <- append(descriptions, sections$section_name[i])
          if (sections$arguments[i] != "") {
            cstr <- sprintf("%s(%s)", sections$function_name[i], sections$arguments[i])
            functionlist <- append(functionlist, cstr)
          } else {
            functionlist <- append(functionlist, sections$function_name[i])
          }
        } else {
          if (!quiet) {
            cat(" - ",sections$section_name[i],"\n")
            cat("\t ** function not yet available\n")
          }
        }
      }
    }
  }
  for(i in c(1:length(names(gh5)))) {
    pind <- pmatch(names(gh5)[i], sections$section_name, nomatch=0)
    if (!quiet) {
      if (pind == 0) {
        cat(" -",names(gh5)[i],"\n")
        cat("\t ** function not yet available\n")
      } else if (sections$function_name[pind] == "" || is.na(sections$function_name[pind]) || nchar(sections$function_name[pind]) == 0) {
        cat(" -",names(gh5)[i],"\n")
        cat("\t ** function not yet available\n")
      }
    }
  }
  #	df <- data.frame()
  #	df <- rbind(df, descriptions, functionlist)
  #	rnames <- c('description','function')

  #	cnames <- seq(1:length(descriptions))
  #	rownames(df) <- rnames
  #	colnames(df) <- cnames
  #	df <- t(df)
  names(functionlist) <- descriptions

  invisible(functionlist)
}



######################################################################################################
# New functions for MplusResultsUI
######################################################################################################
#' Mplus Get Available Outputs
#'
#' Lists all available functions. Similar to mplus.view.results().

#'
#' @param file the quoted name of an existing H5 file
#'
#' @return lists the functions available for the output
#' @export
#'
#' @examples
#'
#' ex3_1 <- system.file("extdata", "ex3_1.h5", package = "mplush5")
#' mplus.get.available.outputs(ex3_1)
#'
#' ex5_5 <- system.file("extdata", "ex5_5.h5", package = "mplush5")
#' mplus.get.available.outputs(ex5_5)
#'
mplus.get.available.outputs <- function(file) {

  if ( !(file.exists(file))) {
    cstr <- paste("- file does not exist:",file,"\n")
    stop(cstr)
  }

  plotlist <- mplus.view.results(file, TRUE)
  #plotlist <- gsub("(.+)\\|.*\\|.*","\\1", plotlist)
  plotlist
}

