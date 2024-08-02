# Functions from results.R

##########################################################################
#' Mplus Print Input Instructions
#'
#' Show the input instructions.
#'
#' @param file the quoted name of an existing H5 file
#' @param quiet logical
#'
#' @return The Mplus input instructions are printed to the screen
#' @export
#'
#' @examples
#'
#' ex3_1 <- system.file("extdata", "ex3_1.h5", package = "mplush5")
#' mplus.print.input.instructions(ex3_1)
#'
#' ex5_5 <- system.file("extdata", "ex5_5.h5", package = "mplush5")
#' mplus.print.input.instructions(ex5_5)
#'
mplus.print.input.instructions <- function(file,quiet=FALSE) {
  if (!(file.exists(file))) {
    cstr <- paste("- file does not exist:",file,"\n")
    cat(cstr)
  }

  #H5close()
  gh5 <- rhdf5::h5dump(file, load=TRUE)

  instructions <- gh5$`Input instructions`
  instructions <- gsub("(^\\s+|\\s+$)", "", instructions, perl=TRUE)

  if (!quiet) {
    for (i in c(1:length(instructions))) {
      cstr <- paste(instructions[i],"\n")
      cat(cstr)
    }
  } else {
    for (i in c(1:length(instructions))) {
      cstr <- paste(instructions[i],"\n")
      instructions[i] <- cstr
    }
    invisible(instructions)
  }
}


##########################################################################
#' Mplus Print Errors and Warnings
#'
#' Show the errors and warnings
#'
#' @param file the quoted name of an existing OH5 file
#' @param quiet logical
#'
#' @return The errors and warning messages are printed to the screen
#' @export
#'
#' @examples
#' \dontrun{
#' mplus.print.errors.and.warnings('ex.oh5')
#' }
#'
mplus.print.errors.and.warnings <- function(file,quiet=FALSE) {
  if (!(file.exists(file))) {
    cstr <- paste("- file does not exist:",file,"\n")
    cat(cstr)
  }

  #H5close()
  gh5 <- rhdf5::h5dump(file, load=TRUE)

  instructions <- gh5$`Errors and warnings`
  instructions <- gsub("(^\\s+|\\s+$)", "", instructions, perl=TRUE)

  if (!quiet) {
    for (i in c(1:length(instructions))) {
      cstr <- paste(instructions[i],"\n")
      cat(cstr)
    }
  } else {
    for (i in c(1:length(instructions))) {
      cstr <- paste(instructions[i],"\r\n")
      instructions[i] <- cstr
    }
    invisible(instructions)
  }
}

##########################################################################
#' Mplus Print Summary Analysis
#'
#' Show the summary analysis
#'
#' @param file the quoted name of an existing H5 file
#' @param section ???
#' @param quiet logical
#'
#' @return The summary analysis is printed to the screen
#' @export
#'
#' @examples
#'
#' ex3_1 <- system.file("extdata", "ex3_1.h5", package = "mplush5")
#' mplus.print.summary.analysis(ex3_1)
#'
mplus.print.summary.analysis <- function(file,section='all',quiet=FALSE) {
  if (!(file.exists(file))) {
    cstr <- paste("- file does not exist:",file,"\n")
    cat(cstr)
  }

  #H5close()
  gh5 <- rhdf5::h5dump(file, load=TRUE)

  # summary.data <- read.csv(summary_analysis_file)

  summaries <- vector()

  gstr <- c('Summary of Analysis')
  cstr <- paste(gstr,"\n")
  if (!quiet) cat(cstr)
  #summaries <- append(summaries, cstr)

  subgroup <- FALSE
  subgrpstr <- NULL
  subgrpindex <- 0
  subgrptotal <- 0

  subgrpskip <- FALSE
  subgrpskipindex <- 0
  subgrpskiptotal <- 0

  subsubgroup <- FALSE
  subsubgrpstr <- NULL
  subsubgrpindex <- 0
  subsubgrptotal <- 0

  indent <- c('')
  groupstr <- gstr

  for (i in c(1:nrow(summary.data))) {
    found <- FALSE
    if (section=='all' || section==summary.data$section[i]) {
      astr <- summary.data$description[i]
      if (subgroup) {
        if (subsubgroup) {
          outstr <- sprintf("*** D in %s/%s:... %s\n", subgrpstr, subsubgrpstr, astr)
        } else {
          outstr <- sprintf("** D in %s:... %s\n", subgrpstr, astr)
        }
      } else {
        outstr <- sprintf("* D:... %s\n", astr)
      }

      if (subgrpskip) {
        if (is.na(subgrpskiptotal) || subgrpskiptotal == -1) {
          if (summary.data$storagetype[i] == 'endheading') {
            subgrpskip <- FALSE
            subgrpskipindex <- 0
            subgrpskiptotal <- 0
          }
        } else {
          subgrpskipindex <- subgrpskipindex + 1
          if (subgrpskipindex == subgrpskiptotal) {
            subgrpskip <- FALSE
            subgrpskipindex <- 0
            subgrpskiptotal <- 0
          }
        }
      } else {
        if (summary.data$storagetype[i] == 'heading') {
          if (subgroup) {
            subsubgroup <- TRUE
            groupstr <- sprintf('%s/%s',groupstr,astr)
            subsubgrpstr <- astr
            subsubgrptotal <- as.numeric(summary.data$datatype[i])
            indent <- sprintf('%s  ', indent)
          } else {
            if (!(astr %in% names(gh5[[gstr]])) ) {
              subgrpskip <- TRUE
              subgrpskiptotal <- as.numeric(summary.data$datatype[i])
            } else {
              cstr <- sprintf("%s%s\n", indent, astr)
              if (!quiet) cat(cstr)
              summaries <- append(summaries, cstr)

              subgroup <- TRUE
              groupstr <- sprintf('%s/%s',gstr,astr)
              subgrpstr <- astr
              subgrptotal <- as.numeric(summary.data$datatype[i])
              indent <- c('  ')
            }
          }
        } else if (summary.data$storagetype[i] == 'endheading') {
          if (subsubgroup) {
            subsubgroup <- FALSE
            subsubgrpstr <- NULL
            subsubgrpindex <- 0
            subsubgrptotal <- 0
            indent <- c('  ')
            groupstr <- sprintf('%s/%s',gstr,subgrpstr)
          } else if (subgroup) {
            subgroup <- FALSE
            subgrpstr <- NULL
            subgrpindex <- 0
            subgrptotal <- 0
            indent <- c('')
            groupstr <- gstr
          }
        } else if (summary.data$storagetype[i] == 'attribute') {
          if (mplus.check.group.attribute(file,groupstr,astr)) {
            valarray <- mplus.get.group.attribute(file,groupstr,astr)
            found <- TRUE
          }
        } else {
          if (subgroup) {
            if ((astr %in% names(gh5[[gstr]][[subgrpstr]])) ) {
              valarray <- mplus.get.group.dataset(file,groupstr,astr)
              found <- TRUE
            }
          } else {
            if ((astr %in% names(gh5[[gstr]])) ) {
              valarray <- mplus.get.group.dataset(file,groupstr,astr)
              found <- TRUE
            }
          }
        }
        if (found && (summary.data$storagetype[i] == 'attribute' || summary.data$storagetype[i] == 'dataset')) {
          if (summary.data$datatype[i] == 'integer') {
            if (length(valarray) == 1) {
              cstr <- sprintf("%s%s:  %d\n", indent, astr, as.numeric(valarray[1]))
              if (!quiet) cat(cstr)
              summaries <- append(summaries, cstr)
            } else {
              for (i in c(1:length(valarray))) {
                cstr <- sprintf("%s%s[%d]:  %d\n", indent, astr, i, as.numeric(valarray[i]))
                if (!quiet) cat(cstr)
                summaries <- append(summaries, cstr)
              }
            }
          } else if (summary.data$datatype[i] == 'float') {
            if (length(valarray) == 1) {
              cstr <- sprintf("%s%s:  %e\n", indent, astr, as.numeric(valarray[1]))
              if (!quiet) cat(cstr)
              summaries <- append(summaries, cstr)
            } else {
              for (i in c(1:length(valarray))) {
                cstr <- sprintf("%s%s[%d]:  %e\n", indent, astr, i, as.numeric(valarray[i]))
                if (!quiet) cat(cstr)
                summaries <- append(summaries, cstr)
              }
            }
          } else {
            if (length(valarray) == 1) {
              cstr <- sprintf("%s%s:  %s\n", indent, astr, valarray[1])
              if (!quiet) cat(cstr)
              summaries <- append(summaries, cstr)
            } else {
              for (i in c(1:length(valarray))) {
                cstr <- sprintf("%s%s[%d]:  %s\n", indent, astr, i, valarray[i])
                if (!quiet) cat(cstr)
                summaries <- append(summaries, cstr)
              }
            }
          }

          if (subsubgroup) {
            subsubgrpindex <- subsubgrpindex + 1
            if (subsubgrpindex == subsubgrptotal) {
              subsubgroup <- FALSE
              subsubgrpstr <- NULL
              subsubgrpindex <- 0
              subsubgrptotal <- 0
              indent <- c('  ')
              groupstr <- sprintf('%s/%s',gstr,subgrpstr)
            }
          }
          if (subgroup) {
            subgrpindex <- subgrpindex + 1
            if (subgrpindex == subgrptotal) {
              subgroup <- FALSE
              subgrpstr <- NULL
              subgrpindex <- 0
              subgrptotal <- 0
              subgrpskiptotal <- 0
              indent <- c('')
              groupstr <- gstr
            }
          }
        }
      }
    }
  }
  if (quiet) {
    invisible(summaries)
  }
}

##########################################################################
#' Mplus Print Model Fit Information
#'
#' Show the model fit information
#'
#' @param file the quoted name of an existing H5 file
#' @param invariance 0 (not invariance model, default)
#               - 1 (configural invariance)
#               - 2 (metric invariance)
#               - 3 (scalar invariance)
#'
#' @return A data frame containing the model fit information
#' @export
#'
#' @examples
#'
#' ex3_1 <- system.file("extdata", "ex3_1.h5", package = "mplush5")
#' mod_fit <- mplus.print.model.fit.information(ex3_1)
#' mod_fit
#'
mplus.print.model.fit.information <- function(file,invariance=0) {
  if (!(file.exists(file))) {
    cstr <- paste("- file does not exist:",file,"\n")
    cat(cstr)
  }

  #H5close()
  gh5 <- rhdf5::h5dump(file, load=TRUE)

  if (invariance == 0) {
    gstr <- c('Model Fit Information')
  } else if (invariance == 1) {
    gstr <- c('Model Fit Information (configural)')
  } else if (invariance == 2) {
    gstr <- c('Model Fit Information (metric)')
  } else if (invariance == 3) {
    gstr <- c('Model Fit Information (scalar)')
  }

  if (!(gstr %in% names(gh5)) ) {
    cstr <- sprintf("The output does not have any %s.\n", gstr)
    stop(cstr)
  }

  Description <- gh5[[gstr]]$Description
  Description <- gsub("(^\\s+|\\s+$)", "", Description, perl=TRUE)

  df <- data.frame(description = Description, value = gh5[[gstr]]$Value)
  df
}

##########################################################################
#' Mplus Print General Results
#'
#' Show the results with given group name
#'
#' @param file the quoted name of an existing OH5 file
#' @param gstr the quoted name of results group
#' @param subgstr the quoted name of results subgroup
#'
#' @return A data frame of the model results
#' @export
#'
#' @examples
#' \dontrun{
#' mplus.print.general.results('ex.oh5')
#' }
#'
mplus.print.general.results <- function(file,gstr,subgstr) {
  if (!(file.exists(file))) {
    cstr <- paste("- file does not exist:",file,"\n")
    cat(cstr)
  }

  #H5close()
  gh5 <- rhdf5::h5dump(file, load=TRUE)

  if (!(gstr %in% names(gh5)) ) {
    cstr <- sprintf("The output does not have any %s.\n", gstr)
    stop(cstr)
  }

  if (missing(subgstr)) {
    results_str <- sprintf("%s/Results", gstr)
    statements_str <- sprintf("%s/Statements", gstr)

    rheadings <- mplus.get.dataset.attribute(file,results_str,"Headings")
    rheadings <- gsub("(^\\s+|\\s+$)", "", rheadings, perl=TRUE)

    sheadings <- mplus.get.dataset.attribute(file,statements_str,"Headings")
    sheadings <- gsub("(^\\s+|\\s+$)", "", sheadings, perl=TRUE)

    statements <- gh5[[gstr]]$Statements
    statements <- gsub("(^\\s+|\\s+$)", "", statements, perl=TRUE)

    num_statements <- dim(gh5[[gstr]]$Results)[1]
    num_values <- dim(gh5[[gstr]]$Results)[2]
  } else {
    if (!(subgstr %in% names(gh5[[gstr]])) ) {
      cstr <- sprintf("The output does not have %s.\n", subgstr)
      stop(cstr)
    }

    results_str <- sprintf("%s/%s/Results", gstr, subgstr)
    statements_str <- sprintf("%s/%s/Statements", gstr, subgstr)

    rheadings <- mplus.get.dataset.attribute(file,results_str,"Headings")
    rheadings <- gsub("(^\\s+|\\s+$)", "", rheadings, perl=TRUE)

    sheadings <- mplus.get.dataset.attribute(file,statements_str,"Headings")
    sheadings <- gsub("(^\\s+|\\s+$)", "", sheadings, perl=TRUE)

    statements <- gh5[[gstr]][[subgstr]]$Statements
    statements <- gsub("(^\\s+|\\s+$)", "", statements, perl=TRUE)

    num_statements <- dim(gh5[[gstr]][[subgstr]]$Results)[1]
    num_values <- dim(gh5[[gstr]][[subgstr]]$Results)[2]
  }

  df <- data.frame()

  colheading <- c()
  idx <- 1
  if (sheadings[idx] == "Group") {
    df <- data.frame(group = statements[,idx])
    colheading <- append(colheading, "Group")
    idx <- idx + 1
  } else if (sheadings[idx] == "Class") {
    df <- data.frame(classlabel = statements[,idx])
    colheading <- append(colheading, "Class")
    idx <- idx + 1
  } else if (sheadings[idx] == "Cluster") {
    df <- data.frame(clusterids = statements[,idx])
    colheading <- append(colheading, "Cluster")
    idx <- idx + 1
  }
  if (sheadings[idx] == "Level") {
    if (idx > 1) {
      df <- cbind(df, level = statements[,idx])
    } else {
      df <- data.frame(level = statements[,idx])
    }
    colheading <- append(colheading, "Level")
    idx <- idx + 1
  }
  sectind <- pmatch("Section", sheadings, nomatch=0)
  modelstmts <- c()
  stmttypes <- c()

  for (i in c(1:num_statements)) {
    if (statements[i,sectind] == "") {
      newstmt <- sprintf("%s %s %s", statements[i,sectind+1], statements[i,sectind+2], statements[i,sectind+3])
    } else if (grepl("Statements", statements[i,sectind], fixed=TRUE)) {
      newstmt <- sprintf("%s %s %s", statements[i,sectind+1], statements[i,sectind+2], statements[i,sectind+3])
    } else {
      newstmt <- sprintf("%s", statements[i,sectind+1])
    }
    stmttypes <- append(stmttypes, statements[i,sectind])
    modelstmts <- append(modelstmts, newstmt)
  }

  if (idx > 1) {
    df <- cbind(df, stmttypes)
  } else {
    df <- data.frame(statement = stmttypes)
  }
  colheading <- append(colheading, "Section")

  df <- cbind(df, modelstmts)
  colheading <- append(colheading, "Statement")

  for (i in c(1:num_values)) {
    if (missing(subgstr)) {
      df <- cbind(df, as.numeric(gh5[[gstr]]$Results[,i]))
    } else {
      df <- cbind(df, as.numeric(gh5[[gstr]][[subgstr]]$Results[,i]))
    }
    colheading <- append(colheading, rheadings[i])
  }
  colnames(df) <- colheading
  if (pmatch("Significance", colnames(df), nomatch=0) > 0)
  {
    df$Significance[df$Significance == 1] <- c('*')
    df$Significance[df$Significance == 0] <- c('')
  }
  df
}

##########################################################################
#' Mplus Print Model Results
#'
#' Show the model results
#'
#' @param file the quoted name of an existing H5 file
#' @param invariance model invariance
#'
#'   * `0`: (the default): not invariance model
#'
#'   * `1`: configural invariance
#'
#'   * `2`: metric invariance
#'
#'   * `3`: scalar invariance
#' @return A data frame containing the model results
#' @export
#'
#' @examples
#'
#' ex3_1 <- system.file("extdata", "ex3_1.h5", package = "mplush5")
#' results <- mplus.print.model.results(ex3_1)
#' results
#' ex5_5 <- system.file("extdata", "ex5_5.h5", package = "mplush5")
#' results <- mplus.print.model.results(ex5_5)
#' results
#'
mplus.print.model.results <- function(file,invariance=0) {
  if (!(file.exists(file))) {
    cstr <- paste("- file does not exist:",file,"\n")
    cat(cstr)
  }

  if (invariance == 0) {
    gstr <- c('Model Results')
  } else if (invariance == 1) {
    gstr <- c('Model Results (configural)')
  } else if (invariance == 2) {
    gstr <- c('Model Results (metric)')
  } else if (invariance == 3) {
    gstr <- c('Model Results (scalar)')
  }

  df <- mplus.print.general.results(file,gstr)
  df
}

##########################################################################
#' Mplus Print Confidence Intervals
#'
#' Show the confidence intervals
#'
#' @param file the quoted name of an existing OH5 file
#' @param cred logical TRUE for Credibility Intervals
#           FALSE for Confidence Intervals (default)
#' @param invariance 0 (not invariance model, default)
#               - 1 (configural invariance)
#               - 2 (metric invariance)
#               - 3 (scalar invariance)
#
#'
#' @return A data frame containing the confidence intervals
#' @export
#'
#' @examples
#'
#' ex3_1 <- system.file("extdata", "ex3_1.h5", package = "mplush5")
#' ci <- mplus.print.confidence.intervals(ex3_1)
#' ci
#'
mplus.print.confidence.intervals <- function(file,cred=FALSE,invariance=0) {
  if (!(file.exists(file))) {
    cstr <- paste("- file does not exist:",file,"\n")
    cat(cstr)
  }

  if (cred) {
    cistr <- c('Credibility')
  } else {
    cistr <- c('Confidence')
  }
  if (invariance == 0) {
    gstr <- sprintf("%s Intervals of Model Results", cistr)
  } else if (invariance == 1) {
    gstr <- sprintf("%s Intervals of Model Results (configural)", cistr)
  } else if (invariance == 2) {
    gstr <- sprintf("%s Intervals of Model Results (metric)", cistr)
  } else if (invariance == 3) {
    gstr <- sprintf("%s Intervals of Model Results (scalar)", cistr)
  }

  df <- mplus.print.general.results(file,gstr)
  df
}


##########################################################################
#' Mplus Print Standardized Model Results
#'
#' Show the standardized model results
#'
#' @param file the quoted name of an existing H5 file
#' @param stype the standardized type ['stdyx','stdy','std']
#' @param cluster logical
#'
#' @return A data frame containing the standardized results
#' @export
#'
#' @examples
#'
#' ex3_1 <- system.file("extdata", "ex3_1.h5", package = "mplush5")
#' results_stdyx <- mplus.print.standardized.model.results(ex3_1, "stdyx")
#' results_stdyx
#'
mplus.print.standardized.model.results <- function(file,stype,cluster=FALSE) {
  if (missing(file)) {
    stop("- name of the GH5 file is required")
  }
  if (!(file.exists(file))) {
    cstr <- paste("- file does not exist:",file,"\n")
    cat(cstr)
  }

  #H5close()
  gh5 <- rhdf5::h5dump(file, load=TRUE)

  if (cluster) {
    gstr <- c('Within-level Standardized Model Results For Each Cluster')
  } else {
    gstr <- c('Standardized Model Results')
  }

  if (!(gstr %in% names(gh5)) ) {
    cstr <- sprintf("The output does not have any %s.\n", gstr)
    stop(cstr)
  }

  if (missing(stype)) {
    if (!("Results" %in% names(gh5[[gstr]])) ) {
      if (("STDYX Standardization" %in% names(gh5[[gstr]])) ||
          ("STD Standardization" %in% names(gh5[[gstr]])) ||
          ("STDY Standardization" %in% names(gh5[[gstr]]))) {
        stop("- type of standardization (std/stdy/stdyx) is required")
        stop(cstr)
      } else {
        cstr <- sprintf("The output does not have any %s.\n", gstr)
        stop(cstr)
      }
    }

    df <- mplus.print.general.results(file,gstr)
  } else {
    stype2 <- tolower(stype)
    if (!(stype2 == "stdyx" || stype2 == "stdy" || stype2 == "std")) {
      cstr <- sprintf("Unknown stype argument:  %s\nUse one of: stdyx, stdy, std", stype)
      stop(cstr)
    } else if (stype2 == "stdyx" && ("STDYX Standardization" %in% names(gh5[[gstr]]))) {
      standstr <- "STDYX Standardization"
    } else if (stype2 == "stdy" && ("STDY Standardization" %in% names(gh5[[gstr]]))) {
      standstr <- "STDY Standardization"
    } else if (stype2 == "std" && ("STD Standardization" %in% names(gh5[[gstr]]))) {
      standstr <- "STD Standardization"
    } else {
      standstr <- NULL
    }

    if (is.null(standstr)) {
      df2 <- mplus.print.general.results(file,gstr)
      standstr <- sprintf("%s estimate", stype)
      stdind <- pmatch(standstr, tolower(colnames(df2)), nomatch=0)
      stmind <- pmatch("Statement", colnames(df2), nomatch=0)
      if (stdind > 0) {
        df <- data.frame(df2[1])
        for (i in c(2:stmind)) {
          df <- cbind(df, df2[i])
        }
        df <- cbind(df, df2[stdind])
      } else {
        df <- df2
      }
    } else {
      df <- mplus.print.general.results(file,gstr,standstr)
    }
  }

  df
}

##########################################################################
#' Mplus Print Standardized Confidence Intervals
#'
#' Show the standardized confidence intervals
#'
#' @param file the quoted name of an existing H5 file
#' @param stype the standardized type ['stdyx','stdy','std']
#' @param cred logical TRUE for Credibility Intervals
#           FALSE for Confidence Intervals (default)
#'
#' @return A data frame containing the standardized confidence intervals
#' @export
#'
#' @examples
#'
#' ex3_1 <- system.file("extdata", "ex3_1.h5", package = "mplush5")
#' ci_stdyx <- mplus.print.standardized.confidence.intervals(ex3_1, "stdyx")
#' ci_stdyx
#'
mplus.print.standardized.confidence.intervals <- function(file,stype,cred=FALSE) {
  if (missing(file)) {
    stop("- name of the GH5 file is required")
  }
  if (missing(stype)) {
    stop("- type of standardization (std/stdy/stdyx) is required")
  }
  if (!(file.exists(file))) {
    cstr <- paste("- file does not exist:",file,"\n")
    cat(cstr)
  }

  if (cred) {
    cistr <- c('Credibility')
  } else {
    cistr <- c('Confidence')
  }
  gstr <- sprintf("%s Intervals of Standardized Model Results", cistr)

  if (stype == "stdyx") {
    standstr <- "STDYX Standardization"
  } else if (stype == "stdy") {
    standstr <- "STDY Standardization"
  } else if (stype == "std") {
    standstr <- "STD Standardization"
  } else {
    cstr <- sprintf("Unknown stype argument:  %s\nUse one of: stdyx, stdy, std", stype)
    stop(cstr)
  }

  df <- mplus.print.general.results(file,gstr,standstr)
  df
}

##########################################################################
#' Mplus Print R Square
#'
#' Show the R-square
#'
#' @param file the quoted name of an existing H5 file
#' @param invariance 0 (not invariance model, default)
#               - 1 (configural invariance)
#               - 2 (metric invariance)
#               - 3 (scalar invariance)
#' @param cluster logical
#'
#' @return A data frame containing the R-square is printed
#' @export
#'
#' @examples
#'
#' ex3_1 <- system.file("extdata", "ex3_1.h5", package = "mplush5")
#' r2 <- mplus.print.rsquare(ex3_1)
#' r2
#'
mplus.print.rsquare <- function(file,invariance=0,cluster=FALSE) {
  if (missing(file)) {
    stop("- name of the GH5 file is required")
  }
  if (!(file.exists(file))) {
    cstr <- paste("- file does not exist:",file,"\n")
    cat(cstr)
  }

  if (cluster) {
    gstr <- c('Within-level R-Square For Each Cluster')
  } else {
    if (invariance == 0) {
      gstr <- c('R-Square')
    } else if (invariance == 1) {
      gstr <- c('R-Square (configural)')
    } else if (invariance == 2) {
      gstr <- c('R-Square (metric)')
    } else if (invariance == 3) {
      gstr <- c('R-Square (scalar)')
    }
  }

  df <- mplus.print.general.results(file,gstr)
  df
}

##########################################################################
#' Mplus Print Model Modification Indices
#'
#' Show the model modification indices
#'
#' @param file the quoted name of an existing H5 file
#' @param getdf logical
#'
#' @return A data frame containing the modification indices
#' @export
#'
#' @examples
#'
#' ex5_5_wlsmv <- system.file("extdata", "ex5_5_wlsmv.h5", package = "mplush5")
#' mi <- mplus.print.model.modification.indices(ex5_5_wlsmv)
#' mi
#'
mplus.print.model.modification.indices <- function(file,getdf=FALSE) {
  if (!(file.exists(file))) {
    cstr <- paste("- file does not exist:",file,"\n")
    cat(cstr)
  }

  gstr <- c('Model Modification Indices')

  if (mplus.check.group.attribute(file, "Model Modification Indices", "Minimum value")) {
    minval <- as.numeric(mplus.get.group.attribute(file, "Model Modification Indices", "Minimum value"))
    cstr <- sprintf("%s:  Minimum value = %f\n", gstr, minval)
  } else {
    cstr <- paste(gstr,"\n")
  }
  cat(cstr)

  if (mplus.check.group.attribute(file, "Model Modification Indices", "Message")) {
    cstr <- mplus.get.group.attribute(file, "Model Modification Indices", "Message")
    stop(cstr)
  }

  gstr <- c('Model Modification Indices')

  df <- mplus.print.general.results(file,gstr)
  df
}


##########################################################################
#' Mplus Print IRT Parameterization
#'
#' Show the IRT parameterization
#'
#' @param file the quoted name of an existing H5 file
#' @param invariance 0 (not invariance model, default)
#               - 1 (configural invariance)
#               - 2 (metric invariance)
#               - 3 (scalar invariance)
#'
#' @return A data frame containing the IRT parameterization results
#' @export
#'
#' @examples
#'
#' ex5_5 <- system.file("extdata", "ex5_5.h5", package = "mplush5")
#' irt_results <- mplus.print.irt.parameterization(ex5_5)
#' irt_results
#'
mplus.print.irt.parameterization <- function(file,invariance=0) {
  if (!(file.exists(file))) {
    cstr <- paste("- file does not exist:",file,"\n")
    cat(cstr)
  }

  if (invariance == 0) {
    gstr <- c('IRT Parameterization')
  } else if (invariance == 1) {
    gstr <- c('IRT Parameterization (configural)')
  } else if (invariance == 2) {
    gstr <- c('IRT Parameterization (metric)')
  } else if (invariance == 3) {
    gstr <- c('IRT Parameterization (scalar)')
  }

  df <- mplus.print.general.results(file,gstr)
  df
}

##########################################################################
#' Mplus Print Results in Probability Scale
#'
#' Show the results in probability scale
#'
#' @param file the quoted name of an existing H5 file
#' @param invariance 0 (not invariance model, default)
#               - 1 (configural invariance)
#               - 2 (metric invariance)
#               - 3 (scalar invariance)
#'
#' @return A data frame containing the results in the probability scale
#' @export
#'
#' @examples
#'
#' ex5_5 <- system.file("extdata", "ex5_5.h5", package = "mplush5")
#' prob_results <- mplus.print.results.in.probability.scale(ex5_5)
#' prob_results
#'
mplus.print.results.in.probability.scale <- function(file,invariance=0) {
  if (!(file.exists(file))) {
    cstr <- paste("- file does not exist:",file,"\n")
    cat(cstr)
  }

  if (invariance == 0) {
    gstr <- c('Results in Probability Scale')
  } else if (invariance == 1) {
    gstr <- c('Results in Probability Scale (configural)')
  } else if (invariance == 2) {
    gstr <- c('Results in Probability Scale (metric)')
  } else if (invariance == 3) {
    gstr <- c('Results in Probability Scale (scalar)')
  }

  #H5close()
  gh5 <- rhdf5::h5dump(file, load=TRUE)

  #cstr <- paste(gstr,"\n")
  #cat(cstr)

  results_str <- sprintf("%s/Results", gstr)
  statements_str <- sprintf("%s/Statements", gstr)

  rheadings <- mplus.get.dataset.attribute(file,results_str,"Headings")
  rheadings <- gsub("(^\\s+|\\s+$)", "", rheadings, perl=TRUE)

  sheadings <- mplus.get.dataset.attribute(file,statements_str,"Headings")
  sheadings <- gsub("(^\\s+|\\s+$)", "", sheadings, perl=TRUE)

  statements <- gh5[[gstr]]$Statements
  statements <- gsub("(^\\s+|\\s+$)", "", statements, perl=TRUE)

  num_statements <- dim(gh5[[gstr]]$Results)[1]
  num_values <- dim(gh5[[gstr]]$Results)[2]

  df <- data.frame()

  colheading <- c()
  idx <- 1
  if (sheadings[idx] == "Group") {
    df <- data.frame(group = statements[,idx])
    colheading <- append(colheading, "Group")
    idx <- idx + 1
  } else if (sheadings[idx] == "Class") {
    df <- data.frame(classlabel = statements[,idx])
    colheading <- append(colheading, "Class")
    idx <- idx + 1
  }
  if (sheadings[idx] == "Level") {
    if (idx > 1) {
      df <- cbind(df, level = statements[,idx])
    } else {
      df <- data.frame(level = statements[,idx])
    }
    colheading <- append(colheading, "Level")
    idx <- idx + 1
  }
  sectind <- pmatch("Section", sheadings, nomatch=0)

  variables <- c()
  modelstmts <- c()

  for (i in c(1:num_statements)) {
    variables <- append(variables, statements[i,sectind])
    modelstmts <- append(modelstmts, statements[i,sectind+1])
  }

  if (idx > 1) {
    df <- cbind(df, variables)
  } else {
    df <- data.frame(variable = variables)
  }
  colheading <- append(colheading, "Variable")

  df <- cbind(df, modelstmts)
  colheading <- append(colheading, "Category")

  for (i in c(1:num_values)) {
    df <- cbind(df, as.numeric(gh5[[gstr]]$Results[,i]))
    colheading <- append(colheading, rheadings[i])
  }
  colnames(df) <- colheading
  df
}

##########################################################################
#' Mplus Print Confidence Intervals in Probability Scale
#'
#' Show the results in probability scale
#'
#' @param file the quoted name of an existing OH5 file
#' @param cred logical TRUE for Credibility Intervals
#           FALSE for Confidence Intervals (default)
#' @param invariance 0 (not invariance model, default)
#               - 1 (configural invariance)
#               - 2 (metric invariance)
#               - 3 (scalar invariance)
#'
#' @return The confidence intervals in probability scale are printed to the screen
#' @export
#'
#' @examples
#' \dontrun{
#' mplus.print.confidence.intervals.in.probability.scale('ex.oh5')
#' }
#'
mplus.print.confidence.intervals.in.probability.scale <- function(file,cred=FALSE,invariance=0) {
  if (!(file.exists(file))) {
    cstr <- paste("- file does not exist:",file,"\n")
    cat(cstr)
  }

  if (cred) {
    cistr <- c('Credibility')
  } else {
    cistr <- c('Confidence')
  }
  if (invariance == 0) {
    gstr <- sprintf('%s Intervals in Probability Scale', cistr)
  } else if (invariance == 1) {
    gstr <- sprintf('%s Intervals in Probability Scale (configural)', cistr)
  } else if (invariance == 2) {
    gstr <- sprintf('%s Intervals in Probability Scale (metric)', cistr)
  } else if (invariance == 3) {
    gstr <- sprintf('%s Intervals in Probability Scale (scalar)', cistr)
  }

  df <- mplus.print.general.results(file,gstr)
  df
}

##########################################################################
#' Mplus Print Latent Class Indicator Odds Ratios
#'
#' Show the latent class indicator odds ratios
#'
#' @param file the quoted name of an existing OH5 file
#' @param invariance  0 (not invariance model, default)
#               - 1 (configural invariance)
#               - 2 (metric invariance)
#               - 3 (scalar invariance)
#'
#' @return The latent class indicator odds ratios is printed to the screen
#' @export
#'
#' @examples
#' \dontrun{
#' mplus.print.latent.class.indicator.odds.ratios('ex.oh5')
#' }
#'
mplus.print.latent.class.indicator.odds.ratios <- function(file,invariance=0) {
  if (!(file.exists(file))) {
    cstr <- paste("- file does not exist:",file,"\n")
    cat(cstr)
  }

  #H5close()
  gh5 <- rhdf5::h5dump(file, load=TRUE)

  if (invariance == 0) {
    gstr <- c('Latent Class Indicator Odds Ratios for the Latent Classes')
  } else if (invariance == 1) {
    gstr <- c('Latent Class Indicator Odds Ratios for the Latent Classes (configural)')
  } else if (invariance == 2) {
    gstr <- c('Latent Class Indicator Odds Ratios for the Latent Classes (metric)')
  } else if (invariance == 3) {
    gstr <- c('Latent Class Indicator Odds Ratios for the Latent Classes (scalar)')
  }

  if (!(gstr %in% names(gh5)) ) {
    cstr <- sprintf("The output does not have any %s.\n", gstr)
    stop(cstr)
  }

  cstr <- paste(gstr,"\n")
  cat(cstr)

  results_str <- sprintf("%s/Results", gstr)
  statements_str <- sprintf("%s/Statements", gstr)

  rheadings <- mplus.get.dataset.attribute(file,results_str,"Headings")
  rheadings <- gsub("(^\\s+|\\s+$)", "", rheadings, perl=TRUE)

  sheadings <- mplus.get.dataset.attribute(file,statements_str,"Headings")
  sheadings <- gsub("(^\\s+|\\s+$)", "", sheadings, perl=TRUE)

  statements <- gh5[[gstr]]$Statements
  statements <- gsub("(^\\s+|\\s+$)", "", statements, perl=TRUE)

  num_statements <- dim(gh5[[gstr]]$Results)[1]
  num_values <- dim(gh5[[gstr]]$Results)[2]

  df <- data.frame()

  colheading <- c()
  idx <- 1
  if (sheadings[idx] == "Group") {
    df <- data.frame(group = statements[,idx])
    colheading <- append(colheading, "Group")
    idx <- idx + 1
  } else if (sheadings[idx] == "Class") {
    df <- data.frame(classlabel = statements[,idx])
    colheading <- append(colheading, "Class")
    idx <- idx + 1
  }
  if (sheadings[idx] == "Level") {
    if (idx > 1) {
      df <- cbind(df, level = statements[,idx])
    } else {
      df <- data.frame(level = statements[,idx])
    }
    colheading <- append(colheading, "Level")
    idx <- idx + 1
  }
  sectind <- pmatch("Section", sheadings, nomatch=0)

  variables <- c()
  modelstmts <- c()

  for (i in c(1:num_statements)) {
    variables <- append(variables, statements[i,sectind])
    modelstmts <- append(modelstmts, statements[i,sectind+1])
  }

  if (idx > 1) {
    df <- cbind(df, variables)
  } else {
    df <- data.frame(variable = variables)
  }
  colheading <- append(colheading, "Variable")

  df <- cbind(df, modelstmts)
  colheading <- append(colheading, "Category")

  for (i in c(1:num_values)) {
    df <- cbind(df, as.numeric(gh5[[gstr]]$Results[,i]))
    colheading <- append(colheading, rheadings[i])
  }
  colnames(df) <- colheading
  df
}

##########################################################################
#' Mplus Print Logistic Regression Odds Ratio Results
#'
#' Show the model results
#'
#' @param file the quoted name of an existing OH5 file
#' @param getdf logical
#'
#' @return The logistic regression odds ratio results is printed to the screen
#' @export
#'
#' @examples
#' \dontrun{
#' mplus.print.logistic.regressions.odds.ratio.results('ex.oh5')
#' }
#'
mplus.print.logistic.regressions.odds.ratio.results <- function(file,getdf=FALSE) {
  if (!(file.exists(file))) {
    cstr <- paste("- file does not exist:",file,"\n")
    cat(cstr)
  }

  #H5close()
  gh5 <- rhdf5::h5dump(file, load=TRUE)

  gstr <- c('Logistic Regression Odds Ratio Results')

  if (!(gstr %in% names(gh5)) ) {
    cstr <- sprintf("The output does not have any %s.\n", gstr)
    stop(cstr)
  }

  cstr <- paste(gstr,"\n")
  cat(cstr)

  rheadings <- mplus.get.dataset.attribute(file,"Logistic Regression Odds Ratio Results/Results", "Headings")
  rheadings <- gsub("(^\\s+|\\s+$)", "", rheadings, perl=TRUE)

  sheadings <- mplus.get.dataset.attribute(file,"Logistic Regression Odds Ratio Results/Statements", "Headings")
  sheadings <- gsub("(^\\s+|\\s+$)", "", sheadings, perl=TRUE)

  statements <- gh5[[gstr]]$Statements
  statements <- gsub("(^\\s+|\\s+$)", "", statements, perl=TRUE)

  num_statements <- dim(gh5[[gstr]]$Results)[1]
  num_values <- dim(gh5[[gstr]]$Results)[2]

  df <- data.frame()

  colheading <- c()
  idx <- 1
  if (sheadings[idx] == "Group") {
    df <- data.frame(group = statements[,idx])
    colheading <- append(colheading, "Group")
    idx <- idx + 1
  } else if (sheadings[idx] == "Class") {
    df <- data.frame(classlabel = statements[,idx])
    colheading <- append(colheading, "Class")
    idx <- idx + 1
  }
  if (sheadings[idx] == "Level") {
    if (idx > 1) {
      df <- cbind(df, level = statements[,idx])
    } else {
      df <- data.frame(level = statements[,idx])
    }
    colheading <- append(colheading, "Level")
    idx <- idx + 1
  }
  sectind <- pmatch("Section", sheadings, nomatch=0)
  modelstmts <- c()

  for (i in c(1:num_statements)) {
    if (statements[i,sectind] == "Variances" || statements[i,sectind] == "Residual Variances") {
      newstmt <- sprintf("%s", statements[i,sectind+3])
    } else if (statements[i,sectind] == "Means") {
      newstmt <- sprintf("[%s]", statements[i,sectind+3])
    } else if (statements[i,sectind] == "Scales") {
      newstmt <- sprintf("{%s}", statements[i,sectind+3])
    } else {
      newstmt <- sprintf("%s %s %s", statements[i,sectind+1], statements[i,sectind+2], statements[i,sectind+3])
    }
    modelstmts <- append(modelstmts, newstmt)
  }

  if (idx > 1) {
    df <- cbind(df, modelstmts)
  } else {
    df <- data.frame(statement = modelstmts)
  }
  colheading <- append(colheading, "Statement")

  for (i in c(1:num_values)) {
    df <- cbind(df, as.numeric(gh5[[gstr]]$Results[,i]))
    colheading <- append(colheading, rheadings[i])
  }
  colnames(df) <- colheading
  df
}


##########################################################################
#' Mplus Print Brant Wald Test
#'
#' Show the Brant Wald Test for Proportional Odds
#'
#' @param file the quoted name of an existing OH5 file
#'
#' @return The Brant Wald Test for Proportional Odds is printed to the screen.
#' @export
#'
#' @examples
#' \dontrun{
#' mplus.print.brant.wald.test('ex.oh5')
#' }
#'
mplus.print.brant.wald.test <- function(file) {
  if (!(file.exists(file))) {
    cstr <- paste("- file does not exist:",file,"\n")
    cat(cstr)
  }

  #H5close()
  gh5 <- rhdf5::h5dump(file, load=TRUE)

  gstr <- c('Brant Wald Test For Proportional Odds')

  if (!(gstr %in% names(gh5)) ) {
    cstr <- sprintf("The output does not have any %s.\n", gstr)
    stop(cstr)
  }

  cstr <- paste(gstr,"\n")
  cat(cstr)

  rheadings <- mplus.get.dataset.attribute(file,"Brant Wald Test For Proportional Odds/Results", "Headings")
  rheadings <- gsub("(^\\s+|\\s+$)", "", rheadings, perl=TRUE)

  sheadings <- mplus.get.dataset.attribute(file,"Brant Wald Test For Proportional Odds/Statements", "Headings")
  sheadings <- gsub("(^\\s+|\\s+$)", "", sheadings, perl=TRUE)

  statements <- gh5[[gstr]]$Statements
  statements <- gsub("(^\\s+|\\s+$)", "", statements, perl=TRUE)

  num_statements <- dim(gh5[[gstr]]$Results)[1]
  num_values <- dim(gh5[[gstr]]$Results)[2]

  df <- data.frame()

  colheading <- c()
  idx <- 1

  sectind <- pmatch("Variable", sheadings, nomatch=0)
  modelstmts <- c()
  stmttypes <- c()

  for (i in c(1:num_statements)) {
    stmttypes <- append(stmttypes, statements[i,sectind])
    modelstmts <- append(modelstmts, statements[i,sectind+1])
  }

  df <- data.frame(statement = stmttypes)
  df <- cbind(df, modelstmts)

  colheading <- append(colheading, "Variable")
  colheading <- append(colheading, "Test/Variable")

  for (i in c(1:num_values)) {
    df <- cbind(df, as.numeric(gh5[[gstr]]$Results[,i]))
    colheading <- append(colheading, rheadings[i])
  }
  colnames(df) <- colheading
  df
}

