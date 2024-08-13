
######################################################################################################
# Supporting functions (from mplus.R)
######################################################################################################


##########################################################################
#' Mplus get group attribute
#'
#' supporting function for getting attribute
#'
#' @param file the quoted name of an existing GH5 file
#' @param groupstr the name of the group for the attribute
#' @param attrstr the name of the attribute
#'
#'
#' @examples
#' \dontrun{
#' mplus.get.group.attribute('ex8.1.gh5','individual_data','var_names')
#' }
#' @noRd
mplus.get.group.attribute <- function(file, groupstr, attrstr) {
  if ( !(file.exists(file))) {
    cstr <- paste("- file does not exist:",file,"\n")
    stop(cstr)
  }

  gh5 <- rhdf5::h5dump(file, load=TRUE)

  fid <- rhdf5::H5Fopen(file)
  gid <- rhdf5::H5Gopen(fid, groupstr)
  atid <- rhdf5::H5Aopen(gid, attrstr)

  attr <- rhdf5::H5Aread(atid)

  rhdf5::H5Aclose(atid)
  rhdf5::H5Gclose(gid)
  rhdf5::H5Fclose(fid)

  attr <- gsub("(^\\s+|\\s+$)", "", attr, perl=TRUE)

  return(attr)
}

##########################################################################
#' Mplus check group attribute
#'
#' supporting function for checking attribute
#'
#' @param file the quoted name of an existing GH5 file
#' @param groupstr the name of the group for the attribute
#' @param attrstr the name of the attribute
#'
#'
#' @examples
#' \dontrun{
#' mplus.check.group.attribute('ex8.1.gh5','individual_data','var_names')
#' }
#' @noRd
mplus.check.group.attribute <- function(file, groupstr, attrstr) {
  if ( !(file.exists(file))) {
    cstr <- paste("- file does not exist:",file,"\n")
    stop(cstr)
  }

  fid <- rhdf5::H5Fopen(file)
  gid <- rhdf5::H5Gopen(fid, groupstr)
  atid <- rhdf5::H5Aexists(gid, attrstr)

  rhdf5::H5Gclose(gid)
  rhdf5::H5Fclose(fid)

  return(atid)
}

##########################################################################
#' Mplus get group dataset
#'
#' supporting function for getting dataset
#'
#' @param file the quoted name of an existing GH5 file
#' @param groupstr the name of the group for the attribute
#' @param datastr the name of the attribute
#'
#'
#' @examples
#' \dontrun{
#' mplus.get.group.dataset('ex8.1.gh5','bayesian_data','statements')
#' }
#' @noRd
mplus.get.group.dataset <- function(file, groupstr, datastr) {
  if ( !(file.exists(file))) {
    cstr <- paste("- file does not exist:",file,"\n")
    stop(cstr)
  }

  gh5 <- rhdf5::h5dump(file, load=TRUE)

  fid <- rhdf5::H5Fopen(file)
  gid <- rhdf5::H5Gopen(fid, groupstr)
  dtid <- rhdf5::H5Dopen(gid, datastr)

  data <- rhdf5::H5Dread(dtid)

  rhdf5::H5Dclose(dtid)
  rhdf5::H5Gclose(gid)
  rhdf5::H5Fclose(fid)

  return(data)
}

# estimate_mode <- function(x) {
#   d <- density(x)
#   d$x[which.max(d$y)]
# }

##########################################################################
#' Mplus Get Dataset Attribute
#'
#' Supporting function for getting attribute
#'
#' @param file the quoted name of an existing GH5 file
#' @param datastr the name of the group for the attribute
#' @param attrstr the name of the attribute
#'
#'
#' @examples
#' \dontrun{
#' mplus.get.dataset.attribute('ex8.1.gh5','individual_data','var_names')
#' }
#' @noRd
mplus.get.dataset.attribute <- function(file, datastr, attrstr) {
  if ( !(file.exists(file))) {
    cstr <- paste("- file does not exist:",file,"\n")
    stop(cstr)
  }

  gh5 <- rhdf5::h5dump(file, load=TRUE)

  fid <- rhdf5::H5Fopen(file)
  did <- rhdf5::H5Dopen(fid, datastr)
  atid <- rhdf5::H5Aopen(did, attrstr)

  attr <- rhdf5::H5Aread(atid)

  rhdf5::H5Aclose(atid)
  rhdf5::H5Dclose(did)
  rhdf5::H5Fclose(fid)

  return(attr)
}

