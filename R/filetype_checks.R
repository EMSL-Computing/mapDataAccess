#' Test if a file is an edata file
#' 
#' @param edata Must be a dataframe or datatable. Required. 
#' 
#' @return A boolean where TRUE means the file is an acceptable edata file. 
#' @examples 
#' \dontrun{
#' 
#' library(pmartRdata)
#' is_edata(pmartRdata::lipid_edata)
#' 
#' }
#' @export
is_edata <- function(edata) {
  
  # If edata is NULL, return FALSE
  if (is.null(edata) || is.data.frame(edata) == FALSE) {
    message("Expression Data must be a data.frame or data.table")
    return(FALSE)
  }
  
  # edata must have at least 2 samples 
  if (ncol(edata) < 3) {
    message("Expression Data must have at least 3 columns, a 'descriptor' column and at least 2 samples.")
    return(FALSE)
  }
  
  ## All columns with the exception of one column MUST be numeric
  
  # Get counts of the number of numeric columns
  LogicCounts <- lapply(1:ncol(edata), function(col) {is.numeric(edata[,col])}) %>% 
    unlist() %>%
    table() 
  
  # If more than one column is not numeric, return error 
  if ("FALSE" %in% names(LogicCounts) && LogicCounts[["FALSE"]] > 1) {
    message("All columns in Expression Data must be numeric, with exception of a 'descriptor' column.")
    return(FALSE)  
  }
  
  # Otherwise, return True
  return(TRUE)
  
}

#' Test if a file is an f_data file. Its companion edata file must be provided. 
#' 
#' @param edata Must be a dataframe or datatable. Required.
#' @param fdata Must be a dataframe or datatable. Required. 
#' 
#' @return A boolean where TRUE means the file is an acceptable f_data file. 
#' @examples 
#' \dontrun{
#' 
#' library(pmartRdata)
#' is_fdata(edata = pmartRdata::metab_edata, fdata = pmartRdata::metab_fdata)
#' 
#' }
#' @export
is_fdata <- function(edata, fdata) {
  
  # Make sure neither file is NULL 
  if (is.null(edata) || is.data.frame(edata) == FALSE) {
    message("Expression Data must be a data.frame or data.table.")
    return(FALSE)
  }
  if (is.null(fdata) || is.data.frame(fdata) == FALSE) {
    message("Sample Information must be a data.frame or data.table.")
    return(FALSE)
  }
  
  # The number of rows in fdata must be the number of columns in edata minus 1
  if (nrow(fdata) != (ncol(edata) - 1)) {
    message("The number of rows in Sample Information must match the number of columns in Expression Data minus 1.")
    return(FALSE)
  }
  
  # Make sure the column names of edata[-1] match the column values of a f_data
  ColumnNameCheck <- lapply(1:ncol(fdata), function(col) {
    NamesToCheck <- colnames(edata)
    fdata[,col] %in% NamesToCheck %>% all()
  }) %>% unlist() %>% any()
  if (ColumnNameCheck == FALSE) {
    message("One column of Sample Information must match the sample names at the header (top) of the Expression Data edata.")
    return(FALSE)
  }
  
  # Otherwise, return true
  return(TRUE)
  
} 

#' Test if a file is an e_meta file. Its companion edata file must be provided.
#' 
#' @param edata Must be a dataframe or datatable. Required.
#' @param emeta Must be a dataframe or datatable. Required.
#' 
#' @return A boolean where TRUE means the file is an acceptable e_meta file. 
#' @examples 
#' \dontrun{
#' 
#' library(pmartRdata)
#' is_emeta(edata = pmartRdata::pep_edata, emeta = pmartRdata::pep_emeta)
#' 
#' }
#' @export
is_emeta <- function(edata, emeta) {
  
  # Make sure neither file is NULL 
  if (is.null(edata) || is.data.frame(edata) == FALSE) {
    message("Expression data must be a data.frame or data.table.")
    return(FALSE)
  }
  if (is.null(emeta) || is.data.frame(emeta) == FALSE) {
    message("Biomolecule Information must be a data.frame or data.table.")
    return(FALSE)
  }
  
  # The number of rows must match
  if (nrow(emeta) < nrow(edata)) {
    message("The number of rows in Biomolecule Information must be equal to or greater than the number of rows in Expression Data.")
    return(FALSE)
  }
  
  # Identifiers in edata must exist in emeta 
  IdentifierCheck <- lapply(1:ncol(emeta), function(col) {
    all(edata[,1] %in% emeta[,col])
  }) %>% unlist() %>% any()
  if (IdentifierCheck == FALSE) {
    message("All identifiers in the Expression Data must exist in the Biomolecule Information file.")
    return(FALSE)
  }
  
  # Otherwise, return True
  return(TRUE)
  
}

#' Test if a file is an f_meta file
#' 
#' @param edata_files A list of edata files. Required. 
#' @param fmeta Must be a data.frame or data.table. Required.
#' 
#' @return A boolean where TRUE means the file is an acceptable f_meta file.
#' @examples \dontrun{
#' 
#' library(pmartRdata)
#' edata_files <- list(pmartRdata::pro_edata, pmartRdata::lipid_edata, pmartRdata::metab_edata)
#' fmeta <- data.frame(
#'    "Proteins" = c(paste0("Mock", 1:3), paste0("Infection", c(1:7)), NA,  "Infection9"),
#'    "Lipids" = c(paste0("Mock", 1:3), paste0("Infection", c(1:4)), NA, paste0("Infection", c(6:9))),
#'    "Metabolites" = c(paste0("Mock", 1:3), paste0("Infection", c(1:9))),
#'    "Condition" = c(rep("A", 3), rep("B", 9))
#' )
#' 
#' is_fmeta(edata_files = edata_files, fmeta = fmeta)
#' 
#' }
#' @export
is_fmeta <- function(edata_files, fmeta) {
  
  # Check that edata_files list is a list of at least length 2
  if (is.list(edata_files) == FALSE || length(edata_files) < 2) {
    message("There must be at least two Expression Data (edata_files).")
    return(FALSE)
  }
  
  # Iterate through each edata and confirm they are all edata
  if (lapply(edata_files, is_edata) %>% unlist() %>% all() == FALSE) {
    message("At least one file in the Expression Data files (edata_files) is not an Expression Data file. See ?is_edata for more details.")
    return(FALSE)
  }
  
  # Assert fmeta is not null
  if (is.null(fmeta) || is.data.frame(fmeta) == FALSE) {
    message("Multi-omic Sample Information must be a data.frame or data.table.")
    return(FALSE)
  }
  
  # 1. The number of rows in f_meta must be as long as the longest number of columns in e_data minus 1.
  allLengths <- lapply(edata_files, ncol) %>% unlist()
  if (nrow(fmeta) >= (max(allLengths) - 1)) {
    message("The number of rows in the Multi-Omic Sample Information file must be equal to the number of columns in the Expression Data files minus 1.")
    return(FALSE)
  }
  
  # 2. All but one of the edata colnames must be in the fmeta file
  EDataColsInFmetaRows <- lapply(edata_files, function(edata) {
    LogicCounts <- (colnames(edata) %in% (fmeta %>% unlist())) %>% table()
    if ("FALSE" %in% names(LogicCounts) && LogicCounts[["FALSE"]] > 1) {
      return(FALSE)
    } else {return(TRUE)}
  }) %>% unlist()
  
  # Check that each edata colnames passed
  if (all(EDataColsInFmetaRows) == FALSE) {
    message("All but the 'descriptor' column in Expression Data must be in the Multi-Omic Sample Information file.")
    return(FALSE)
  }
  
  # Otherwise, return true
  return(TRUE)
  
}
