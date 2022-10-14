## GENERICS---------------------------------------------------------------------

#' @export
.get_file_name <- function(filepath) {
  
  # If is null, return blank string
  if (is.null(filepath)) {return("")}
  
  # Make filepath a string
  filepath <- as.character(filepath)
  
  # Get the end of the name, remove extension, and then remove non-alphanumeric
  cleanname <- strsplit(filepath, "/") %>% 
    unlist() %>% 
    tail(1) %>%
    gsub(pattern = ".csv|.tsv|.txt", replacement = "") %>%
    .scrub_clean()
  
  # Return cleaned name
  return(cleanname)
  
}

## MAIN FUNCTION----------------------------------------------------------------

#' Reads the MAP object (project or midpoint) and generates appropriate tags
#' 
#' @param object A MAP object, either a project or midpoint. Required.
#' 
#' @examples 
#' \dontrun{
#' 
#' library(pmartRdata)
#' 
#' # Make a pmart project object
#' project_pmart <- project_pmart(projectname = "My Peptide Data",
#'              datatype = "Peptide-level Label Free",
#'              edata = pmartRdata::pep_edata,
#'              fdata = pmartRdata::pep_fdata,
#'              emeta = pmartRdata::pep_emeta,
#'              edata_filename = "pep_edata",
#'              fdata_filename = "pep_fdata",
#'              emeta_filename = "pep_emeta")
#' pull_tags_from_object(project_pmart)
#'              
#'}
#' 
#' @export
pull_tags_from_object <- function(object) {
  
  # Check object class
  if (class(object) %in% c("project edata", "project omic", "project multiomics", "midpoint pmart", "midpoint ipmart") == FALSE) {
    stop("object class not known. Must be a project or midpoint.")
  } 
  
  # Return the tags for a project pmart object
  if (class(object) %in% c("project edata", "project omic")) {
    return(
      list(
        "ObjectType" = class(object), 
        "DataType" = object$Project$DataType,
        "ProjectName" = object$Project$Name, 
        "e_data_filename" = .get_file_name(object$Data$e_data_filename),
        "f_data_filename" = .get_file_name(object$Data$f_data_filename),
        "e_meta_filename" = .get_file_name(object$Data$e_meta_filename)
      )
    )
  }
  
  # Return the tags for a project ipmart object
  if (class(object) == "project multiomics") {
    return(
      list(
        "ObjectType" = class(object),
        "DataType" = object$Project$DataType,
        "ProjectName" = object$Project$Name,
        "Objects" = object$Objects %>% names() %>% paste(collapse = ", ")
      )
    )
  }
  
  # Return the tags for a midpoint pmart object
  if (class(object) == "midpoint pmart") {
    return(
      list(
        "ObjectType" = class(object),
        "DataType" = object$Tracking$`Original Files`$Project$DataType,
        "ProjectName" = object$Tracking$Name,
        "Tab" = object$Tracking$Tab,
        "SaveTime" = object$Tracking$Timestamp %>% as.character()
      )
    )
  }
  
  # Return the tags for a midpoint ipmart object
  if (class(object) == "midpoint ipmart") {
    return(
      list(
        "ObjectType" = class(object),
        "DataType" = "multi-omics",
        "ProjectName" = object$Tracking$Name,
        "Tab" = object$Tracking$Tab,
        "SaveTime" = object$Tracking$Timestamp %>% as.character(),
        "Objects" = names(object$`Data Objects`) %>% paste(collapse = ", ")
      )
    )
  }
  
}