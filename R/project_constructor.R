## GENERICS---------------------------------------------------------------------

#' Check object multi-omics data type - specific to MAP
#' @export
.is_datatype <- function(datatype) {
  
  # Make datatype a string
  datatype <- as.character(datatype)
  
  # Data Type must be of the following classes
  return(datatype %in% c("Peptide-level Label Free","Peptide-level Isobaric",
    "Protein-level Label Free", "Protein-level Isobaric", "Lipidomics-Negative",
    "Lipidomics-Positive", "Metabolomics-GC/LC-MS", "Metabolomics-NMR", "Transcriptomics")
  )
  
}

#' Remove all non-alphanumeric characters
#' @export
.scrub_clean <- function(string) {
  return(gsub("[^[:alnum:]]|[[:space:]]", "", as.character(string)))
}


## PROJECT OBJECT CONSTRUCTORS--------------------------------------------------

#' Generate a simple e_data only project that can only be opened in MODE
#' 
#' @description Construct a project edata object where only edata is required.
#' 
#' @param projectname Any string to name the project. All spaces and non-alphanumeric
#'    characters will be removed to prevent issues with the visualizations. Required.
#' @param datatype Must be of the acceptable MAP omic class. See .is_datatype for list. Required. 
#' @param edata Must be a dataframe or datatable. Required. 
#' @param edata_name The path or name for the edata file. Optional. 
#' 
#' @return A project edata object
#' @examples 
#' \dontrun{
#' 
#' library(pmartRdata)
#' project_edata(projectname = "LipidToMode",
#'              datatype = "Lipidomics-Positive",
#'              edata = pmartRdata::lipid_edata_pos,
#'              edata_filename = "lip_edata")
#' 
#' }
#' @export
project_edata <- function(projectname, datatype, edata, edata_filename = NULL) {
  
  # Check data type
  if (.is_datatype(datatype) == FALSE) {
    stop("Data Type is not of the appropriate class.")
  }
  
  # Check edata 
  if (is_edata(edata) == FALSE) {
    stop("edata was not recognized as a proper edata file.")
  }
  
  # Construct project object
  ProjectObject <- list(
    "Project" = list(
      "Name" = .scrub_clean(projectname),
      "DataType" = datatype 
    ),
    "Data" = list(
      "e_data" = edata,
      "e_data_filename" = edata_filename
    )
  )
  
  # Assign the class attribute
  class(ProjectObject) <- "project edata"
  
  # Return
  return(ProjectObject)
  
}

#' Generate a project object to pass data from MAP to PMART/MODE. 
#' 
#' @description Constructs a omic project object where edata and fdata are required. 
#'     
#' @param projectname Any string to name the project. All spaces and non-alphanumeric
#'    characters will be removed to prevent issues with the visualizations. Required.
#' @param datatype Must be of the acceptable MAP omic class. See ?.is_datatype for list. Required. 
#' @param edata Must be a dataframe or datatable. Required. 
#' @param fdata Must be a dataframe or datatable. Required.
#' @param emeta Must be a dataframe or datatable. Optional. 
#' @param edata_name The path or name for the edata file. Optional. 
#' @param fdata_name The path or name for the fdata file. Optional. 
#' @param emeta_name The path or name for the emeta file. Optional. 
#' 
#' @return An omics project object
#' @examples 
#' \dontrun{
#' 
#' library(pmartRdata)
#' project_omic(projectname = "My Peptide Data",
#'              datatype = "Peptide-level Label Free",
#'              edata = pmartRdata::pep_edata,
#'              fdata = pmartRdata::pep_fdata,
#'              emeta = pmartRdata::pep_emeta,
#'              edata_filename = "pep_edata",
#'              fdata_filename = "pep_fdata",
#'              emeta_filename = "pep_emeta")
#' 
#' }
#' @export
project_omic <- function(projectname, datatype, edata, fdata, emeta = NULL,
                          edata_filename = NULL, fdata_filename = NULL, emeta_filename = NULL) {
  
  # Check data type
  if (.is_datatype(datatype) == FALSE) {
    stop("Data Type is not of the appropriate class.")
  }
  
  # Check edata 
  if (is_edata(edata) == FALSE) {
    stop("edata was not recognized as a proper edata file.")
  }
  
  # Check fdata 
  if (is_fdata(edata, fdata) == FALSE) {
    stop("fdata was not recognized as a proper fdata file.")
  }
  
  # Check emeta (optional)
  if (is.null(emeta) == FALSE) {
    if (is_emeta(edata, emeta) == FALSE) {
      stop("emeta was not recognized as a proper emeta file.")
    }
  }
  
  # Construct project object
  ProjectObject <- list(
    "Project" = list(
      "Name" = .scrub_clean(projectname),
      "DataType" = datatype 
    ),
    "Data" = list(
      "e_data" = edata,
      "e_data_filename" = edata_filename,
      "f_data" = fdata,
      "f_data_filename" = fdata_filename,
      "e_meta" = emeta,
      "e_meta_filename" = emeta_filename
    )
  )
  
  # Assign the class attribute
  class(ProjectObject) <- "project omic"
  
  # Return
  return(ProjectObject)
  
}

#' Generate a multi-omics project object to pass data from MAP to IPMART/MODE.
#' 
#' @description Constructs a multiomics project object from omic projects or pmart midpoints. Required. 
#' 
#' @param projectname Any string to name the project. All spaces and non-alphanumeric
#'    characters will be removed to prevent issues with the visualizations. Required.
#' @param objects List of all omic projects or all pmart midpoints at the same tab (normalization or statistics).
#'    Mixing of projects and midpoints is not allowed. Must contain 2-5 objects. There can be
#'    no more than 2 metabolomics (1 of: NMR or GC/LC-MS), no more than 2 lipidomics datasets, 
#'    and no more than 1 proteomics (peptide or protein) dataset. Required. 
#' @param fmeta Must be a dataframe or data table. If not provided, users can built it in 
#'    ipmart. Default is NULL. 
#'
#' @return A multiomics project object
#' @examples 
#' \dontrun{
#' 
#' library(pmartRdata)
#' 
#' # Generate midpoint with the examples in midpoint_pmart and save result as "midpoint"
#' 
#' # Make a metabolomics GC/LC MS project
#' metab_project <- project_omic(projectname = "My Metab Data",
#'                                datatype = "Metabolomics-GC/LC-MS",
#'                                edata = pmartRdata::metab_edata,
#'                                fdata = pmartRdata::metab_fdata,
#'                                edata_filename = "metab_edata",
#'                                fdata_filename = "metab_fdata")
#'                           
#'                                                     
#' # Make a lipidomics project
#' lipid_project <- project_omic(projectname = "My Lipid Data",
#'                                datatype = "Lipidomics-Positive",
#'                                edata = pmartRdata::lipid_edata_pos,
#'                                fdata = pmartRdata::lipid_fdata_pos,
#'                                edata_filename = "lipid_edata_pos",
#'                                fdata_filename = "lipid_fdata_pos")
#'                                
#' # Make a proteomics project
#' protein_project <- project_omic(projectname = "My Protein Data",
#'                                 datatype = "Protein-level Label Free",
#'                                 edata = pmartRdata::pro_edata,
#'                                 fdata = pmartRdata::pro_fdata,
#'                                 edata_filename = "pro_edata",
#'                                 fdata_filename = "pro_fdata")
#'                                 
#' # Create an f_meta file
#' fmeta <- data.frame(
#'   "Proteins" = c(paste0("Mock", 1:3), paste0("Infection", c(1:4,6,7,9))),
#'   "Lipids" = c(paste0("Mock", 1:3), paste0("Infection", c(1:4,6,7,9))),
#'   "Metabolites" = c(paste0("Mock", 1:3), paste0("Infection", c(1:4,6,7,9)))
#' )
#'                           
#' # Finally, make the ipmart midpoint object
#' project_multiomics(projectname = "projects", objects = list(metab_project, lipid_project, protein_project))
#' 
#' # Or use the pmart_midpoint examples 
#' project_multiomics(projectname = "midpoints", objects = list(pep_midpoint, lipid_midpoint))                          
#' 
#' }
#' @export
project_multiomics <- function(projectname, objects, fmeta = NULL) {
  
  # Check the length of the object. It must be between 2 and 5.
  if (length(objects) < 2 | length(objects) > 5) {
    stop("objects must be at least length 2, and no more than 5.")
  }
  
  # Iterate through class information
  get_classes <- lapply(objects, class) %>% unlist()
  
  # Make sure they are all pmart projects or midpoints
  ProjectData <- grepl("project omic", get_classes) %>% all()
  MidpointData <- grepl("midpoint pmart", get_classes) %>% all()
  
  # If both are FALSE, trigger a warning
  if (ProjectData == FALSE & MidpointData == FALSE) {
    stop("objects must be either all project omic objects or midpoint pmart objects.")
  }
  
  # Check the f_meta object - will be added later 
  #if (is.null(fmeta) == FALSE) {
   
   # Pull edata objects
  #  edata_list <- lapply(objects, function(object) {
  #    if (class(object) == "project pmart") {object$Data$e_data} else
  #    if (class(object) == "midpoint pmart") {object$`Data Objects`$OmicsData$e_data}
  #  })
   
   # Run f_meta check
  #  if (is_fmeta(edata_list, fmeta) == FALSE) {
  #    stop("Multi-omics Sample Information (f_meta) file is not valid. No worries, you can create one in iPMART.")
  #  }
   
  #}
  
  # Get project types 
  ProjectTypes <- lapply(objects, function(object) {
    
    # Extract the object type
    if (class(object) == "midpoint pmart") {
      DataType <- object$Tracking$`Original Files`$Project$DataType
    } else if (class(object) == "project omic") {
      DataType <- object$Project$DataType
    }
    
    # Simplify data type to metabolomics/lipidomics/proteomics
    if (grepl("Peptide|Protein", DataType)) {return("Proteomics")} else
      if (grepl("Lipidomics", DataType)) {return("Lipidomics")} else {return(DataType)}
    
  }) %>% unlist() %>% table() %>% c()
  
  # Ensure there is at most 1 peptide/proteomics, 2 lipidomics, 1 metabolommics GC/LC-MS, 1 metabolomics NMR
  if ("Proteomics" %in% names(ProjectTypes) && ProjectTypes[["Proteomics"]] > 1) {
    stop("ipmart cannot accept more than 1 proteomics (peptide or protein) dataset.")
  }
  if ("Lipidomics" %in% names(ProjectTypes) && ProjectTypes[["Lipidomics"]] > 2) {
    stop("ipmart cannot accept more than 2 lipidomics datasets.")
  }
  if ("Metabolomics-GC/LC-MS" %in% names(ProjectTypes) && ProjectTypes[["Metabolomics-GC/LC-MS"]] > 1) {
    stop("ipmart cannot accept more than 1 metabolomics GC/LC-MS dataset.")
  }
  if ("Metabolomics-NMR" %in% names(ProjectTypes) && ProjectTypes[["Metabolomics-NMR"]] > 1) {
    stop("ipmart cannot accept more than 1 metabolomics NMR dataset.")
  }
  
  # Load project objects
  if (ProjectData) {
    
    # Name the objects by their data types
    names(objects) <- lapply(objects, function(x) {x$Project$DataType}) %>% unlist()
    
    # All of the data is unprocessed. Generate the project object. 
    ProjectObject <- list(
      "Project" = list(
        "Name" = .scrub_clean(projectname),
        "DataType" = "project omic"
      ),
      "Objects" = objects,
      "fmeta" = fmeta
    )
    
  } else if (MidpointData) {
    
    # Check that all the midpoints are from the same tab, with the exception of peptide data
    SameTab <- (lapply(objects, function(x) {x$Tracking$Tab}) %>% 
      unique() %>% length()) == 1    
    
    # If they're not all from the same tab, export warning
    if (SameTab == FALSE) {
      stop(paste0("pmart midpoints included in 'objects' must be exported from the same tab. ",
        "The exported tabs are: ", paste(lapply(objects, function(x) {x$Tracking$Tab}) %>% unique()),
        collapse = ", "))
    } else {
      
      # Name the objects by their data types
      names(objects) <- lapply(objects, function(x) {x$Tracking$`Original Files`$Project$DataType}) %>% unlist()
      
      # Build project object
      ProjectObject <- list(
        "Project" = list(
          "Name" = .scrub_clean(projectname),
          "DataType" = "midpoint pmart"
        ),
        "Objects" = objects,
        "fmeta" = fmeta
      )
      
    }
    
  }
  
  # Give the object its appropriate class
  class(ProjectObject) <- "project multiomics"
  
  # Return result
  return(ProjectObject)
  
}



