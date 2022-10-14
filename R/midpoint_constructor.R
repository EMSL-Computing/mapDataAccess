#' Generate a midpoint analysis file for pmart
#' 
#' @description Possible "exit" points to generate a midpoint in pmart are
#'    normalization, peptide statistics, peptide rollup, and statistics. 
#'    
#' @param omics_data An omicsData object as defined in pmartR. Required.
#' @param tab The pmart tab the data was exported from. Acceptable entries are 
#'    "normalization_tab", "peptide_statistics_tab", "protein_rollup_tab",
#'    and "statistics_tab". Required. 
#' @param project A project omic object. Required. 
#' @param name A string to indicate the name of the midpoint file. Optional. 
#' @param omics_stats An omicsStats object as defined in pmartR. Optional. 
#' @param omics_stats_pep A peptide omicsStats object as defined in pmartR. Optional. 
#' 
#' @return A midpoint pmart object
#' @examples 
#' \dontrun{
#' 
#' library(pmartR)
#' library(pmartRdata)
#' 
#' ## First, let's simulate getting to the normalization tab in pmart
#' 
#' # Load peptide data
#' myPepObject <- pmartRdata::pep_object
#' 
#' # Log transform data
#' myPepObject <- edata_transform(myPepObject, "log2")
#' 
#' # Set group designation
#' myPepObject <- group_designation(myPepObject, main_effects = "Condition")
#' 
#' # Apply filters
#' myPepObject <- applyFilt(molecule_filter(myPepObject), myPepObject, min_num = 2)
#' myPepObject <- applyFilt(rmd_filter(myPepObject), myPepObject, pvalue_threshold = 0.0001)
#' 
#' # Run Normalization
#' myPepObject <- normalize_global(myPepObject, "all", "mean", apply_norm = TRUE)
#' 
#' ## Now, let's simulate exporting a midpoint file at this step 
#' 
#' pep_midpoint <- midpoint_pmart(omics_data = myPepObject,
#'                tab = "normalization_tab", 
#'                project = project_omic(projectname = "My Peptide Data",
#'                                        datatype = "Peptide-level Label Free",
#'                                        edata = pmartRdata::pep_edata,
#'                                        fdata = pmartRdata::pep_fdata,
#'                                        emeta = pmartRdata::pep_emeta,
#'                                        edata_filename = "pep_edata",
#'                                        fdata_filename = "pep_fdata",
#'                                        emeta_filename = "pep_emeta")
#'                )
#'                
#' # Load lipidomics data 
#' myLipidObject <- pmartRdata::lipid_object_pos
#' 
#' # Log transform data
#' myLipidObject <- edata_transform(myLipidObject, "log2")
#' 
#' # Set group designation
#' myLipidObject <- group_designation(myLipidObject, main_effects = "Condition")
#' 
#' # Apply filters
#' myLipidObject <- applyFilt(rmd_filter(myLipidObject), myLipidObject, pvalue_threshold = 0.0001)
#' 
#' # Run Normalization
#' myLipidObject <- normalize_global(myLipidObject, "all", "mean", apply_norm = TRUE)
#' 
#' lipid_midpoint <- midpoint_pmart(omics_data = myLipidObject,
#'                   tab = "normalization_tab", 
#'                   project = project_omic(projectname = "My Lipid Data",
#'                                           datatype = "Lipidomics-Positive",
#'                                           edata = pmartRdata::lipid_edata_pos,
#'                                           fdata = pmartRdata::lipid_fdata_pos,
#'                                           emeta = NULL,
#'                                           edata_filename = "lipid_edata_pos",
#'                                           fdata_filename = "lipid_fdata_pos",
#'                                           emeta_filename = NULL)
#'                )
#' 
#' 
#' }
#' @export
midpoint_pmart <- function(omics_data, tab, project = NULL, name = "exportedFromPMART", omics_stats = NULL, omics_stats_pep = NULL) {
  
  # Check omics_data is an omics data object
  if (any(class(omics_data) %in% c("pepData", "proData", "metabData", "lipidData", "seqData", "nmrData")) == FALSE) {
    stop("omics_data must be of the pmartR class pepData, proData, metabData, lipidData, or seqData.")
  }
  
  # Tab must be of the correct class
  tab <- as.character(tab)
  if (tab %in% c("normalization_tab", "peptide_statistics_tab", "protein_rollup_tab", "statistics_tab") == FALSE) {
    stop("tab name not recognized for pmart.")
  }
  
  # Check that project is of the project omic class
  if (class(project) != "project omic") {
    stop("project must be a project omic object.")
  }

  # Check that omics_stats and omics_stats_pep are statRes objects
  if (is.null(omics_stats_pep) == FALSE) {
    if (!inherits(omics_stats_pep, "statRes")) {
      stop("omics_stats_pep must be a peptide statRes object from pmartR::imd_anova.")
    }
  }
  if (is.null(omics_stats) == FALSE) {
    if (!inherits(omics_stats, "statRes")) {
      stop("omics_stats must be a statRes object from pmartR::imd_anova.")
    }
  }
  
  # Statistics will only be added if that tab name is enabled
  if (tab == "normalization_tab" & is.null(omics_stats_pep) == FALSE) {
    message("omics_stats_pep ignored since the tab is normalization.")
    omics_stats_pep <- NULL
  }
  if (tab != "statistics_tab" & is.null(omics_stats) == FALSE) {
    message("omics_stats ignored since the tab is not statistics.")
    omics_stats <- NULL
  }
  
  # Construct MidPoint object
  MidPoint <- list(
    "Data Objects" =
      list(
        "OmicsData" = omics_data,
        "OmicsStatsPep" = omics_stats_pep,
        "OmicsStats" =  omics_stats
      ),
    "Tracking" = 
      list(
        "Name" = .scrub_clean(name), 
        "Timestamp" = Sys.time(),
        "Tab" = tab,
        "Original Files" = project
      )
  )
  
  # Assign the midpoint pmart class
  class(MidPoint) <- "midpoint pmart"
  
  return(MidPoint)
  
}

#' Generate a midpoint analysis file for ipmart
#' 
#' @description Possible "exit" points to generate an midpoint file in ipmart are
#'     normalization, peptide statistics, peptide rollup, and statistics. 
#'
#' @param midpoints List of midpoint pmart objects from the same tab. Must contain 2-5 objects. There can be
#'    no more than 2 metabolomics (1 of: NMR or GC/LC-MS), no more than 2 lipidomics datasets, 
#'    and no more than 1 proteomics (peptide or protein) dataset. Required. 
#' @param tab The ipmart tab the data was exported from. Acceptable entries are 
#'    "normalization_tab", "statistics_tab", or "integration_tab". Required.
#' @param fmeta Must be a dataframe or data table. If not provided, users can built it in 
#'    ipmart. Default is NULL. 
#'
#' @return A midpoint ipmart object
#' 
#' @examples
#' \dontrun{
#' 
#' # Generate the two midpoint pmart objects from in the "midpoint_pmart" example
#' 
#' midpoint_ipmart(midpoints = list(pep_midpoint, lipid_midpoint),
#'                 tab = "normalization_tab", 
#'                 fmeta = NULL)
#' 
#' }
#' @export
midpoint_ipmart <- function(midpoints, tab, name = "exportedFromIPMART", fmeta = NULL) {
  
  # All midpoints must be midpoint pmart objects 
  getClasses <- lapply(midpoints, class) %>% unlist() %>% unique()
  if (((getClasses != "midpoint pmart") %>% any())) {
    stop("midpoints must all be midpoint pmart objects.")
  }
  
  # Tab must be "normalization_tab", "statistics_tab", or "integration_tab". 
  if (as.character(tab) %in% c("normalization_tab", "statistics_tab", "integration_tab") == FALSE) {
    stop("tab must be normalization_tab, statistics_tab, or integration_tab.")
  }
  
  # Check the f_meta object
  if (is.null(fmeta) == FALSE) {

   # Pull edata objects
    edata_list <- lapply(midpoints, function(object) {
      if (class(object) == "midpoint pmart") {object$`Data Objects`$OmicsData$e_data}
    })
   
   # Run f_meta check
    #if (is_fmeta(edata_list, fmeta) == FALSE) {
    #  stop("Multi-omics Sample Information (f_meta) file is not valid.")
    #}
   
  }
  
  # Get project types 
  ProjectTypes <- lapply(midpoints, function(midpoint) {
    
    # Get data type
    DataType <- midpoint$Tracking$`Original Files`$Project$DataType
    
    # Simplify data type to metabolomics/lipidomics/proteomics
    if (grepl("Peptide|Protein", DataType)) {return("Proteomics")} else
      if (grepl("Lipidomics", DataType)) {return("Lipidomics")} else {return(DataType)}
    
  }) %>% unlist() %>% table() %>% c()
  
  # Ensure there is at most 1 peptide/proteomics, 2 lipidomics, 1 metabolomics GC/LC-MS, 1 metabolomics NMR
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
  
  # Name the objects by their data types
  names(midpoints) <- lapply(midpoints, function(x) {x$Tracking$`Original Files`$Project$DataType}) %>% unlist()
  
  # Create Object
  MidPoint <- list(
    "Data Objects" = midpoints,
    "Tracking" = 
      list(
        "Name" = .scrub_clean(name),
        "Timestamp" = Sys.time(),
        "Tab" = tab,
        "fmeta" = fmeta
      )
  )
  
  # Add class
  class(MidPoint) <- "midpoint ipmart"
  
  return(MidPoint)
  
}