% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/project_constructor.R
\name{project_multiomics}
\alias{project_multiomics}
\title{Generate a multi-omics project object to pass data from MAP to IPMART/MODE.}
\usage{
project_multiomics(projectname, objects, fmeta = NULL)
}
\arguments{
\item{projectname}{Any string to name the project. All spaces and non-alphanumeric
characters will be removed to prevent issues with the visualizations. Required.}

\item{objects}{List of all omic projects or all pmart midpoints at the same tab (normalization or statistics).
Mixing of projects and midpoints is not allowed. Must contain 2-5 objects. There can be
no more than 2 metabolomics (1 of: NMR or GC/LC-MS), no more than 2 lipidomics datasets,
and no more than 1 proteomics (peptide or protein) dataset. Required.}

\item{fmeta}{Must be a dataframe or data table. If not provided, users can built it in
ipmart. Default is NULL.}
}
\value{
A multiomics project object
}
\description{
Constructs a multiomics project object from omic projects or pmart midpoints. Required.
}
\examples{
\dontrun{

library(pmartRdata)

# Generate midpoint with the examples in midpoint_pmart and save result as "midpoint"

# Make a metabolomics GC/LC MS project
metab_project <- project_omic(projectname = "My Metab Data",
                               datatype = "Metabolomics-GC/LC-MS",
                               edata = pmartRdata::metab_edata,
                               fdata = pmartRdata::metab_fdata,
                               edata_filename = "metab_edata",
                               fdata_filename = "metab_fdata")
                          
                                                    
# Make a lipidomics project
lipid_project <- project_omic(projectname = "My Lipid Data",
                               datatype = "Lipidomics-Positive",
                               edata = pmartRdata::lipid_edata_pos,
                               fdata = pmartRdata::lipid_fdata_pos,
                               edata_filename = "lipid_edata_pos",
                               fdata_filename = "lipid_fdata_pos")
                               
# Make a proteomics project
protein_project <- project_omic(projectname = "My Protein Data",
                                datatype = "Protein-level Label Free",
                                edata = pmartRdata::pro_edata,
                                fdata = pmartRdata::pro_fdata,
                                edata_filename = "pro_edata",
                                fdata_filename = "pro_fdata")
                                
# Create an f_meta file
fmeta <- data.frame(
  "Proteins" = c(paste0("Mock", 1:3), paste0("Infection", c(1:4,6,7,9))),
  "Lipids" = c(paste0("Mock", 1:3), paste0("Infection", c(1:4,6,7,9))),
  "Metabolites" = c(paste0("Mock", 1:3), paste0("Infection", c(1:4,6,7,9)))
)
                          
# Finally, make the ipmart midpoint object
project_multiomics(projectname = "projects", objects = list(metab_project, lipid_project, protein_project))

# Or use the pmart_midpoint examples 
project_multiomics(projectname = "midpoints", objects = list(pep_midpoint, lipid_midpoint))                          

}
}
