# mapDataAccess

Functions in this package exist for 2 specific functionalities. 

## 1) Pushing/pulling data from minio

These functions include: 
  * get_all_data_ids: extract all ids of objects in the minio bucket
* get_data: retrieve Rdata object from minio using its id
* get_file: retrieve a file from minio using its id
* get_tags: get a list of tags for the object (useful for tracking data across applications)
* map_data_connection: track important variables for connecting to minio
* put_data: place an Rdata object on minio and generate its id 
* put_file: place a file on minio and generate its id
* remove_data: delete an Rdata object from minio using its id
* remove_tags: delete tags from an object
* set_tags: add tags to a minio object to track what the data is and where it came from

## 2) MAP (multiomics analysis portal) specific functions

The functions include:
  * is_edata: check if the data.frame is in the correct expression matrix format for pmartR 
* is_emeta: check if the data.frame is in the correct biomolecule information format for pmartR
* is_fdata: check if the data.frame is in the correct sample information format for pmartR
* is_fmeta: check if the data.frame is in the correct sample meta-information format for the iPMART application
* midpoint_ipmart: generate a midpoint file (normalized or statistics results) for iPMART
* midpoint_pmart: generate a midpoint file (normalized or statistics results) for pmartR
* project_edata: create a simple project with just an e_data file
* project_omic: create a project with an omic dataset of with an expression matrix and sample information, and optional biomolecule information
* project_multiomic: create a project with multiple omic datasets, including sample meta-information
* pull_tags_from_object: generate the appropriate map tags (see set_tags) for midpoints and projects

Projects are data that have not been processed by pmartR or iPMART. 
