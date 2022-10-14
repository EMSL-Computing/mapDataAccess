#' Get all existing object IDs
#' 
#' @details If the SHINYPROXY_USERNAME environment variable is set this will 
#' return all IDs for that user, otherwise it will return all IDs in the 
#' data connection.
#' 
#' @param con \code{\link{map_data_connection}} object 
#' @param filter_tags (optional) Either a character vector or a named list/vector 
#' with character elements.  If named, will return every object id with at least
#' one tag value that matches the corresponding value in the named list.  
#' If unnamed, object ids with at least one tag key in filter_tags will be 
#' returned.
#'
#' @return a character vector of IDs of existing data
#' @export
get_all_data_ids <- function(con, filter_tags = NULL, ...) UseMethod("get_all_data_ids")

#' @export
get_all_data_ids.map_dir_connection <- function(con, ...) {
  if (con$verbose) message("Getting all data IDs")
  curr_ids <- list.files(con$dir, recursive=TRUE)
  return(curr_ids)
}

#' @export
get_all_data_ids.map_minio_connection <- function(con, filter_tags = NULL, ...) {
  if (con$verbose) message("Getting all data IDs")
  curr_ids = reticulate::iterate(
    con$client$list_objects(con$bucket, prefix=con$directory, recursive=TRUE), 
    function(x) {
        if(!is.null(filter_tags)){
          obj_tags <- con$client$get_object_tags(
            bucket_name=con$bucket,
            object_name=x$object_name
          )
          if(is.null(names(filter_tags))) {
            is_returned <- any(names(obj_tags) %in% filter_tags)
          } else if(inherits(obj_tags, "minio.commonconfig.Tags")){
            is_returned <- any(
              sapply(names(filter_tags), function(tagname) {
                isTRUE(filter_tags[[tagname]] == obj_tags$get(tagname))
              }) 
            )
          } else {
            is_returned <- FALSE
          }
          return(if(is_returned) x$object_name else NULL)
        } else return(x$object_name)
    }
  )
  
  curr_ids = unlist(curr_ids)
  
  if (!is.null(con$directory)) {
    curr_ids <- gsub(paste0(con$directory, "/"), "", curr_ids)
  }
  
  return(curr_ids)
}
