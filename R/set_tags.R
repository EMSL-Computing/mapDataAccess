#' Set tags associated with an ID
#' 
#' @details Only valid when using Min.io as the backend storage mechanism.  
#'
#' @param con \code{\link{map_data_connection}} object using Min.io as the backend
#' @param id data object ID, typically returned by \code{\link{put_data}}
#' @param tags list of key/value pairs
#'
#' @return nothing
#' @seealso \code{\link{get_tags}}
#' @export
set_tags <- function(con, id, tags) UseMethod("set_tags")

#' @export
set_tags.map_dir_connection <- function(con, id, tags) {
  stop("'set_tags' is not implemented for directory connections")
}

#' @export 
set_tags.map_minio_connection <- function(con, id, tags) {
  tag_obj <- NULL
  if (!is.list(tags)) {
    stop("tags parameter must be a list")
  }
  minio <- reticulate::import('minio')
  tag_obj = minio$tagging$Tags$new_object_tags()
  tag_obj$update(tags)

  if (!is.null(con$directory)) {
    id <- paste(con$directory, id, sep="/")
  }
  
  if (con$verbose) message(sprintf("Writing tags to %s/%s", con$bucket, id))
  con$client$set_object_tags(bucket_name=con$bucket,object_name=id, 
                                tags=tag_obj)
  
  invisible()
}