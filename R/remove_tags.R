#' Remove tags associated with an ID
#' 
#' @details Only valid when using Min.io as the backend storage mechanism.  
#'
#' @param con \code{\link{map_data_connection}} object using Min.io as the backend
#' @param id data object ID, typically returned by \code{\link{put_data}}
#'
#' @return nothing
#' @seealso \code{\link{get_tags}}, \code{\link{set_tags}}
#' @export
remove_tags <- function(con, id) UseMethod("remove_tags")

#' @export
remove_tags.map_dir_connection <- function(con, id) {
  stop("'remove_tags' is not implemented for directory connections")
}

#' @export 
remove_tags.map_minio_connection <- function(con, id) {
  if (!is.null(con$directory)) {
    id <- paste(con$directory, id, sep="/")
  }
  if (con$verbose) message(sprintf("Removing tags from object %s/%s", con$bucket, id))
  con$client$delete_object_tags(bucket_name=con$bucket,object_name=id)
  invisible()
}