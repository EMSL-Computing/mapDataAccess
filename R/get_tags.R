#' Get tags associated with an ID
#' 
#' @details Only valid when using Min.io as the backend storage mechanism. Tags are not 
#' guaranteed to be in the same order as provided in {\link{set_tags}}.
#'
#' @param con \code{\link{map_data_connection}} object using Min.io as the backend
#' @param id data object ID, typically returned by \code{\link{put_data}}
#'
#' @return a list object with key/value pairs
#' @seealso \code{\link{set_tags}}
#' @export
get_tags <- function(con, id) UseMethod("get_tags")

#' @export
get_tags.map_dir_connection <- function(con, id) {
  stop("'get_tags' is not implemented for directory connections")
}
  
#' @export 
get_tags.map_minio_connection <- function(con, id) {
  if (!is.null(con$directory)) {
    id <- paste(con$directory, id, sep="/")
  }
  if (con$verbose) message(sprintf("Getting tags for object %s/%s", con$bucket, id))
  tag_obj <- con$client$get_object_tags(bucket_name=con$bucket,
                                        object_name=id)
  result <- list()
  if (is.null(tag_obj)) return(result)

  for (i in 1:length(tag_obj)) {
    pr <- tag_obj$popitem()
    result[[pr[[1]]]] <- pr[[2]]
  }
  return(result)
}