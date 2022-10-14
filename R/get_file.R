#' Retrieve file object from a map_data_connection object using a UUID 
#'
#' @param con \code{\link{map_data_connection}} object
#' @param id file object ID, typically returned by \code{\link{put_file}}
#' @param filename (optional) local path to store object, if none provided, object is stored in a temp file.
#'
#' @return path to local file copy (\code{filename} if specified, otherwise generated with \code{\link{tempfile}} function)
#' @seealso \code{\link{put_file}}, \code{\link{map_data_connection}}
#' @export
get_file <- function(con, id, filename = NULL, ...) UseMethod("get_file")

#' @export
get_file.map_dir_connection <- function(con, id, filename = NULL, ...) {
  if (con$verbose) message(sprintf("Retrieving %s/%s", con$bucket, id))
  fname <- file.path(con$dir, id)
  if (!file.exists(fname)) 
    stop(sprintf("Invalid id: %s", id))
  dest_path <- if(is.null(filename)) tempfile() else filename
  file.copy(from = fname, to = dest_path)
  
  return(dest_path)
}

#' @export
get_file.map_minio_connection <- function(con, id, filename = NULL, use_dir = T, ...) {
  if (!is.null(con$directory) && use_dir) {
    id <- paste(con$directory, id, sep="/")
  }
  
  if (con$verbose) message(sprintf("Retrieving %s/%s", con$bucket, id))
  
  dest_path <- if(is.null(filename)) tempfile() else filename
  con$client$fget_object(bucket_name=con$bucket, object_name=id, file_path=dest_path)
  return(dest_path)
}