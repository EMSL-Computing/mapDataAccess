#' Retrieve data object from a moap_data_connection object using a UUID 
#'
#' @param con \code{\link{map_data_connection}} object
#' @param id data object ID, typically returned by \code{\link{put_data}}
#'
#' @return R object stored at the specified ID
#' @seealso \code{\link{put_data}}, \code{\link{map_data_connection}}
#' @export
get_data <- function(con, id, ...) UseMethod("get_data")

#' @export
get_data.map_dir_connection <- function(con, id, ...) {
  if (con$verbose) message(sprintf("Reading %s/%s", con$bucket, id))
  fname <- file.path(con$dir, id)
  if (!file.exists(fname)) 
    stop(sprintf("Invalid id: %s", id))
  
  data <- readRDS(fname)
  return(data)
}

#' @export
get_data.map_minio_connection <- function(con, id, ...) {
  if (!is.null(con$directory)) {
    id <- paste(con$directory, id, sep="/")
  }

  if (con$verbose) message(sprintf("Reading %s/%s", con$bucket, id))
  io <- reticulate::import("io")
  #this will return a urllib3.response.HTTPResponse object
  
  result4 <- con$client$get_object(bucket_name=con$bucket, object_name=id)
  len <- as.integer(result4$getheader('Content-Length'))
  bytearray <- reticulate::r_to_py(raw(len))
  result4$readinto(bytearray)
  obj <- unserialize(reticulate::py_to_r(bytearray))
  return(obj)
}