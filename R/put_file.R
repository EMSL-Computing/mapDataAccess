#' Send file to a moap_data_connection object and get back a UUID
#'
#' @param con \code{\link{map_data_connection}} object
#' @param filename path to file
#'
#' @return UUIDthat can be used for retrieving the file (see \code{\link{get_file}})
#' @seealso \code{\link{get_file}}, \code{\link{map_data_connection}}
#' @export
put_file <- function(con, filename, ...) UseMethod("put_file")

#' @export
put_file.map_dir_connection <- function(con, filename, id=NA, ...) {
  if (is.na(id)) 
    id <- get_unique_id(con)

  if (!file.exists(filename)) stop(sprintf("File does not exist: '%s'", filename))
  file.copy(filename, file.path(con$dir, id))
  return(id)
}

#' @export
put_file.map_minio_connection <- function(con, filename, id=NA, ...) {
  if (is.na(id)) 
    id <- get_unique_id(con)

  internal_id <- id
  if (!is.null(con$directory)) {
    internal_id <- paste(con$directory, id, sep="/")
  }
  
  if (con$verbose) message(sprintf("Writing file to %s/%s", con$bucket, internal_id))
  if (!file.exists(filename)) stop(sprintf("File does not exist: '%s'", filename))
  result <- con$client$fput_object(bucket_name=con$bucket,object_name=internal_id, 
                               file_path=filename)

  return(id)
}