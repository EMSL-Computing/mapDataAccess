#' Send data object to a map_data_connection object and get back a UUID
#'
#' @param con \code{\link{map_data_connection}} object
#' @param data any R data object
#'
#' @return UUID that can be used for retrieving the data (see \code{\link{get_data}})
#' @seealso \code{\link{get_data}}, \code{\link{map_data_connection}}
#' @export
put_data <- function(con, data, ...) UseMethod("put_data")

#' @export
put_data.map_dir_connection <- function(con, data, id=NA, ...) {
  if (is.na(id)) 
    id <- get_unique_id(con)

  saveRDS(data, file.path(con$dir, id))
  return(id)
}

#' @export
put_data.map_minio_connection <- function(con, data, id=NA, ...) {
  if (is.na(id)) 
    id <- get_unique_id(con)

  io <- reticulate::import("io")
  bytes <-serialize(data, connection=NULL)
  
  internal_id <- id
  if (!is.null(con$directory)) {
    internal_id <- paste(con$directory, id, sep="/")
  }
  
  result <- con$client$put_object(bucket_name=con$bucket,object_name=internal_id, 
                               data=io$BytesIO(bytes), length=length(bytes))

  if (con$verbose) message(sprintf("Writing object to %s/%s", con$bucket, internal_id))
  return(id)
}