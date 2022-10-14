# internal function

get_unique_id <- function(con, ...) UseMethod("get_unique_id")


get_unique_id.map_dir_connection <- function(con, ...) {
  id <- uuid::UUIDgenerate()
  # make sure it's unique
  while (file.exists(file.path(con$dir, id))) {
    id <- uuid::UUIDgenerate()
  }
  return(id)
}

get_unique_id.map_minio_connection <- function(con, ...) {
  id <- uuid::UUIDgenerate()
  # make sure it's unique
  curr_ids = reticulate::iterate(
    con$client$list_objects(con$bucket, prefix=con$directory, recursive=TRUE), 
    function(x) x$object_name)
  if (length(curr_ids) == 0) return(id)
  while (any(endsWith(curr_ids, id))) {
    id <- uuid::UUIDgenerate()
  }
  return(id)
}