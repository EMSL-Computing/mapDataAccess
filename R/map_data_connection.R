# API for data access methods

# classes: map_dir_connection, map_minio_connection
# 2 versions currently: 1 that reads from directory another that uses Min.io


# How it would work:
# con <- map_data_connection()
# mynewdata <- get_data(con, id="some uuid")
# uuid <- put_data(con, iris)


#' Construct data connection object from config file or environment variables
#'
#' @param config_file optional config file
#'
#' @export
#' @examples 
#' \dontrun{
#' con <- map_data_connection(config_file=system.file("example_directory_config.yml", package="mapDataAccess"))
#' id <- put_data(con, iris)
#' iris2 <- get_data(con, id)
#' identical(iris, iris2)
#' }
#' @rdname map_data_connection
map_data_connection <- function(config_file=NA) {
  if (is.na(config_file)) {
    # configure from environment variables
    data_source <- get_env_variable("MAP_DATA_SOURCE")
  } else {
    if (!file.exists(config_file)) 
      stop(sprintf("Invalid config file name: %s", config_file))
    cfg <- yaml::read_yaml(config_file)
    if (!("type" %in% names(cfg))) 
      stop("Cannot find 'type' in config file")
    data_source <- cfg$type
  }
  
  message(sprintf("data source is %s", data_source))
  
  if (tolower(data_source) == "directory") {
    return(map_dir_connection(config_file=config_file))
  } else if (tolower(data_source) == "minio") {
    return(map_minio_connection(config_file=config_file))
  } else {
    stop(sprintf("Unknown data source type: %s", data_source))
  }
  
}

map_dir_connection <- function(config_file=NA) {
  if (is.na(config_file)) {
    dirname <- get_env_variable("MAP_DATA_DIR")
    verbose <- get_env_variable("MAP_DATA_ACCESS_VERBOSE", can_be_empty=TRUE)
  } else {
    cfg <- yaml::read_yaml(config_file)
    dirname <- get_config_variable(cfg, "dirname")
    verbose <- get_config_variable(cfg, "verbose", can_be_empty=TRUE)
  }
  
  # transform secure and verbose into boolean
  verbose <- str_to_bool(verbose, default=FALSE)
  
  username <- get_env_variable("SHINYPROXY_USERNAME", can_be_empty = TRUE)
  if (nchar(username) > 0) {
    dirname <- file.path(dirname, make.names(username))
  }
  
  if (!dir.exists(dirname)) {
    if (verbose) message(sprintf("Creating directory '%s'", dirname))
    dir.create(dirname, recursive = TRUE)
  }
  
  result <- structure(list(dir=dirname, verbose=verbose), class = c("map_dir_connection", "map_data_connection"))
  if (verbose) message(toString(result))
  return(result)
}

map_minio_connection <- function(config_file=NA) {
  if (is.na(config_file)) {
    endpoint <- get_env_variable("MINIO_ENDPOINT")
    access_key <- get_env_variable("MINIO_ACCESS_KEY")
    secret_key <- get_env_variable("MINIO_SECRET_KEY")
    bucket <- get_env_variable("MINIO_BUCKET")
    secure <- get_env_variable("MINIO_SECURE", can_be_empty=TRUE)
    region <- get_env_variable("MINIO_REGION", can_be_empty=TRUE)
    verbose <- get_env_variable("MAP_DATA_ACCESS_VERBOSE", can_be_empty=TRUE)
    python_venv <- get_env_variable("MAP_PYTHON_VENV")
  } else {
    cfg <- yaml::read_yaml(config_file)
    endpoint <- get_config_variable(cfg, "endpoint")
    access_key <- get_config_variable(cfg, "access_key")
    secret_key <- get_config_variable(cfg, "secret_key")
    bucket <- get_config_variable(cfg, "bucket")
    secure <- get_config_variable(cfg, "secure", can_be_empty=TRUE)
    region <- get_config_variable(cfg, "region", can_be_empty=TRUE)
    verbose <- get_config_variable(cfg, "verbose", can_be_empty=TRUE)
    python_venv <- get_config_variable(cfg, "python_venv")
  }
  # transform secure and verbose into boolean
  secure <- str_to_bool(secure, default=TRUE)
  verbose <- str_to_bool(verbose, default=FALSE)
  
  # connect to minio
  
  # check if the specified environment is a conda environment
  conda_envs <- tryCatch(
    {
      reticulate::conda_list()$python
    },
    error = function(e) {
      NULL
    }
  )
  
  is_conda <- any(sapply(conda_envs, function(envpath) {
    grepl(sprintf("^%s", normalizePath(python_venv)), envpath)
  }))
  
  if (is_conda) {
    reticulate::use_condaenv(python_venv, required = TRUE)
  } else {
    reticulate::use_virtualenv(python_venv, required = TRUE)
  }
  
  username <- get_env_variable("SHINYPROXY_USERNAME", can_be_empty = TRUE)
  if (nchar(username) > 0) {
    username <- make.names(username)
  } else {
    username <- NULL
  }
  
  minio <- reticulate::import('minio')
  tryCatch({
      client <- minio$Minio(endpoint, access_key=access_key, secret_key=secret_key, secure=secure)
      if (!client$bucket_exists(bucket)) 
        client$make_bucket(bucket)
    },
    error=function(e) { message("Error connecting to minio endpoint"); stop(e) })
  
  result <- structure(list(client=client, endpoint=endpoint, access_key=access_key, 
                           secret_key=secret_key, bucket=bucket, secure=secure, 
                           region=region, verbose=verbose, directory=username), 
                   class = c("map_minio_connection", "map_data_connection"))
  if (verbose) message(toString(result))
  return(result)
}

# internal function: throws error message if environment variable is not found and can_be_empty==FALSE
get_env_variable <- function(varname, can_be_empty=FALSE) {
  value <- Sys.getenv(varname)
  if (!can_be_empty & (length(value) == 0 | nchar(value) == 0) ) {
      stop(sprintf("Cannot find %s environment variable", varname))
  }
  return(value)
}

# internal function: throws error message if variable is not found and can_be_empty==FALSE
get_config_variable <- function(cfg, varname, can_be_empty=FALSE) {
  if (!(varname %in% names(cfg))) {
    if (!can_be_empty)
      stop(sprintf("Cannot find '%s' in config file", varname))
    else
      return('')
  }
  value <- cfg[[varname]]
  return(value)
}

# internal function: transform string to boolean
str_to_bool <- function(val_str, default=FALSE) {
  if (length(val_str) == 0 | nchar(val_str) == 0) 
    return(default)
  else if (toupper(val_str) == "TRUE")
    return(TRUE)
  else if (toupper(val_str) == "FALSE")
    return(FALSE)
  else
    stop(sprintf("Invalid boolean: %s", val_str))
}