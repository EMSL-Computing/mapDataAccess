#' @export
print.map_data_connection <- function(x, ...) {
  cat(paste0("type: ", class(x)[1], "\n"))
  for (k in names(x)) {
    cat(paste0(k, ": ", as.character(x[[k]]), "\n"))
  }
  invisible()
}

#' @export
toString.map_data_connection <- function(x, ...) {
  s <- paste0("{type=", class(x)[1], "; ")
  vals <- as.character(unlist(x))
  vals[names(x) %in% c("secret_key", "password")] <- "**********"
  s <- paste0(s, paste0(names(x), "=", vals, collapse="; "), "}")
  return(s)
}