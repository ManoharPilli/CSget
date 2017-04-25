#' web scraping
#' @param url is a string
#' @return a R object
#' @export

getData <- function(url) {
  usePackage("curl")
  usePackage("jsonlite")
  usePackage("XML2R")
  usePackage("R.cache")
  setCacheRootPath(path="~/.Rcache")
  #load from cache
  out_data <- loadCache(list(url), suffix=".Rcache")
  if (!is.null(out_data)) {
    return(out_data)
  }
  else {
    h <- new_handle()
    handle_setheaders(h, customheader = "CS")
    req <- curl_fetch_memory(url=url, h)
    if (req$status_code >= 400) {
      cat("error calling url :", parse_headers(req$headers))
    }
    else {
      header = parse_headers(req$headers)
      header_content = grep("Content-Type:",header, value = TRUE)
      subtype = ""
      subtype = findType(header_content)
      if (subtype == "json") {
        out_data = fromJSON(url)
      } else if (subtype == "xml") {
        out_data = XML2R(url)
      } else if (subtype == "csv") {
        out_data = read.csv(file = url)
      } else if (subtype == "image") {
        out_data = getBinaryURL(url)
      } else if (subtype == "") {
        out_data = readLines(url)
      }
      path = saveCache(out_data, key=list(url), suffix=".Rcache")
      out_data
    }
  }
}
