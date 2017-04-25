findType <- function(header) {
  subtype = ""
  if (length(grep("json",header, value = TRUE))>0) {
    subtype = "json"
  } else if (length(grep("xml",header, value = TRUE))>0) {
    subtype = "xml"
  } else if (length(grep("csv",header, value = TRUE))>0) {
    subtype = "csv"
  } else if (length(grep("image",header, value = TRUE))>0) {
    subtype = "image"
  }
  return(subtype)
}
