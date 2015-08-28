
#' Title
#'
#' @param pkg The name of the bioconductor package
#'
#' @return The total number of reverse dependencies
#' @import rvest
#' @import magrittr
#' @export
#'
#' @examples
#' getRevDeps("S4Vectors")
getRevDeps <- function(pkg) {
  
  
  # Swap this for the package you're investigating
  #pkg <- "S4Vectors"
  
  # Pull the html
  pkgUrl <- paste0("http://www.bioconductor.org/packages/release/bioc/html/", pkg, ".html")
  bio <- html(pkgUrl)
  
  # Extract the rows of the details table into a list
  bio %>% html_node("table.details") %>% html_nodes("tr") -> biotr
  
  # Function to count the number of hyper links in a cell
  getNAnchors <- function(x) {
    td <- x %>% html_nodes("td")
    return(td[[2]] %>% html_nodes("a") %>% length)
  }
  
  # Function to extract the name from a cell
  tdName <- function(x) {
    td <- x %>% html_nodes("td")
    return(td[[1]] %>% html_text)
  }
  
  # Extract text from left hand column
  field=sapply(biotr, tdName)
  # Extract number of anchors from right hand column
  nAnchors=sapply(biotr, getNAnchors)
  
  detailsFrame <- data.frame(Field=field, NAnchors=nAnchors)
  #print(detailsFrame)
  
  #nRevDeps <- sum(nAnchors[field %in% c("Depends On Me", "Imports Me")])
  #cat("Total Reverse Dependencies:", nRevDeps)
  
  biotr[which(field %in% c("Depends On Me", "Imports Me"))] %>% 
    html_nodes("a") %>% html_text %>% unique %>% length -> nRevDeps
  
  return(nRevDeps)
  
}