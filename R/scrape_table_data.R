#'scrape_table_data
# Uses an xpath for a table on a webpage and gathers the information into a list
# multiple tables option returns all of the tables on a webpage if there is more than one.
#'
#' @param xpath xpath as a character string
#' @param multipleTables  boolean to whether there are multiple tables present or not
#'
#' @return list of data
#' @export
#'
#' @examples
#' NA
scrape_table_data <- function(xpath,
                           multipleTables = F) {

  # Initial variable checks
  checkmate::assert(
    checkmate::check_character(xpath),
    checkmate::checkLogical(multipleTables)
  )

  # find the element of the main data table being scraped
  data_table <- RSelenium::remDr$findElement(using = 'xpath', xpath)

  if (multipleTables == F) {
    data_table_html <- data_table$getPageSource()
    page <- rvest::read_html(data_table_html %>% unlist())
    return(html_table(page))

  } else {
    data_table_html <- data_table$getPageSource()
    page <- read_html(data_table_html %>% unlist())
    dt <- html_table(page)
    dt <- lapply(dt, function(x) {
      if (dim(x)[[1]] == 0) {
        x <- NA
      } else {x <- x}
    })
    dt <- dt[!is.na(dt)]
    list_dt <- lapply(dt, function(x) {
      x <- as.data.frame(x)
      colnames(x) <- c(1:dim(x)[2])
      return(x)
    })

    new_list <- rbindlist(list_dt)
    return(new_list)
  }
}
