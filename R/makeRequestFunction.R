#' @import(httr)
#' @import(jsonlite)
#' @import(dplyr)

#' Make an API Request
#'
#' Sends an HTTP GET request to a specified URL with headers and query parameters, handling errors.
#'
#' @name makeRequest
#' @title Helper function that builds Reddit API Request
#' @param url A string containing the API endpoint.
#' @param headers Declares Oauth Authentication.
#' @param params A list of query parameters to include in the request.
#' @return Parsed JSON content from the response.
#' @export

makeRequest <- function(url, headers, params) {
  response <- GET(url, add_headers(.headers = headers), query = params) #send get request
  if (http_status(response)$category != "Success") {
    stop("API request failed with status: ", http_status(response)$message) #check if get status is Success
  }
  content(response, as = "parsed", type = "application/json") #return content
}
