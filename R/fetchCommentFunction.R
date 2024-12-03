#' @import(httr)
#' @import(jsonlite)
#' @import(dplyr)

#' Fetch Comments for a Reddit Post
#'
#' Retrieves comments for a Reddit post with optional pagination and error handling.
#'
#' @name fetchComments
#' @title Helper function to fetch Comments from Reddit Post
#' @param permalink The permalink to the Reddit post.
#' @param headers Declares Oauth Authentication.
#' @param maxComments Maximum number of comments to fetch.
#' @return A list of comments with details like body, author, and creation date per post.
#' @export

fetchComments <- function(permalink, headers, maxComments) {
  comments <- list() # List to hold comments
  after <- NULL # Hold after param for pagination
  totalFetched <- 0 # Keep track of results

  fetchPage <- function(url) {
    tryCatch({
      makeRequest(url, headers, list()) #Try to make request
    }, error = function(e) {
      message("Error fetching comments: ", e)
      return(NULL)
    })
  }

  # Setup functions to deal with nested list from api and extract relevant data
  processComments <- function(commentList) {
    lapply(commentList, function(comment) {
      commentData <- comment$data
      list(
        comment = commentData$body,
        author = commentData$author,
        created = as.POSIXct(commentData$created_utc, origin = "1970-01-01", tz = "UTC")
      )
    })
  }
  # Repeat processing till after is null or limit is reached for each post
  repeat {
    commentsUrl <- paste0('https://www.reddit.com', permalink, '.json?limit=100', if (!is.null(after)) paste0('&after=', after) else '')
    commentsData <- fetchPage(commentsUrl)

    if (is.null(commentsData)) break

    newComments <- processComments(commentsData[[2]]$data$children)
    comments <- c(comments, newComments)
    totalFetched <- length(comments)

    if (totalFetched >= maxComments) {
      comments <- comments[1:maxComments]
      break
    }

    after <- commentsData[[2]]$data$after
    if (is.null(after)) break
    Sys.sleep(1) # prevent hitting rate limits
  }

  return(comments)
}
