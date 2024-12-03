#' @import(httr)
#' @import(jsonlite)
#' @import(dplyr)

#' Search Reddit Posts
#'
#' Searches Reddit for posts matching a query with optional filters, sorting, and pagination.
#'
#' @name searchReddit
#' @title Helper function to search reddit posts
#' @param userAgent username used for Oauth authentication.
#' @param subreddit (Optional) restricts search to subreddit if set to 1.
#' @param query The search term or author name, depending on `type`.
#' @param startTime (Optional) Start of the search range (POSIXct).
#' @param endTime (Optional) End of the search range (POSIXct).
#' @param sort Sorting criteria: "relevance", "new", "hot", etc.
#' @param time Time range: "all", "day", "week", "year".
#' @param type Type of search: "query" or "author".
#' @param batchSize Number of posts to fetch per keyword.
#' @param getComments Whether to fetch comments for each post.
#' @param maxComments Maximum number of comments to fetch per post.
#' @return A list of posts, each with details such as title, body, created time, subreddit, and comments.
#' @export

searchReddit <- function(userAgent, subreddit = NA, query = NA, startTime = NA, endTime = NA, sort = "relevance", time = "all", type = "query", batchSize = 100, getComments = FALSE, maxComments = 100) {
  results <- list() # Holds results
  after <- NULL # Holds after value
  # Choose url given params
  url <- if (!is.na(subreddit)) {
    paste0('https://www.reddit.com/r/', subreddit, '/search.json')
  } else {
    'https://www.reddit.com/search.json'
  }
  # Setup API params
  params <- list(
    q = ifelse(type == "author", paste0("author:", query), query),
    sort = sort,
    limit = 100,
    t = time,
    restrict_sr = ifelse(!is.na(subreddit), 1, 0)
  )

  # api headers for authorization
  headers <- c('User-Agent' = paste0("praw:reddit.pagination:v1.0 (by /u/", userAgent, ")"))

  while (length(results) < batchSize) { #keep searching till after is null or limit is reached
    if (!is.null(after)) {
      params$after <- after
    }

    data <- makeRequest(url, headers, params)

    if (is.null(data$data) || length(data$data$children) == 0) {
      print("No data found in response")
      break
    }

    # Filter results if time frame is specified
    batchResults <- lapply(data$data$children, function(submission) {
      submissionData <- submission$data
      createdUtc <- as.POSIXct(submissionData$created_utc, origin = "1970-01-01", tz = "UTC")

      if ((is.na(startTime) || createdUtc >= as.numeric(as.POSIXct(startTime))) &&
          (is.na(endTime) || createdUtc <= as.numeric(as.POSIXct(endTime)))) {

        postData <- list(
          title = submissionData$title,
          body = ifelse(is.null(submissionData$selftext), NA, submissionData$selftext),
          created = createdUtc,
          keyword = query,
          subreddit = submissionData$subreddit
        )

        # Process comments if specified
        if (getComments) {
          postData$comments <- fetchComments(submissionData$permalink, headers, maxComments)
        }

        return(postData)
      } else {
        NULL
      }
    })

    # Check if data was received then append new results
    batchResults <- batchResults[!sapply(batchResults, is.null)]

    if (length(batchResults) > 0) {
      results <- append(results, batchResults)
      cat("Batch results:", length(batchResults), "posts found.\n")
    } else {
      cat("No posts found in this batch for keyword ", query, "\n")
    }

    # exit if no more after result
    after <- data$data$after
    if (is.null(after)) {
      cat("No more pages to fetch.\n")
      break
    }

    Sys.sleep(1) # prevent hitting rate limits
  }

  return(results)
}
