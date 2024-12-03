
#' @import(httr)
#' @import(jsonlite)
#' @import(dplyr)

#' Run Reddit Search and Create DataFrame
#'
#' Executes a Reddit search for multiple keywords and subreddits, returning results in a DataFrame.
#'
#' @name runSearch
#' @title main function to search with custom parameters.
#' @param userAgent (required) username for Oauth authentication.
#' @param keywords A vector of keywords to search for.
#' @param subreddits A vector of subreddits to search for.
#' @param startTime Start of the search range (POSIXct).
#' @param endTime End of the search range (POSIXct).
#' @param sort (default = "relevance") Sorting criteria: "relevance", "new", "hot", "top", "comments", "rising".
#' @param time (default = "all") Time range: "all", "hour", "day", "month", week", "year".
#' @param type (default = "query") Type of search: "query" or "author".
#' @param batchSize (default/minimum = 100) Number of posts to fetch per unique query.
#' @param getComments (default = FALSE) Whether to fetch comments for each post (logical).
#' @param maxComments (default = 100) Maximum number of comments to fetch per post.
#' @return A DataFrame containing posts and optionally their comments.
#' @examples
#' \dontrun{
#' # Query search
#'  runSearch(
#'  userAgent = "OauthUsername",
#'  keywords = c("data", "ml"),
#'  subreddits = c("all", "datascience"),
#'  batchSize = 300)
#'
#' # author search
#' runSearch(
#' userAgent = "OauthUsername",
#' type = "author",
#' keywords = c("profileName"),
#' subreddits = c("all"),
#' startTime = "2024-06-01 12:00:00"
#' )
#' }
#' @export

runSearch <- function(userAgent = "username", keywords = NA, subreddits = NA, startTime = NA, endTime = NA, sort = "relevance", time = "all", type = "query", batchSize = 100, getComments = FALSE, maxComments = 100) {
  #initialize df for storage
  df <- data.frame(title = character(), body = character(), created = as.POSIXct(character()), keyword = character(), subreddit = character(), comments = I(list()), stringsAsFactors = FALSE)

  for (x in keywords) { # Loop over keywords if specifed
    for (y in subreddits) { # Loop over Subreddits if specified
      print(paste("Starting search for keyword: '", x, "' in subreddit '", y, "'", sep = ""))

      #Run searchReddit helper function
      results <- searchReddit(userAgent = userAgent, query = x, subreddit = y, startTime = startTime, endTime = endTime, sort = sort, time = time, type = type, batchSize = batchSize, getComments = getComments, maxComments = maxComments)

      # Store new results
      newDf <- do.call(rbind, lapply(results, function(post) {
        postData <- data.frame(
          title = post$title,
          body = post$body,
          created = post$created,
          keyword = post$keyword,
          subreddit = post$subreddit,
          comments = I(list(post$comments)),
          stringsAsFactors = FALSE
        )
        return(postData)
      }))

      # bind new results
      df <- bind_rows(df, newDf)

      print(paste("Retrieved posts for keyword '", x, "' in subreddit '", y, "':", sep = ""))
      print(paste("Finished search for keyword '", x, "' in subreddit '", y, "'", sep = ""))


    }
  }
  # Return results and keep necessary columns
  return(if(getComments) {
    return(df)
  } else {
    return(df[-6])
  })
}

