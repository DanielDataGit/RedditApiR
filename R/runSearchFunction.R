
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

runSearch <- function(userAgent = "useragent", headers = header, keywords = NA, subreddits = NA, startTime = NA, endTime = NA, sort = "relevance", time = "all", type = "query", batchSize = 100, getComments = FALSE, maxComments = 100) {
  #initialize df for storage
  df <- data.frame(title = character(), body = character(), author = character(), created = as.POSIXct(character()), keyword = character(), subreddit = character(), url = character(), comments = I(list()), stringsAsFactors = FALSE)

  for (x in keywords) { # Loop over keywords if specifed
    for (y in subreddits) { # Loop over Subreddits if specified
      print(paste("Starting search for keyword: '", x, "' in subreddit '", y, "'", sep = ""))

      #Run searchReddit helper function
      results <- searchReddit(userAgent = userAgent, headers, query = x, subreddit = y, startTime = startTime, endTime = endTime, sort = sort, time = time, type = type, batchSize = batchSize, getComments = getComments, maxComments = maxComments)

      # Store new results
      newDf <- do.call(rbind, lapply(results, function(post) {
        postData <- data.frame(
          title = ifelse(!is.null(post$title), post$title, NA),
          body = ifelse(!is.null(post$body), post$body, NA),
          author = ifelse(!is.null(post$author), post$author, NA),
          created = post$created,
          keyword = ifelse(!is.null(post$keyword), post$keyword, NA),
          subreddit = ifelse(!is.null(post$subreddit), post$subreddit, NA),
          url = ifelse(!is.null(post$url), post$url, NA),
          comments = I(list(ifelse(!is.null(post$comments), post$comments, list()))),
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
    return(df[-7])
  })
}

