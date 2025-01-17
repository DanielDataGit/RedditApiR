% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/runSearchFunction.R
\name{runSearch}
\alias{runSearch}
\title{main function to search with custom parameters.}
\usage{
runSearch(
  userAgent = user_agent,
  headers = header,
  keywords = NA,
  subreddits = NA,
  startTime = NA,
  endTime = NA,
  sort = "relevance",
  time = "all",
  type = "query",
  batchSize = 100,
  getComments = FALSE,
  maxComments = 100
)
}
\arguments{
\item{userAgent}{(required) username for Oauth authentication.}

\item{headers}{(required) headers for Oauth authentication.}

\item{keywords}{A vector of keywords to search for.}

\item{subreddits}{A vector of subreddits to search for.}

\item{startTime}{Start of the search range (POSIXct).}

\item{endTime}{End of the search range (POSIXct).}

\item{sort}{(default = "relevance") Sorting criteria: "relevance", "new", "hot", "top", "comments", "rising".}

\item{time}{(default = "all") Time range: "all", "hour", "day", "month", week", "year".}

\item{type}{(default = "query") Type of search: "query" or "author".}

\item{batchSize}{(default/minimum = 100) Number of posts to fetch per unique query.}

\item{getComments}{(default = FALSE) Whether to fetch comments for each post (logical).}

\item{maxComments}{(default = 100) Maximum number of comments to fetch per post.}
}
\value{
A DataFrame containing posts and optionally their comments.
}
\description{
main function to search with custom parameters.
}
\examples{
\dontrun{
# Query search
 runSearch(
 userAgent = user_agent,
 headers = header,
 keywords = c("data", "ml"),
 subreddits = c("all", "datascience"),
 batchSize = 300)

# author search
runSearch(
userAgent = user_agent,
headers = header,
type = "author",
keywords = c("profileName"),
subreddits = c("all"),
startTime = "2024-06-01 12:00:00"
)
}
}
