################################################################################
# RedditApiR Technology Subreddit Example
# 
# This script demonstrates how to use the RedditApiR package to authenticate 
# with the Reddit API and fetch posts from the r/technology subreddit.
#
# Prerequisites:
# 1. R version 3.5 or higher
# 2. Reddit account with API credentials
# 3. Reddit API application created at https://www.reddit.com/prefs/apps/
#
# Instructions:
# 1. Replace the placeholder credentials below with your actual Reddit API credentials
# 2. Install and load the required packages
# 3. Run the script to fetch and display technology posts
# 4. Optionally save results to CSV file
################################################################################

# STEP 1: Install and load the RedditApiR package
# Uncomment the following lines if this is your first time using the package:
# install.packages("devtools")
# library(devtools)
# devtools::install_github("DanielDataGit/redditApiR")

# Load required libraries
library(redditApiR)
library(httr)
library(jsonlite)
library(dplyr)

# STEP 2: Set up Reddit API credentials
# IMPORTANT: Replace these placeholder values with your actual Reddit API credentials
# 
# To get Reddit API credentials:
# 1. Go to https://www.reddit.com/prefs/apps/
# 2. Click "Create App" or "Create Another App"  
# 3. Choose "script" as the app type
# 4. Fill in the form:
#    - Name: Choose any name for your app
#    - App type: Select "script"
#    - Description: Optional description
#    - About URL: Can leave blank
#    - Redirect URI: Use http://localhost:8080 (required but not used for script apps)
# 5. Click "Create app"
# 6. Your client_id is the string under the app name
# 7. Your client_secret is the "secret" string shown

# Your Reddit API credentials (replace with actual values)
client_id <- "YOUR_CLIENT_ID_HERE"        # From Reddit app settings (under app name)
client_secret <- "YOUR_CLIENT_SECRET_HERE"  # From Reddit app settings (secret field)
reddit_user <- "YOUR_REDDIT_USERNAME_HERE" # Your Reddit username

# User agent (should be descriptive and include your username)
user_agent <- paste0("r/technology-fetcher:v1.0 (by /u/", reddit_user, ")")

# IMPORTANT: Check that credentials have been updated
if (client_id == "YOUR_CLIENT_ID_HERE" || 
    client_secret == "YOUR_CLIENT_SECRET_HERE" || 
    reddit_user == "YOUR_REDDIT_USERNAME_HERE") {
  stop("Please update the Reddit API credentials with your actual values before running this script!")
}

# STEP 3: Obtain OAuth2 token from Reddit API
# This function handles the OAuth2 authentication flow
get_reddit_token <- function(client_id, client_secret, user_agent) {
  # Reddit OAuth2 token endpoint
  token_url <- "https://www.reddit.com/api/v1/access_token"
  
  # Create authentication credentials
  auth <- authenticate(client_id, client_secret, type = "basic")
  
  # Request token with client credentials grant
  response <- POST(
    url = token_url,
    auth,
    add_headers(`User-Agent` = user_agent),
    body = list(grant_type = "client_credentials"),
    encode = "form"
  )
  
  # Check if request was successful
  if (http_status(response)$category != "Success") {
    stop("Failed to obtain access token. Check your credentials.")
  }
  
  # Parse response and extract token
  token_data <- content(response, as = "parsed")
  return(token_data$access_token)
}

# STEP 4: Set up authentication headers
cat("Authenticating with Reddit API...\n")

# Get OAuth2 access token
tryCatch({
  access_token <- get_reddit_token(client_id, client_secret, user_agent)
  cat("✓ Authentication successful!\n")
}, error = function(e) {
  cat("✗ Authentication failed:", conditionMessage(e), "\n")
  cat("Please check your credentials and try again.\n")
  stop("Authentication failed")
})

# Create headers for API requests
header <- list(
  Authorization = paste("bearer", access_token),
  `User-Agent` = user_agent
)

# STEP 5: Fetch posts from r/technology subreddit
cat("\nFetching posts from r/technology subreddit...\n")

# Set search parameters
num_posts <- 10  # Number of posts to fetch
subreddit <- "technology"
keywords <- "*"  # Use wildcard to get general posts (works with search endpoint)

# Use runSearch to fetch posts
tryCatch({
  technology_posts <- runSearch(
    userAgent = user_agent,
    headers = header,
    keywords = keywords,
    subreddits = subreddit,
    sort = "hot",          # Sort by hot posts
    time = "day",          # From today
    batchSize = num_posts,
    getComments = FALSE    # Don't fetch comments for faster execution
  )
  
  cat("✓ Successfully fetched", nrow(technology_posts), "posts from r/technology\n")
  
}, error = function(e) {
  cat("✗ Failed to fetch posts:", conditionMessage(e), "\n")
  stop("Post fetching failed")
})

# STEP 6: Display post summaries
cat("\n", paste(rep("=", 80), collapse = ""), "\n")
cat("TECHNOLOGY SUBREDDIT POSTS SUMMARY\n")
cat(paste(rep("=", 80), collapse = ""), "\n\n")

if (nrow(technology_posts) > 0) {
  for (i in 1:min(nrow(technology_posts), num_posts)) {
    post <- technology_posts[i, ]
    
    cat("Post", i, ":\n")
    cat("  Title:", substr(post$title, 1, 70), 
        if(nchar(post$title) > 70) "..." else "", "\n")
    cat("  Author: u/", post$author, "\n")
    cat("  Subreddit: r/", post$subreddit, "\n")
    
    # Note: Reddit API doesn't provide scores in search results for OAuth apps
    # This is a limitation of the Reddit API for application-only OAuth
    cat("  Score: [Not available via API]\n")
    
    cat("  URL:", post$url, "\n")
    cat("  Created:", format(post$created, "%Y-%m-%d %H:%M:%S"), "\n")
    cat("  ---\n\n")
  }
} else {
  cat("No posts found.\n")
}

# STEP 7: Optional - Save data to CSV file
save_to_csv <- readline(prompt = "Save results to CSV file? (y/n): ")

if (tolower(save_to_csv) == "y" || tolower(save_to_csv) == "yes") {
  # Prepare data for CSV export
  csv_data <- technology_posts %>%
    select(title, author, subreddit, url, created) %>%
    mutate(
      title = gsub("[\\r\\n]", " ", title),  # Remove line breaks
      created = format(created, "%Y-%m-%d %H:%M:%S")
    )
  
  # Generate filename with current date
  filename <- paste0("reddit_technology_posts_", 
                     format(Sys.Date(), "%Y%m%d"), ".csv")
  
  # Write to CSV
  tryCatch({
    write.csv(csv_data, file = filename, row.names = FALSE)
    cat("✓ Data saved to:", filename, "\n")
  }, error = function(e) {
    cat("✗ Failed to save CSV:", conditionMessage(e), "\n")
  })
} else {
  cat("CSV export skipped.\n")
}

# STEP 8: Additional usage examples and tips
cat("\n", paste(rep("=", 80), collapse = ""), "\n")
cat("ADDITIONAL USAGE EXAMPLES\n")
cat(paste(rep("=", 80), collapse = ""), "\n\n")

cat("1. Search for specific keywords in r/technology:\n")
cat("   technology_ml_posts <- runSearch(\n")
cat("     userAgent = user_agent,\n")
cat("     headers = header,\n")
cat("     keywords = c('machine learning', 'artificial intelligence'),\n")
cat("     subreddits = 'technology',\n")
cat("     batchSize = 20\n")
cat("   )\n\n")

cat("2. Get posts from multiple subreddits:\n")
cat("   tech_posts <- runSearch(\n")
cat("     userAgent = user_agent,\n")
cat("     headers = header,\n")
cat("     keywords = 'AI',\n")
cat("     subreddits = c('technology', 'programming', 'MachineLearning'),\n")
cat("     batchSize = 10\n")
cat("   )\n\n")

cat("3. Search posts with comments:\n")
cat("   posts_with_comments <- runSearch(\n")
cat("     userAgent = user_agent,\n")
cat("     headers = header,\n")
cat("     keywords = 'quantum computing',\n")
cat("     subreddits = 'technology',\n")
cat("     getComments = TRUE,\n")
cat("     maxComments = 50\n")
cat("   )\n\n")

cat("4. Time-based filtering:\n")
cat("   recent_posts <- runSearch(\n")
cat("     userAgent = user_agent,\n")
cat("     headers = header,\n")
cat("     keywords = 'innovation',\n")
cat("     subreddits = 'technology',\n")
cat("     time = 'week',  # Options: hour, day, week, month, year, all\n")
cat("     sort = 'top'    # Options: relevance, new, hot, top, comments\n")
cat("   )\n\n")

cat("✓ Example completed successfully!\n")
cat("For more information, check the package documentation: ?runSearch\n")