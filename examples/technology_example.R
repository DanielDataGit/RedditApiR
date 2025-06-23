################################################################################
# RedditApiR Technology Subreddit Example
# 
# This script demonstrates how to use the RedditApiR package to authenticate 
# with the Reddit API and fetch posts from the r/technology subreddit.
#
# PREREQUISITES:
# 1. You need a Reddit account
# 2. Create a Reddit app at: https://www.reddit.com/prefs/apps
# 3. Note down your client_id, client_secret, and reddit username
# 4. Install required packages: devtools, httr, jsonlite, dplyr
#
# USAGE:
# 1. Replace the placeholder values in the "Authentication Setup" section
# 2. Run the script section by section
# 3. The script will fetch 10 posts from r/technology and display them
# 4. Optionally, uncomment the CSV export section to save data
################################################################################

# Install and load required packages
if (!require(devtools)) install.packages("devtools")
if (!require(httr)) install.packages("httr")
if (!require(jsonlite)) install.packages("jsonlite")
if (!require(dplyr)) install.packages("dplyr")

# Install RedditApiR package from GitHub
# devtools::install_github("DanielDataGit/redditApiR")

# Load libraries
library(httr)
library(jsonlite)
library(dplyr)
library(redditApiR)

################################################################################
# Authentication Setup
################################################################################

# REPLACE THESE VALUES WITH YOUR ACTUAL REDDIT APP CREDENTIALS
# Instructions: https://danieltlis4370.blogspot.com/2024/12/blog-post.html
client_id <- "YOUR_CLIENT_ID_HERE"        # From your Reddit app
client_secret <- "YOUR_CLIENT_SECRET_HERE" # From your Reddit app  
reddit_user <- "YOUR_REDDIT_USERNAME"      # Your Reddit username
reddit_password <- "YOUR_REDDIT_PASSWORD"  # Your Reddit password

# User agent string (required by Reddit API)
user_agent <- paste0("R:redditApiR:v1.0 (by /u/", reddit_user, ")")

# OAuth2 Authentication Function
authenticate_reddit <- function(client_id, client_secret, reddit_user, reddit_password, user_agent) {
  
  cat("Setting up Reddit OAuth2 authentication...\n")
  
  # Reddit OAuth2 endpoint
  token_url <- "https://www.reddit.com/api/v1/access_token"
  
  # Prepare authentication request
  auth_data <- list(
    grant_type = "password",
    username = reddit_user,
    password = reddit_password
  )
  
  # Make token request with basic authentication
  response <- POST(
    url = token_url,
    authenticate(client_id, client_secret),
    body = auth_data,
    encode = "form",
    add_headers(`User-Agent` = user_agent)
  )
  
  # Check if authentication was successful
  if (status_code(response) != 200) {
    stop("Authentication failed! Status code: ", status_code(response), 
         "\nResponse: ", content(response, "text"))
  }
  
  # Extract access token
  token_data <- content(response, "parsed")
  access_token <- token_data$access_token
  
  if (is.null(access_token)) {
    stop("Failed to obtain access token!")
  }
  
  cat("Authentication successful! Token obtained.\n")
  
  # Create headers for API requests
  headers <- c(
    Authorization = paste("bearer", access_token),
    `User-Agent` = user_agent
  )
  
  return(list(
    token = access_token,
    headers = headers,
    user_agent = user_agent
  ))
}

################################################################################
# Authentication and API Setup
################################################################################

# Authenticate with Reddit (uncomment and fill in credentials to use)
# auth_info <- authenticate_reddit(client_id, client_secret, reddit_user, reddit_password, user_agent)
# 
# # Extract authentication components
# header <- auth_info$headers
# user_agent <- auth_info$user_agent

# For demonstration purposes (when credentials are not available):
# Create dummy headers - REPLACE WITH ACTUAL AUTHENTICATION
header <- c(
  Authorization = "bearer YOUR_ACCESS_TOKEN_HERE",
  `User-Agent` = user_agent
)

cat("RedditApiR Authentication Setup Complete!\n")
cat("User Agent:", user_agent, "\n")
cat("Headers configured for API requests.\n\n")

################################################################################
# Fetch Posts from r/technology
################################################################################

cat("Fetching posts from r/technology subreddit...\n")

# Use runSearch function to fetch posts from r/technology
# Parameters:
# - userAgent: Required for Reddit API
# - headers: OAuth authentication headers
# - keywords: Search terms (use NA for all posts)
# - subreddits: Target subreddit(s)
# - batchSize: Number of posts to fetch (we want 10)
# - sort: Sort order for posts
# - time: Time range for posts
tryCatch({
  technology_posts <- runSearch(
    userAgent = user_agent,
    headers = header,
    keywords = NA,                    # No specific keywords - get all posts
    subreddits = "technology",        # Target the technology subreddit
    batchSize = 10,                   # Fetch 10 posts
    sort = "hot",                     # Get hot posts
    time = "day",                     # From the last day
    getComments = FALSE               # Don't fetch comments for faster retrieval
  )
  
  cat("Successfully fetched", nrow(technology_posts), "posts from r/technology!\n\n")
  
}, error = function(e) {
  cat("Error fetching posts:", e$message, "\n")
  cat("This is expected if authentication credentials are not properly configured.\n\n")
  
  # Create sample data for demonstration
  technology_posts <- data.frame(
    title = c(
      "New AI breakthrough announced",
      "Latest smartphone technology trends",
      "Quantum computing advances",
      "Cybersecurity update released",
      "Tech industry merger news"
    ),
    author = c("tech_user1", "gadget_lover", "quantum_fan", "security_pro", "industry_watcher"),
    created = as.POSIXct(Sys.time() - (1:5) * 3600),  # Sample timestamps
    subreddit = rep("technology", 5),
    url = paste0("https://reddit.com/r/technology/post_", 1:5),
    keyword = rep(NA, 5),
    stringsAsFactors = FALSE
  )
  
  cat("Using sample data for demonstration purposes.\n\n")
})

################################################################################
# Display Post Summary
################################################################################

cat("=== TECHNOLOGY SUBREDDIT POST SUMMARY ===\n\n")

if (nrow(technology_posts) > 0) {
  
  # Display each post with key information
  for (i in 1:min(10, nrow(technology_posts))) {
    cat("Post", i, ":\n")
    cat("  Title:", technology_posts$title[i], "\n")
    cat("  Author:", technology_posts$author[i], "\n")
    cat("  Created:", format(technology_posts$created[i], "%Y-%m-%d %H:%M:%S"), "\n")
    cat("  URL:", technology_posts$url[i], "\n")
    
    # Note: Score information is not currently extracted by the RedditApiR package
    # The Reddit API provides score data, but it's not included in the current implementation
    # Available fields from runSearch: title, body, author, created, keyword, subreddit, url
    
    cat("\n")
  }
  
  # Summary statistics
  cat("=== SUMMARY STATISTICS ===\n")
  cat("Total posts fetched:", nrow(technology_posts), "\n")
  cat("Subreddit:", unique(technology_posts$subreddit), "\n")
  cat("Date range:", 
      format(min(technology_posts$created, na.rm = TRUE), "%Y-%m-%d"), 
      "to", 
      format(max(technology_posts$created, na.rm = TRUE), "%Y-%m-%d"), "\n")
  
  # Display data structure information
  cat("\nData structure:\n")
  cat("Columns available:", paste(names(technology_posts), collapse = ", "), "\n")
  cat("Data types:\n")
  for (col in names(technology_posts)) {
    cat("  ", col, ":", class(technology_posts[[col]])[1], "\n")
  }
  
  # Most active authors
  if (nrow(technology_posts) > 1) {
    author_counts <- table(technology_posts$author)
    cat("Most active author:", names(author_counts)[which.max(author_counts)], 
        "(", max(author_counts), "posts )\n")
  }
  
} else {
  cat("No posts were retrieved.\n")
  cat("Please check your authentication credentials and try again.\n")
}

################################################################################
# Optional: Save Data to CSV
################################################################################

# Uncomment the following lines to save the fetched data to a CSV file
# 
# if (nrow(technology_posts) > 0) {
#   
#   # Prepare data for CSV export
#   csv_data <- technology_posts %>%
#     select(title, author, created, subreddit, url) %>%
#     mutate(
#       created = format(created, "%Y-%m-%d %H:%M:%S"),
#       title = gsub("[\n\r\t]", " ", title),  # Clean up titles for CSV
#       title = gsub("\"", "'", title)         # Replace quotes to avoid CSV issues
#     )
#   
#   # Save to CSV file
#   csv_filename <- paste0("technology_posts_", format(Sys.Date(), "%Y%m%d"), ".csv")
#   write.csv(csv_data, csv_filename, row.names = FALSE)
#   
#   cat("\n=== DATA EXPORT ===\n")
#   cat("Data saved to:", csv_filename, "\n")
#   cat("File contains", nrow(csv_data), "posts with the following columns:\n")
#   cat("  -", paste(names(csv_data), collapse = "\n  - "), "\n")
#   
#   # Display first few rows as preview
#   cat("\nPreview of exported data:\n")
#   print(head(csv_data, 3))
# }

################################################################################
# Additional Usage Examples
################################################################################

cat("\n=== ADDITIONAL USAGE EXAMPLES ===\n\n")

cat("1. Search for specific keywords in r/technology:\n")
cat('   technology_ai <- runSearch(\n')
cat('     userAgent = user_agent,\n')
cat('     headers = header,\n')
cat('     keywords = c("artificial intelligence", "machine learning"),\n')
cat('     subreddits = "technology",\n')
cat('     batchSize = 20\n')
cat('   )\n\n')

cat("2. Get recent posts with comments:\n")
cat('   recent_posts <- runSearch(\n')
cat('     userAgent = user_agent,\n')
cat('     headers = header,\n')
cat('     keywords = NA,\n')
cat('     subreddits = "technology",\n')
cat('     sort = "new",\n')
cat('     time = "week",\n')
cat('     batchSize = 5,\n')
cat('     getComments = TRUE,\n')
cat('     maxComments = 50\n')
cat('   )\n\n')

cat("3. Search by author:\n")
cat('   author_posts <- runSearch(\n')
cat('     userAgent = user_agent,\n')
cat('     headers = header,\n')
cat('     type = "author",\n')
cat('     keywords = c("specific_username"),\n')
cat('     subreddits = "technology",\n')
cat('     batchSize = 10\n')
cat('   )\n\n')

cat("For more examples and detailed documentation, see:\n")
cat("- Package vignettes: vignette('vignetteGuide', package = 'redditApiR')\n")
cat("- Function help: ?runSearch, ?searchReddit\n")
cat("- Authentication guide: https://danieltlis4370.blogspot.com/2024/12/blog-post.html\n")

################################################################################
# Troubleshooting Guide
################################################################################

cat("\n=== TROUBLESHOOTING GUIDE ===\n\n")

cat("Common issues and solutions:\n\n")

cat("1. Authentication Errors:\n")
cat("   - Verify your client_id and client_secret are correct\n")
cat("   - Check that your Reddit app type is set to 'script'\n")
cat("   - Ensure your username and password are correct\n")
cat("   - Make sure your Reddit account is not suspended\n\n")

cat("2. Rate Limiting:\n")
cat("   - Reddit API has rate limits (60 requests per minute)\n")
cat("   - If you get rate limited, wait a minute before retrying\n")
cat("   - Consider reducing batchSize for large requests\n\n")

cat("3. No Data Returned:\n")
cat("   - Check if the subreddit exists and is public\n")
cat("   - Try different time ranges (day, week, month)\n")
cat("   - Verify your search parameters are valid\n\n")

cat("4. Package Installation Issues:\n")
cat("   - Make sure you have devtools installed\n")
cat("   - Check your internet connection\n")
cat("   - Try: devtools::install_github('DanielDataGit/redditApiR', force = TRUE)\n\n")

cat("5. Missing Fields:\n")
cat("   - The package returns: title, body, author, created, keyword, subreddit, url\n")
cat("   - Score data is not currently extracted (feature request for future versions)\n")
cat("   - For more fields, you may need to modify the package functions\n\n")

cat("\n=== SCRIPT COMPLETED ===\n")
cat("Thank you for using RedditApiR!\n")