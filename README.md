# redditApiR

redditApiR is an R package designed to interact with the Reddit API, providing functions to search, retrieve, and organize Reddit data efficiently. The package helps in extracting posts and comments from specific subreddits using keywords or author names and handles pagination to retrieve useful data for performing data science tasks. The main function "runSearch" offers many customizable parameters to meet most data retrieval needs, however only the user-agent param is necessary. Default parameters allow for ease of use for new api users. Further information can be found under vingettes and help/man.

## Installation

You can install the redditApiR package directly from GitHub using the remotes or devtools package:

### Prerequisites

- R version 3.5 or higher
- The remotes or devtools package installed
- an authenticated reddit account
  
install.packages("devtools")

library(devtools)

devtools::install_github("DanielDataGit/redditApiR")

library(redditApiR)
