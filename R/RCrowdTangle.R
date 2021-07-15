# ----- RCrowdTangle.R -----
#
# Fork of cpbuschmann's RCrowdTangle library
#
# CC-By Jan Eggers


require("jsonlite")
require("dplyr")


# ---- Definition section ----

# Versions as date strings
ctRVersion <- "2021-07-14"
apiVersion <- "2021"

# Rate limits: 
# Default rate limit is 6 calls/minute, except for /links calls, who are
# limited to 2 calls/minute. Every function waits for the appropriate time
# beforte trying again. If not needed, just set to NULL. 

ct_api_limit <- function(calls = 6, links=FALSE) {
  if (calls > 0) {
    if(links) {
      apiWaitLinks <<- 60/calls
    } else {
      # Default wait
      apiWait <<- 60/calls
    }
  }
}

ct_api_limit(6)
ct_api_limit(2,links=TRUE)

# Global variables for last access of API to determine wait
apiWaitTill <- NULL

# Helper functions

# Call before accessing the API. 
ct_wait <- function(t = apiWait) {
  if (!is.null(apiWaitTill)) {
    # If before wait point, wait. 
    while(now() < apiWaitTill) {
      Sys.sleep(1)
    }
  }
  # Save new wait point
  apiWaitTill <<- now()+t
}

# Global variable to store token from environment 
ct_token <- NULL

# ---- Manage API token ---- TODO
# Takes token and stores it in the .Renviron file for future reference
require(stringr)

ct_auth <- function(token = NULL, overwrite = FALSE) {
  # Nicked large parts of this function from Bene Witzenberger's great
  # datawRappr package. 
  # It just reads the .Renviron file, looks if the API key already exists, 
  # returns a warning if it does (or overwrites, if ordered to),
  # or adds it, if necessary
  
  # Access global environment file:
  filename <- paste0(Sys.getenv("HOME"), "/.Renviron")
  
  # If no token is given, read key from ./Renviron and proceed to trying out
  # immediately. 
  if (is.null(token)) {
    token <- Sys.getenv("CROWDTANGLE_API_TOKEN")
    if (token=="") stop("No token set")
  } else {
    # Get on with it. Write key (or ignore if already exists and no overwrite)
    hook_name <- "CROWDTANGLE_API_TOKEN"
    
    if (!file.exists(filename)) { # create .Renviron, if it doesn't exist
      file.create(filename)
      warning("No Renviron file found")
    }
    
    # check if key already exists - if yes, check for overwrite = TRUE, else: write new key
    if (Sys.getenv(token) != "") {
      
      if (overwrite == TRUE) {
        
        
        # base R-solution:
        txt_vector <- readLines(filename, n = -1)
        lines_delete <- which(grepl(paste0("^",hook_name,".*$"), txt_vector))
        output_txt <- txt_vector[-lines_delete]
        
        # add new key:
        new_key <- paste0(hook_name,' = ', token)
        output_txt <- c(output_txt, new_key)
        writeLines(output_txt, filename)
        
        # reload Renviron after changing it:
        readRenviron(filename)
        
      } else if (overwrite == FALSE) { # if key exists, but overwrite is FALSE: throw warning and end function
        # Query existing token 
        token <- Sys.getenv("CROWDTANGLE_API_TOKEN")
        
        warning(paste0("API key ", hook_name, " already exists on this system.\nSet `overwrite = TRUE` to delete it."), immediate. = TRUE)
        
      }
      
      
    } else {
      
      # write new Key to to environment file
      new_key <- paste0(hook_name,' = ', token)
      write(new_key, file = filename, append = TRUE)
      readRenviron(filename)
    }
    
  }  
  # Try accessing the API with token; just a blank default query
  endpoint.posts <- "https://api.crowdtangle.com/posts"
  query.string <- paste0(endpoint.posts, "?token=", token)
  ct_wait(apiWait)
  response.json <- try(fromJSON(query.string), silent = TRUE)
  # Check if query returned OK 
  if (response.json$status==200) {
    # Set global variable to token
    ct_token <<- token
    return("OK")
  } else {
    stop(paste0("Token not valid - returns ",response.json$status))
  }
}

# not_run
# ct_auth("your_api_key_here",overwrite=TRUE)
# Read with Sys.getenv("CROWDTANGLE_API_TOKEN")


# ---- API call: links ----
# Retrieve a set of posts matching a certain link. This will return up to 
# 1000 posts. 

ct_get_links <- function(x = "", platforms = "", count = 100, 
                         startDate = "", endDate = "", 
                         token = NULL)
{
  # if no token is given, try to retrieve token from environment
  if (is.null(token)) {
    # No token given? Try to read it from global variable. 
    if (is.null(ct_token)) {
    # Call auth function without parameters - writes API token to 
    # global variable ct_token and stops if no API token is set
      ct_auth()
    }
    token <- ct_token
    token <- Sys.getenv("CROWDTANGLE_API_TOKEN")
  }
  # get on with it!
  endpoint.links <- "https://api.crowdtangle.com/links"
  query.string <- paste0(endpoint.links, 
                         "?link=", x, 
                         "&platforms=", platforms, 
                         "&count=", count, 
                         "&startDate=", startDate, "&endDate=", endDate, 
                         "&token=", token)
  ct_wait(apiWaitList)
  response.json <- try(fromJSON(query.string), silent = TRUE)
  if (!class(response.json) == "try-error")
  {
    status <- response.json$status
    postcount <- nrow(response.json$result$posts)
    if (status == 200 & !is.null(postcount))
    {
      nextpage <- response.json$result$pagination$nextPage
      posts <- response.json$result$posts
      if("expandedLinks" %in% colnames(posts)) posts <- select(posts, -expandedLinks)
      if("media" %in% colnames(posts)) posts <- select(posts, -media)
      posts <- jsonlite::flatten(posts)
      # Wait a fraction of a second to stay below the API rate limit
      return(posts)
    }
  }
}

# ---- API call: posts ----
# Retrieve a set of posts for the given parameters.


ct_get_posts <- function(x = "", searchTerm = "", 
                         language = "", types= "", 
                         minInteractions = 0, count = 100, 
                         startDate = "", endDate = "", 
                         token = NULL)
{
  # if no token is given, try to retrieve token from environment
  if (is.null(token)) {
    # No token given? Try to read it from global variable. 
    if (is.null(ct_token)) {
      # Call auth function without parameters - writes API token to 
      # global variable ct_token and stops if no API token is set
      ct_auth()
    }
    token <- ct_token
    token <- Sys.getenv("CROWDTANGLE_API_TOKEN")
  }
  # get on with it!
  endpoint.posts <- "https://api.crowdtangle.com/posts"
  query.string <- paste0(endpoint.posts, 
                         "?listIds=", x, 
                         "&searchTerm=", searchTerm, 
                         "&language=", language, 
                         "&types=", types, 
                         "&minInteractions=", minInteractions, 
                         "&count=", count, 
                         "&startDate=", startDate, 
                         "&endDate=", endDate, 
                         "&token=", token)
  ct_wait(apiWait)
  response.json <- try(fromJSON(query.string), silent = TRUE)
  status <- response.json$status
  if (status == 200)
  {
    nextpage <- response.json$result$pagination$nextPage
    posts <- response.json$result$posts %>% select(-expandedLinks, -media) %>% flatten()
    return(posts)
  } else { 
    # return error 
  }
}


# ---- API call: posts-search ----
# Retrieve a set of posts for the given parameters.


ct_search_posts <- function(x = "", and = "", not = "", 
                            inAccountIds = "", inListIds = "", 
                            notInAccountIds = "", notInListIds = "", 
                            notInTitle = "", platforms = "", types= "", 
                            minInteractions = 0, minSubscriberCount = 0, 
                            verifiedOnly = "false",  count = 100, 
                            startDate = "", endDate = "", token = "")
{
  # if no token is given, try to retrieve token from environment
  if (is.null(token)) {
    # No token given? Try to read it from global variable. 
    if (is.null(ct_token)) {
      # Call auth function without parameters - writes API token to 
      # global variable ct_token and stops if no API token is set
      ct_auth()
    }
    token <- ct_token
    token <- Sys.getenv("CROWDTANGLE_API_TOKEN")
  }
  # get on with it!
  endpoint.posts <- "https://api.crowdtangle.com/posts"
  query.string <- paste0(endpoint.posts,
                         "?searchTerm=", x,
                         "&and=", and,
                         "&or=", or,
                         "&inAccountIds=", inAccountIds,
                         "&inListIds=", inListIds,
                         "&inAccountIds=", inAccountIds,
                         "&inAccountIds=", inAccountIds,
                         "&inAccountIds=", inAccountIds,

                         "&language=", language, "&types=", 
                         types, "&minInteractions=", minInteractions, 
                         "&count=", count, 
                         "&startDate=", startDate, "&endDate=", endDate, 
                         "&token=", token)
  ct_wait(apiWait)
  response.json <- try(fromJSON(query.string), silent = TRUE)
  status <- response.json$status
  nextpage <- response.json$result$pagination$nextPage
  posts <- response.json$result$posts %>% select(-expandedLinks, -media) %>% flatten()
  return(posts)
}


# ---- API call: /posts/:id ---- 
# Retrieves a specific post. There are two versions of this endpoint, depending 
# upon what you need. Both return the same data. Please note that you must use 
# a dashboard token that corresponds to the post platform - i.e. an Instagram 
# token for Instagram posts, and a Facebook token for Facebook posts.
# 
# Please also note that the ID format for Facebook and Instagram are different. 
# For Instagram, it's [post_id]_[page_id], while for Facebook, 
# it's [page_id]_[post_id]. While Page and Post IDs can be found in Facebook 
# post URLs, Instagram does not expose the IDs in its URLs. You can pull 
# the necessary Instagram IDs from our API.

ct_get_post_by_id <- function(id = NULL, 
                         redditAccount = NULL,
                         includeHistory = FALSE,
                         token = NULL)
{
  # if no token is given, try to retrieve token from environment
  if (is.null(token)) {
    # No token given? Try to read it from global variable. 
    if (is.null(ct_token)) {
      # Call auth function without parameters - writes API token to 
      # global variable ct_token and stops if no API token is set
      ct_auth()
    }
    token <- ct_token
  }
  # get on with it!
  # Error check: No valid ID?
  if (is.null(id)) stop("No ID given")
  
  endpoint.posts <- "https://api.crowdtangle.com/post"
  query.string <- paste0(endpoint.posts, "/",id,
                         "?",
                        ifelse(is.null(redditAccount),"",
                               paste0("redditAccount=",
                                      redditAccount,"&")),
                        ifelse(includeHistory,
                               "includeHistory=1&",""),
                         "token=", token)
  ct_wait(apiWait)
  response.json <- try(fromJSON(query.string), silent = TRUE)
  status <- response.json$status
  if (status == 200)
  {
    nextpage <- response.json$result$pagination$nextPage
    posts <- response.json$result$posts %>% select(-expandedLinks, -media) %>% flatten()
    return(posts)
  } else { 
    # return error 
  }
}

# Wrapper for Facebook and Instagram posts: Return data for post by link
# If you call this function in a dplyr pipeline (e.g. with mutate()),
# use rowwise(). 

ct_get_fb_post <- function (link="",
                includeHistory = FALSE,
                token = NULL) {
  if (link=="") stop("No Link")
  # Zahl vor dem Wort "post"
  page_id <- str_extract(link,"[0-9]+(?=\\/posts)")
  # Zahl am Ende des Links
  post_id <- str_extract(link,"[0-9]+$")
  return(ct_get_post_by_id(
    id = paste0(page_id,"_",post_id),
    includeHistory = includeHistory,
    token = token))
} 


# ---- Instagram post query ----
# This is more than a simple wrapper for ct_get_post_by_id:
# To query the stats for an Instagram post, you have to grab
# the post ID from the source code, and the profile page of the source, 
# and the page ID from that source.

# Source URL: https://www.instagram.com/p/CQ1iXf_tJ37/

# Instagram page id of post can be grabbed from 
# <meta property="instapp:owner_user_id" content="3173761227" />
# Instagram post id can be grabbed from 
# <meta property="al:ios:url" content="instagram://media?id=2609142707615211003" />
# or calculated from code, using the helper fn InstaToPostID() below.

# Just to keep track of this: if you query 
# https://www.instagram.com/p/CQ1iXf_tJ37/?__a=1
# a post is returned as a JSON with all relevant info.

# Helper function to convert the Instagram URL to a post ID, 
# which you need to query Crowdtangle for a single Insta post. 
#
# Makes use of the gmp library which is for handling, like, really big figures.
# It *does* work if you try to use integers but whether it works correctly
# may depend on how double and integer are represented on your very system - 
# max number is 64^10-1. 
#
# Are you sure you won't want to use that obscure gmp library?

library(gmp)
library(stringr)
b64_str_split <- unlist(
  strsplit("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_",""))
# 
instaToPostID <- function(x = "x") {
  # Function expects either a url, or the isolated code. 
  # If url, extract. 
  if (str_detect(x,"instagram\\.com")) {
    # Extract instagram.com/p/ as well as instagram.com/tv/
    x <- str_extract(x,"(?<=instagram\\.com\\/p\\/)[a-zA-Z0-9_\\-]+|(?<=instagram\\.com\\/tv\\/)[a-zA-Z0-9_\\-]+")
    if (is.na(x)) stop("Could not extract URL ID code")
  }
  # convert input string to vector of integers
  t <- unlist(strsplit(x,""))
  lt <- length(t)
  r = as.bigz(0L)
  for (i in 1:lt) {
    r <- r+((which(b64_str_split==t[i])-1)*as.bigz(64)^(lt-i))
  }
  return(as.character(r))
}


ct_get_insta_post <- function (link="",
                            includeHistory = FALSE,
                            token = NULL) {
  if (link=="") stop("No Link")
  # Zahl vor dem Wort "post"
  page_id <- str_extract(link,"[0-9]+(?=\\/posts)")
  # Zahl am Ende des Links
  post_id <- str_extract(link,"[0-9]+$")
  return(ct_get_post_by_id(
    id = paste0(page_id,"_",post_id),
    includeHistory = includeHistory,
    token = token))
} 


# ---- API call: /posts/search ---- TODO
# ** Note: Access to the Search is restricted to a limited set customers** 
# and usage requires prior approval by CrowdTangle. 
# 
# Retrieve a set of posts for the given parameters and search terms. 
# This endpoint, unlike the main /posts endpoint, searches the entire, 
# cross-platform CrowdTangle system of posts. It can be limited by lists and 
# accounts, but by default will search beyond the dashboard the token is 
# associated with.


# ---- API call: /leaderboard ---- TODO
# Retrieves leaderboard data for a certain list or set of accounts.


# ---- API call: /lists ---- TODO
# Retrieve the lists, saved searches and saved post lists of the dashboard 
# associated with the token sent in.


# ---- API call: /lists/:listid/accounts ---- TODO
# Retrieve the accounts for a given list. Accounts may only be retrieved for 
# lists of type LIST, as saved searches and saved posts do not have 
# associated accounts.

# ---- API call: /ctpost/:id ---- TODO
# Gives data on Crowdtangle post - only glimpsed from the demo json file at
# https://ct-staticfiles.s3-us-west-1.amazonaws.com/api/API-Demo-2020.postman_collection.json
