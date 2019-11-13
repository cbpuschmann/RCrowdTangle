ct_get_links <- function(x = "", platforms = "", count = 100, startDate = "", endDate = "", token = "")
{
  endpoint.links <- "https://api.crowdtangle.com/links"
  query.string <- paste0(endpoint.links, "?link=", x, "&platforms=", platforms, "&count=", count, "&startDate=", startDate, "&endDate=", endDate, "&token=", token)
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
      return(posts)
    }
    else if (status == 429)
    {
      print("API rate limit hit, sleeping...")
      Sys.sleep(60)
    }
  }
}

ct_get_posts <- function(x = "", searchTerm = "", language = "", types= "", minInteractions = 0, count = 100, startDate = "", endDate = "", token = "")
{
  endpoint.posts <- "https://api.crowdtangle.com/posts"
  query.string <- paste0(endpoint.posts, "?listIds=", x, "&searchTerm=", searchTerm, "&language=", language, "&types=", types, "&minInteractions=", minInteractions, "&count=", count, "&startDate=", startDate, "&endDate=", endDate, "&token=", token)
  response.json <- try(fromJSON(query.string), silent = TRUE)
  status <- response.json$status
  nextpage <- response.json$result$pagination$nextPage
  posts <- response.json$result$posts %>% select(-expandedLinks, -media) %>% flatten()
  return(posts)
}

ct_search_posts <- function(x = "", and = "", not = "", inAccountIds = "", inListIds = "", notInAccountIds = "", notInListIds = "", notInTitle = "", platforms = "", types= "", minInteractions = 0, minSubscriberCount = 0, verifiedOnly = "false",  count = 100, startDate = "", endDate = "", token = "")
{
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

                         "&language=", language, "&types=", types, "&minInteractions=", minInteractions, "&count=", count, "&startDate=", startDate, "&endDate=", endDate, "&token=", token)
  response.json <- try(fromJSON(query.string), silent = TRUE)
  status <- response.json$status
  nextpage <- response.json$result$pagination$nextPage
  posts <- response.json$result$posts %>% select(-expandedLinks, -media) %>% flatten()
  return(posts)
}

