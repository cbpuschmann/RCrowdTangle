# RCrowdTangle
A Wrapper To Retrieve Data From The CrowdTangle API

This package provides programmatic accces to the [CrowdTangle API](https://help.crowdtangle.com/en/articles/1189612-crowdtangle-api) with R. Users need to have a [CrowdTangle](https://www.crowdtangle.com/) account in order to make API calls. 

The package currently supports using the *links* and *posts* endpoints of the CrowdTangle API, as well as the *post/:id* query (Facebook only!) to retrieve data on specific URLs and posts as well as searching for posts. 

## Installing

Download the RCrowdTangle.R file from the R folder, place it in your 
working directory, and include it with ```source("RCrowdTangle.R")```

As it's not a proper R library *yet*, installing it with

```devtools::install_github("untergeekDE/RCrowdTangle")```

will lead to errors. Yet.

## Function calls

- **ct_auth(token, overwrite=FALSE)** - Set Crowdtangle token as an environment variable
- **ct_get_links()** Call Links endpoint (consult [CT API documentation](https://github.com/CrowdTangle/API/wiki/Links) )
- **ct_get_posts()** Call Posts endpoint (consult [CT API documentation](https://github.com/CrowdTangle/API/wiki/posts) )
- **ct_search_posts** - Basically ct_get_posts() with a focus on search terms.
- **ct_get_post_by_id(id)** - Call Post by ID endpoint (consult [CT API doc](https://github.com/CrowdTangle/API/wiki/Posts#get-postid)) 
- **ct_get_fb_post(url)** - Return information on single FB post

## Examples

(TODO) 

## Rate limit

The default rate limit for CrowdTangle API calls is 6 per minute (with the exception
of the /links call which is limited to 2 calls per minute). The function calls
wait for a fraction of a second before returning. 

If you wish to change the rate limit to something lower, use

- **ct_set_api_limit(n)** 
- **ct_set_api_limit(n, links=TRUE)**

to set the limit to n calls per minute. Whenever a query is done, a timer is set
via the 

## Todo

- Examples and use cases
- Convert to a proper R library (anybody any advice how to do this?)
- /posts/search call (invitation only!)
- /leaderboard call
- /lists call
- /lists/:listid/accounts call
- /ctpost/:id call (hidden, possibly deprecated)
- clean up the rather messy parameter structure for the calls
- Error handling
