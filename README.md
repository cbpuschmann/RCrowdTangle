# RCrowdTangle
A Wrapper To Retrieve Data From The CrowdTangle API

This package provides programmatic accces to the [CrowdTangle API](https://help.crowdtangle.com/en/articles/1189612-crowdtangle-api) with R. Users need to have a [CrowdTangle](https://www.crowdtangle.com/) account in order to make API calls. 

The package currently supports using the *links* and *posts* endpoints of the CrowdTangle API to retrieve data on specific URLs and posts as well as searching for posts. 

## Function calls

- **ct_auth(token, overwrite=FALSE)** - Set Crowdtangle token as an environment variable
- **ct_get_links()** Call Links endpoint (consult [CT API documentation](https://github.com/CrowdTangle/API/wiki/Links) )
- **ct_get_posts()** Call Posts endpoint (consult [CT API documentation](https://github.com/CrowdTangle/API/wiki/posts) )
- **ct_search_posts** - Basically ct_get_posts() with a focus on search terms.

## Examples

(TODO) 

## Rate limit

The default rate limit for CrowdTangle API calls is 6 per minute (with the exception
of the /links call which is limited to 2 calls per minute). The function calls
wait for a fraction of a second before returning. 

If you wish to change the rate limit to something lower, use

- **ct_set_api_limit(n)** 
- **ct_set_api_limit(n, links=TRUE)**

to set the limit to n calls per minute.

This sets a simple Sys.sleep() delay - assuming the return time of the API call itself
is negligible. 

## Todo

- 