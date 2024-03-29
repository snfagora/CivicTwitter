#' Set the bearer token for API authentication
#'
#' need to run this first to set the headers variable which is used in API calls
#'
#' @param bearer_token An API bearer token from a credentialed twitter app
#'
#' @export
set_bearer_token <- function(bearer_token) {
  headers <- c(`Authorization` = sprintf('Bearer %s', bearer_token))

  return(headers)
}


#' Get details about a specified user by user_id
#' see https://developer.twitter.com/en/docs/twitter-api/users/lookup/api-reference/get-users-id
#'
#' @param user.id A Twitter user_id
#' @return A tibble
#' @export
get_user_by_id <- function(user.id) {
  user.fields <- 'created_at,description,entities,pinned_tweet_id,public_metrics,url,verified&expansions=pinned_tweet_id'

  url <- paste0("https://api.twitter.com/2/users/",user.id,"?user.fields=",user.fields)

  headers <- set_bearer_token(bearer_token)
  res <- httr::GET(url, httr::add_headers(.headers=headers))

  waited <- check_rate_limit_header(res)
  if (waited==1) {
    res <- httr::GET(url, httr::add_headers(.headers=headers),query=params)  # resend last request
  }

  res <- httr::content(res)

  df <- json_user_to_df(res)
  return(df)
}

#' Get details about a specified user by username
#' see https://developer.twitter.com/en/docs/twitter-api/users/lookup/api-reference/get-users-id
#'
#' @param username A Twitter username
#' @return A tibble
#' @export
get_user_by_username <- function(username) {
  user.fields <- 'created_at,description,entities,pinned_tweet_id,public_metrics,url,verified'

  url <- paste0("https://api.twitter.com/2/users/by/username/",username,"?user.fields=",user.fields)

  headers <- set_bearer_token(bearer_token)
  res <- httr::GET(url, httr::add_headers(.headers=headers))

  waited <- check_rate_limit_header(res)
  if (waited==1) {
    res <- httr::GET(url, httr::add_headers(.headers=headers),query=params)  # resend last request
  }

  res <- httr::content(res)

  df <- json_user_to_df(res)
  return(df)
}


#' Turn JSON object for single users into a tidy dataframe
#'
#' @param res A json object returned by the Twitter API with info about a user
#'
#' @return A dataframe with 11 columns
#'
#' @importFrom tidyjson enter_object
#' @importFrom tidyjson gather_array
#' @importFrom tidyjson spread_all
#' @importFrom tidyjson as_tibble
#' @importFrom dplyr left_join
#' @importFrom dplyr select
#' @export
json_user_to_df <- function(res) {
	url <- res$data$entities$url$urls[[1]]$expanded_url
	desc_url <- res$data$entities$description$urls[[1]]$expanded_url

	df <- res %>% spread_all %>% select(document.id,id,name,username,description,verified,created_at) %>% as_tibble
	pmetrics <- res %>% enter_object(public_metrics) %>% spread_all %>% as_tibble

	#url <- res %>% enter_object(entities) %>% enter_object(url) %>% enter_object(urls) %>% gather_array %>% spread_all %>% as_tibble %>% select(document.id,expanded_url)

	df <- df %>% left_join(pmetrics) %>% mutate(url=url, description_url=desc_url) %>% select(-document.id)

	return(df)
}

#' Get list of accounts a user is following
#'
#' This calls get_following itteratively if pagination is needed
#'
#' @param user.id The Twitter user_id of the account whose following you want to find
#'
#' @return A dataframe
#'
#' @examples
#' \dontrun{
#'   get_user_following("13205222")
#' }
#' @export
get_user_following <- function(user.id) {

  #check_rate_limit("following")
  this_res <- get_following(user.id)
  df <- json_following_to_df(this_res)

  while (!is.null(this_res$meta$next_token)) {
    #check_rate_limit("following")
    this_res <- get_following(user.id,this_res$meta$next_token)
    this_df <- json_following_to_df(this_res)

    df <- rbind(df,this_df)
  }

  return(df)
}

#' Helper function for get_user_following
#'
#' This gets called with pagination by get_user_following
#'
#' @param user.id A Twitter user_id
#' @param next_token A pagination token from a previous API call.
#' @return A JSON object returned by the Twitter API
#' @export
get_following <- function(user.id, next_token) {

  headers <- set_bearer_token(bearer_token)
  params = list(
    "max_results" = 1000,
    "user.fields" = "created_at,description,entities,location,pinned_tweet_id,public_metrics,url,verified"
    #"expansions" = "pinned_tweet_id"
    )

  if (!missing(next_token)) {
    params[["pagination_token"]] <- next_token
  }

  url <- paste0("https://api.twitter.com/2/users/",user.id,"/following")

  res <- httr::GET(url, httr::add_headers(.headers=headers),query=params)

  # check for rate limit here. maybe more elegant to make this a while loop.
  waited <- check_rate_limit_header(res)
  if (waited==1) {
    res <- httr::GET(url, httr::add_headers(.headers=headers),query=params)  # resend last request
  }

  res <- httr::content(res)
  return(res)


}

#' Get list of accounts following a user
#'
#' This calls get_followers itteratively if pagination is needed
#'
#' @param user.id The Twitter user_id of the account whose following you want to find
#' @param max.pages The maximum number of pages of results to query. Each page returns 1000 users
#'
#' @return A dataframe
#'
#' @examples
#' \dontrun{
#'   get_user_followers("13205222")
#' }
#' @export
get_user_followers <- function(user.id, max.pages=100) {
  cur.page <- 1

  #check_rate_limit("following")
  this_res <- get_followers(user.id)
  df <- json_following_to_df(this_res)

  while (!is.null(this_res$meta$next_token) & cur.page <= max.pages) {
    #check_rate_limit("following")
    this_res <- get_followers(user.id,this_res$meta$next_token)
    this_df <- json_following_to_df(this_res)

    df <- rbind(df,this_df)
    cur.page <- cur.page + 1
  }

  return(df)
}


#' Helper function for get_user_followers
#'
#' This gets called with pagination by get_user_followers
#'
#' @param user.id A Twitter user_id
#' @param next_token A pagination token from a previous API call.
#' @return A JSON object returned by the Twitter API
#' @export
get_followers <- function(user.id, next_token) {

  headers <- set_bearer_token(bearer_token)
  params = list(
    "max_results" = 1000,
    "user.fields" = "created_at,description,location,entities,pinned_tweet_id,public_metrics,url,verified"
    #"expansions" = "pinned_tweet_id"
  )

  if (!missing(next_token)) {
    params[["pagination_token"]] <- next_token
  }

  url <- paste0("https://api.twitter.com/2/users/",user.id,"/followers")

  res <- httr::GET(url, httr::add_headers(.headers=headers),query=params)

  # check for rate limit here. maybe more elegant to make this a while loop.
  waited <- check_rate_limit_header(res)
  if (waited==1) {
    res <- httr::GET(url, httr::add_headers(.headers=headers),query=params)  # resend last request
  }

  res <- httr::content(res)
  return(res)
}




#' Turn JSON list of users into a tidy dataframe
#'
#' @param res A json object returned by the Twitter API with info about a users following
#'
#' @return A dataframe with 11 columns
#'
#' @importFrom tidyjson enter_object
#' @importFrom tidyjson gather_array
#' @importFrom tidyjson spread_all
#' @importFrom tidyjson as_tibble
#' @importFrom dplyr left_join
#' @importFrom dplyr select
json_following_to_df <- function(res) {
	df <- res$data %>% spread_all %>% select(document.id,id,name,username,description,location,pinned_tweet_id,verified,created_at) %>% as_tibble
	pmetrics <- res$data %>% enter_object(public_metrics) %>% spread_all %>% as_tibble
	urls <- res$data %>% enter_object(entities) %>% enter_object(url) %>% enter_object(urls) %>% gather_array %>% spread_all %>% as_tibble %>% select(document.id,expanded_url)

	df <- df %>% left_join(pmetrics) %>% left_join(urls) %>% select(-document.id)

	return(df)
}

#' Get user timeline tweets
#'
#' see https://developer.twitter.com/en/docs/twitter-api/tweets/timelines/api-reference/get-users-id-tweets
#'
#' @param user.id A Twitter user_id
#' @param since.id A tweet id. Function then retrieves tweets more recent then that id
#' @param start.time YYYY-MM-DDTHH:mm:ssZ (ISO 8601/RFC 3339)
#' @param end.time YYYY-MM-DDTHH:mm:ssZ (ISO 8601/RFC 3339)
#'
#' @return A dataframe
#'
#' @importFrom httr GET
#' @importFrom httr content
#' @importFrom httr add_headers
#' @export
get_user_timeline <- function(user.id, since.id, start.time, end.time) {

  if(!missing(start.time)) {
    this_res <- get_timeline(user.id, start.time=start.time, end.time=end.time)
  }

  if (!missing(since.id)) {
    this_res <- get_timeline(user.id, since.id=since.id)
  }

  if (missing(since.id) & missing(start.time) & missing(end.time)) {
    this_res <- get_timeline(user.id)
  }

  df <- json_tweets_to_df(this_res)

  while (!is.null(this_res$meta$next_token)) {
    next_token <- this_res$meta$next_token
    check_rate_limit("timeline")
    this_res <- get_timeline(user.id,this_res$meta$next_token)
    this_df <- json_tweets_to_df(this_res)

    df <- rbind(df,this_df)
  }

  return(df)
}


#' Helper function for get_user_timeline
#'
#' @param user.id A Twitter user_id
#' @param next_token A pagination token from a previous API call.
#' @param since.id A tweet id. Function then retrieves tweets more recent then that id
#' @param start.time YYYY-MM-DDTHH:mm:ssZ (ISO 8601/RFC 3339)
#' @param end.time YYYY-MM-DDTHH:mm:ssZ (ISO 8601/RFC 3339)
#'
#' @return A dataframe
#'
#' @importFrom httr GET
#' @importFrom httr content
#' @importFrom httr add_headers
get_timeline <- function(user.id, next_token, since.id, start.time, end.time) {

  headers <- set_bearer_token(bearer_token)

  params = list(
    "max_results" = 100,
    "tweet.fields" = "id,author_id,created_at,text,geo,source,entities,public_metrics",  #skipping context_annotations for now
    "expansions" = "geo.place_id",
    "place.fields"="contained_within,country,country_code,full_name,geo,id,name,place_type"
    )

  if(!missing(since.id)) {
    params[["since.id"]] <- since.id
  }

  if (!missing(next_token)) {
    params[["pagination_token"]] <- next_token
  }

  if(!missing(start.time)) {
    params[["start_time"]] <- start.time
  }

  if(!missing(end.time)) {
    params[["end_time"]] <- end.time
  }

  url <- paste0("https://api.twitter.com/2/users/",user.id,"/tweets")

  res <- httr::GET(url, httr::add_headers(.headers=headers),query=params)

  # check for rate limit here. maybe more elegant to make this a while loop.
  waited <- check_rate_limit_header(res)
  if (waited==1) {
    res <- httr::GET(url, httr::add_headers(.headers=headers),query=params)  # resend last request
  }

  res <- httr::content(res)
  return(res)
}

#' Turn JSON list of tweets into a tidy dataframe
#'
#' @param res A json object returned by the Twitter API with info about tweets
#'
#' @return A dataframe
#'
#' @importFrom tidyjson enter_object
#' @importFrom tidyjson gather_array
#' @importFrom tidyjson spread_all
#' @importFrom tidyjson as_tibble
#' @importFrom dplyr left_join
#' @importFrom dplyr select
json_tweets_to_df <- function(res) {
  df <- res$data %>% spread_all %>% select(document.id,id, author_id, created_at, text, source) %>% as_tibble
  pmetrics <- res$data %>% enter_object(public_metrics) %>% spread_all %>% as_tibble

  df <- df %>% left_join(pmetrics) %>% select(-document.id)

  return(df)
}

#' Get API Rate Limit Statuses
#'
#'
#'
#' @return A dataframe
#'
#' @importFrom httr GET
#' @importFrom httr add_headers
#' @importFrom tibble tibble
#' @export
rate_limits <- function() {
  url <- "https://api.twitter.com/1.1/application/rate_limit_status.json"

  #res <- httr::GET(url, httr::add_headers(.headers=headers),query=params)
  res <- httr::GET(url, httr::add_headers(.headers=headers))
  res <- httr::content(res)

  resources <- unlist(unname(res$resources), recursive = FALSE)
  df <- tibble(
    resource = names(resources),
    limit = unlist(lapply(resources, "[[", "limit"), use.names = FALSE),
    remaining = unlist(lapply(resources, "[[", "remaining"), use.names = FALSE),
    reset_at = unlist(lapply(resources, "[[", "reset"), use.names = FALSE),
  )
  df$reset_at <- .POSIXct(df$reset_at)
  df$reset <- round(difftime(df$reset_at, Sys.time(), units = "secs"))

  return(df)
}

#' Check Rate Limit Status and Sleep If Need Be
#'
#' @param rsrc A resource to check for rate limiting
#' @param threshold The remaining API calls below which to force a System Sleep.
#'
#' @return No object returned
#'
#' @importFrom dplyr filter
check_rate_limit <- function(rsrc, threshold=2) {
  rl <- rate_limits()

  # translate a few shorthands here. else just use the resource name as given
  if (rsrc == "following") {
  	rsrc <- "/users/:id/following"
  }
  if (rsrc == "timeline") {
  	rsrc <- "/users/:id/tweets"
  }

  rl <- rl %>% filter(resource == rsrc)

  # should error check here that rl has one row now
  if (rl$remaining < threshold) {
    cat(paste0("Approaching API threshould for ", rsrc,". Sleeping for ", rl$reset, "seconds..."))
    Sys.sleep(as.numeric(rl$reset))
    cat("continuing!\n")
  }

  return(rl)
}

#' Check Rate Limit Status and Sleep If Need Be - this is an updated function for API v2.0
#'
#' @param res The result of an httr call to the twitter API.
#' @param threshold The remaining API calls below which to force a System Sleep.
#'
#' @return binary indicator of whether the API calls had to wait
#'
#' @importFrom dplyr filter
#' @importFrom httr headers
#' @export
check_rate_limit_header <- function(res, threshold=2) {
  rl <- httr::headers(res)

  reset.at <- rl$`x-rate-limit-reset`
  reset.sec <- as.numeric(rl$`x-rate-limit-reset`) - as.numeric(Sys.time())

  # should error check here that rl has one row now
  waited <- 0
  if (as.numeric(rl$`x-rate-limit-remaining`) < threshold) {
    cat(paste0("Approaching API threshould for this resource. Sleeping for ", reset.sec, "seconds..."))
    Sys.sleep(as.numeric(reset.sec))
    cat("continuing!\n")
    waited <- 1
  }

  return(waited)
}

