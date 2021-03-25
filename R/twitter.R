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

  res <- httr::GET(url, httr::add_headers(.headers=headers))
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

  res <- httr::GET(url, httr::add_headers(.headers=headers))
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
#' @importFrom dplyr left_join
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

#' Get list of accounts following a user
#'
#' This calls get_followers itteratively if pagination is needed
#'
#' @param user.id The Twitter user_id of the account whose followers you want to find
#'
#' @return A dataframe
#'
#' @examples
#' \dontrun{
#'   get_user_followers("13205222")
#' }
#' @export
get_user_followers <- function(user.id) {

  this_res <- get_followers(user.id)
  res <- this_res

  while (!is.null(this_res$meta$next_token)) {
    next_token <- res$meta_next_token
    this_res <- get_followers(user.id,res$meta$next_token)

    res <- rbind(res,this_res)
  }

  df <- json_followers_to_df(res)

  return(df)
}

#' Helper function for get_user_followers
#'
#' This gets called with pagination by get_user_followers
#'
#' @param user.id A Twitter user_id
#' @param next_token A pagination token from a previous API call.
#' @return A JSON object returned by the Twitter API
get_followers <- function(user.id, next_token='') {

  params = list(
    "max_results" = 1000,
    "user.fields" = "created_at,description,entities,pinned_tweet_id,public_metrics,url,verified"
    #"expansions" = "pinned_tweet_id"
    )

  if (next_token != '') {
    params[["pagination_token"]] <- next_token
  }

  url <- paste0("https://api.twitter.com/2/users/",user.id,"/followers")

  res <- httr::GET(url, httr::add_headers(.headers=headers),query=params)
  res <- httr::content(res)
  return(res)


}

#' Turn JSON list of users into a tidy dataframe
#'
#' @param res A json object returned by the Twitter API with info about a users followers
#'
#' @return A dataframe with 11 columns
#'
#' @importFrom tidyjson enter_object
#' @importFrom tidyjson gather_array
#' @importFrom tidyjson spread_all
#' @importFrom dplyr left_join
json_followers_to_df <- function(res) {
	df <- res$data %>% spread_all %>% select(document.id,id,name,username,description,pinned_tweet_id,verified,created_at) %>% as_tibble
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

  this_res <- get_timeline(user.id, since.id, start.time, end.time)
  res <- this_res

  while (!is.null(this_res$meta$next_token)) {
    next_token <- res$meta_next_token
    this_res <- get_followers(user.id,res$meta$next_token)

    res <- rbind(res,this_res)
  }

  df <- json_tweets_to_df(res)

  return(df)
}


#' Helper function for get_user_timeline
#'
#' @param user.id A Twitter user_id
#'
#' @return A dataframe
#'
#' @importFrom httr GET
#' @importFrom httr content
#' @importFrom httr add_headers
get_timeline <- function(user.id, since.id, start.time, end.time) {

  if(!missing(since.id)) {

  }

  params = list(
    "max_results" = 100,
    "tweet.fields" = "id,author_id,created_at,text,geo,source,entities,public_metrics",  #skipping context_annotations for now
    "expansions" = "geo.place_id",
    "place.fields"="contained_within,country,country_code,full_name,geo,id,name,place_type"
    )

  url <- paste0("https://api.twitter.com/2/users/",user.id,"/tweets")

  res <- httr::GET(url, httr::add_headers(.headers=headers),query=params)
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
#' @importFrom dplyr left_join
json_tweets_to_df <- function(res) {
  df <- res$data %>% spread_all %>% select(document.id,id, author_id, created_at, text, source) %>% as_tibble
  pmetrics <- res$data %>% enter_object(public_metrics) %>% spread_all %>% as_tibble
  df <- df %>% left_join(pmetrics) %>% select(-document.id)

  return(df)
}



