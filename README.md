# CivicTwitter
CivicTwitter R Package

## Basic Usage

#### Set bearer token

You must do this first as the API calls require this authorization header to work

```r
## set bearer token
bearer_token <- '#your_bearer_token_here#'
headers <- set_bearer_token(bearer_token)
```

#### Get User Info

```r
## by username
df <- get_user_by_username("SNFAgoraJHU")
```

```r
## by userid
df <- get_user_by_id("13205222")
```

#### Get Followers of a User

```r
## requires user_id
df <- get_user_followers("1164992903468343297")
```

### Get user timeline tweets

```r
## optional parameters for since.id, start.time, and end.time
df <- get_user_timeline("26657119")
```
