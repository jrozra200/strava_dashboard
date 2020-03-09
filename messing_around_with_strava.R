## JUST TO TEST OUT THE API

library(rStrava)

## ESTABLISH THE APP CREDENTIALS
name <- 'jakelearnsdatascience' 
client_id  <- '31528' 
secret <- 'MY_SECRET_KEY'

## CREATE YOUR STRAVA TOKEN
token <- httr::config(token = strava_oauth(name, client_id, secret, 
                                           cache = TRUE, 
                                           app_scope = "read,read_all,activity:read,activity:read_all,profile:read_all")) 

stoken <- httr::config(token = readRDS('.httr-oauth')[[1]])

#REGINA -> 6034935
#JAKE -> 5954092
#JEANA -> 51784376

myinfo <- get_athlete(stoken, id = '6034935')
