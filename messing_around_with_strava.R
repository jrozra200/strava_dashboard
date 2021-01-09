## JUST TO TEST OUT THE API

library(rStrava)

## ESTABLISH THE APP CREDENTIALS
name <- 'jakelearnsdatascience' 
client_id  <- '31528' 
secret <- 'MY_SECRET_KEY'

## CREATE YOUR STRAVA TOKEN
token <- httr::config(token = strava_oauth(name, client_id, secret, 
                                           cache = TRUE, 
                                           app_scope = "activity:read_all")) 

stoken <- httr::config(token = readRDS('.httr-oauth')[[1]])

#REGINA -> 6034935
#JAKE -> 5954092
#JEANA -> 51784376

myinfo <- get_athlete(stoken, id = '5954092')
jake <- get_activity_list(stoken)

for(i in 1:260){
    print(jake[[i]]$athlete$id)
}

rdc <- get_club(stoken, id = '592304', request = "activities")
as.data.frame(rdc)


library(calendR)
library(lubridate)

jake_df <- jake_df[jake_df$start_time_2 >= "2021-01-01", ]

cal_df <- data.frame(text = paste0(jake_df$type, "\n", round(jake_df$miles, 2), 
                                   " miles\n", round(jake_df$minutes, 2), 
                                   " minutes"),
                     position = day(jake_df$start_time_2))

calendR(year = year(Sys.Date()), 
        month = month(Sys.Date()),
        text = cal_df$text,
        text.pos = cal_df$position, 
        text.size = 4.5,
        text.col = 4,
        special.days = c((day(Sys.Date()) + 1):31),
        special.col = "light gray",
        title = paste0("Performance for ", 
                       month(Sys.Date(), label = TRUE, abbr = FALSE), 
                       " ", year(Sys.Date())),
        subtitle = "Future Days in Gray; No Workout on Blank Days")
