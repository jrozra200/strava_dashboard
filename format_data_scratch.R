## LET"S GET THIS THING GOING

library(rStrava)
library(lubridate)

stoken <- httr::config(token = readRDS('.httr-oauth')[[1]])

jake <- get_activity_list(stoken)

jake_df <- data.frame()

for(i in 1:length(jake)){
    tmp <- data.frame(act_name = jake[[i]]$name,
                      distance = jake[[i]]$distance,
                      moving_time = jake[[i]]$moving_time,
                      elapsed_time = jake[[i]]$elapsed_time,
                      total_elevation_gain = jake[[i]]$total_elevation_gain,
                      type = jake[[i]]$type,
                      start_time = jake[[i]]$start_date_local,
                      lat = ifelse(is.null(jake[[i]]$start_latlng[[1]]), 
                                   NA, jake[[i]]$start_latlng[[1]]),
                      lon = ifelse(is.null(jake[[i]]$start_latlng[[2]]), 
                                   NA, jake[[i]]$start_latlng[[2]]),
                      timezone = jake[[i]]$timezone,
                      utc_offset = jake[[i]]$utc_offset,
                      achievement_count = jake[[i]]$achievement_count,
                      elev_high = ifelse(is.null(jake[[i]]$elev_high), NA, 
                                         jake[[i]]$elev_high),
                      elev_low = ifelse(is.null(jake[[i]]$elev_low), NA, 
                                        jake[[i]]$elev_low),
                      pr_count = jake[[i]]$pr_count)
    
    jake_df <- rbind(jake_df, tmp)
}

jake_df$start_time_2 <- as.POSIXct(jake_df$start_time, "%Y-%m-%dT%H:%M:%SZ", 
                                   tz = "America/New_York")

jake_df$miles <- jake_df$distance / 1609.34
jake_df$minutes <- jake_df$moving_time / 60
jake_df$total_elevation_gain_ft <- jake_df$total_elevation_gain * 3.28084
