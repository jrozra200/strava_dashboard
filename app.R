library(shiny)
library(httr)
library(rStrava)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(scales)
library(RColorBrewer)
library(calendR)
library(lubridate)
library(knitr)

stoken <- httr::config(token = readRDS('.httr-oauth')[[1]])

header <- dashboardHeader(
    title = "Run Jake Run!", 
    
    tags$li(a(href = paste0('https://twitter.com/intent/tweet?text=Check%20out',
                            '%20how%20great%20of%20a%20runner%20Jake%20(@Stats',
                            'ManPHL)%20is%20on%20@Strava&url=https%3a%2f%2fjakelea',
                            'rnsdatascience.shinyapps.io%2fJakeOnStrava%2f'),
              target = "_blank",
              icon("twitter"),
              title = "Share this app on Twitter"),
            class = "dropdown"),
    
    tags$li(a(href = 'https://github.com/jrozra200/strava_dashboard',
              target = "_blank",
              icon("github"),
              title = "Check out the code on Github"),
            class = "dropdown"),
    
    tags$li(a(href = 'https://www.jakelearnsdatascience.com',
              target = "_blank",
              icon("bar-chart"),
              title = "Back to Jake Learns Data Science"),
            class = "dropdown")
)

sidebar <- dashboardSidebar(
    disable = TRUE
)

body <- dashboardBody(
    fluidRow(
        box(
            width = "100%",

            dateInput("start_date", "Jake's Performance Since", 
                      value = "2020-12-25")
            )
        ),
    
    fluidRow(
        tabBox(title = "Run Jake Run!",
               id = "tabset1",
               height = "100%",
               width = "100%",
               
               tabPanel(
                   "Cumulative Stats",
                   
                   fluidRow(
                       box(
                           width = "100%",
                           title = "Cumulative Mileage, Minutes, & Elevation",
                           
                           valueBoxOutput("cumulative_mileage_num"),
                           valueBoxOutput("cumulative_minutes_num"),
                           valueBoxOutput("cumulative_elevation_num")
                       )
                   ),
                   
                   fluidRow(
                       box(
                           width = "100%",
                           title = "Cumulative Graphs",
                           splitLayout(
                               cellWidths = c("33%", "33%", "33%"),
                               
                               plotOutput("cumulative_mileage"),
                               plotOutput("cumulative_minutes"),
                               plotOutput("cumulative_elevation")
                           )
                        )
                    )
               ),
               
               tabPanel(
                   "Individual Workout Details",
                   fluidRow(
                       box(
                           width = "100%",
                           title = "Individual Workout Stats",
                           splitLayout(
                               cellWidths = c("50%", "50%"),
                               
                               plotOutput("daily_activity"),
                               plotOutput("daily_minutes")
                           )
                       ),
                   ),
                   
                   fluidRow(
                       box(
                           width = "100%",
                           title = "Average Speed and Time Worked",
                           splitLayout(
                               cellWidths = c("50%", "50%"),
                               
                               plotOutput("change_in_mph"),
                               plotOutput("change_in_mins")
                           )
                       )    
                   ),
                   
                   fluidRow(
                       box(
                           width = "100%",
                           title = "All Activities",
                           tableOutput("table_activities")
                       )
                   )
               ),
               
               tabPanel(
                   "Current Month Performance",
                   box(
                       width = "100%",
                       title = "Current Month",
                       plotOutput("current_month")
                   )
               )
        )
    )
)

ui <- dashboardPage(header, sidebar, body)

server <- function(input, output) {

    format_df <- reactive({
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
        
        jake_df$date <- as_date(jake_df$start_time_2)
        
        jake_df <- jake_df[jake_df$date >= input$start_date, ]
        
        jake_df <- jake_df[order(jake_df$date), ]
        jake_df$cumulative_mileage <- cumsum(jake_df$miles)
        jake_df$cumulative_time <- cumsum(jake_df$minutes)
        jake_df$cumulative_gain <- cumsum(jake_df$total_elevation_gain_ft)
        
        return(jake_df)
    })
    
    cum_graph_df <- reactive({
        dat <- format_df()
        
        graph_dat <- dat %>%
            group_by(date) %>% 
            summarise(miles = sum(miles),
                      minutes = sum(minutes),
                      total_elevation_gain_ft = sum(total_elevation_gain_ft))
        
        graph_dat <- graph_dat[order(graph_dat$date), ]
        
        graph_dat$cumulative_mileage <- cumsum(graph_dat$miles)
        graph_dat$cumulative_time <- cumsum(graph_dat$minutes)
        graph_dat$cumulative_gain <- cumsum(graph_dat$total_elevation_gain_ft)
        
        return(graph_dat)
    })
    
    moving_stats <- reactive({
        dat <- format_df()
        dat <- dat[dat$type == "Run", ]
        dat <- dat[dat$distance > 0, ]
        
        dat <- dat[order(dat$date), ]
        
        sum_dat <- data.frame()
        
        for(i in 1:dim(dat)[1]){
            if((i - 7) < 1){
                tmp <- data.frame(
                    date = dat$date[i],
                    total_time = sum(dat$minutes[1:i]),
                    total_miles = sum(dat$miles[1:i]),
                    total_activites = dim(dat[1:i, ])[1]
                )
            } else {
                tmp <- data.frame(
                    date = dat$date[i],
                    total_time = sum(dat$minutes[(i-7):i]),
                    total_miles = sum(dat$miles[(i-7):i]),
                    total_activites = dim(dat[(i-7):i, ])[1]
                )
            }
            sum_dat <- rbind(sum_dat, tmp)
        }
        
        sum_dat$avg_speed <- sum_dat$total_miles / (sum_dat$total_time / 60)
        sum_dat$avg_time <- sum_dat$total_time / sum_dat$total_activites
        sum_dat$min_per_mile <- 1 / (sum_dat$avg_speed / 60)
        
        return(sum_dat)
    })
    
    output$current_month <- renderPlot({
        dat <- format_df()
        
        current_month <- paste0(year(Sys.Date()), "-", month(Sys.Date()), "-01")
        
        dat <- dat[dat$start_time_2 >= current_month, ]
        
        cal_df <- data.frame(text = paste0(dat$type, "\n", round(dat$miles, 2), 
                                           " miles\n", round(dat$minutes, 2), 
                                           " minutes"),
                             position = day(dat$start_time_2))
        
        calendR(year = year(Sys.Date()), 
                month = month(Sys.Date()),
                text = cal_df$text,
                text.pos = cal_df$position, 
                text.size = 3.5,
                text.col = 4,
                special.days = c((day(Sys.Date()) + 1):31),
                special.col = "light gray",
                title = paste0("Performance for ", 
                               month(Sys.Date(), label = TRUE, abbr = FALSE), 
                               " ", year(Sys.Date())),
                subtitle = "Future Days in Gray; No Workout on Blank Days")
        
    })
    
    output$cumulative_mileage_num <- renderValueBox({
        dat <- format_df()
        
        valueBox(
            comma(round(sum(dat$miles), 2), accuracy = 0.01), 
            paste0("Miles Traveled Since ", 
                   format(input$start_date, "%b %d, %Y")), 
            icon = icon("map-marked-alt", lib = "font-awesome"), color = "blue"
        )
    })
    
    output$cumulative_minutes_num <- renderValueBox({
        dat <- format_df()
        
        valueBox(
            comma(round(sum(dat$minutes), 2), accuracy = 0.01), 
            paste0("Minutes Worked Since ", 
                   format(input$start_date, "%b %d, %Y")), 
            icon = icon("stopwatch", lib = "font-awesome"), color = "light-blue"
        )
    })
    
    output$cumulative_elevation_num <- renderValueBox({
        dat <- format_df()
        
        valueBox(
            comma(round(sum(dat$total_elevation_gain_ft), 2), accuracy = 0.01), 
            paste0("Elevation Gained (feet) Since ", 
                   format(input$start_date, "%b %d, %Y")), 
            icon = icon("mountain", lib = "font-awesome"), 
            color = "aqua"
        )
    })
    
    output$cumulative_mileage <- renderPlot({
        dat <- cum_graph_df()
        
        dat <- dat[dat$miles > 0, ]
        
        ## CUMULATIVE MILEAGE
        ggplot(data = dat, aes(x = date, y = cumulative_mileage)) + 
            geom_line(color = "#3944BC", size = 2) + 
            scale_y_continuous(label = comma_format()) +
            ggtitle(paste0("Miles Traveled Since ", 
                           format(input$start_date, "%b %d, %Y"))) +
            ylab("Miles") +
            theme(panel.background = element_blank(), 
                  panel.grid.major.x = element_blank(),
                  panel.grid.major.y = element_line(color = "grey"),
                  legend.position = "top", legend.text = element_text(size = 12),
                  legend.title = element_blank(), title = element_text(size = 16),
                  axis.text = element_text(size = 12),
                  axis.title.x = element_blank(),
                  axis.ticks = element_blank(),
                  plot.background = element_rect(fill = "white", 
                                                 color = "light gray", size = 1))
    })
    
    output$cumulative_minutes <- renderPlot({
        dat <- cum_graph_df()
        
        ## CUMULATIVE MINUTES
        ggplot(data = dat, aes(x = date, y = cumulative_time))+ 
            geom_line(color = "#4682B4", size = 2) + 
            scale_y_continuous(label = comma_format()) +
            ggtitle(paste0("Minutes Worked Since ", 
                           format(input$start_date, "%b %d, %Y"))) +
            ylab("Minutes") +
            theme(panel.background = element_blank(), 
                  panel.grid.major.x = element_blank(),
                  panel.grid.major.y = element_line(color = "grey"),
                  legend.position = "top", legend.text = element_text(size = 12),
                  legend.title = element_blank(), title = element_text(size = 16),
                  axis.text = element_text(size = 12),
                  axis.title.x = element_blank(),
                  axis.ticks = element_blank(),
                  plot.background = element_rect(fill = "white", 
                                                 color = "light gray", size = 1))
    })
    
    output$cumulative_elevation <- renderPlot({
        dat <- cum_graph_df()
        
        ## CUMULATIVE ELEVATION
        ggplot(data = dat, aes(x = date, y = cumulative_gain))+ 
            geom_line(color = "#40C4FF", size = 2) + 
            scale_y_continuous(label = comma_format()) +
            ggtitle(paste0("Elevation Gained (feet) Since ", 
                           format(input$start_date, "%b %d, %Y"))) +
            ylab("Feet") +
            theme(panel.background = element_blank(), 
                  panel.grid.major.x = element_blank(),
                  panel.grid.major.y = element_line(color = "grey"),
                  legend.position = "top", legend.text = element_text(size = 12),
                  legend.title = element_blank(), title = element_text(size = 16),
                  axis.text = element_text(size = 12),
                  axis.title.x = element_blank(),
                  axis.ticks = element_blank(),
                  plot.background = element_rect(fill = "white", 
                                                 color = "light gray", size = 1))
    })
    
    output$daily_activity <- renderPlot({
        dat <- format_df()
        
        ## EACH ACTIVITY
        ggplot(data = dat, aes(x = date, y = miles, 
                               fill = type)) + 
            geom_bar(stat = "identity", position = "stack") + 
            scale_fill_manual(values = rev(brewer.pal(length(unique(dat$type)), "Blues"))) +
            geom_hline(yintercept = mean(dat$miles), linetype = "dashed")  + 
            scale_y_continuous(label = comma_format()) +
            ggtitle("Miles per Day", 
                    subtitle = paste0("Average Workout Length: ", 
                                      round(mean(dat$miles), 2), " miles")) +
            ylab("Miles") +
            theme(panel.background = element_blank(), 
                  panel.grid.major.x = element_blank(),
                  panel.grid.major.y = element_line(color = "grey"),
                  legend.position = "top", legend.text = element_text(size = 12),
                  legend.title = element_blank(), title = element_text(size = 16),
                  axis.text = element_text(size = 12),
                  axis.title.x = element_blank(),
                  axis.ticks = element_blank(),
                  plot.background = element_rect(fill = "white", 
                                                 color = "light gray", size = 1))
    })
    
    output$daily_minutes <- renderPlot({
        dat <- format_df()
        
        ## EACH ACTIVITY
        ggplot(data = dat, aes(x = date, y = minutes, 
                               fill = type)) + 
            geom_bar(stat = "identity", position = "stack") + 
            scale_fill_manual(values = rev(brewer.pal(length(unique(dat$type)), "Blues"))) +
            geom_hline(yintercept = mean(dat$minutes), linetype = "dashed")  + 
            scale_y_continuous(label = comma_format()) +
            ggtitle("Minutes Worked per Day", 
                    subtitle = paste0("Average Workout: ", 
                                      round(mean(dat$minutes), 2), " minutes")) +
            ylab("Minutes") +
            theme(panel.background = element_blank(), 
                  panel.grid.major.x = element_blank(),
                  panel.grid.major.y = element_line(color = "grey"),
                  legend.position = "top", legend.text = element_text(size = 12),
                  legend.title = element_blank(), title = element_text(size = 16),
                  axis.text = element_text(size = 12),
                  axis.title.x = element_blank(),
                  axis.ticks = element_blank(),
                  plot.background = element_rect(fill = "white", 
                                                 color = "light gray", size = 1))
    })
    
    output$table_activities <- renderTable({
        dat <- format_df()
        
        dat <- dat[, c("act_name", "type", "start_time_2", "miles", "minutes", 
                       "total_elevation_gain_ft")]
        
        dat$start_time_2 <- format(dat$start_time_2, "%b %d, %Y %H:%M:%S")
        dat$miles <- comma(dat$miles, accuracy = 0.01)
        dat$minutes <- comma(dat$minutes, accuracy = 0.01)
        dat$total_elevation_gain_ft <- comma(dat$total_elevation_gain_ft, 
                                             accuracy = 0.01)
        
        names(dat) <- c("Activity", "Type", "Start Time", "Miles", "Minutes",
                        "Total Elevation Gain (ft)")
        
        dat
    })
    
    output$change_in_mph <- renderPlot({
        dat <- moving_stats()
        
        ggplot(data = dat, aes(x = date, y = min_per_mile)) + 
            geom_line(color = "navy", size = 2) +
            scale_y_continuous(label = comma_format(), limits = c(0, max(dat$min_per_mile) + 0.25)) +
            ggtitle("Average Run Speed (Mile Time)", 
                    subtitle = "7 Run Moving Average") +
            ylab("Minutes per Mile") +
            theme(panel.background = element_blank(), 
                  panel.grid.major.x = element_blank(),
                  panel.grid.major.y = element_line(color = "grey"),
                  legend.position = "top", legend.text = element_text(size = 12),
                  legend.title = element_blank(), title = element_text(size = 16),
                  axis.text = element_text(size = 12),
                  axis.title.x = element_blank(),
                  axis.ticks = element_blank(),
                  plot.background = element_rect(fill = "white", 
                                                 color = "light gray", size = 1))
    })
    
    output$change_in_mins <- renderPlot({
        dat <- moving_stats()
        
        ggplot(data = dat, aes(x = date, y = avg_time)) + 
            geom_line(color = "navy", size = 2) +
            scale_y_continuous(label = comma_format(), limits = c(0, max(dat$avg_time) + 0.25)) +
            ggtitle("Average Minutes Worked",
                    subtitle = "7 Workout Moving Average") +
            ylab("Minutes") +
            theme(panel.background = element_blank(), 
                  panel.grid.major.x = element_blank(),
                  panel.grid.major.y = element_line(color = "grey"),
                  legend.position = "top", legend.text = element_text(size = 12),
                  legend.title = element_blank(), title = element_text(size = 16),
                  axis.text = element_text(size = 12),
                  axis.title.x = element_blank(),
                  axis.ticks = element_blank(),
                  plot.background = element_rect(fill = "white", 
                                                 color = "light gray", size = 1))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
