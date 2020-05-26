import_data <- function(){
  lads <- readRDS("data/lads.rds")
  fb <- readRDS("movement_per_lad.rds")
  fb$lad <- snakecase::to_snake_case(fb$lad)
  fb$n_trips<-fb$n_trips_adjusted
  #add region and county to fb data
  fb <- merge(fb, 
              lads, 
              by.x = "lad",
              by.y = "local_authority_district")
  
  fb$date <- as.Date(fb$date)

  
  return(fb)
}


filter_by_date <- function(fb){
  #get consistent date range
  first_date <- min(fb$date)
  last_date <- max(fb$date)
  fb <- fb[fb$date >= first_date & 
             fb$date <= last_date,]

  return(fb)
}

get_percent_change <- function(data, loc_col, value_col, 
                               country_col, date_col = "date"){
  
  #get total per region
  regional_totals <- aggregate(data[[value_col]],
                               by = list(loc = data[[loc_col]],
                                         date = data[[date_col]]),
                               FUN = sum)
  names(regional_totals) <- c(loc_col, date_col, "n_trips")
  
  #date range of first week of data
  first_date <- as.Date("2020-03-10")
  first_week <- seq(first_date, first_date + 6, by = "day")
  
  #get first weeks data
  d_first_week <- 
    regional_totals[regional_totals[[date_col]] %in% first_week,]
  
  baseline_trips <- aggregate(d_first_week$n_trips,
                              by = list(loc = d_first_week[[loc_col]]),
                              FUN = mean)
  names(baseline_trips) <- c(loc_col, "n_baseline")
  
  #restore country column
  baseline_trips <- merge(baseline_trips, 
                          unique(data[,c(loc_col, country_col)]))
  baseline_trips[[country_col]] <- 
    snakecase::to_snake_case(baseline_trips[[country_col]])
  names(baseline_trips) <- c("region", "n_baseline", "country")
  
  #percent difference 
  d <- merge(baseline_trips,regional_totals, 
             by.x = "region", by.y = loc_col)
  d$percent_diff <- ((d$n_trips - d$n_baseline)/d$n_baseline) * 100
  
  d <- d[order(d$region, d$date),]
  
  return(d)
}

