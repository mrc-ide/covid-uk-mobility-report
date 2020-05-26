get_pop_density <- function(mno1, shp_lads){
  shp_lads$lad <- snakecase::to_snake_case(shp_lads$lad)

  pop_density <- merge(shp_lads, 
            unique(mno1[,c("local_authority_district", "population")]), 
            by.x = "lad", 
            by.y = "local_authority_district")

  #convert to sf
  pop_density <- st_as_sf(pop_density)

  pop_density <- sf::st_set_crs(pop_density, 27700)
  pop_density <- st_transform(pop_density, 4326) #transform to same as data

  #original units are m2 so converting to km2
  pop_density$area <- as.numeric(st_area(pop_density)/1000000)

  #people per square kilometre
  pop_density$density <- pop_density$population/
                          pop_density$area
  pop_density$density_quartile <- dplyr::ntile(pop_density$density,
                                              4)

  pop_density$density_quartile <- factor(pop_density$density_quartile)

  return(pop_density)
}

cities_and_countries <- function(facebook_data, movement_data){
  movement_data <-   
    mutate(movement_data, date = as.Date(date, format = "%y_%m_%d"))

  all_dates <- seq(min(movement_data$date), max(movement_data$date), by = 1)
  missing_dates <- all_dates[which(!(all_dates %in% unique(movement_data$date)))]

  missing_data <- lapply(1:length(missing_dates),
        function(n){
  movement_data %>% 
    filter(date == as.Date(min(movement_data$date))) %>%
    mutate(date = as.Date(missing_dates[n]),
          trips_starting = NA,
          trips_ending = NA)}
  )

  movement_data <- movement_data %>%
    bind_rows(missing_data)

  facebook_data <- facebook_data %>%
    clean_names() %>%
    mutate(lad = to_snake_case(lad),
          country = to_snake_case(country)) %>%
    rename(local_authority_district = lad) %>%
    mutate(date = as.Date(date))

  facebook_data <- facebook_data %>% 
    select(!(n_trips)) %>%
    rename(n_trips = n_trips_adjusted)

  mob_baseline_movement <- movement_data %>%
    filter(journey_purpose == "sum") %>%
    filter(date >= as.Date("2020-03-10")) %>%
    filter(date <= as.Date("2020-03-16")) %>%
    group_by(local_authority_district) %>%
    summarize(baseline_trips_starting = mean(trips_starting))

  mob_data <- movement_data %>%
    filter(journey_purpose == "sum") %>%
    left_join(mob_baseline_movement, 
              by = c("local_authority_district")) %>%
    mutate(diff = 100*(trips_starting - baseline_trips_starting) / baseline_trips_starting,
          provider = "mobile_phone") 

  fb_baseline_movement <- facebook_data %>%
    filter(date >= as.Date("2020-03-10")) %>%
    filter(date <= as.Date("2020-03-16")) %>%
    group_by(local_authority_district) %>%
    summarize(baseline_trips_starting = mean(n_trips))

  fb_data <- facebook_data %>%
    select(date, local_authority_district, n_trips) %>%
    left_join(fb_baseline_movement, 
              by = c("local_authority_district")) %>%
    mutate(diff = 100*(n_trips - baseline_trips_starting) / baseline_trips_starting) %>%
    mutate(provider = "facebook")

  fb_mob_lad <- bind_rows(fb_data %>% select(date, 
                                            local_authority_district,
                                            diff,
                                            provider),
                          mob_data %>% select(date, 
                                              local_authority_district,
                                              diff,
                                              provider))

  country_mob_data <- movement_data %>% 
    filter(journey_purpose == "sum") %>%
    group_by(date, journey_purpose, nation) %>%
    summarize(trips_starting = sum(trips_starting),
              trips_ending = sum(trips_ending),
              population = sum(population)) %>%
    mutate( trips_starting_per_pop = trips_starting / population) %>%
    ungroup()

  country_mob_baseline_movement <- country_mob_data %>%
    filter(date >= as.Date("2020-03-10")) %>%
    filter(date <= as.Date("2020-03-16")) %>%
    group_by(nation) %>%
    summarize(baseline_trips_starting = mean(trips_starting))

  country_mob_data <- country_mob_data %>%
    left_join(country_mob_baseline_movement, 
              by = c("nation")) %>%
    mutate(diff = 100*(trips_starting - baseline_trips_starting) / baseline_trips_starting) %>%
    mutate(provider = "mobile_phone") %>%
    rename(country = nation)

  country_fb_data <- facebook_data %>% 
    group_by(date, country) %>%
    summarize(trips_starting = sum(n_trips)) %>%
    ungroup()

  country_fb_baseline_movement <- country_fb_data %>%
    filter(date >= as.Date("2020-03-10")) %>%
    filter(date <= as.Date("2020-03-16")) %>%
    group_by(country) %>%
    summarize(baseline_trips_starting = mean(trips_starting))

  country_fb_data <- country_fb_data %>%
    select(date, country, trips_starting) %>%
    left_join(country_fb_baseline_movement, 
              by = c("country")) %>%
    mutate(diff = 100*(trips_starting - baseline_trips_starting) / baseline_trips_starting) %>%
    mutate(provider = "facebook")

  fb_mob_country <- bind_rows(country_fb_data %>% select(date, 
                                            country,
                                            diff,
                                            provider),
                          country_mob_data %>% select(date, 
                                              country,
                                              diff,
                                              provider)) %>%
    mutate(area_type = "country") %>%
    rename(area = country)

   
  mob_london_data <- movement_data %>%
    filter(journey_purpose == "sum") %>% 
    filter(region == "london") %>%
    group_by(date) %>%
    summarize(trips_starting_sum = sum(trips_starting)) 

  london_lads <- unique(movement_data %>%
    filter(journey_purpose == "sum") %>% 
    filter(region == "london") %>%
      select(local_authority_district))

  mob_london_baseline <- mob_london_data %>%
    filter(date >= as.Date("2020-03-10")) %>%
    filter(date <= as.Date("2020-03-16")) %>%
    summarize(baseline_trips_starting = mean(trips_starting_sum))

  mob_london_baseline <- mob_london_baseline$baseline_trips_starting

  mob_london_data <- mob_london_data %>%
    mutate(diff = 100*(trips_starting_sum - mob_london_baseline) / mob_london_baseline) %>%
    mutate(provider = "mobile_phone")

  fb_london_data <- facebook_data %>%
    filter(local_authority_district %in% london_lads$local_authority_district) %>%
    group_by(date) %>%
    summarize(trips_starting_sum = sum(n_trips))

  fb_london_baseline <- fb_london_data %>%
    filter(date >= as.Date("2020-03-10")) %>%
    filter(date <= as.Date("2020-03-16")) %>%
    summarize(baseline_trips_starting = mean(trips_starting_sum))

  fb_london_baseline <- fb_london_baseline$baseline_trips_starting

  fb_london_data <- fb_london_data %>%
    mutate(diff = 100*(trips_starting_sum - fb_london_baseline) / fb_london_baseline) %>%
    mutate(provider = "facebook")

  fb_mob_london <- bind_rows(fb_london_data %>% select(date, 
                                            diff,
                                            provider),
                          mob_london_data %>% select(date, 
                                              diff,
                                              provider)) %>%
    mutate(area = "london",
          area_type = "region") 

  cities <- c("belfast", "cardiff", "glasgow_city") 

  city_data <- fb_mob_lad %>% 
    filter(local_authority_district %in% cities) %>%
    rename(area = local_authority_district) %>%
    mutate(area_type = "local_authority_district") %>%
    bind_rows(fb_mob_london)

  city_country <- bind_rows(city_data,
                          fb_mob_country)

  return(list(city_country, mob_data))
}