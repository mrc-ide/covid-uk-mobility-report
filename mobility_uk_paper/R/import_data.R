import_data <- function(){
  lads <- readRDS("data/lads.rds")
  shp_lads <- readRDS("data/shp_lad.rds")

  fb <- readRDS("movement_per_lad.rds")
  # fb$lad <- snakecase::to_snake_case(fb$lad)
  # #add region and county to fb data
  # fb <- merge(fb, 
  #             lads, 
  #             by.x = "lad",
  #             by.y = "local_authority_district")
          
  fb_pop <- readRDS("uk_facebook_population_per_tile.rds")

  key <- cyphr::data_key()
  mno1 <- cyphr::decrypt(readRDS("uk_movement_data.rds"), key)

  return(list(fb, fb_pop, mno1, lads, shp_lads))
}