
# Packages ----------------------------------------------------------------
library(readxl)
library(purrr)
library(tidyr)
library(dplyr)
library(glue)
library(tictoc)
library(logger)

# Local functions ---------------------------------------------------------
box::use(
  scripts/logic/webscraping_tripadvisor[
    get_places_from_destination,
    get_attraction_reviews,
    get_place_reviews,
  ]
)

# Import helpers ----------------------------------------------------------
destinations_metadata <- read_excel("data/tripadvisors/helpers/places_metadata.xlsx")

# Get places from all destinations ----------------------------------------

# destinations <- destinations_metadata |>
#   mutate(
#     places = map(destination_url, get_places_from_destination, .progress = TRUE)
#   )
# 
# saveRDS(destinations, "data/tripadvisors/destinations.rds")

destinations <- readRDS("data/tripadvisors/destinations.rds")

# places <- destinations |>
#   unnest(places) |>
#   distinct() |>
#   filter(!is.na(type)) |>
#   tibble::rowid_to_column("place_id") |>
#   dplyr::mutate(place_id = as.character(place_id))
# 
# saveRDS(places, "data/tripadvisors/places.rds")

places <- readRDS("data/tripadvisors/places.rds")

places_by_type <- split(places, places$type)

# Get reviews -------------------------------------------------------------

attractions <- places_by_type[["Attraction"]]

attractions_reviews <- vector(length = nrow(attractions), mode = "list") |>
  set_names(attractions$place_id)

empty <- which(sapply(attractions_reviews, \(x) is.null(x)))
progress <- 1

if (length(empty) > 0) {
  for (index in empty) {
    progress_pct <- round(progress / length(empty) * 100, 2)

    attraction_data <- attractions[index, ] 
    
    base_page <- attraction_data$url
    current_result <- get_attraction_reviews(base_page, sleep = 5, n_pages = 4)
    
    log_info("{progress_pct} %: {nrow(current_result)} reviews from {attraction_data$name}")
    progress <- progress + 1

    attractions_reviews[[index]] <- current_result
    saveRDS(atrractions_reviews, "data/tripadvisor/cache/attraction_reviews.rds")
  }
}

attractions_reviews_df <- attractions_reviews |>
  purrr::list_rbind(names_to = "place_id") |> 
  dplyr::group_by(title, content) |>
  dplyr::slice(1) |>
  dplyr::ungroup()

attractions |>
  left_join(attractions_reviews_df) |>
  saveRDS(here::here("data/tripadvisors/reviews/attractions.rds"))


# Restaurants ------------------------------------------------------------------

restaurants <- places_by_type[["Restaurant"]]

restaurants_reviews <- vector(length = nrow(restaurants), mode = "list") |>
  set_names(restaurants$place_id)

empty <- which(sapply(restaurants_reviews, \(x) is.null(x)))

progress <- 1

if (length(empty) > 0) {
  for (index in empty) {
    tic()
    progress_pct <- round(progress / length(empty) * 100, 2)
    
    restaurant_data <- restaurants[index, ] 
    
    base_page <- restaurant_data$url
    current_result <- get_place_reviews(base_page, sleep = 5, n_pages = 4)
    
    log_info("{progress_pct} %: {nrow(current_result)} reviews from {restaurant_data$name}")
    progress <- progress + 1
    
    restaurants_reviews[[index]] <- current_result
    saveRDS(restaurants_reviews, "data/tripadvisors/cache/restaurants_reviews.rds")
    toc()
  }
}

restaurants_reviews_df <- restaurants_reviews |>
  purrr::list_rbind(names_to = "place_id") |> 
  dplyr::group_by(title, content) |>
  dplyr::slice(1) |>
  dplyr::ungroup()

restaurants |>
  left_join(restaurants_reviews_df) |>
  saveRDS(here::here("data/tripadvisors/reviews/restaurants.rds"))


# Hotels -------------------------------------------------------------------

hotels <- places_by_type[["Hotel"]]

hotels_reviews <- vector(length = nrow(hotels), mode = "list") |>
  set_names(hotels$place_id)

empty <- which(sapply(hotels_reviews, \(x) is.null(x)))

progress <- 1

if (length(empty) > 0) {
  for (index in empty) {
    tic()
    progress_pct <- round(progress / length(empty) * 100, 2)
    
    place_data <- hotels[index, ] 
    
    base_page <- place_data$url
    current_result <- get_place_reviews(base_page, sleep = 5, n_pages = 4)
    
    log_info("{progress_pct} %: {nrow(current_result)} reviews from {place_data$name}")
    progress <- progress + 1
    
    hotels_reviews[[index]] <- current_result
    saveRDS(hotels_reviews, "data/tripadvisors/cache/hotels_reviews.rds")
    toc()
  }
}

hotels_reviews_df <- hotels_reviews |>
  purrr::list_rbind(names_to = "place_id") |> 
  dplyr::group_by(title, content) |>
  dplyr::slice(1) |>
  dplyr::ungroup()

hotels |>
  left_join(hotels_reviews_df) |>
  saveRDS(here::here("data/tripadvisors/reviews/hotels.rds"))
