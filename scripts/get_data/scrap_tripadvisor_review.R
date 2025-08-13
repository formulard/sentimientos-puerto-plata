
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

# NOTE: There commented code because it's not needed to do every thing from the start.
# If you want to, remove the comments and exclude the cache data

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

scrape_reviews_for_type <- function(
    type, 
    places_by_type, 
    get_reviews_fn, 
    cache_path, 
    output_path,
    sleep = 5, 
    n_pages = 1
) {
  places <- places_by_type[[type]]
  
  if (file.exists(cache_path)) {
    reviews_list <- readRDS(cache_path)
  } else {
    reviews_list <- vector("list", nrow(places)) |>
      set_names(places$place_id)
  }

  empty <- which(sapply(reviews_list, is.null))
  progress <- 1
  
  if (length(empty) > 0) {
    for (index in empty) {
      tic()
      progress_pct <- round(progress / length(empty) * 100, 2)
      
      place_data <- places[index, ]
      base_page <- place_data$url
      
      current_result <- get_reviews_fn(base_page, sleep = sleep, n_pages = n_pages)
      
      log_info("{progress_pct} %: {nrow(current_result)} reviews from {place_data$name}")
      progress <- progress + 1
      
      reviews_list[[index]] <- current_result
      saveRDS(reviews_list, cache_path)
      toc()
    }
  }
  
  reviews_df <- reviews_list |>
    purrr::list_rbind(names_to = "place_id") |>
    dplyr::group_by(title, content) |>
    dplyr::slice(1) |>
    dplyr::ungroup()
  
  places |>
    dplyr::left_join(reviews_df, by = "place_id") |>
    saveRDS(here::here(output_path))
}


# Get reviews -------------------------------------------------------------

scrape_reviews_for_type(
  type = "Attraction",
  places_by_type = places_by_type,
  get_reviews_fn = get_attraction_reviews,
  cache_path = "data/tripadvisors/cache/attraction_reviews.rds",
  output_path = glue("data/tripadvisor/reviews/{Sys.date())_attractions.rds")
)

scrape_reviews_for_type(
  type = "Restaurant",
  places_by_type = places_by_type,
  get_reviews_fn = get_place_reviews,
  cache_path = "data/tripadvisor/cache/restaurants_reviews.rds",
  output_path = glue("data/tripadvisor/reviews/{Sys.Date()}_restaurants.rds")
)

scrape_reviews_for_type(
  type = "Hotel",
  places_by_type = places_by_type,
  get_reviews_fn = get_place_reviews,
  cache_path = "data/tripadvisor/cache/hotels_reviews.rds",
  output_path = glue("data/tripadvisor/reviews/{Sys.date()}_hotels.rds")
)

# All reviews -------------------------------------------------------------

all_places_reviews <- list.files(
  "data/tripadvisor/reviews/", 
  full.names = TRUE,
  pattern = "2025"
) |>
  purrr::map(\(file) readRDS(file))

all_places_reviews <- all_places_reviews |>
  bind_rows() |> 
  mutate(
    quarter = lubridate::floor_date(date, "quarter"),
    month = lubridate::floor_date(date, "month")
  ) |>
  relocate(c(month, quarter), .after = date)


if (file.exists("data/tripadvisor/reviews/all_places.rds")) {
  existing_reviews <- readRDS("data/tripadvisor/reviews/all_places.rds")
  updated_reviews <- all_places_reviews |>
    bind_rows(existing_reviews) |>
    distinct()
  
  saveRDS(updated_reviews, "data/tripadvisor/reviews/all_places.rds")
}

