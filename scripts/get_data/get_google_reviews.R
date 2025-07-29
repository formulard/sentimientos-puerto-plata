# Packages ----------------------------------------------------------------
library(httr2)
library(jsonlite)
library(googleway)
library(tibble)
library(dplyr)
library(purrr)
library(tidyr)
library(lubridate)

# Setup -------------------------------------------------------------------
api_key <- Sys.getenv("GOOGLE_API_KEY")
set_key(api_key)

# Import data -------------------------------------------------------------
places <- readRDS("data/tripadvisors/places.rds") |> as_tibble()

place_info_from_google <- function(query) {
  raw_result <- google_places(query, page_token = NULL)
  
  if (raw_result$status == "ZERO_RESULTS") return(data.frame())
  
  raw_result$results |> 
    as_tibble() |> 
    select(
      any_of(c("place_id","name", "rating", "types", "user_rating_total"))
    ) |>
    cbind(place_location(raw_result))
}

places_with_google_info <- places |>
  rename(tripadvisor_id = place_id, tripadvisor_name = name) |>
  mutate(
    google_place_info = purrr::map(
      paste(tripadvisor_name, destination, sep = ","),
      place_info_from_google,
      .progress = TRUE
    )
  )

places_from_google <- places_with_google_info |>
  rename(tripadvisor_rating = rating) |> 
  unnest(google_place_info) |>
  rename(
    google_rating = rating,
    google_name = name,
    google_place_id = place_id
  ) |> 
  group_by(google_place_id) |>
  slice(1) |>
  ungroup()

places_from_google |> saveRDS("data/google/places_from_google.rds")


google_revies_raw <- places_from_google$google_place_id |>
  map(google_place_details, .progress = TRUE) |>
  setNames(places_from_google$google_place_id)


get_place_reviews <- function(place_details) {
  place_details$result$reviews
}

google_reviews <- map(google_revies_raw, get_place_reviews) |>
  list_rbind(names_to = "google_place_id") |>
  mutate(
    time = as.numeric(time), 
    date = as.POSIXct(time, origin = "1970-01-01", tz = "UTC") |> as.Date(),
    month = floor_date(date, "month"),
    quarter = floor_date(date, "quarter")
  ) |>
  as_tibble() |>
  select(google_place_id, date, month, quarter, text, author_name, review_rating = rating)

saveRDS(google_reviews, "data/google/reviews_from_google.rds")


# Reviews form other places -----------------------------------------------
other_places <- readRDS("data/google/other_places_from_google.rds") |>
  as_tibble()

other_places_revies_raw <- other_places$place_id |>
  purrr::set_names() |> 
  purrr::map(googleway::google_place_details, .progress = TRUE)

other_places_reviews <- map(other_places_revies_raw, get_place_reviews) |>
  list_rbind(names_to = "place_id") |>
  as_tibble() |>
  mutate(
    time = as.numeric(time), 
    date = as.POSIXct(time, origin = "1970-01-01", tz = "UTC") |> as.Date(),
    month = floor_date(date, "month"),
    quarter = floor_date(date, "quarter")
  ) |>
  select(place_id, date, month, quarter, text, author_name, rating)

other_places_reviews |>
  distinct() |>
  saveRDS("data/google/reviews_from_other_places.rds")

