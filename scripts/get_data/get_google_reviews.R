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

places_from_google <- readRDS("data/google/places.rds") |> as_tibble()

# Approach when we just had places from tripadvisor -----------------------

#' when there weren't any places from google, I took the place name from 
#' tripadvisor to query similar names in google places. This is not needed anymore
#' there is a commit with the former logic.

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

google_revies_raw <- places_from_google$place_id |>
  set_names() |> 
  map(google_place_details, .progress = TRUE)

get_place_reviews <- function(place_details) {
  place_details$result$reviews
}

google_reviews <- map(google_revies_raw, get_place_reviews) |>
  list_rbind(names_to = "place_id") |>
  mutate(
    time = as.numeric(time), 
    date = as.POSIXct(time, origin = "1970-01-01", tz = "UTC") |> as.Date(),
    month = floor_date(date, "month"),
    quarter = floor_date(date, "quarter")
  ) |>
  as_tibble() |>
  select(place_id, date, month, quarter, text, author_name, rating)



if (file.exists("data/google/reviews.rds")) {
  existing_reviews <- readRDS("data/google/reviews.rds")
  google_reviews <- google_reviews |>
    bind_rows(existing_reviews) |>
    distinct()
}

saveRDS(google_reviews, "data/google/reviews.rds")
