# Packages ----------------------------------------------------------------
library(httr2)
library(jsonlite)
library(googleway)
library(tibble)
library(dplyr)

# Setup -------------------------------------------------------------------
api_key <- Sys.getenv("GOOGLE_API_KEY")
set_key(api_key)

# Import data -------------------------------------------------------------
places <- readRDS("data/tripadvisors/places.rds") |> as_tibble()

place_info_from_google <- function(query) {
  raw_result <- googleway::google_places(query)
  
  if (raw_result$status == "ZERO_RESULTS") return(data.frame())
  
  raw_result$results |> 
    dplyr::as_tibble() |> 
    dplyr::select(
      any_of(c("place_id","name", "rating", "types", "user_rating_total"))
    ) |>
    cbind(googleway::place_location(raw_result))
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
  tidyr::unnest(google_place_info) |>
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
  purrr::map(google_place_details, .progress = TRUE) |>
  setNames(places_from_google$google_place_id)


get_place_reviews <- function(place_details) {
  place_details$result$reviews
}

google_reviews <- purrr::map(google_revies_raw, get_place_reviews) |>
  purrr::list_rbind(names_to = "google_place_id") |>
  dplyr::mutate(
    time = as.numeric(time), 
    date = as.POSIXct(time, origin = "1970-01-01", tz = "UTC") |> as.Date(),
    month = lubridate::floor_date(date, "month"),
    quarter = lubridate::floor_date(date, "quarter")
  ) |>
  as_tibble()

google_reviews |> 
  count(month) |>
  ggplot(aes(x = month, y = n)) +
  geom_col()

details_test <- places_from_google$google_place_id[1] |>
  google_place_details()

details_test$result$reviews


place <- googleway::google_places("Teleférico, Puerto Plata, República Dominicana", )
place_id = place$results$place_id

place_details <- google_place_details(place_id)


res_details <- request("https://maps.googleapis.com/maps/api/place/details/json") %>%
  req_url_query(
    place_id = place_id,
    fields = "name,rating,reviews,user_ratings_total",
    key = api_key
  ) %>%
  req_perform() %>%
  resp_body_json()

reviews <- res_details$result$reviews

library(httr2)

place_id <- "ChIJN1t_tDeuEmsRUsoyG83frY4"

res <- request("https://maps.googleapis.com/maps/api/place/details/json") %>%
  req_url_query(
    place_id = place_id,
    fields = "name,rating,reviews,formatted_address",
    key = api_key
  ) %>%
  req_perform() %>%
  resp_body_json()

# Example: print name and address
res$result$name
res$result$formatted_address

res$result$reviews

