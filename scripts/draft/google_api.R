library(googleway)
library(httr2)
library(jsonlite)

api_key <- Sys.getenv("GOOGLE_API_KEY")
set_key(api_key)

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

