library(dplyr)
library(sf)

map_puerto_plata <- readRDS("data/municipios_sf.rds") |>
  filter(provincia_label == "Puerto Plata")

tripadvisor_reviews <- readRDS("data/tripadvisor/reviews/all_places.rds")
tripadvisor_places <- readRDS("data/tripadvisor/places.rds") |> as_tibble() |>
  mutate(
    rating = as.numeric(rating),
    destination = recode(
      destination,
      "Maimon" = "Otro",
      "Sosua" = "Sosúa"
    )
  ) |> 
  select(place_id, destination, name, rating, type)

google_places <- readRDS("data/google/places.rds") |>
  group_by(place_id) |>
  slice(1) |>
  ungroup()

google_reviews <- readRDS("data/google/reviews.rds")


google_destinations <- google_places |> 
  select(place_id, name, rating, types, lat, lng) |>
  st_as_sf(coords = c("lng", "lat"), crs = 4326) |>
  st_join(map_puerto_plata , join = st_within) |>
  st_drop_geometry() |>
  select(place_id, destination =  municipio_label) |>
  mutate(
    destination = recode(
      destination,
      "Altamira" = "Otro",
      "Guananico" = "Otro",
      "Luperón" = "Otro", 
      "Villa Isabela" = "Otro",
      "Villa Montellano" = "Otro",
    ),
    destination = ifelse(is.na(destination), "Otro", destination)
  )


google_places <- google_places |>
  left_join(google_destinations, by = "place_id") |>
  relocate(destination, .after = place_id)

all_places <- bind_rows(
  google = google_places,
  tripadvisor = tripadvisor_places,
  .id = "source"
)

tripadvisor_reviews <- tripadvisor_reviews |>
  select(
    place_id,
    date,
    quarter,
    month,
    title,
    text = content
  )

google_reviews <- google_reviews |>
  select(
    place_id,
    date,
    quarter,
    month,
    text
  )

all_reviews <- bind_rows(
  google = google_reviews,
  tripadvisor = tripadvisor_reviews,
  .id = "source"
) |>
  filter(!is.na(text))

all_reviews <- all_reviews |> 
  mutate(
    review_id = paste0(source, place_id, date, text),
    review_id = sapply(review_id, \(x) digest::digest(x, algo = "md5"))
  ) |>
  distinct() |> 
  relocate(review_id)

saveRDS(all_places, "data/places.rds")
saveRDS(all_reviews, "data/reviews.rds")
