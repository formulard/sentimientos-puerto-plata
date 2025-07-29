

# Get more places from google ---------------------------------------------
localidades <- c("Puerto Plata", "Cabarete", "Imbert", "Punta Rucia", "San Felipe", "Yasica", "Sosua")
tipos <- c("hoteles", "restaurantes", "museos", "Bares", "Spa", "Tours", "Discotecas", "Cafeterías", "Playas")

get_all_places <- function(search_string, key = api_key, sleep = 2, verbose = TRUE) {
  all_results <- list()
  page_token <- NULL
  attempt <- 1
  
  repeat {
    if (verbose) message("Requesting page ", attempt, if (!is.null(page_token)) " (next page)" else "")
    
    res <- googleway::google_places(
      search_string = search_string,
      key = key,
      page_token = page_token
    )
    
    if (!"results" %in% names(res)) {
      warning("No results field found in response.")
      break
    }
    
    all_results[[attempt]] <- res
    
    if (!is.null(res$next_page_token)) {
      page_token <- res$next_page_token
      attempt <- attempt + 1
      Sys.sleep(sleep)  # Required delay before next_page_token becomes active
    } else {
      break
    }
  }
  
  gr <- \(raw_result) {
    raw_result$result |> 
      dplyr::as_tibble() |> 
      dplyr::select(
        dplyr::any_of(c("place_id","name", "rating", "types", "user_rating_total"))
      ) |>
      cbind(googleway::place_location(raw_result))
  }
  
  purrr::map(all_results, possibly(gr, otherwise = data.frame()))
}

places <- expand.grid(localidad = localidades, tipo = tipos) |>
  as_tibble() |>
  mutate(
    query = paste(tipo, "en", localidad, ", República Dominicana"),
    places = purrr::map(query, possibly(\(q) get_all_places(q), data.frame()), .progress = TRUE)
  )

ohter_places <- places$places |> 
  purrr::map(bind_rows) |>
  bind_rows() |>
  distinct() 



library(leaflet)

pp_bbox <- list(
  lat_min = 19.530,
  lat_max = 19.970,
  lng_min = -71.140,
  lng_max = -70.420
)
# Assume 'colmados' is your data frame with lat/lng columns



other_places <- ohter_places |>
  as_tibble() |> 
  filter(
    lat >= pp_bbox$lat_min,
    lat <= pp_bbox$lat_max,
    lng >= pp_bbox$lng_min,
    lng <= pp_bbox$lng_max
  ) 

names(other_places)


all_other_places <- places_from_google |> 
  filter(
    lat >= pp_bbox$lat_min,
    lat <= pp_bbox$lat_max,
    lng >= pp_bbox$lng_min,
    lng <= pp_bbox$lng_max
  ) |>
  select(
    place_id = google_place_id,
    name = google_name,
    rating = google_rating,
    types,
    lat,
    lng
  ) |>
  bind_rows(other_places) |>
  distinct()

all_other_places |> saveRDS("data/google/other_places_from_google.rds")

places_from_google |> 
  as_tibble() |>
  leaflet() |>
  addTiles() |>
  addCircleMarkers(stroke = FALSE, radius = 4, fillOpacity = 0.8)

library(sf)

municipios_puerto_plata <- readRDS("data/municipios_sf.rds")

filter(municipios_puerto_plata, provincia_label == "Puerto Plata")