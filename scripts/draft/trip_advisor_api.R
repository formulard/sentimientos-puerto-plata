library(httr)

location_df <- function(api_response) {
  address <- api_response$address_obj |> tibble::as_tibble()
  place_name_id <- api_response[c("location_id", "name")] |> tibble::as_tibble()
  
  dplyr::bind_cols(place_name_id, address)
}

get_location <- function(
    search_query, 
    api_key = Sys.getenv("TRIPADVISOR_API_KEY"), 
    lan = "en"
) {
  if (api_key == "") stop("Provide an API key, set it an env variable TRIPADVISOR_API_KEY ")
  
  url <- "https://api.content.tripadvisor.com/api/v1/location/search"
  res <- httr::GET(
    url = url,
    query = list(
      searchQuery = search_query,
      language = "en",
      key = api_key
    ),
    httr::add_headers(
      accept = "application/json"
    )
  )
  
  parsed <- httr::content(res, as = "parsed", type = "application/json")
  
  if (length(parsed$data) == 0) return(tibble::tibble())
  
  parsed$data |> 
    purrr::map(location_df) |> 
    purrr::list_rbind()
}


locations <- get_location("Malecon Puerto Plata, República Dominicana")


review_df <- function(review) {
  dplyr::tibble(
    id = review$id,
    published_date = review$published_date,
    rating = review$rating,
    title = review$title,
    text = review$text,
    user = review$user$username,
    lang = review$language
  )
}

get_reviews <- function(
    location_id,
    limit = 15,
    api_key = Sys.getenv("TRIPADVISOR_API_KEY"),
    lan = "en",
    skip = 0
) {
  if (api_key == "") stop("Provide an API key, or set it as env var TRIPADVISOR_API_KEY")
  
  url <- sprintf("https://api.content.tripadvisor.com/api/v1/location/%s/reviews", location_id)
  
  res <- httr::GET(
    url = url,
    query = list(
      language = lan,
      limit = limit,
      key = api_key,
      offset = skip
    ),
    httr::add_headers(
      accept = "application/json"
    )
  )
  
  parsed <- httr::content(res, as = "parsed", type = "application/json")
  
  if (length(parsed$data) == 0) return(tibble::tibble())
  
  parsed$data |>
    purrr::map(review_df) |>
    purrr::list_rbind()
}

rvs <- get_reviews(locations$location_id[2])

rvs$text

get_location("Cabarete, Puerto Plata")
