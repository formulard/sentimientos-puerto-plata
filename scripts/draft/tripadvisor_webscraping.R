
get_places_from_destination <- function(page_url) {
  box::use(
    rvest[read_html_live, html_elements, html_element, html_text, html_attr],
    stringr[str_replace_all],
    purrr[map, map_chr],
    dplyr[tibble]
  )
  
  html <- read_html_live(page_url)
  
  # It looks like if this selectors are dynamic, I think should be changed in the near future.
  # Consider reviewing this if the function fails in the future.
  # TODO: use the url container as a reference for this selector
  place_name <- html_elements(html, '.biGQs._P.fiohW.alXOW.NwcxK.GzNcM.ytVPx.UTQMg.RnEEZ.OgHoE') |>
    html_text()
  
  place_ulr_pattern <- place_name |> 
    str_replace_all("[^A-z\\d]", "_") |>
    str_replace_all("_+", "_")
  
  place_url_selector <- paste0('[href*="', place_ulr_pattern, '"]') |>
        as.character()
  
  place_rating_selector <- paste(place_url_selector, '[data-automation="bubbleRatingValue"]')
  
  place_ratings <- map_chr(place_rating_selector, \(selector) {
    html_element(html, selector) |>
      html_text()
  })
  
  
  place_url <- place_url_selector |>
    map_chr(
      \(selector) {      
        html_element(html, css = selector) |>
          html_attr("href")
      }
    )
  
  tibble(
    name = place_name,
    rating = place_ratings,
    url = paste0("https://www.tripadvisor.com", place_url),
    type = stringr::str_extract(url, "Attraction|Hotel|Restaurant")
  )
}


places <- get_places_from_destination(
  "https://www.tripadvisor.com/Tourism-g317144-Cabarete_Puerto_Plata_Province_Dominican_Republic-Vacations.html")


place_url <- places$url[1]


get_attraction_reviews <- function(place_url, sleep = 5) {
  # Attractions have a different reviews layout than places
  box::use(
    rvest[read_html_live, html_elements, html_text],
    stringr[str_remove],
    lubridate[mdy],
    dplyr[tibble],
    purrr[list_rbind, map]
  )
  
  html <- read_html_live(place_url)
  Sys.sleep(sleep)
  
  reviews <- html_elements(html, '[data-automation="reviewCard"]')
  
  data <- purrr::map(
    seq_along(reviews),
      \(node_index) {
        title <- reviews[[node_index]] |>
          html_elements("a .yCeTE") |>
          html_text()

        content <- reviews[[node_index]] |>
          html_elements(".JguWG .yCeTE") |>
          html_text()

        date <- reviews[[node_index]] |>
          html_elements(".biGQs._P.pZUbB.ncFvv.navcl") |>
          html_text() |>
          str_remove("Written ") |>
          mdy()
        
        tibble(date, title, content)
      }
    ) |>
    list_rbind()
  
  data
}

urls <- places |> 
  dplyr::filter(type == "Attraction") |>
  dplyr::pull(url)


reviews <- list()


if (length(reviews) < length(urls)) {
  for (index in seq(length(reviews) + 1, length(urls))) {
    reviews[[index]] <- get_attraction_reviews(urls[index])
  }
}

reviews |>
  purrr::list_rbind() |> 
  dplyr::group_by(title, content) |>
  dplyr::slice(1) |>
  dplyr::ungroup()









  
