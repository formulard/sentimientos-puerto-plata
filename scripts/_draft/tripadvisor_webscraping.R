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

get_attraction_reviews_several_pages <- function(url, sleep = 5, n_pages = 1) {
  safe_get_attraction_reviews <- purrr::possibly(get_attraction_reviews, otherwise = data.frame())

    base_page <-  url

  following_pages <- purrr::map_chr(
    seq(10, n_pages * 10, 10),
    \(page) stringr::str_replace(base_page, "(^.+-Reviews-)(.+$)", paste0("\\1or", page, "-\\2"))
  )
  
  c(base_page, following_pages) |>
    purrr::map(safe_get_attraction_reviews) |>
    purrr::list_rbind()
}

get_place_reviews <- function(place_url, sleep = 5) {
  # At some point it was rendering a different page layout
  # check with `html$view` if date and content are empty

  box::use(
    rvest[read_html_live, html_elements, html_element, html_text, html_attr],
    lubridate[mdy],
    tibble[tibble],
    purrr[map, list_rbind, ]
  )

  html <- read_html_live(place_url)
  Sys.sleep(sleep)

  review_card_selector <- "div.JVaPo.Gi.kQjeB"

  df <- html_elements(html, css = review_card_selector) |>
    map(
      \(card) {
        review_title <- html_element(card, '[data-test-target="review-title"]') |>
          html_text()
        
        review_content <- html_element(card, '[data-automation="ugcReviewReadMore_Content"]') |>
          html_text()
        
        review_date <- html_element(card, 'span[title]') |>
          html_attr("title") |>
          mdy()
        
        tibble(
          date = review_date,
          title = review_title,
          content = review_content
        ) 
      }) |>
    list_rbind()
  
  html$session$close()
  
  df
}

get_place_reviews_several_pages <- function(url, sleep = 5, n_pages = 1) {
  safe_get_place_reviews <- purrr::possibly(get_place_reviews, otherwise = data.frame())
  
  base_page <-  url
  
  following_pages <- purrr::map_chr(
    seq(10, n_pages * 10, 10),
    \(page) stringr::str_replace(base_page, "(^.+-Reviews-)(.+$)", paste0("\\1or", page, "-\\2"))
  )
  
  c(base_page, following_pages) |>
    purrr::map(safe_get_place_reviews) |>
    purrr::list_rbind()
}


