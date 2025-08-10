## No need to run this if there aren't new reviews

# Dependencies ------------------------------------------------------------
library(dplyr)
library(purrr)

box::purge_cache()

box::use(
  scripts/logic/classification_open_ai[
    classify_reviews,
    classify_reviews_in_batch,
  ]
)

# Import reviews ----------------------------------------------------------
reviews <- readRDS("data/reviews.rds") |>
  dplyr::as_tibble()


existing_reviews <- readRDS("data/classified_reviews.rds")

new_reviews <- anti_join(reviews, existing_reviews) |> 
  filter(text != "")
  
classified_reviews <- new_reviews |>
  select(review_id, text) |>
  filter(text != "") |>
  classify_reviews_in_batch(
    cache_path = "data/cache/reviews_in_progress.rds",
    batch_size = 10, 
    model = "gpt-4o-mini"
  ) |> 
  as_tibble()


new_reviews |> 
  left_join(classified_reviews) |>
  filter(!is.na(category)) |>
  bind_rows(existing_reviews) |>
  saveRDS("data/classified_reviews.rds")