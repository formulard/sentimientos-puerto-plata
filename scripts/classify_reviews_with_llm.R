# Dependencies ------------------------------------------------------------
library(dplyr)

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

classified_reviews <- reviews |>
  select(review_id, text) |>
  filter(text != "") |>
  classify_reviews_in_batch(
    cache_path = "data/cache/reviews_in_progress.rds",
    batch_size = 10
  )



reviews |>
  left_join(classified_reviews) |>
  as_tibble() |>
  filter(!is.na(category)) |>
  saveRDS("data/classified_reviews.rds")

reviews

