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

clasified_reviews <- reviews |>
  select(review_id, text) |>
  classify_reviews_in_batch(batch_size = 15)


clasified_reviews |> 
  purrr::list_rbind() |>
  left_join(all_reviews) |>
  as_tibble() |>
  saveRDS("data/all_reviews/reviews_with_ai_classification.rds")
