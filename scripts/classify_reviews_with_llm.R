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


# Outsorced reviews -------------------------------------------------------

outsourced_reviews <- readxl::read_excel("data/redes_sociales/reviews-redes-sociales.xlsx") |>
  as_tibble() |> 
  janitor::clean_names() |>
  tidyr::fill(actividad, entidad, red_social) |>
  dplyr::filter(!is.na(comentarios), !is.na(fecha)) |> 
  dplyr::mutate(
    date = janitor::excel_numeric_to_date(as.numeric(fecha)),
    quarter = lubridate::floor_date(date, "quarter"),
    month   = lubridate::floor_date(date, "month")
  ) |> 
  select(
    source = red_social, 
    actividad,  
    place = entidad, 
    date,
    quarter,
    month,
    text = comentarios
  ) |> 
  mutate(
    review_id = map_chr(paste(date, actividad, place, text), \(x) digest::digest(x))
  ) |>
  relocate(review_id)


classified_out_rv <- outsourced_reviews |>
  distinct() |> 
  select(review_id, text) |>
  filter(text != "") |>
  classify_reviews_in_batch(
    cache_path = "data/cache/outsourced_in_progress.rds",
    batch_size = 10, 
    model = "gpt-4o-mini"
  ) |> 
  as_tibble()
  
outsourced_reviews |>
  left_join(classified_out_rv) |>
  saveRDS("data/classified_sotial_network_reviews.rds")
