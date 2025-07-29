
# Packages ----------------------------------------------------------------
library(tidytext)
library(dplyr)
library(ellmer)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(lubridate)

# Import places and reviews -----------------------------------------------
tripadvisor_places <- readRDS("data/tripadvisors/places.rds")
tripadvisor_reviews <- readRDS("data/tripadvisors/reviews/all_places.rds")

google_places <- readRDS("data/google/places_from_google.rds")
google_reviews <- readRDS("data/google/reviews_from_google.rds")

# Standarize places and reviews -------------------------------------------

# google_reviews <- google_reviews |>
#   select(
#     place_id = google_place_id,
#     date,
#     month,
#     quarter,
#     text,
#     rating = review_rating
#   ) |>
#   mutate(source = "google")
# 
# 
# tripadvisor_reviews <- tripadvisor_reviews |>
#   select(
#     place_id,
#     date,
#     month,
#     quarter,
#     text = content,
#     rating
#   ) |>
#   mutate(
#     source = "tripadvisor",
#     rating = as.numeric(rating)
#   )
# 
# all_revies <- bind_rows(tripadvisor_reviews, google_reviews) |>
#   distinct() |> 
#   relocate(source) |>
#   mutate(
#     review_id = paste0(source, place_id, date, text),
#     review_id = sapply(review_id, \(x) digest::digest(x, algo = "md5"))
#   ) |>
#   relocate(review_id)

# saveRDS(all_revies, "data/all_reviews/all_reviews.rds")

classified_reviews <- readRDS("data/all_reviews/reviews_with_ai_classification.rds")

# Classification with tidytext --------------------------------------------
sentiments <- get_sentiments("bing")

tokens <- classified_reviews |>
  unnest_tokens(output = "word", input = text) |>
  anti_join(get_stopwords())

tokens_summary <- tokens %>%
  count(quarter, source, word, sort = TRUE) |>
  left_join(sentiments) |>
  filter(!is.na(sentiment))


classified_reviews |>
  summarise(
    score = mean(score, na.rm = TRUE),
    .by = quarter
  ) |>
  filter(year(quarter) >= 2020) |> 
  ggplot(aes(x = quarter, y = score)) +
  geom_col()

classified_reviews |>
  mutate(
    short_category = recode(
      category, 
      "very positive" = "positive",
      "very negative" = "negative"
    )
  ) |>
  drop_na() |> 
  relocate(short_category, .after = category) |>
  count(quarter, short_category, sort = TRUE) |>
  pivot_wider(names_from = short_category, values_from = n) |>
  mutate(
    total = positive + negative + neutral,
    positive_p = positive / total * 100,
    negative_p = negative / total * 100,
    saldo = positive_p - negative_p
  ) |>
  filter(year(quarter) > 2020) |> 
  ggplot(aes(x = quarter, y = saldo)) +
  geom_col() +
  labs(
    x = NULL, 
    y = "Saldo de opinión", 
    title = "Saldo de valoración del destino Puerto Plata",
    subtitle = "Trimestra: Marzo 2021 - Junio 2025"
  )

summary_reviews <- classified_reviews |>
  summarise(
    frequency = n(),
    .by = c(category, quarter) 
  ) |>
  arrange(desc(quarter)) |>
  mutate(
    precent = frequency / sum(frequency) * 100,
    .by = quarter
  )




summary_reviews |>
  filter(year(quarter) > 2020) |> 
  ggplot(aes(x = quarter, y = precent, colour = category)) +
  geom_line()

# Classification with LLM -------------------------------------------------

all_revies

get_sentiment_scores <- function(
    data,
    text_column = "text",
    model = "gpt-3.5-turbo",
    batch_size = 10,
    sleep_seconds = 1,
    max_reviews = Inf,
    verbose = TRUE
) {
  openai_key <- Sys.getenv("OPENAI_API_KEY")
  if (openai_key == "") stop("OPENAI_API_KEY env var not set.")
  
  # Helper: format reviews into prompt
  build_prompt <- function(texts) {
    items <- paste0(seq_along(texts), ". ", texts, collapse = "\n")
    paste0(
      "For each of the following reviews, classify the sentiment in one of the following categories: ",
      "very negative, negative, neutral, positive, or very positive. ",
      "Also assign a numeric score from 1 (very negative) to 5 (very positive).\n",
      "Respond with a pure JSON array (no markdown or code formatting), like this:\n",
      '[{"category": "positive", "score": 4}, {"category": "neutral", "score": 3}, ...]\n\n',
      "Reviews:\n", items
    )
  }
  
  # Helper: send a batch of reviews and parse response
  classify_batch <- function(texts) {
    prompt <- build_prompt(texts)
    
    req <- httr2::request("https://api.openai.com/v1/chat/completions") |>
      httr2::req_headers(
        Authorization = paste("Bearer", openai_key),
        `Content-Type` = "application/json"
      ) |>
      httr2::req_body_json(list(
        model = model,
        messages = list(
          list(role = "system", content = "You are a helpful sentiment analysis assistant."),
          list(role = "user", content = prompt)
        ),
        temperature = 0
      ))
    
    result <- tryCatch({
      resp <- httr2::req_perform(req)
      json <- httr2::resp_body_json(resp)
      content <- json$choices[[1]]$message$content
      parsed <- jsonlite::fromJSON(content)
      if (!is.data.frame(parsed) || !all(c("category", "score") %in% names(parsed))) {
        stop("Invalid JSON structure returned.")
      }
      parsed
    }, error = function(e) {
      if (verbose) message("Batch failed: ", e$message)
      data.frame(category = rep(NA_character_, length(texts)),
                 score = rep(NA_real_, length(texts)))
    })
    
    Sys.sleep(sleep_seconds)
    result
  }
  
  # Limit and split data
  data_subset <- dplyr::slice_head(data, n = max_reviews)
  text_list <- split(data_subset[[text_column]],
                     ceiling(seq_along(data_subset[[text_column]]) / batch_size))
  
  if (verbose) {
    cat("Processing", nrow(data_subset), "reviews in", length(text_list), "batches...\n")
  }
  
  # Run all batches
  all_results <- purrr::map(text_list, classify_batch)
  results_df <- dplyr::bind_rows(all_results)
  
  # Combine with original data
  dplyr::bind_cols(data_subset, results_df)
}


all_revies |>
  head(20) |>
  get_sentiment_scores()

# Plots -------------------------------------------------------------------
all_revies |>
  filter(year(quarter) > 2021) |> 
  summarise(
    rating = mean(rating, na.m = TRUE),
    .by = c(source, quarter)
  ) |>
  ggplot(aes(x = quarter, y = rating, color = source)) +
  geom_line()

ellmer::openchat_openai(model = "gpt-3.5-turbo", system_prompt = "What are you good for?", )

saldos_tokens |>
  ggplot(aes(x = quarter, y = saldo, color = source)) +
  geom_line()

