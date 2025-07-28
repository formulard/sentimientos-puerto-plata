

#' Helper to build prompt for a batch of texts
build_prompt <- function(texts) {
  items <- paste0(seq_along(texts), ". ", texts, collapse = "\n")
  paste0(
    "For each of the following reviews, classify the sentiment in one of the following categories: ",
    "very negative, negative, neutral, positive, or very positive. ",
    "Also assign a numeric score from 1 (very negative) to 5 (very positive).",
    "Respond with a pure JSON array (no markdown or code formatting), like this:\n",
    '[{"category": "positive", "score": 4}, {"category": "neutral", "score": 3}, ...]\n\n',
    "Reviews:\n", items
  )
}

#' Classify reviews uing OpenAI llm
#' 
#' @param reviews a dataframe with two columns: review_id and text
#' @export
classify_reviews <- function(
    reviews,
    model = "gpt-3.5-turbo", 
    openai_key = Sys.getenv("OPENAI_API_KEY"),
    sleep = 1
) {
  prompt <- build_prompt(reviews$text)

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
  
  tryCatch({
    resp <- httr2::req_perform(req)
    json <- httr2::resp_body_json(resp)

    content <- json$choices[[1]]$message$content
    content_clean <- gsub("^```json\\s*|\\s*```$", "", content)
    
    parsed <- jsonlite::fromJSON(content_clean)

    if (!is.data.frame(parsed) || !all(c("category", "score") %in% names(parsed))) {
      stop("Invalid JSON structure returned.")
    }

    Sys.sleep(sleep)
    cbind(reviews[, c("review_id")], parsed)
  }, error = function(e) {
    return(data.frame())
  })
}


#' Split dataframe into batches
split_into_batches <- function(data, batch_size = 10) {
  n <- nrow(data)
  batches <- split(seq_len(n), ceiling(seq_len(n) / batch_size))
  
  lapply(batches, \(batch_indexs) data[batch_indexs, ])
}

#' @export
classify_reviews_in_batch <- function(
    reviews,
    model = "gpt-3.5-turbo", 
    batch_size = 10,
    openai_key = Sys.getenv("OPENAI_API_KEY"),
    sleep = 1,
    cache_path = "data/all_reviews/cache/classified_reviews.rds",
    output_path = "data/all_reviews/classified_reviews.rds",
    max_reviews = Inf
) {

  if (openai_key == "") stop("OPENAI_API_KEY env var not set.")
  data_subset <- dplyr::slice_head(reviews, n = max_reviews)
  
  data_batches <- split_into_batches(data_subset, batch_size)
  
  if (!is.null(cache_path) && file.exists(cache_path)) {
    logger::log_info("Loading cached results from: {cache_path}")
    result_batches <- readRDS(cache_path)
  } else {
    result_batches <- vector("list", length(data_batches))
  }
  
  empty <- which(sapply(result_batches, is.null))
  total_batches <- length(data_batches)
  batches_done <- total_batches - length(empty)
  
  logger::log_info("Processing {length(empty)} batches of {batch_size} reviews each.")
  
  if (length(empty) > 0) {
    for (index in empty) {
      tictoc::tic()
      current_data <- data_batches[[index]]
      current_result <- classify_reviews(current_data)
      
      result_batches[[index]] <- current_result
      
      if (!is.null(cache_path)) saveRDS(result_batches, cache_path)
      
      batches_done <- batches_done + 1
      progress_pct <- round(batches_done / length(empty) * 100, 2)
      
      tictoc::toc(log = TRUE)
      logger::log_info("Iteration {batches_done} out of {total_batches}: {progress_pct} %")
    }
  }
  
  result_batches |>
    purrr::list_rbind()
}