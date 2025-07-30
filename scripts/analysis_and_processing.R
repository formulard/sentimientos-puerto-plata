# Packages ----------------------------------------------------------------
library(tidytext)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(forcats)
library(lubridate)

# Import places and reviews -----------------------------------------------
places <- readRDS("data/places.rds") |> 
  as_tibble() |>
  mutate(
    destination = recode(destination, "Inbert" = "Imbert")
  )


reviews <- readRDS("data/classified_reviews.rds") |>
  as_tibble()

# Data wrangling ----------------------------------------------------------
review_data <- places |>
  left_join(reviews) |>
  filter(!is.na(category)) |> 
  mutate(
    short_category = recode(
      category, 
      "very positive" = "positive",
      "very negative" = "negative",
      "mixed" = "neutral"
    )
  )

saldos_global <- review_data |>
  count(quarter, short_category) |>
  pivot_wider(names_from = short_category, values_from = n, values_fill = 0) |>
  mutate(
    total = positive + negative + neutral,
    across(where(is.numeric), \(x) x / total * 100),
    saldo = positive - negative
  )

saldo_por_destino_serie <- review_data |>
  count(quarter, destination, short_category) |>
  pivot_wider(names_from = short_category, values_from = n, values_fill = 0) |>
  mutate(
    total = positive + negative + neutral,
    across(where(is.numeric), \(x) x / total * 100),
    saldo = positive - negative,
    destination = fct_reorder(destination, saldo)
  )

saldo_por_destino <- review_data |>
  count(destination, short_category) |>
  pivot_wider(names_from = short_category, values_from = n, values_fill = 0) |>
  mutate(
    total = positive + negative + neutral,
    across(where(is.numeric), \(x) x / total * 100),
    saldo = positive - negative,
    destination = fct_reorder(destination, saldo)
  ) |>
  arrange(destination)

saldo_por_destino_anual <- review_data |>
  mutate(year = year(quarter) |> as.character()) |> 
  count(year, destination, short_category) |>
  pivot_wider(names_from = short_category, values_from = n, values_fill = 0) |>
  mutate(
    total = positive + negative + neutral,
    across(where(is.numeric), \(x) x / total * 100),
    saldo = positive - negative,
    destination = fct_reorder(destination, saldo)
  )


saldo_por_destino_anual |>
  filter(year > 2020) |> 
  ggplot(aes(x= year, y = saldo)) +
  geom_col() +
  facet_wrap(~destination)




# Word and tokenizations --------------------------------------------------
sentiments <- get_sentiments("bing")

bigrams <- review_data %>%
  select(destination, text, quarter) |> 
  unnest_tokens(bigram, text, token = "ngrams", n = 2) |>
  separate(bigram, into = c("word1", "word2"), sep = " ") |>
  anti_join(get_stopwords(), c("word1" = "word")) |> 
  anti_join(get_stopwords(), c("word2" = "word")) |> 
  mutate(bigram = paste(word1, word2)) |> 
  drop_na()

tokens <- review_data |>
  select(destination, text, quarter) |> 
  unnest_tokens(output = "word", input = "text") |>
  anti_join(get_stopwords()) 

token_count <- tokens |>
  count(word, sort = T)

bigrams_count <- bigrams |>
  count(bigram, sort = T)

# Visualizations ----------------------------------------------------------
review_data |>
  count(quarter) |>
  ggplot(aes(x = quarter,  y = n)) +
  geom_col() +
  labs(x = NULL, y = NULL, title = "Cantidad de reviews, según trimestre")

review_data |>
  count(destination, sort =  T) |>
  mutate(destination = fct_reorder(destination, n)) |> 
  ggplot(aes(x = n,  y = destination)) +
  geom_col() +
  labs(x = NULL, y = NULL, title = "Cantidad de reviews, según destino")

review_data |>
  count(quarter, short_category) |>
  filter(year(quarter) > 2020) |>
  ggplot(aes(x = quarter, y = n, fill = short_category)) +
  geom_col(position = "fill") +
  theme(legend.position = "bottom") +
  scale_y_continuous(labels = scales::percent) +
  labs(
    x = NULL, 
    fill = NULL,
    y = "Porcentaje", 
    title = "Distribución porcentual de las impresiones"
  )

saldos_global |>
  filter(year(quarter) > 2020) |> 
  ggplot(aes(x = quarter, y = saldo)) +
  geom_col() +
#  geom_hline(color = "red", yintercept = 50) +
  labs(
    x = NULL, 
    y = "Saldo de opinión", 
    title = "Saldo de valoración del destino Puerto Plata",
    subtitle = "Trimestra: Marzo 2021 - Junio 2025"
  )
  
saldo_por_destino |>
  ggplot(aes(x = saldo, y = destination)) +
  geom_col() +
  labs(
    x = "Saldo de opinión", 
    y = NULL,
    title = "Valocarión según destino",
    subtitle = "Promedio 2021 - 2025"
  )

# Save excel outputs ------------------------------------------------------

review_data |>
  mutate(
    types = purrr::map_chr(types, \(x) paste(x, collapse = ", "))
  ) |> 
  writexl::write_xlsx("data/outputs/review_data.xlsx")

places |> 
  mutate(
    types = purrr::map_chr(types, \(x) paste(x, collapse = ", "))
  ) |>
  writexl::write_xlsx("data/outputs/places_data.xlsx")

list(
  saldos_global = saldos_global,
  saldo_por_destino = saldo_por_destino,
  saldo_por_destino_serie = saldo_por_destino_serie
) |>
  writexl::write_xlsx("data/outputs/saldos_data.xlsx")

list(
  palabras = tokens,
  bigramas = bigrams,
  frecuencia_palabras = token_count,
  frecuecnia_bigramas = bigrams_count
) |>
  writexl::write_xlsx("data/outputs/frecuencia_palabras_y_bigramas.xlsx")
