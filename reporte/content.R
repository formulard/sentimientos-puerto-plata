
# Packages ----------------------------------------------------------------

library(here)
library(tidytext)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(forcats)
library(lubridate)
library(highcharter)
library(sf)

color_primary <- "#213550"
color_accent <- "#1e4c4c"
color_accent <- "#0096a0"

# Import places and reviews -----------------------------------------------
places <- readRDS(here("data/places.rds")) |> 
  as_tibble() |>
  mutate(
    destination = recode(destination, "Inbert" = "Imbert")
  )

reviews <- readRDS(here("data/classified_reviews.rds")) |>
  as_tibble()

mapa_pp <- readRDS(here::here("data/municipios_sf.rds")) |>
  filter(provincia_label == "Puerto Plata")


mapa_pp_json <- mapa_pp |> 
  select(id, municipio = municipio_label) |>
  rmapshaper::ms_simplify(keep = 0.05) |>
  geojsonio::geojson_list(shapes_macroregion)

# Data wrangling ----------------------------------------------------------

review_data <- places |>
  left_join(reviews) |>
  filter(!is.na(category)) |> 
  mutate(
    #quarter = quarter - months(1),
    short_category = recode(
      category, 
      "very positive" = "positive",
      "very negative" = "negative",
      "mixed" = "neutral"
    )
  ) 

# filter(quarter(date) <= "2025-06-1")

saldos_global <- review_data |>
  count(quarter, short_category) |>
  pivot_wider(names_from = short_category, values_from = n, values_fill = 0) |>
  mutate(
    total = positive + negative + neutral,
    across(where(is.numeric), \(x) x / total * 100),
    saldo = positive - negative
  )

saldo_destino <- review_data |>
  count(destination, short_category) |>
  pivot_wider(names_from = short_category, values_from = n, values_fill = 0) |>
  mutate(
    total = positive + negative + neutral,
    across(where(is.numeric), \(x) x / total * 100),
    saldo = positive - negative,
    destination = fct_reorder(destination, saldo)
  ) |>
  arrange(destination)

# Mapas -------------------------------------------------------------------

frecuencia_municipio <- review_data |>
  mutate(
    destination_label = recode(
      destination,
      "Cabarete" = "Sosúa y Cabarete",
      "Sosúa" = "Sosúa y Cabarete",
      "Puerto Plata" = "Puerto Plata",
      "Imbert"= "Imbert",
      "Punta Rucia" = "Villa Isabela: Punta Rucia",
      .default = "Otro"
    ),
    municipio = recode(
      destination,
      "Cabarete" = "Sosúa",
      "Sosúa" = "Sosúa",
      "Imbert" = "Imbert",
      "Puerto Plata" = "Puerto Plata",
      "Punta Rucia" = "Villa Isabela",
      .default = "Otro"
    )
  ) |> 
  count(municipio, destination_label, name = "value") |>
  mutate(
    value_label = scales::comma(value)
  )

mapa_reviews <- highchart(type = "map") %>%
  hc_add_series(
    mapData = mapa_pp_json,
    data = frecuencia_municipio,
    type = 'map',
    joinBy = c('municipio', 'municipio'),
    value = "value",
    name = 'Reviews',
    dataLabels = list(
      enabled = TRUE, 
      #format = "{point.destination_label}: {point.value_label}",
      formatter = JS("
        function() {
          if (this.point.value !== null && this.point.value !== undefined) {
            return this.point.destination_label + ': ' + this.point.value_label;
          }
          return this.point.municipio;
        }
      ")
    )
  ) %>%
  hc_colorAxis(
    minColor = "#e0f0f0",
    maxColor = "#1e4c4c",
    dataClassColor = "gradient"
  ) |>
  hc_tooltip(
    useHTML = TRUE,
    pointFormat = paste0(
      "<b>{point.destination_label}</b><br>",
      "Reviews: {point.value_label}"
    )
  ) |>
  hc_title(
    text = "Cantidad de comentarios, según municipio",
    style = list(
      color = color_primary,
      fontWeight = "bold"
    )
  )


# Visualizations ----------------------------------------------------------

plot_indicador_global <- saldos_global |>
  filter(year(quarter) > 2020) |>
  mutate(saldo = round(saldo, 2)) |> 
  hchart("column", hcaes(x = quarter, y = saldo), name = "IPDPP") |> 
  hc_colors(colors = color_accent) |>
  hc_title(
    text = "Indicador de percepción del destino",
    style = list(
      color = color_primary,
      fontWeight = "bold"
    )
  )

plot_valoracion_destino <- saldo_destino |>
  arrange(desc(saldo)) |> 
  mutate(saldo = round(saldo, 2)) |> 
  hchart("bar", hcaes(x = destination, y = saldo), name = "IPDPP") |> 
  hc_colors(colors = color_accent) |>
  hc_title(
    text = "Índice de valoración de los destinos",
    style = list(
      color = color_primary,
      fontWeight = "bold"
    )
  )
  

plot_distribucion_respuestas <- review_data |>
  count(quarter, short_category) |>
  filter(year(quarter) > 2020) |>
  mutate(
    short_category = factor(
      short_category,
      levels = c("negative", "neutral", "positive"),
      labels = c("Negativo", "Neutral", "Positivo")
    )
  ) |>
  mutate(
    n = round(n / sum(n) * 100, 2),
    .by = quarter
  ) |>
  hchart("column", hcaes(x = quarter, y = n, group = short_category)) |>
  hc_plotOptions(
    column = list(stacking = "percent", marker = list(enabled = FALSE))
  ) |>
  hc_colors(c("#a02f2f", "#b3b3b3", color_accent)) |>
  hc_title(
    text = "Distribución porcentual de las opiniones",
    style = list(
      color = color_primary,
      fontWeight = "bold"
    )
  ) |>
  hc_legend(
    layout = "horizontal",
    align = "center",
    verticalAlign = "top"
  ) |>
  highcharter::hc_tooltip(pointFormat = "{series.name}: <b>{point.y:.2f} %</b>")
