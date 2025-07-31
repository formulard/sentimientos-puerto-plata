box::use(
  htmltools[tags, span, div],
  glue[glue],
  dplyr[
    mutate,
    across,
    everything,
    case_when
  ],
  scales[comma],
)

#' @export
info_header <- function(previous_date_label, current_time) {
  div(
    class = "info-header",
    div(
      class = "info-container",
      shiny::icon("calendar-days", class = "icon"),
      glue("Comparación respecto al trimestre {previous_date_label}")
    )
  )
}
 
sign <- function(value) {
  value <- as.numeric(value)
  dplyr::case_when(
    value == 0 ~ "=",
    value  > 0 ~ "+",
    value  < 0 ~ "",
  )
}

change_class <- function(value) {
  value <- as.numeric(value)
  dplyr::case_when(
    value == 0 ~ "",
    value  > 0 ~ "increase",
    value  < 0 ~ "decrease"
  )
}

#' @export
summary_cards <- function(values, benchmark = "trimestre pasado") {
  
  checkmate::assert_names(
    names(values),
    subset.of = c(
      "saldo", "lag_saldo", "d_saldo", 
      "reviews","lag_reviews", "d_reviews", 
      "places"
    )
  )
  
  tags$dl(
    class = "stats-container",
    
    tags$div(
      tags$div(
        class = "stat-header",
        tags$dt(class = "stat-name", "Índice de percepción"),
        tags$div(
          class = glue("stat-change {change_class(values$d_saldo)}"),
          glue("{sign(values$d_saldo)}{values$d_saldo}")
        )
      ),
      tags$dd(
        class = "stat-details",
        tags$div(
          class = "stat-value",
          glue("{values$saldo}"),
          tags$span(class = "stat-previous", glue("{benchmark}: {values$lag_saldo}"))
        )
      )
    ),
    
    tags$div(
      tags$div(
        class = "stat-header",
        tags$dt(class = "stat-name", "Cantidad de reviews"),
        tags$div(
          class = glue("stat-change {change_class(values$d_reviews)}"),
          glue("{sign(values$d_reviews)}{scales::comma(values$d_reviews)}")
        )
      ),
      tags$dd(
        class = "stat-details",
        tags$div(
          class = "stat-value",
          glue("{values$reviews}"),
          tags$span(class = "stat-previous", glue("{benchmark}: {scales::comma(values$lag_reviews)}"))
        )
      )
    ),
    
    tags$div(
      tags$dt(class = "stat-name", "Lugares consultados"),
      tags$dd(
        class = "stat-details",
        tags$div(
          class = "stat-value",
          values$places
        )
      )
    )
  )
}

#' Icon to indicate trend: unchanged, up, down, or new
#' @export
trend_indicator <- function(variation) {
  if (is.na(variation)) return()
  value <- dplyr::case_when(
    variation == 0 ~ "unchanged",
    variation  > 0 ~ "up",
    variation   < 0 ~ "down"
  )
  
  label <- switch(
    value,
    unchanged = "Unchanged",
    up = "Trending up",
    down = "Trending down", 
    new = "New"
  )
  
  # Add img role and tooltip/label for accessibility
  args <- list(role = "img", title = label)
  
  if (value == "unchanged") {
    args <- c(args, list("–", style = "color: #6B7280; font-weight: 700"))
  } else if (value == "up") {
    args <- c(args, list(shiny::icon("caret-up"), style = "color: #22C55E"))
  } else if (value == "down") {
    args <- c(args, list(shiny::icon("caret-down"), style = "color: #EF4444"))
  } else {
    args <- c(args, list(shiny::icon("circle"), style = "color: #2e77d0; font-size: 0.6rem"))
  }
  do.call(htmltools::tags$span, args)
}

#' @export
report_table <- function(tasas_to_table, onClick = "select") {
  box::use(reactable[reactable, reactableTheme, colFormat, colDef])
  
  tasas_to_table |> 
    reactable(
      compact = TRUE,
      pagination = FALSE,
      defaultColDef = colDef(
        headerClass = "table-header",
        format = colFormat(separators = TRUE, digits = 2),
        minWidth = 50,
        footerStyle = list(fontWeight = "bold")
      ),
      class = "tasas-table",
      theme = reactableTheme(cellPadding = "8px 12px"),
      highlight = TRUE, 
      striped = TRUE,
      columns = list(
        tipo_entidad = colDef(name = "Tipo de entidad", minWidth = 150),
        entidad = colDef(name = "Entidad", minWidth = 150),
        efectivo_compra = colDef(name = "Compra"),
        efectivo_venta = colDef(name = "Venta"),
        cheques_venta = colDef(name = "Venta"),
        cheques_compra = colDef(name = "Compra"),
        transferencia_compra = colDef(name = "Compra"),
        transferencia_venta = colDef(name = "Venta")
      ),
      columnGroups = list(
        colGroup(name = "Efectivo", columns = c("efectivo_compra", "efectivo_venta")),
        colGroup(name = "Cheques", columns = c("cheques_compra", "cheques_venta")),
        colGroup(name = "Transferencias", columns = c("transferencia_compra", "transferencia_venta"))
      ),
      onClick = onClick
    )
}

value_change_span <- function(data, value, column, index) {
  if (is.na(value)) return("")
  
  lag_column <- data[[glue("lag_{column}")]][index] 
  d_column <- data[[glue("d_{column}")]][index] 
  trend_icon <-  trend_indicator(d_column)
  
  sign <- case_when(
    d_column == 0 ~ "=",
    d_column  > 0 ~ "+",
    d_column  < 0 ~ "",
  )
  
  d_column <- scales::comma(d_column, 0.01, prefix = sign)
  value <- scales::comma(value, 0.01)
  
  span(
    span(span(style="margin-right: 3px; display: inline-block;", trend_icon), value),
    span(class = "var", style="color: #6a7282; margin-left: 5px;", glue("({d_column})"))
  )
}

average_change_span <- function(data, column) {
  value <- data[[column]] |> mean(na.rm = TRUE)
  d_value <- data[[glue("d_{column}")]] |>  mean(na.rm = TRUE)
  trend_icon <-  trend_indicator(d_value)
  
  sign <- case_when(
    d_value == 0 ~ "=",
    d_value  > 0 ~ "+",
    d_value  < 0 ~ "",
  )
  
  d_column <- scales::comma(d_value, 0.01, prefix = sign)
  value <- scales::comma(value, 0.01)
  
  span(
    span(span(style="margin-right: 3px; display: inline-block;", trend_icon), value),
    span(class = "var", style="color: #6a7282; margin-left: 5px;", glue("({d_column})"))
  )
  
}