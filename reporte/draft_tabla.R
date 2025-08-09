library(reactable)
library(htmltools)

tabla <- tibble::tibble(
 img    = c("google.svg", "tripadvisor.svg", "instagram.svg", "twitter.png", "facebook.svg"),
 source = c("google", "tripadvisor", "instragram", "twitter", "facebook"),
 h = c("24px", "24px", "24px", "16px", "16px"),
 positive = sample(seq(0.5, 1, 0.01), 5),
 neutral  = (1 - positive)  / 2,
 negative =  1 - neutral - positive,
 saldo  = (positive - negative) * 100
)

tabla |> 
  reactable(
    columns = list(
      h = colDef(show = FALSE),
      source = colDef(show = FALSE),
      img = colDef(cell = function(value, i) {
        img(src = sprintf("/assets/%s", value), style = glue::glue("height: {tabla$h[i]};"), alt = value)
      })
  ),
)

