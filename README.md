# Indicador de Precepción del destino Puerto Plata

El proyecto extrae información de fuentes abiertas con opiniones y comentarios sobre localidades, negocios y destinos en Puerto Plata, con la finalidad de clasificarlos con LLM y sintetizar un índice sobre la valoración del destino.

### Fuentes consultadas

1. Google reviews
2. TripAdvisor
3. Trivago
4. Instagram
5. Facebook

## Variables de ambiente

- `OPENAI_API_KEY`: Para clasificar los reviews usando chatgpt
- `TRIPADVISOR_API_KEY`: Para consultar lugares y reviews usando el API de TripAdvisor
- `GOOGLE_API_KEY`: Para consultar ubicación, ratings y reviews de lugares usando google place API
