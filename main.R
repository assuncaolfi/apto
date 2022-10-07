# Libraries --------------------------------------------------------------------

library(glue)
library(hablar)
library(httr2)
library(janitor)
library(jsonlite)
library(knitr)
library(lubridate)
library(readr)
library(telegram)
library(tidyverse)
library(vctrs)

# Functions --------------------------------------------------------------------

repair_names <- partial(
  vec_as_names,
  repair = "unique",
  quiet = TRUE
)

is_interesting <- function(neighbourhood) {
  neighbourhoods <- c(
    "Anchieta",
    "Cidade Jardim",
    "Colégio Batista",
    "Cruzeiro",
    "Funcionários",
    "Gutierrez",
    "Horto Florestal",
    "Luxemburgo",
    "Padre Eustáquio",
    "Prado",
    "Sagrada Família",
    "Santa Efigênia",
    "Santa Lúcia",
    "Santa Tereza",
    "Santo Antônio",
    "Savassi",
    "Serra",
    "Sion",
    "São Lucas",
    "São Pedro"
  )
  neighbourhood %in% neighbourhoods
}

# Wrangle ----------------------------------------------------------------------

data <- "data/url.txt" |>
  read_lines() |>
  request() |>
  req_perform() |>
  resp_body_json() |>
  chuck("hits") |>
  chuck("hits") |>
  map("_source") |>
  map(flatten) |>
  map(as_tibble, .name_repair = repair_names) |>
  bind_rows()
data |> distinct(neighbourhood) |> pull() |> sort() |> dput()

path <- "data/aptos.csv"
if (file.exists(path)) {
  old <- read_csv(path)
} else {
  old <- tibble(id = 0)
}

aptos <- data |>
  filter(
    bedrooms >= 3,
    is_interesting(neighbourhood),
    parkingSpaces >= 1,
    totalCost <= 2300
  ) |>
  mutate(url = glue("https://www.quintoandar.com.br/imovel/{id}/")) |>
  anti_join(old, by = "id") |>
  select(id, totalCost, neighbourhood, bedrooms, url) |>
  arrange(desc(totalCost)) |>
  print(n = Inf)

old |>
  bind_rows(aptos) |>
  write_csv(path)

# Telegram ---------------------------------------------------------------------

bot <- TGBot$new(token = bot_token("aptobot"))
bot$set_default_chat_id("-763183926")
aptos |>
  pmap(list) |>
  map(function(x) paste(names(x), "=", x)) |>
  map_chr(paste, collapse = "\n") |>
  walk(bot$sendMessage)
