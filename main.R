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
  map(
    as_tibble,
    .name_repair = ~ vctrs::vec_as_names(
      ...,
      repair = "unique",
      quiet = TRUE
    )
  ) |>
  bind_rows()

regions <- c(
  "Anchieta",
  "Carmo",
  "Cidade Jardim",
  "Cruzeiro",
  "Floresta",
  "Luxemburgo",
  "Prado",
  "Sagrada Família",
  "Santa Efigenia",
  "Santa Tereza",
  "Santo Antônio",
  "Sion",
  "São Lucas",
  "São Pedro"
)

path <- "data/aptos.csv"
if (file.exists(path)) {
  old <- read_csv(path)
} else {
  old <- tibble(id = 0)
}

aptos <- data |>
  filter(
    bedrooms >= 2,
    forRent == TRUE,
    # regionName %in% regions,
    parkingSpaces >= 1,
    totalCost <= 2500
  ) |>
  mutate(url = glue("https://www.quintoandar.com.br/imovel/{id}/")) |>
  anti_join(old, by = "id") |>
  select(id, totalCost, regionName, bedrooms, url) |>
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
