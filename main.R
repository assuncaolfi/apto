# Libraries --------------------------------------------------------------------

library(httr2)
library(janitor)
library(jsonlite)
library(knitr)
library(lubridate)
library(readr)
library(telegram)
library(tidyverse)

# Functions --------------------------------------------------------------------

fetch <- function(url) {
  url |>
    httr2::request() |>
    httr2::req_perform() |>
    httr2::resp_body_string() |>
    jsonlite::fromJSON() |>
    purrr::chuck("lista") |>
    dplyr::as_tibble()
}

# Data -------------------------------------------------------------------------

urls <- read_lines("data/url.txt")
aptos <- urls |>
  map(fetch) |>
  bind_rows() |>
  clean_names() |>
  print() |>
  rename(id = imovel_san_id) |>
  distinct(id, .keep_all = TRUE)

data <- aptos |>
  mutate(
    data_atualizacao = as_date(as_datetime(data_hora)),
    valor_total = valor_locacao + valor_condominio + valor_iptu
  ) |>
  filter(
    data_atualizacao == today(),
    valor_total <= 2300
  ) |>
  mutate(
    bairro = nome_bairro,
    data_atualizacao = as.character(data_atualizacao),
    url = file.path("https://netimoveis.com", url_detalhe_imovel)
  ) |>
  arrange(data_atualizacao, valor_total) |>
  select(id, data_atualizacao, valor_total, bairro, url) |>
  print()

# Telegram ---------------------------------------------------------------------

bot <- TGBot$new(token = bot_token("aptobot"))
bot$set_default_chat_id("-763183926")
msg <- paste(as.character(kable(data, "simple")), collapse = "%0A")

sep <- "\n"
data |>
  pmap(list) |>
  map(function(x) paste(names(x), "=", x)) |>
  map_chr(paste, collapse = sep) |>
  paste(collapse = paste0(sep, sep)) |>
  bot$sendMessage()
