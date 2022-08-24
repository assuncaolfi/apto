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

grid <- expand_grid(
  url = read_lines("data/url.txt"),
  pag = as.character(1:3)
)
urls <- with(
  grid,
  map2_chr(url, pag, str_replace_all, pattern = "<PAGINA>")
)
aptos <- urls |>
  map(fetch) |>
  bind_rows() |>
  clean_names() |>
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
    data_atualizacao = as.character(data_atualizacao),
    bairro = nome_bairro,
    ponto_real = round(pontuacao / valor_total),
    url = file.path("https://netimoveis.com", url_detalhe_imovel),
  ) |>
  arrange(data_atualizacao, valor_total) |>
  select(
    id, data_atualizacao, bairro,
    valor_total, pontuacao, ponto_real,
    url
  ) |>
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
