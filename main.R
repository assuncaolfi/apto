# Libraries --------------------------------------------------------------------

library(hablar)
library(httr2)
library(janitor)
library(jsonlite)
library(knitr)
library(lubridate)
library(readr)
library(telegram)
library(tidyverse)

# Functions --------------------------------------------------------------------

wrangle <- function(data) {
  listing <- flatten(data$listing)
  required <- c(
    "externalId", "pricingInfos", "neighborhood",
    "bathrooms", "bedrooms", "suites", "parkingSpaces",
    "totalAreas"
  )
  if (!all(required %in% names(listing))) return(NULL)
  with(
    listing,
    tibble(
      id = externalId,
      aluguel = pricingInfos$price,
      bairro = neighborhood,
      area = totalAreas,
      banheiros = bathrooms,
      condominio = pricingInfos$monthlyCondoFee,
      iptu = pricingInfos$yearlyIptu,
      quartos = bedrooms,
      suites = suites,
      vagas = parkingSpaces,
      link = data$link$href
    )
  )
}

# Wrangle ----------------------------------------------------------------------

url <- read_lines("data/url.txt")
json <- url |>
  request() |>
  req_headers(`x-domain` = "www.vivareal.com.br") |>
  req_perform() |>
  resp_body_json()

path <- "data/aptos.rds"
old <- read_rds(path)
aptos <- json |>
  chuck("search") |>
  chuck("result") |>
  chuck("listings") |>
  map(wrangle) |>
  bind_rows() |>
  retype() |>
  mutate(
    preco = aluguel + condominio + iptu / 12,
    link = file.path("vivareal.com.br", link)
  ) |>
  filter(
    banheiros >= 2,
    preco <= 2300,
    quartos >= 2,
    suites >= 1,
    vagas >= 1
  ) |>
  anti_join(old, by = "id") |>
  print(n = 10)
old |>
  bind_rows(apto) |>
  write_rds(path)

# Telegram ---------------------------------------------------------------------

bot <- TGBot$new(token = bot_token("aptobot"))
bot$set_default_chat_id("-763183926")
aptos |>
  pmap(list) |>
  map(function(x) paste(names(x), "=", x)) |>
  map_chr(paste, collapse = "\n") |>
  walk(bot$sendMessage)
