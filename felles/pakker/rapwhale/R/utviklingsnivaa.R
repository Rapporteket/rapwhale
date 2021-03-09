utviklingsnivaa = function(mappe = "H:/kvalreg/felles/pakker/rapwhale/man") {
  # Lag vektor med adresser til hjelpefilene
  funksjonar = list.files(mappe, pattern = "\\.Rd$", full.names = TRUE)

  # Lag vektor med funksjonsnamn
  funksjonar_namn = list.files(mappe, pattern = "\\.Rd$") %>%
    stringr::str_replace("\\.Rd", "()")

  # Les inn linjene i hjelpefilene
  funksjonar_parsed = paste0(mappe, "/", funksjonar) %>%
    map(parse_Rd)

  # Lag vektor med utviklingsnivåa til funksjonane
  nivaa = funksjonar_parsed %>%
    as.character() %>%
    map_chr(~ .x %>%
      stringr::str_extract("lifecycle-[[:alpha:]]+\\.svg") %>%
      stringr::str_remove_all("lifecycle-|\\.svg"))

  tibble(funksjon = funksjonar_namn, utviklingsnivaa = nivaa) %>%
    arrange(utviklingsnivaa)
}
