#' Lag oversikt over utviklingsnivå
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Sjekkar alle .Rd-filer i ei mappe, og lagar ein oversikt over utviklingsnivåa.
#'
#' @param mappe Adresse til mappe med hjelpefiler. Standard er "H:/kvalreg/felles/pakker/rapwhale/man".
#'
#' @details
#' Funksjonen tek inn adressa til ei mappe, og ser etter utviklingsnivå
#' (`lifecycle`-badge) i alle .Rd-filene i mappa. Den gjev så ut ein oversikt
#' over alle funksjonsnamn og utviklingsnivå i en tibble.
#'
#'
#' @return Tibble med kolonnane `funksjon` og `utviklingsnivaa` som gjev ein
#'         oversikt over utviklingsnivåa til alle funksjonane som har
#'         hjelpefil i `mappe`.
#' @export
#'
#' @examples
#' utviklingsnivaa()
#'
#' utviklingsnivaa("H:/kvalreg/felles/pakker/rapwhale/man")
utviklingsnivaa = function(mappe = "H:\\kvalreg\\fellesting\\r-pakkar\\rapwhale\\man") {
  # Lag vektor med filnamn
  funksjonar = list.files(mappe, pattern = "\\.Rd$")

  # Lag vektor med funksjonsnamn
  funksjonar_namn = funksjonar %>%
    stringr::str_replace("\\.Rd", "()")

  # Les inn linjene i hjelpefilene
  parse_Rd_mapper = purrr::as_mapper(~ tools::parse_Rd(., permissive = TRUE))
  funksjonar_parsed = paste0(mappe, "/", funksjonar) %>%
    purrr::map(parse_Rd_mapper)

  # Lag vektor med utviklingsnivåa til funksjonane
  nivaa = funksjonar_parsed %>%
    as.character() %>%
    purrr::map_chr(~ .x %>%
      stringr::str_extract("lifecycle-[[:alpha:]]+\\.svg") %>%
      stringr::str_remove_all("lifecycle-|\\.svg"))

  eksterne_funksjonar = paste0(ls("package:rapwhale"), "()")

  tibble::tibble(funksjon = funksjonar_namn, utviklingsnivaa = nivaa) %>%
    dplyr::arrange(utviklingsnivaa) %>%
    dplyr::mutate(ekstern = funksjon %in% !!eksterne_funksjonar)
}
