#' Lag oversikt over utviklingsnivå
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Sjekkar alle .Rd-filer i ei mappe, og lagar ein oversikt over utviklingsnivåa.
#'
#' @param mappe Adresse til mappe med hjelpefiler. Standard er "H:\\kvalreg\\fellesting\\r-pakkar\\rapwhale\\man".
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
#' utviklingsnivaa("H:\\kvalreg\\fellesting\\r-pakkar\\rapwhale\\man")
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

  # Hent utviklingsnivå for en funksjon
  hent_nivaa = function(funksjon_rd) {
    desc_rd = purrr::keep(
      funksjon_rd,
      ~ attr(., "Rd_tag") == "\\description"
    )
    nivaa = unlist(desc_rd) %>%
      stringr::str_subset("^lifecycle-[[:alpha:]]+\\.svg$") %>%
      stringr::str_remove_all("lifecycle-|\\.svg")
    if (length(nivaa) == 0) {
      NA_character_
    } else {
      nivaa[1]
    }
  }
  nivaa = map_chr(funksjonar_parsed, hent_nivaa)

  # Lag vektor som angir om funksjonane er interne eller ei
  er_intern = function(funksjon_rd) {
    funksjon_rd = purrr::keep(
      funksjon_rd,
      ~ attr(., "Rd_tag") == "\\keyword"
    )
    any(purrr::map_chr(funksjon_rd, 1) == "internal")
  }
  intern = map_lgl(funksjonar_parsed, er_intern)

  tibble::tibble(funksjon = funksjonar_namn, utviklingsnivaa = nivaa, intern = intern) %>%
    dplyr::arrange(utviklingsnivaa)
}
