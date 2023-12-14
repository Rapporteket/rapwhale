#' Lag oversikt over utviklingsnivå
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Sjekkar alle .Rd-filer i ei mappe, og lagar ein oversikt over utviklingsnivåa.
#'
#' @param mappe
#' Adresse til mappe med hjelpefiler. Standard er "man".
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
#' \dontrun{
#' utviklingsnivaa()
#'
#' utviklingsnivaa("H:\\kvalreg\\fellesting\\r-pakkar\\rapwhale\\man")
#' }
utviklingsnivaa = function(mappe = "man") {
  # Lag vektor med filnamn
  funksjonar = list.files(mappe, pattern = "\\.Rd$")

  # Lag vektor med funksjonsnamn
  funksjonar_namn = str_replace(funksjonar, "\\.Rd", "()")

  # Les inn linjene i hjelpefilene
  parse_rd_mapper = \(x) tools::parse_Rd(x, permissive = TRUE)
  funksjonsadresser = paste0(mappe, "/", funksjonar)
  funksjonar_parsed = map(funksjonsadresser, parse_rd_mapper)

  # Hent utviklingsnivå for en funksjon
  hent_nivaa = function(funksjon_rd) {
    desc_rd = purrr::keep(
      funksjon_rd,
      \(x) attr(x, "Rd_tag") == "\\description"
    )
    nivaa = unlist(desc_rd) |>
      str_subset("^lifecycle-[[:alpha:]]+\\.svg$") |>
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
      \(x) attr(x, "Rd_tag") == "\\keyword"
    )
    any(map_chr(funksjon_rd, 1) == "internal")
  }
  intern = map_lgl(funksjonar_parsed, er_intern)

  tibble(
    funksjon = funksjonar_namn,
    utviklingsnivaa = nivaa,
    intern = intern
  ) |>
    arrange(utviklingsnivaa)
}
