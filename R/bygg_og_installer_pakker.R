#' @importFrom magrittr %>%
#' @importFrom stringr str_subset str_extract
NULL
#' Bygg og installer pakker
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Dokumenterer, bygger og installerer pakker (tar.gz-filer).
#'
#' @param pakkemapper Tekstvektor med mappeadresser til pakker. Standard er NULL.
#' @param installer Logisk variabel. Skal pakkene installeres? Standard er TRUE.
#' @param stille Logisk variabel. Skal det kun gis ut oppsummerte advarsler og
#'     feilmeldinger? Standard er TRUE.
#'
#' @details
#' Funksjonen dokumenterer, bygger og eventuelt installerer pakkene som finnes i
#' `pakkemapper`. Hvis `pakkemapper = NULL` (standard) velges alle pakker som
#' ligger i mapper som heter `r-pakke` eller `r-pakkar` under `H:/kvalreg/`.
#' Hvis `installer = TRUE` blir pakkene installert. Hvis `stille = TRUE` blir
#' det kun gitt ut oppsummerte advarsler og feilmeldinger.
#'
#' @return `NULL`
#' @export
#'
#' @examples
#' \dontrun{
#' bygg_og_installer_pakker()
#' }
bygg_og_installer_pakker = function(pakkemapper = NULL,
                                    installer = TRUE,
                                    stille = TRUE) {
  if (is.null(pakkemapper)) {
    pakkemapper = list.files("H:/kvalreg",
      pattern = "r-pakk(e|ar)",
      recursive = TRUE, full.names = TRUE,
      include.dirs = TRUE
    ) %>%
      list.files(pattern = "^[[:alnum:]]*$", full.names = TRUE)
  }

  for (pakkemappe in pakkemapper) {
    pakke = str_extract(pakkemappe, "[[:alnum:]]*$")
    cat("Dokumenter og bygg tar.gz-fil:", pakke, "\n")
    devtools::document(pakkemappe,
      roclets = c("rd", "collate", "namespace"),
      quiet = stille
    )
    adresse_pakkefil = devtools::build(pakkemappe, quiet = stille)
    pkgload::unload(pakke, quiet = stille)
    if (installer) {
      devtools::install_local(adresse_pakkefil,
        dependencies = FALSE,
        quiet = stille
      )
    }
  }
}
