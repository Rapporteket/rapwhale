#' @importFrom stringr str_subset str_extract
NULL
#' Bygg og installer R-pakker
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Dokumenterer, bygger og installerer R-pakker.
#'
#' @param pakkemapper Tekstvektor med mappeadresser til R-pakker.
#' @param installer Logisk variabel. Skal pakkene installeres (som standard ja).
#' @param stille Logisk variabel. Skal det kun gis ut oppsummerte advarsler og
#'     feilmeldinger (som standard ja)?
#'
#' @details
#' For hver mappeadresse i `pakkemapper` bygges først pakkedokumentasjonen
#' og så pakken (som en `.tar.gz`-pakkefil).
#' Hvis `installer = TRUE`, blir også pakken installert.
#' Hvis `stille = TRUE`,
#' blir det kun gitt ut oppsummerte advarsler og feilmeldinger.
#'
#' @return `NULL`
#' @export
#'
#' @examples
#' \dontrun{
#' # Finn først alle DESCRIPTION-filer, som indikerer
#' # at tilhørende foreldermappe er en R-pakkemappe
#' desc_filer = list.files(
#'   "m:\\r-pakker\\",
#'   pattern = "^DESCRIPTION$",
#'   full.names = TRUE, recursive = TRUE
#' )
#' 
#' # Hent ut mappeadressene, og bygg og installer pakkene
#' pakkemapper = dirname(desc_filer)
#' bygg_og_installer_pakker(pakkemapper)
#' }
bygg_og_installer_pakker = function(pakkemapper,
                                    installer = TRUE,
                                    stille = TRUE) {
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
