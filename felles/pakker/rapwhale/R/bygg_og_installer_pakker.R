#' Bygg og installer pakker
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Dokumenterer, bygger og installerer pakker (tar.gz-filer).
#'
#' @param pakkemappe Mappe for pakker.
#' @param installer Skal pakkene installeres? Standard er TRUE.
#' @param stille Skal det kun gis ut oppsummerte advarsler og
#'     feilmeldinger? Standard er TRUE.
#'
#' @return `NULL`
#' @export
#'
#' @examples
#' \dontrun{
#' bygg_og_installer_pakker()
#' }
bygg_og_installer_pakker = function(pakkemappe = "H:\\kvalreg\\felles\\pakker\\",
                                    installer = TRUE,
                                    stille = TRUE) {
  pakker = list.dirs(pakkemappe, full.names = FALSE, recursive = FALSE)
  for (pakke in pakker) {
    adresse_pakke = paste0(pakkemappe, pakke)
    cat("Dokumenter og bygg tar.gz-fil:", pakke, "\n")
    devtools::document(adresse_pakke,
      roclets = c("rd", "collate", "namespace", "vignette"),
      quiet = stille
    )
    adresse_pakkefil = devtools::build(adresse_pakke, quiet = stille)
    pkgload::unload(pakke, quiet = stille)
    devtools::install_local(adresse_pakkefil, dependencies = FALSE, quiet = stille)
  }
}
