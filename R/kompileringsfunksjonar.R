#' Kompiler .Rnw til .tex
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Funksjonen kompilerer ei .Rnw-fil til .tex-fil
#'
#' @param adresse Tekststreng med adresse til .Rnw-fila som skal kompilerast.
#'
#' @details
#' Dersom kompileringa ikkje vert fullført utan feil,
#' slettar funksjonen .tex-fila som svarar til `adresse`.
#'
#' @return
#' Adressa til .tex-fila, viss den vart kompilert utan feil, usynleg.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' kompiler_rnw("H:/adresse/til/fil.Rnw")
#' }
kompiler_rnw = function(adresse) {
  cat(paste0(basename(adresse), " (knitr): "))
  tex_adresse = str_replace(adresse, ".Rnw", ".tex")

  # Byt arbeidsmappe slik at all output kjem på rett plass
  old_wd = setwd(dirname(adresse))

  knit_res = try(
    callr::r(
      function(...) {
        knitr::knit(..., quiet = TRUE, envir = globalenv())
      },
      args = list(input = adresse)
    ),
    silent = TRUE
  )

  # Byt tilbake til gamal arbeidsmappe
  setwd(old_wd)

  knit_ok = !inherits(knit_res, "try-error")
  if (knit_ok) {
    cat("OK\n")
    invisible(tex_adresse)
  } else {
    cat("FEIL!\n")
    # Fjern .tex-fila, for å unngå forsøk på kompilering
    if (file.exists(tex_adresse)) {
      file.remove(tex_adresse)
    }
  }
}

#' Kompiler .tex til .pdf
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Funksjonen kompilerer ei .tex-fil til .pdf-fil,
#' med maks `maksiter` LuaLaTeX-køyringar.
#'
#' @param adresse
#' Tekststreng med adresse til .tex-fila som skal kompilerast.
#' @param maksiter
#' Heiltal for maks antal LuaLaTeX-køyringar. Standard = 5.
#' @param vis_feilmeldingar
#' Logisk variabel som seier om feilmeldingar skal visast. Standard = TRUE.
#'
#' @return
#' Adressa til .pdf-fila, usynleg.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' kompiler_tex("H:/adresse/til/fil.tex")
#' }
kompiler_tex = function(adresse, maksiter = 5, vis_feilmeldingar = TRUE) {
  # Gjenta kompilering til alle kryssreferansar og
  # slikt er i orden, men maks «maksiter» gongar.
  iter = 0
  repeat {
    iter = iter + 1
    filnamn = basename(adresse)
    cat(paste0(filnamn, " (LuaLaTex): ", iter, ": ")) # Vis statusmelding

    # Vis åtvaringar når dei skjer
    # (for eksempel viss PDF-fila er låst for skriving)
    old_opts = options(warn = 1)

    # Byt arbeidsmappe til rapportmappa
    old_wd = setwd(dirname(adresse))

    processx::run(
      "lualatex",
      args = c(
        "--interaction=nonstopmode", "--halt-on-error", "--file-line-error",
        adresse
      ),
      stdout = NULL
    )

    # Loggen me får ut på kommandolinja har ikkje eit format me lett kan bruka.
    # Les derfor heller inn den *lagra* loggfila (som har fint format på grunn
    # av bruken av «--file-line-error»-argumentet).
    loggfil = str_replace(adresse, ".tex$", ".log")
    logg = read_lines(loggfil)

    # Loggteksten har linjelengd på 80 teikn, som kan bli delt midt i ei
    # feilmelding (til og med midt inni eit ord!). Det gjer det vanskelegare
    # å automatisk finna spesifikke feilmeldingar. Lagar derfor ein versjon
    # der loggen er éin long tekst utan linjeskift.
    logg1l = str_c(logg, collapse = "")

    # Byt tilbake til gamle instillingar
    options(old_opts)
    setwd(old_wd)

    feil = str_detect(logg1l, "no output PDF file produced")
    ferdig = !str_detect(logg1l, "run LaTeX again|Rerun to|Rerun LaTeX")

    # Skil loggen inn i separate «loggmeldingar», som me definerer
    # til å vera tekst etterfølgt av ei *tom* linje
    loggmeldingar = str_c(logg, collapse = "\n") |>
      str_split("\n\n+") |>
      pluck(1)
    # Hent ut aktuelle loggmeldingar
    logg_akt = str_subset(loggmeldingar, "([Ww]arning|[Ee]rror):")

    # Vis eventuelle feilmeldingar/åtvaringar i loggen
    vis_loggfeil = function() {
      if (vis_feilmeldingar && (length(logg_akt) > 0)) {
        cat(
          "Åtvaringar/feil: ",
          # Innrykk på alle linjer
          str_c("  ", str_replace_all(logg_akt, "\n", "\n  ")),
          sep = "\n"
        )
      }
    }

    if (feil) {
      cat("FEIL (klarte ikkje laga PDF)\n")
      vis_loggfeil()
      break
    } else if (ferdig) {
      cat("OK\n")
      vis_loggfeil() # Det kan vera åtvaringar sjølv om kompileringa fungerte
      break
    } else if (iter >= maksiter) {
      cat("GJEV OPP (for mange rekompileringar)\n")
      vis_loggfeil()
      break
    } else {
      cat("Treng rekompilering ...\n")
      # vis_loggfeil()
      # Er typisk så mange åtvaringar viss me treng rekompilering
      # at det er betre å ikkje visa dei
    }
  }
  pdf_adressse = str_replace(adresse, ".tex$", ".pdf")
  invisible(pdf_adressse)
}
