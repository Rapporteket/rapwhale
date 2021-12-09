# Kompileringsfunksjonar --------------------------------------------------

# Kompiler ei .Rnw-fil til .tex-fil
kompiler_rnw = function(adresse) {
  # Køyr først .Rnw-fila gjennom R for å få ut ei .tex-fil
  cat(paste0(basename(adresse), " (knitr): "))
  knit_res = try(
    suppressPackageStartupMessages(
      knit(
        adresse,
        encoding = "utf-8", quiet = TRUE, envir = globalenv()
      )
    ),
    silent = TRUE
  )
  knit_ok = !inherits(knit_res, "try-error")
  if (knit_ok) {
    cat("OK\n")
  } else {
    cat("FEIL!\n")
    suppressWarnings(file.remove(str_replace(adresse, ".Rnw", ".tex"))) # Fjern .tex-fila, for å unngå forsøk på kompilering
  }
}

# Kompiler ei .tex-fil til PDF-fil (med maks «maksiter» LuaLaTeX-køyringar)
kompiler_tex = function(adresse, maksiter = 5) {
  # Gjenta kompilering til alle kryssreferansar og
  # slikt er i orden, men maks «maksiter» gongar.
  iter = 0
  repeat {
    iter = iter + 1
    filnamn = basename(adresse)
    cat(paste0(filnamn, " (LuaLaTex): ", iter, ": ")) # Vis statusmelding
    old_opts = options(warn = 1) # Vis åtvaringar når dei skjer (for eksempel viss PDF-fila er låst for skriving)
    logg = suppressWarnings(system2(
      "lualatex",
      args = paste("--interaction=nonstopmode --halt-on-error --file-line-error", filnamn),
      stdout = TRUE
    ))

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

    options(old_opts)
    feil = str_detect(logg1l, "no output PDF file produced")
    ferdig = !str_detect(logg1l, "run LaTeX again|Rerun to|Rerun LaTeX")

    # Skil loggen inn i separate «loggmeldingar», som me definerer
    # til å vera tekst etterfølgt av ei *tom* linje
    loggmeldingar = str_c(logg, collapse = "\n") %>% # Gjer loggteksten om til éin stor streng
      str_split("\n\n+") %>%
      pluck(1) # Del opp i loggmeldingar
    logg_akt = str_subset(loggmeldingar, "([Ww]arning|[Ee]rror):") # Hent ut aktuelle loggmeldingar

    # Vis eventuelle feilmeldingar/åtvaringar i loggen
    vis_loggfeil = function() {
      if (vis_feilmeldingar && (length(logg_akt) > 0)) {
        cat("Åtvaringar/feil: ",
          str_c("  ", str_replace_all(logg_akt, "\n", "\n  ")),
          sep = "\n"
        )
      } # Innrykk på alle linjer
    }

    if (feil) {
      cat("FEIL (klarte ikkje laga PDF)\n")
      vis_loggfeil()
      break
    } else if (ferdig) {
      cat("OK\n")
      vis_loggfeil() # Det kan vera åtvaringar sjølv om kompileringa fungerte ...
      break
    } else if (iter >= maksiter) {
      cat("GJEV OPP (for mange rekompileringar)\n")
      vis_loggfeil()
      break
    } else {
      cat("Treng rekompilering ...\n")
      # vis_loggfeil() # Er typisk så mange åtvaringar viss me treng rekompilering at det er betre å ikkje visa dei
    }
  }
}
