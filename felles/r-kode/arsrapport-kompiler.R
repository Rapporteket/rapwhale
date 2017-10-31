# Kompiler alle årsrapportane (om og om igjen, ved kvar endring)

# No kan du trykka «Ctrl + Shift + S», so vert
# alle årsrapportfilene automatisk kompilert til PDF
# i bakgrunnen (når du gjer endringar i .Rnw-filene).

# Nødvendige pakkar
library(knitr)
library(tibble)
library(readr)
library(purrr)
library(stringr)



# Årsrapportoversikt ------------------------------------------------------

# Filer me skal kompilera
adresser = list.files("h:/kvalreg/",
  pattern = "arsrapport.*\\.Rnw$",
  recursive = TRUE, full.names = TRUE
)

# Ikkje kompiler LKG-registeret, sidan me ikkje vedlikeheld det lenger
adresser = setdiff(adresser, "h:/kvalreg//lkg-registeret/arsrapport-lkg.Rnw")

# Oversikt over alle årsrapportfiler med tilhøyrande endringsdato
filinfo = tibble(
  adresse_rnw = adresser,
  adresse_tex = str_replace(adresser, ".Rnw", ".tex"),
  adresse_pdf = str_replace(adresser, ".Rnw", ".pdf")
)

# Opna alle PDF-filene (som eksisterer) i ein PDF-lesar som ikkje blokkerer for skriving
filinfo$adresse_pdf %>%
  keep(file.exists) %>%
  walk(~ shell(
    paste("C:\\Programfiler\\RStudio\\bin\\sumatra\\SumatraPDF.exe", .),
    wait = FALSE
  ))



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
      args = paste("--interaction=errorstopmode --file-line-error", filnamn),
      stdout = TRUE
    ))

    # Loggen me får ut på kommandolinja har ikkje eit format me lett kan bruka.
    # Les derfor heller inn den *lagra* loggfila (som har fint format på grunn
    # av bruken av «--file-line-error»-argumentet).
    loggfil = str_replace(adresse, ".tex$", ".log")
    logg = read_lines(loggfil)

    options(old_opts)
    feil = any(str_detect(logg, "no output PDF file produced"))
    ferdig = !any(str_detect(logg, "run LaTeX again|Rerun to|Rerun LaTeX"))

    # Loggteksten har linjelengd på 80 teikn, med automatiske linjeskift
    # Me fjernar desse og definerer ei loggmelding til å vera tekst etterfølgt av ei tom linje
    loggmeldingar = str_c(logg, collapse = "\n") %>% # Gjer loggteksten om til éin stor streng
      str_split("\n\n+") %>%
      pluck(1) # Del opp i loggmeldingar
    logg_akt = str_subset(loggmeldingar, "([Ww]arning|[Ee]rror):") # Hent ut aktuelle loggmeldingar

    # Vis eventuelle feilmeldingar/åtvaringar i loggen
    vis_loggfeil = function() {
      if (length(logg_akt) > 0) {
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



# Kompiler filer som treng det -------------------------------------------

# Gå gjennom alle filene og kompiler
# viss dei er endra frå førre gjennomgang
repeat {
  # Merk me gjer berre *éin* type operasjon (eks. Rnw --> .tex eller .tex --> PDF)
  # for kvar fil ved kvar gjennomgang av lista. Då unngår me å kompilera
  # PDF-fila for ei .Rnw-fil som alt er oppdatert når me er ferdig med
  # første kompilering til .tex. Me unngår òg kompileringsfunksjonane
  # «går seg fast» i arbeid med ein årsrapport (der for eksempel kompilering
  # av .Rnw-fila aldri er vellykka).

  # Sjekk om ei fil x er nyare enn ei anna fil y.
  # Viss y-fila ikkje eksisterer, er x-fila per def. nyare.
  er_nyare_enn = function(x, y) {
    x_tid = file.mtime(x)
    y_tid = file.mtime(y)
    (x_tid >= y_tid) | is.na(y_tid)
  }
  for (i in 1:nrow(filinfo)) {
    # Kortnamn til adressene til dei ulike filene
    rnw = filinfo$adresse_rnw[i]
    tex = filinfo$adresse_tex[i]
    pdf = filinfo$adresse_pdf[i]

    # Må vera i årsrapportmappa for at rekompileringa skal fungera
    setwd(dirname(rnw))

    # Kompiler om nødvendig
    if (rnw %>% er_nyare_enn(tex)) {
      kompiler_rnw(rnw)
    } else if (tex %>% er_nyare_enn(pdf)) {
      kompiler_tex(tex)
    }
  }
  Sys.sleep(.5) # Vent litt mellom kvar gjennomgang, for ikkje å overbelasta filsystemet når det *ikkje* er nokon endringar
}
