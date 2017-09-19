# Kompiler alle årsrapportane (om og om igjen, ved kvar endring)

# No kan du trykka «Ctrl + Shift + S», so vert
# alle årsrapportfilene automatisk kompilert til PDF
# i bakgrunnen (når du gjer endringar i .Rnw-filene).

# Nødvendige pakkar
library(knitr)
library(tibble)
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
filinfo = tibble(adresse = adresser, endra = file.info(adresser)$mtime)

# Opna alle filene i ein PDF-lesar som ikkje blokkerer for skriving
for (fil_rnw in filinfo$adresse) {
  fil_pdf = str_replace(fil_rnw, ".Rnw$", ".pdf")
  if (file.exists(fil_pdf)) {
    shell(paste("C:\\Programfiler\\RStudio\\bin\\sumatra\\SumatraPDF.exe", fil_pdf), wait = FALSE)
  }
}



# Kompileringsfunksjon ----------------------------------------------------

# Kompiler ei .Rnw-fil (med maks «maksiter» LuaLaTeX-køyringar)
kompiler = function(rnw_fil, maksiter = 5) {
  # Køyr først .Rnw-fila gjennom R for å få ut ei .tex-fil
  cat(paste0(basename(rnw_fil), " (knitr)\n"))
  knit_res = try(knit(rnw_fil, encoding = "utf-8", quiet = TRUE, envir = globalenv()), silent = TRUE)
  knit_ok = !inherits(knit_res, "try-error")

  # Kompiler .tex-fila, men berre viss knitr-operasjonen gjekk greitt
  if (knit_ok) {
    tex_fil = str_replace(rnw_fil, ".Rnw$", ".tex")
    iter = 0
    # Gjenta kompilering til alle kryssreferansar og
    # slikt er i orden, men maks «maksiter» gongar.
    repeat {
      iter = iter + 1
      cat(paste0(basename(tex_fil), " (TeX): ", iter, "\n")) # Vis statusmelding
      logg = system2(
        "lualatex",
        args = paste("--interaction=errorstopmode", tex_fil),
        stdout = TRUE
      )
      ferdig = !any(str_detect(logg, "run LaTeX again|Rerun to get cross-references right"))
      if (ferdig | (iter >= maksiter)) {
        break
      }
    }
  }
}




# Sjølve kompileringsfunksjonen -------------------------------------------

# Gå gjennom alle filene og kompiler
# viss dei er endra frå førre gjennomgang
repeat {
  for (i in 1:nrow(filinfo)) {
    rnw_fil = filinfo$adresse[i]
    setwd(dirname(rnw_fil)) # Slik at utfiler vert plasserte rett plass
    endra = file.info(rnw_fil)$mtime
    if (endra != filinfo$endra[i]) {
      filinfo$endra[i] = endra
      kompiler(rnw_fil)
    }
    Sys.sleep(1) # Vent litt mellom kvar gjennomgang, for ikkje å overbelasta filsystemet når det *ikkje* er noko endringar
  }
}
