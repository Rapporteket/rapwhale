# Dette skriptet inneholder en rekke funksjoner som er potensielt nyttige
# i registersammenheng og i andre sammenheng.
# Disse funksjonene tilhører ingen annen kategori,
# og er ikke flere enn én i samme kategori, derav navnet ymse.

#' @importFrom magrittr %>%
#' @importFrom stringr str_replace_all
#' @importFrom tibble tibble
#' @import dplyr

### Normaliser variabelnamn til å ha _ som skiljeteikn og berre små bokstavar

# Eksempel på bruk:
#   c("hopp og.SprettTest", "SykdomsAktivitet.PasientGlobalSykdomsaktivitet") %>% normaliser_varnamn
#   som gjev
#   c("hopp_og_sprett_test", "sykdoms_aktivitet_pasient_global_sykdomsaktivitet")
#' @export
normaliser_varnamn = function(x) {
  teikn = x %>%
    stringr::str_split("") # Splitt i enkeltteikn

  # Putt inn _ før alle store bokstavar (utanom første teikn i strengen)
  teikn = teikn %>%
    purrr::map(~ str_replace_all(., "([[:upper:]])", "_\\1"))

  teikn %>%
    map_chr(~ paste0(., collapse = "")) %>% # Slå saman til lange strengar igjen
    str_replace_all("[\\._ ]+", "_") %>% # Erstatt etterfølgjande punktum, mellomrom og/eller _ med éin _,
    str_replace_all("^_", "") %>% # Fjern ev. _ på starten av strengane
    tolower() # Gjer om til små bokstavar
}

### Variant av table()-funksjonen som tar med NA-verdiar om dei finst

# Lag tabell som også viser NA-verdiar om dei finst
#' @export
tab = function(...) {
  table(..., useNA = "ifany")
}


### Avrundig med valfri presisjon og funksjon

# tatt frå plyr-pakken (nyttige verdiar
# av f er round, floor og ceiling)
#' @export
round_any = function(x, accuracy, f = round) {
  f(x / accuracy) * accuracy
}


#### Konfidensintervall for binomisk fordeling

# Brukar Wilson-intervallet, som anbefalt i
# «Binomial confidence intervals and contingency tests:
# mathematical fundamentals and the evaluation of alternative methods», av Sean Wallis, University College London
#' @export
regn_ki_bin = function(x, n) {
  ki = binom::binom.wilson(x, n)
  tibble(
    low = pmax(0, ki$lower), # Fiks for at grensene av og til kan gå *bitte litt* utanfor [0,1]
    high = pmin(1, ki$upper)
  )
}

### Konfidenstinervall basert på gjennomsnittet til en  kontinuerlig variabel
# med mulighet for bootstrap lagt inn i funksjonen
#' @export
regn_ki_univar = function(x, bootstrap = FALSE, antall, ...) {
  # Hvis det er for få eller for lite varierende
  # observasjoner til å regne ut konfidensintervall,
  # returner NA for konfidensintervallene
  if ((length(x) < 2) | (sd(x, na.rm = TRUE) == 0)) {
    tibble(
      low = NA,
      mean = mean(x, na.rm = TRUE),
      high = NA
    )
  } else {

    # Hvis man ønsker boostrap kjøres denne koden,
    if (bootstrap) {
      snitt_ki = function(x) {
        n = sum(!is.na(x))
        snitt = mean(x)
        if ((length(x) > 1) & (sd(x) > 0)) {
          b = simpleboot::one.boot(x, mean, R = 9999)
          ki = simpleboot::boot.ci(b, type = "perc")$percent[4:5]
        } else {
          ki = c(NA, NA)
        }
        tibble(
          mean = snitt,
          low = ki[1],
          high = ki[2],
          n = n
        )
      }
    } else {
      mod = t.test(x)
      tibble(
        low = mod$conf.int[1],
        mean = mod$estimate,
        high = mod$conf.int[2]
      )
    }
  }
}

# For å lage pene LaTeX-tabeller i et standardisert format for alle årsrapporter,
# med mulighet for å gjøre den stor nok til hele siden (wide = TRUE).
# optional arguments inkluderer colheads=c() og caption = paste0("").

lag_tab_latex = function(dataframe, label, caption, wide = FALSE, ...) {

  # Viss dataramma ikkje har nokon radar, bryt latex()-funksjonen
  # heilt saman dersom numeric.dollar er FALSE (og det er FALSE
  # me *vil* ha, for å få rett formatering av tal).
  #
  # fixme: Feilen er meldt inn til forfattaren av Hmisc-pakken
  #        i januar 2018, og er lovd retta. Fjern derfor følgjande
  #        if()-test når dette er retta og Hmisc-pakken er oppdatert.
  #
  #        Kan bruka følgjande kodesnutt for å sjekka om feilen
  #        er retta:
  #          latex(head(iris, 0), file="", numeric.dollar=FALSE)
  #        (skal gje tabell, ikkje feilmelding)
  if (nrow(dataframe) == 0) {
    tabell = paste0(
      "\\begin{table}[htbp]\n",
      "\\caption{", caption, "\\label{", label, "}}\n",
      "{\\color{errorcolor}(Tabellen har 0 rader. Her må noko vera gale!)}\n",
      "\\end{table}"
    )
  } else {
    tabell = capture.output(hmisc::latex(dataframe,
      file = "", center = "centering",
      label = label, caption = caption, rowname = NULL,
      where = "htbp", booktabs = TRUE, numeric.dollar = FALSE, ...
    ))
    if (wide) {
      tabell %<>% str_replace("^\\\\(begin|end)\\{table\\}", "\\\\\\1\\{widetable\\}") # Superrobust ... ;)
    }
    tabell = paste0(tabell, sep = "\n")
  }

  # Returner tabellen (eller feilmelding)
  tabell
}
