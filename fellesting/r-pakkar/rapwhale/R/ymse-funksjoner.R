# Dette skriptet inneholder en rekke funksjoner som er potensielt nyttige
# i registersammenheng og i andre sammenheng.
# Disse funksjonene tilhører ingen annen kategori,
# og er ikke flere enn én i samme kategori, derav navnet ymse.

#' @importFrom magrittr %>%
#' @importFrom stringr str_replace_all str_split
#' @importFrom tibble tibble
#' @importFrom purrr map map_chr
NULL

### Normaliser variabelnamn til å ha _ som skiljeteikn og berre små bokstavar

# Eksempel på bruk:
#   c("hopp og.SprettTest", "SykdomsAktivitet.PasientGlobalSykdomsaktivitet") %>% normaliser_varnamn
#   som gjev
#   c("hopp_og_sprett_test", "sykdoms_aktivitet_pasient_global_sykdomsaktivitet")

#' Normaliser variabelnamn
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Funksjon for å konvertere variabelnavn til standardformat.
#'
#' @details
#' Standardformatering er "snake_case", hvor "_" blir brukt som skilletegn, og kun små bokstaver benyttes.
#'
#' @param x Tekststreng med variabelnavn som skal konverteres.
#' @export
normaliser_varnamn = function(x) {
  teikn = x %>%
    str_split("") # Splitt i enkeltteikn

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

#' Tabell inkludert NA-verdier
#'
#' #' @description
#' `r lifecycle::badge("experimental")`
#'
#' Variant av table()-funksjonen som tar med NA-verdiar om dei finst.
#' @param ... Tilsvarende argumenter som brukes i table()-funksjonen.
#' @export
tab = function(...) {
  table(..., useNA = "ifany")
}


#' Konfidensintervall for binomisk fordeling
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Brukar Wilson-intervallet, som anbefalt i «Binomial confidence intervals
#' and contingency tests: mathematical fundamentals and the evaluation of
#' alternative methods», av Sean Wallis, University College London.
#'
#' Returnerer en tibble med nedre og øvre grense for et 95 \%
#' wilson-konfidensintervall.
#' @param x Antall suksesser i forsøket.
#' @param n Antall uavhengige forsøk.
#' @export
regn_ki_bin = function(x, n, conf.level) {
  ki = binom::binom.wilson(x, n, conf.level)
  tibble(
    lower = pmax(0, ki$lower), # Fiks for at grensene av og til kan gå *bitte litt* utanfor [0,1]
    upper = pmin(1, ki$upper)
  )
}



#' Konfidenstinervall basert på gjennomsnittet til en  kontinuerlig variabel

#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Konfidenstinervall basert på gjennomsnittet til en  kontinuerlig variabel
#' med mulighet for bootstrap lagt inn i funksjonen
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


#' Lag LaTeX-tabell
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' For å lage pene LaTeX-tabeller i et standardisert format for alle årsrapporter,
#' med mulighet for å gjøre den stor nok til hele siden (wide = TRUE).
#' optional arguments inkluderer colheads=c() og caption = paste0("").
#'
#' @param dataframe dataramme som inneholder data som skal vises i tabell
#' @param label label for tabellen som brukes for å lage referanser i teksten
#' @param caption forklaringstekst for tabellen
#' @param wide valgmulighet for om tabellen skal være breiere enn standard tekstbredde. Må være TRUE eller FALSE, default er FALSE
#' @param ... Ytterligere argumenter som kan gis til Hmisc::latex funksjon
#' @export
#' @examples
#' cars_top_mpg = mtcars %>%
#'   arrange(desc(mpg)) %>%
#'   head()
#' cars_top_mpg_tab = lag_tab_latex(cars_top_mpg,
#'   label = "mpg_table",
#'   caption = "Cars with best mileage"
#' )
#' cat(cars_top_mpg_tab)
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
    tabell = capture.output(Hmisc::latex(dataframe,
      file = "", center = "centering",
      label = label, caption = caption, rowname = NULL,
      where = "htbp", booktabs = TRUE, numeric.dollar = FALSE, ...
    ))
    if (wide) {
      tabell %<>% stringr::str_replace("^\\\\(begin|end)\\{table\\}", "\\\\\\1\\{widetable\\}") # Superrobust ... ;)
    }
    tabell = paste0(tabell, sep = "\n")
  }

  # Returner tabellen (eller feilmelding)
  tabell
}

#' Lag statistisk samandragsfunksjon med trunkerte grenser for konfidensintervall
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Lagar ein statistisk samandragsfunksjon tilsvarande
#' [ggplot2::mean_cl_normal()] der grensene for konfidensintervallet
#' vert trunkerte.
#'
#' @param ymin Nedre grense for konfidensintervall. Standard verdi `NA`.
#' @param ymax Øvre grense for konfidensintervall. Standard verdi `NA`.
#'
#' @details
#' Tek inn ei nedre grense `ymin` og ei øvre grense `ymax` og gjev ut ein
#' funksjon tilsvarande [ggplot2::mean_cl_normal()] der grensene for
#' konfidensintervallet er trunkerte til desse verdiane.
#'
#' Dersom `ymin` og/eller `ymax` er `NA`, vert ikkje tilhøyrande grenser
#' trunkerte.
#'
#' Denne funksjonen kan nyttast til å laga trunkerte konfidensintervall
#' viss inndata har kjende nedre og/eller øvre verdigrenser.
#'
#' Funksjonen er også laga for å brukast i [ggplot2::stat_summary()].
#'
#' @return
#' Ein funksjon tilsvarande [ggplot2::mean_cl_normal()], men der grensene for
#' konfidensintervallet vert trunkerte
#' @export
#'
#' @examples
#' # Vektorar med eksempel på svar frå undersøkjing
#'
#' # På ein skala frå 1-5, kor godt nøgd er du med x?
#' x = c(5, 5, 4, 5)
#' # På ein skala frå 1-5, kor godt nøgd er du med y?
#' y = c(1, 2, 1, 1)
#'
#' ggplot2::mean_cl_normal(x)
#' mean_cl_normal_truncated(ymin = 1, ymax = 5)(x)
#'
#' d = tibble(spm = rep(c("x", "y"), each = 4), verdi = c(x, y))
#' p = ggplot(d, aes(spm, verdi)) +
#'   geom_point(position = position_dodge2(width = 0.05))
#'
#' p + stat_summary(fun.data = mean_cl_normal, size = 1, color = "red")
#' p + stat_summary(fun.data = mean_cl_normal_truncated(ymin = 1, ymax = 5), size = 1, color = "red")
mean_cl_normal_truncated = function(ymin = NA, ymax = NA) {
  function(x, ...) {
    res = ggplot2::mean_cl_normal(x, ...)
    res$ymin = pmax(ymin, res$ymin, na.rm = TRUE)
    res$ymax = pmin(ymax, res$ymax, na.rm = TRUE)
    res
  }
}
