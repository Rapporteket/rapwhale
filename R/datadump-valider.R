# dd_er_gyldig er en funksjon for å
# sjekke at en datadump fra et register er gyldig gitt
# informasjonen som er tilgjengelig om datadumpen i
# tilhørende kodebok (dokumentasjon).

# I dd_er_gyldig lages et regelsett fra aktuell kodebok automatisk
# via funksjonen lag_regelsett. Deretter sjekkes om reglene er oppfylt (TRUE).
# Hvis datadumpen ikke er gydlig, vil man få en oppsummering av feilene og hvor de er
# å finne, på format utarbeidet av ruler-pakken.

# Funksjon for å lage regler basert på informasjon
# fra kodeboka. Krever en kodebok.
# Argumentet "oblig" gjør det mulig å velge om man ønsker å sjekke obligatoriske felt.
# dette fordi MRS-kodebøker har feil i sin obligatorisk-koding,
# og dermed er uaktuelle for testing av dette feltet. Standard er TRUE.
# Argumentet "rekkefolge" gjør det mulig å velge å sjekke om
# datadumpen har samme rekkefølge på variabelnavn som i kodeboka.
# Dette fordi OpenQREG har dokumentert at det ikke nødvendigvis er i samme rekkefølge.

#' Lag regelsett
#'
#' @description
#' `r lifecycle::badge("maturing")`
#'
#' Funksjon som automatisk lager et regelsett basert på informasjon fra
#' en kodebok. Brukes for å validere en datadump.
#'
#' @param kodebok kodeboken datadumpen skal sjekkes mot.
#' @param oblig Argument for om obligatoriske felt skal sjekkes.
#' @param rekkefolge Argument for å sjekke om datadump har samme rekkefølge på
#' variabelnavn som det som er oppgitt i kodeboken.
#'
#' @keywords internal
lag_regelsett = function(kodebok, oblig = TRUE, rekkefolge = TRUE) {
  # for å lage regler må kodeboka ha følgende kolonner:
  nodvar = c("variabel_id", "variabeltype", "min", "maks", "desimalar", "verdi", "verditekst", "obligatorisk")
  # kolonner i kodebok
  kol = names(kodebok)

  # Stopp hvis kodeboka mangler helt nødvendige kolonner
  if (!all(nodvar %in% kol)) {
    manglende_nodvar = which(!nodvar %in% kol)
    kb_mangler = nodvar[manglende_nodvar]
    stop("Kodeboka mangler obligatoriske kolonner: ", str_c("'", kb_mangler, "'", collapse = ", "), ".")
  }

  # Stopp hvis variabel_id eller variabeltype mangler verdi
  for (kol in c("variabel_id", "variabeltype")) {
    if (anyNA(kodebok[[kol]])) {
      stop("Kodeboka har manglende verdier for variabel_id og/eller variabeltype.")
    }
  }

  # funksjon for å hente ut ønsket område av kodeboka
  # som brukes i en gitt test. F.eks, om man ønsker å teste min-verdier
  # i en datadump, så trenger man bare informasjon om min-verdier
  # for variabler som faktisk *har* min-verdier.
  kb_filter = function(kodebok, kolonne) {
    # henter ut delen av kodeboka som
    # har en verdi i en aktuelle kolonnen
    kb_utsnitt = kodebok |>
      filter(!is.na(!!as.symbol(kolonne))) |>
      select(variabel_id, all_of(kolonne)) |>
      rename(varnamn = "variabel_id")
    # rename funksjonen støtter ikke expressions.
    # altså kan man ikke ha gverdi = past0(kolonne) i kallet til rename() ovenfor
    # kjører koden under for å endre navn på kolonne til noe som brukes generelt i alle tester
    kolonnenavn_plassering = which(names(kb_utsnitt) == kolonne)
    names(kb_utsnitt)[kolonnenavn_plassering] = "gverdi"

    # returner objektet
    kb_utsnitt
  }

  kb_min = kb_filter(kodebok, "min")
  kb_maks = kb_filter(kodebok, "maks")
  kb_des = kb_filter(kodebok, "desimalar")
  kb_oblig = kb_filter(kodebok, "obligatorisk") |>
    filter(gverdi == "ja")

  # trenger 4 filter for kodeboka for ulike typer variabel
  kb_rename = rename(kodebok, varnamn = "variabel_id") # bruker denne lenger nede også
  kb_kat = kb_rename |>
    filter(variabeltype == "kategorisk") |>
    select(varnamn, verdi) |>
    rename(gverdi = "verdi")
  kb_num = kb_rename |>
    filter(variabeltype == "numerisk" | variabeltype == "utrekna") |>
    distinct(varnamn)
  kb_boolsk = kb_rename |>
    filter(variabeltype == "boolsk") |>
    select(varnamn)
  kb_tekst = kb_rename |>
    filter(variabeltype == "tekst") |>
    select(varnamn)

  # Stopp hvis kategoriske variabler mangler verdi eller verditekst
  if (anyNA(kb_kat$gverdi)) {
    stop("Kategoriske variabler mangler verdier for verdi.")
  }

  #------------------min-verdier------------------------------------------------

  # Lager regel for at verdier i data
  # skal være større eller lik min-verdi i kodebok.
  # Lager en egen regel for hver aktuelle rad i
  # datarammen.

  # lager tom liste som vi fyller reglene i
  l_min_maks = list()

  if (nrow(kb_min) > 0) {
    sjekk_min = pmap(kb_min, \(varnamn, gverdi) {
      rlang::new_function(
        alist(df = ),
        rlang::expr(mutate(df,
          across(all_of(varnamn),
            .fns = ruler::rules(min_ok = (. >= !!gverdi) | is.na(.))
          ),
          .keep = "none"
        ))
      )
    }) |>
      setNames(paste0("min_", kb_min$varnamn))

    # fyller regelen i lista
    l_min_maks = append(l_min_maks, sjekk_min)
  }
  #  -----------------------------maks------------------------------------------

  # Lager "rules" som tester maks-verdier
  if (nrow(kb_maks) > 0) {
    sjekk_maks = pmap(kb_maks, \(varnamn, gverdi) {
      rlang::new_function(
        alist(df = ),
        rlang::expr(mutate(df,
          across(all_of(varnamn),
            .fns = ruler::rules(maks_ok = (. <= !!gverdi) | is.na(.))
          ),
          .keep = "none"
        ))
      )
    }) |>
      setNames(paste0("maks_", kb_maks$varnamn))
    # fyller regelen i lista
    l_min_maks = append(l_min_maks, sjekk_maks)
  }
  # lager en cell-pack med maks-sjekkene
  er_innfor_min_og_maks = ruler::cell_packs(l_min_maks)

  #----------------------desimaler----------------------------------------------

  # Lager "rules" som tester at variablene har riktig antall desimaler
  sjekk_des = list()
  if (nrow(kb_des) > 0) {
    sjekk_des = pmap(kb_des, \(varnamn, gverdi) {
      rlang::new_function(
        alist(df = ),
        rlang::expr(mutate(df,
          across(all_of(varnamn),
            .fns = ruler::rules(des_ok = is.na(.) | (round(., gverdi) == .))
          ),
          .keep = "none"
        ))
      )
    }) |>
      setNames(paste0("des_", kb_des$varnamn))
  }

  # lager en cell-pack med des-sjekkene
  har_riktig_ant_des = ruler::cell_packs(sjekk_des)

  #--------------------obligatoriske felt---------------------------------------

  if (oblig) {
    # Lager "rules" på at obligatoriske variabler ikke skal ha noen missing.
    sjekk_oblig = pmap(kb_oblig, \(varnamn, gverdi) {
      rlang::new_function(
        alist(df = ),
        rlang::expr(mutate(df,
          across(all_of(varnamn),
            .fns = ruler::rules(gverdi = !is.na(.))
          ),
          .keep = "none"
        ))
      )
    }) |>
      setNames(paste0("oblig_", kb_oblig$varnamn))

    # lager en cell-pack med oblig-sjekkene
    oblig_har_ingen_missing = ruler::cell_packs(sjekk_oblig)
    oblig_har_ingen_missing
  }
  #--------------------kategoriske verdier--------------------------------------

  # Lager "rules" som sier at verdiene til kategoriske variabler må være tilstede i kodeboka
  if (nrow(kb_kat) != 0) {
    kb_kat_kompakt = nest(kb_kat, data = c(gverdi))
    sjekk_kat = pmap(kb_kat_kompakt, \(varnamn, data) {
      gverdi = data$gverdi
      rlang::new_function(
        alist(df = ),
        rlang::expr(mutate(df,
          across(all_of(varnamn),
            .fns = ruler::rules(gyl_kat = . %in% !!gverdi | is.na(.))
          ),
          .keep = "none"
        ))
      )
    }) |>
      setNames(paste0("kat_", kb_kat_kompakt$varnamn))

    # lager en cell-pack med verdi-sjekkene
    kat_er_innfor_verdier = ruler::cell_packs(sjekk_kat)
  }
  #--------------------variabeltype---------------------------------------------

  # lager en liste for å få inn regler avhengig av hvilke som eksisterer i kodeboka
  l_vartype = list()
  # Lager "rules" som tester om en kolonne i datasettet er samme som i kodeboka.
  # en sjekk for numeriske variabler
  if (nrow(kb_num) > 0) {
    sjekk_num = pmap(kb_num, \(varnamn) {
      rlang::new_function(
        alist(df = ),
        rlang::expr(summarise(
          df,
          across(all_of(varnamn),
            .fns = ruler::rules(vartype_ok = is.numeric(.))
          )
        ))
      )
    }) |>
      setNames(paste0("num_", kb_num$varnamn))
    # appender regelen til lista
    l_vartype = append(l_vartype, sjekk_num)
  }
  # boolske
  if (nrow(kb_boolsk) > 0) {
    sjekk_boolsk = pmap(kb_boolsk, \(varnamn) {
      rlang::new_function(
        alist(df = ),
        rlang::expr(summarise(
          df,
          across(all_of(varnamn),
            .fns = ruler::rules(vartype_ok = is.logical(.))
          )
        ))
      )
    }) |>
      setNames(paste0("boolsk_", kb_boolsk$varnamn))
    # appender regelen til lista
    l_vartype = append(l_vartype, sjekk_boolsk)
  }
  # tekstvariabler
  if (nrow(kb_tekst) > 0) {
    sjekk_tekst = pmap(kb_tekst, \(varnamn) {
      rlang::new_function(
        alist(df = ),
        rlang::expr(summarise(
          df,
          across(all_of(varnamn),
            .fns = ruler::rules(vartype_ok = is.character(.))
          )
        ))
      )
    }) |>
      setNames(paste0("tekst_", kb_tekst$varnamn))
    # appender regelen til lista
    l_vartype = append(l_vartype, sjekk_tekst)
  }

  # lager en col-pack med variabeltype-sjekkene
  er_riktig_variabeltype = ruler::col_packs(l_vartype)

  #----------------------alle variabelnavn tilstede-----------------------------
  # Test sjekker at alle variablenavn i datadump er med i kodeboka (samtidig)
  # og at alle varibelnavn i kodebok er med i datadump
  alle_var_er_med = ruler::data_packs(
    sjekk_alle_varnavn_dd = function(datasett) {
      summarise(datasett,
        alle_varnavn = all(names(datasett) %in% distinct(kodebok, variabel_id)$variabel_id)
      )
    },
    sjekk_alle_varnavn_kb = function(datasett) {
      summarise(datasett,
        alle_varnavn = all(distinct(kodebok, variabel_id)$variabel_id %in% names(datasett))
      )
    }
  )

  #---------------------lik rekkefølge på variabelnavn som i kodebok------------

  # sjekk at rekkefølgen på kolonner er lik mellom data og kodebok
  if (rekkefolge) {
    er_lik_rekkefolge = ruler::data_packs(
      sjekk_rekkefolge = function(datasett) {
        summarise(datasett,
          rekkefolge_varnavn = identical(
            names(datasett),
            distinct(kodebok, variabel_id)$variabel_id
          )
        )
      }
    )
  }
  regelsett = list(
    er_innfor_min_og_maks,
    har_riktig_ant_des,
    er_riktig_variabeltype,
    alle_var_er_med
  )
  # legger kun til objekt for oblig og rekkefolge hvis vi ønsker å teste det
  if (oblig) {
    regelsett = c(regelsett, list(oblig_har_ingen_missing))
  }
  if (rekkefolge) {
    regelsett = c(regelsett, list(er_lik_rekkefolge))
  }
  if (nrow(kb_kat) > 0) {
    regelsett = c(regelsett, list(kat_er_innfor_verdier))
  }
  # returnerer lista
  regelsett
}

#' Validering av datadump
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Funksjon som automatisk lager et regelsett basert på en kodebok, og sjekker at
#' datadumpen er gyldig gitt dette regelsettet. \cr \cr
#' Returnerer TRUE viss og berre viss datadumpen er gyldig. \cr
#' Hvis FALSE, vil funksjonen gi ut en oppsummering på et format fra ruler-pakken
#' på hva som er feil og hvor disse er å finne.
#'
#' @param d Datasett som skal valideres.
#' @param kodebok Kodebok med informasjon om variablene i valgt datadump.
#' Skal være kodebok på kanonisk form og på Nasjonalt servicemiljø for
#' medisinske kvalitetsregistre region vest (NASERVE)
#' sitt standard kodebokformat.
#' @param ... Eventuelle argument som blir gitt videre til [lag_regelsett()].
#' @export
dd_er_gyldig = function(d, kodebok, ...) {
  regelsett = lag_regelsett(kodebok, ...)
  test_res = ruler::expose(d, regelsett)

  # Sjekk om det var noen feil + generer feilrapport
  er_gyldig = !ruler::any_breaker(test_res)
  rapport = ruler::get_report(test_res)

  # Returner gyldighetsstatus + feilrapport
  attr(er_gyldig, "rapport") = rapport
  er_gyldig
}
