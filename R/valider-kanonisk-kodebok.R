#' Valider kodebok
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Funksjon for å validere kodebok for å sikre at den er i henhold til
#' vårt standardformat `(kanonisk kodebok)`.
#' Tar inn kanonisk kodebok og kjører stille gitt at kodeboken oppfyller
#' kravene til standardformat.
#'
#' Merk at denne funksjonen kontrollerer om kodebok er riktig
#' strukturert og følger de krav og forventninger vi har til en kodebok.
#' Dette gjelder *ikke* validering av registerspesifikke regler.
#'
#' @param kodebok kanonisk kodebok. Det vil si en liste med de tre tibblene:
#' \itemize{
#'    \item Kodebok
#'    \item Kategoriske
#'    \item Regler
#'    }
#'
#' @keywords internal
valider_kanonisk_kodebok = function(kodebok) {

  valider_kanonisk_skjema(kodebok)
  valider_kanonisk_kolonner(kodebok)
  valider_kanonisk_variabler(kodebok)

}

#' Valider skjema
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Et sett med tester for å validere kodebok på skjemanivå.
#'
#' @param kodebok kodebok på standard format.
#'
#' @keywords internal
valider_kanonisk_skjema = function(kodebok) {

# Unike kombinasjoner av skjema_id og skjemanavn   
  assert_that(all(kodebok$Kodebok |> 
                    group_by(skjema_id) |> 
                    summarise(n_skjemanavn = n_distinct(skjemanavn)) |> 
                    pull(n_skjemanavn) == 1),
              msg = "Skjema_id kan bare ha ett unikt tilhørende skjemanavn.")

# Unike kombinasjoner er variabelnavn innenfor hvert skjema   
  assert_that(nrow(kodebok$Kodebok |> 
                    group_by(skjema_id) |> 
                    count(variabel_id) |> 
                    filter(n > 1)) == 0,
              msg = "En variabel må være unikt definert innenfor et skjema.")
  
# Alle obligatoriske kolonner er fylt ut
  assert_that(all(!is.na(kodebok$Kodebok$skjema_id) & 
                    !is.na(kodebok$Kodebok$skjemanavn) & 
                    !is.na(kodebok$Kodebok$variabel_id) & 
                    !is.na(kodebok$Kodebok$variabeltype) &
                    !is.na(kodebok$Kodebok$obligatorisk) & 
                    !is.na(kodebok$Kodebok$regler)), 
              msg = "Alle obligatoriske variabler må være fylt ut")
}

#' 
#' Valider kolonner
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Et sett med tester for å validere kodebok på kolonnenivå.
#'
#' @param kodebok kodebok på standard format
#'
#' @keywords internal
valider_kanonisk_kolonner = function(kodebok) {
  
  aksepterte_variabeltyper = c(
    "kategorisk", "tekst", "boolsk",
    "dato", "numerisk", "kl", "dato_kl"
  )

  # Sjekk at variabeltyper er kjent
  assert_that(all(kodebok$Kodebok$variabeltype %in% aksepterte_variabeltyper),
              msg = paste0("Variabeltype '", 
              kjed_ord(unique(kodebok$Kodebok$variabeltype[!kodebok$Kodebok$variabeltype %in% aksepterte_variabeltyper]), skiljeteikn = "', '", og = "' og '"), 
              "' er ikke støttet. Må være en av: 'kategorisk', 'tekst', 'boolsk', 'dato', 'numerisk', 'kl' eller 'dato_kl'")
              )
  
  # sjekk at obligatorisk og regler kolonne er boolske
  assert_that(is.flag(all(kodebok$Kodebok$obligatorisk)),
              msg = "'obligatorisk' må være TRUE eller FALSE.")
              
  assert_that(is.flag(all(kodebok$Kodebok$regler)),
              msg = "'regler' må være TRUE eller FALSE.")

  # sjekk at desimaler er positivt heltall hvis det er inkludert
  assert_that(is.integer(kodebok$Kodebok$desimaler) && 
                all(kodebok$Kodebok$desimaler[kodebok$Kodebok$desimaler >= 0], na.rm = TRUE),
              msg = "Desimaler må være ikke-negative heltall.")
  
  # sjekk at variabel_id kun er tall, bokstaver og _. må begynne med en bokstav
  ugyldig_varnavn = kodebok$Kodebok$variabel_id[stringr::str_detect(kodebok$Kodebok$variabel_id,
                                                   pattern = "^[a-zA-Zæøå](\\w+)$", negate = TRUE)]

  assert_that(length(ugyldig_varnavn) == 0,
              msg = paste0("Variabelnavn: '", 
                           kjed_ord(unique(ugyldig_varnavn,
                                           skiljeteikn = "', '", 
                                           og = "' og '")
                                    ), "' er ikke gyldig. Variabelnavn må kun 
                           inneholde bokstaver, tall og '_'. Kan ikke starte med et tall.")
              )
  }

#' Valider variabler
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Et sett med tester for å valider kodebok på variabelnivå.
#'
#' @param kodebok kodebok på standardformat
#'
#' @keywords internal
valider_kanonisk_variabler = function(kodebok) {

  # sjekke at variabeltyper er entydige
  ulike_vartyper = kodebok$Kodebok |>
    group_by(variabel_id) |>
    summarise(antall_ulike = n_distinct(variabeltype)) |>
    filter(antall_ulike > 1) |>
    pull(variabel_id)
  
  assert_that(length(ulike_vartyper) == 0,
              msg = paste0("Variabler må ha entydige variabeltyper:\n'", 
                           kjed_ord(ulike_vartyper, skiljeteikn = "', '", og = "' og '"),
                           "'.")
              )
  
  assert_that(length(ulike_vartyper) == 0,
              msg = paste0("Variabler må ha entydige variabeltyper:\n'", 
                           kjed_ord(ulike_vartyper, skiljeteikn = "', '", og = "' og '"),
                           "'.")
              )
  
  # sjekk at variabeletiketter er entydige
  ulike_variabeletiketter = kodebok$Kodebok |>
    group_by(variabel_id) |>
    summarise(antall_ulike = n_distinct(variabeletikett)) |>
    filter(antall_ulike > 1) |>
    pull(variabel_id)

  assert_that(length(ulike_variabeletiketter) == 0,
              msg = paste0("Variabler må ha entydige variabelettiketter:\n'",
                           kjed_ord(ulike_variabeletiketter, 
                                    skiljeteikn = "', '", 
                                    og = "' og '"), "'.")
              )

  # sjekk at kategoriske variabler har tilsvarende verditekst for hver verdi på tvers av skjema
  flere_verditekster = kodebok$Kategoriske |>
    group_by(variabel_id, verdi) |>
    summarise(antall_verditekst = n_distinct(verditekst), 
              .groups = "drop") |>
    filter(antall_verditekst > 1) |> 
    pull(variabel_id)

  assert_that(length(flere_verditekster) == 0,
              msg = paste0("Kategoriske variabler må ha entydige verdi-verditekstkombinasjon på tvers av skjema:\n'",
                           kjed_ord(flere_verditekster, 
                                    skiljeteikn = "', '", 
                                    og = "' og '"), "'.")
              )

  # sjekke diverse ting med kategoriske variabler
  duplikat_verdi = kodebok$Kategoriske |>
    group_by(skjema_id, variabel_id) |>
    add_count(verdi, name = "antall_av_verdi") |>
    filter(antall_av_verdi > 1) |>
    distinct(variabel_id) |>
    pull(variabel_id)

  na_verdi = kodebok$Kategoriske |>
    filter(
      is.na(verdi)
    ) |>
    pull(variabel_id)

  antall_alternativ = kodebok$Kategoriske |>
    group_by(variabel_id) |>
    summarise(antall_alternativ = n()) |>
    filter(antall_alternativ < 2) |>
    pull(variabel_id)

  assert_that(length(duplikat_verdi) == 0,
              msg = paste0("Kategoriske variabler må ha unike verdier:\n'",
                           kjed_ord(duplikat_verdi, 
                                    skiljeteikn = "', '", 
                                    og = "' og '")
                           , "'.")
              )
  
  assert_that(length(na_verdi) == 0,
              msg = paste0("Kategoriske variabler kan ikke ha NA som verdi:\n'",
                           kjed_ord(na_verdi, 
                                    skiljeteikn = "', '", 
                                    og = "' og '"), 
                           "'.")
              )
  
  assert_that(length(antall_alternativ) == 0,
              msg = paste0("Kategoriske variabler må ha minst to svaralternativ:\n'",
                           kjed_ord(antall_alternativ, 
                                    skiljeteikn = "', '", 
                                    og = "' og '"), 
                           "'.")
              )
  
  assert_that(nrow(kodebok$Kodebok |> 
                     filter(variabeltype == "kategorisk", 
                            !is.na(desimaler))) == 0, 
              msg = "Kategoriske variabler kan ikke ha antall desimaler oppgitt.")
  
}
