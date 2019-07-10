# Valider kodebok (som skal vera på vårt standardformat)

#' @importFrom rlang enquo quo_text quos quo_name
#' @importFrom magrittr %>%
#' @importFrom stringr str_c str_detect
#' @importFrom tidyr nest fill
#' @importFrom purrr walk map_lgl
#' @import dplyr
NULL

# Gjer kodeboka om til kanonisk form, dvs. slik at
# implisitte verdiar er fylde ut.
# fixme! funksjonen mangler å ta inn den nye variabelen nokkel (per i dag blir den kastet ut av kodeboka)
# fixme! funksjonen håndterer ikke ekstra variabler som f.eks variabel_id_checkware.
# Når man er på det, bør funksjonen heller ha en option hvor man kan skrive inn kolonner man ønsker å godta selv
# om det ikke er en del av standard-kolonnene. Da kan man ha med variabel_id_checkware, maal_id, nokkel og andre aktuelle kolonner


#' Valider kodebok
#'
#' Gjer kodeboka om til kanonisk form, dvs. funksjonen fyller inn implisitte
#' verdiar for variablane i kodeboka.
#'
#' @param kb Kodebok som skal valideres.
#' @export

kb_til_kanonisk_form = function(kb) {
  # Avgrupper (i tilfelle dataramma alt er gruppert,
  # noko som kan føra til problem nedanfor
  kb = ungroup(kb)

  # Legg til standardverdiar for kolonnen 'kolnamn' dersom kolonnen manglar
  leggtil_std = function(df, kolnamn, verdiar) {
    kolnamn = quo_text(enquo(kolnamn))
    if (!hasName(df, kolnamn)) {
      df[[kolnamn]] = verdiar
    }
    df
  }

  # Det kan vera at nokre ikkje-essensielle kolonnar manglar.
  # Då legg med dei til, med NA-verdiar eller eksempeldata,
  # alt etter kva som trengst.
  kb = kb %>%
    leggtil_std(skjema_id, "fiktiv_skjema_id")
  kb = kb %>%
    leggtil_std(skjemanamn, paste0("Skjemanamn for ", kb$skjema_id))
  kb = kb %>%
    leggtil_std(kategori, NA_character_)
  kb = kb %>%
    leggtil_std(innleiing, NA_character_)
  kb = kb %>%
    leggtil_std(variabeletikett, NA_character_)
  kb = kb %>%
    leggtil_std(forklaring, NA_character_)
  kb = kb %>%
    leggtil_std(manglande, NA_character_)
  kb = kb %>%
    leggtil_std(eining, NA_character_)
  kb = kb %>%
    leggtil_std(unik, "nei")
  kb = kb %>%
    leggtil_std(min, NA_real_)
  kb = kb %>%
    leggtil_std(maks, NA_real_)
  kb = kb %>%
    leggtil_std(min_rimeleg, NA_real_)
  kb = kb %>%
    leggtil_std(maks_rimeleg, NA_real_)
  kb = kb %>%
    leggtil_std(kommentar_rimeleg, NA_character_)
  kb = kb %>%
    leggtil_std(utrekningsformel, NA_character_)
  kb = kb %>%
    leggtil_std(logikk, NA_character_)
  kb = kb %>%
    leggtil_std(kommentar, NA_character_)

  # Gjer kodeboka om til ikkje-glissen form,
  # dvs. at skjema_id, variabel_id og sånt er gjentatt nedover.
  mogleg_glisne_kol = quos(skjema_id, variabel_id)
  kb = kb %>%
    fill(!!!mogleg_glisne_kol)

  # Nokre andre kolonnar må òg gjerast om til ikkje-glissen form,
  # men no berre innanfor variabel-ID. For eksempel skal «forklaring»
  # gjentakast nedanfor, men skal ikkje kryssa variabelgrenser
  # (noko som kunne skje viss me brukte metoden over, sidan «forklaring»
  # òg skal kunna stå tom).
  mogleg_glisne_kol = quos(variabeletikett, forklaring, variabeltype, unik, obligatorisk)
  kb = kb %>%
    mutate(radnr = 1:n()) %>%
    group_by(variabel_id) %>% # Endrar rekkjefølgja på radene
    fill(!!!mogleg_glisne_kol) %>%
    ungroup() %>%
    arrange(radnr) %>% # Gjenopprett radrekkefølgja
    select(-radnr)

  # Tilsvarande men no innanfor skjema_id
  mogleg_glisne_kol = quos(skjemanamn, kategori)
  kb = kb %>%
    mutate(radnr = 1:n()) %>%
    group_by(skjema_id) %>% # Endrar rekkjefølgja på radene
    fill(!!!mogleg_glisne_kol) %>%
    ungroup() %>%
    arrange(radnr) %>% # Gjenopprett radrekkefølgja
    select(-radnr)

  # Fyll ut implisitte obligatoriskverdiar og unikverdiar
  # til «nei». «ja*» tyder obligatorisk berre under visse
  # vilkår, så det skal òg reknast som «nei», med mindre det
  # er snakk om boolske variablar, som alltid er obligatoriske.
  kb$obligatorisk[kb$variabeltype == "boolsk"] = "ja"
  kb$obligatorisk[is.na(kb$obligatorisk) | kb$obligatorisk == "ja*"] = "nei"
  kb$unik[is.na(kb$unik) | kb$unik == "ja*"] = "nei"

  # Fyll ut implisitte verdiar for «manglande» (gjeld berre kategoriske variablar)
  kb$manglande[is.na(kb$manglande) & kb$variabeltype == "kategorisk"] = "nei"

  # Verdikolonnen skal vera ein tekstkolonne
  # (ofte er det tal, men det kan vera tekst,
  # så det må det vera tekst)
  kb$verdi = as.character(kb$verdi)

  # Desimalkolonnen må vera ei heiltalskolonne
  stopifnot(all(kb$desimalar == round(kb$desimalar), na.rm = TRUE))
  kb$desimalar = as.integer(kb$desimalar)

  # I vårt kanoniske kodebokformat er «obligatorisk» ein
  # tekstvariabel med verdiane «ja» og «nei». Litt lite R-aktig,
  # men det gjer iallall at kodeboka er lett å utveksla med andre.
  # Viss me får kodebok som heller brukar boolske verdiar,
  # gjer om til tekstverdiar.
  if (is.logical(kb$obligatorisk)) {
    kb$obligatorisk = ifelse(kb$obligatorisk, "ja", "nei")
  }

  # Sorter kolonnane i kodeboka etter vårt standardformat
  # og fjern eventuelle ekstrakolonnar som ikkje skal vera med
  std_namn = c(
    "skjema_id", "skjemanamn", "kategori", "innleiing", "variabel_id",
    "variabeletikett", "forklaring", "variabeltype", "eining", "unik",
    "obligatorisk", "verdi", "verditekst", "manglande", "desimalar",
    "min", "maks", "min_rimeleg", "maks_rimeleg", "kommentar_rimeleg",
    "utrekningsformel", "logikk", "kommentar"
  )
  ekstravars = setdiff(names(kb), std_namn)
  if (length(ekstravars) > 0) {
    warning(paste0(
      "Fjernar kolonnar som ikkje skal vera med i kodeboka:\n",
      paste0(ekstravars, collapse = ", ")
    ))
  }
  kb = kb %>%
    select(!!std_namn)

  # Returner kodeboka på kanonisk form
  kb
}

# Funksjon som går gjennom en rekke tester for om en kodebok er gyldig
# Tar imot en kodebok på glissen form, som er på fagsenterets standardform.
# Argument:
#   sjekk_varnamn: Skal variabelnavn også sjekkes for gyldighet (bare små bokstaver, _ og siffer)
#             ...: Andre argument som skal videresendes til intern funksjon varnamn_er_gyldig()


#' Tester om kodebok er gyldig
#'
#' Funksjonen går gjennom en rekke tester for å se om en kodebok er gyldig. Tar inn en kodebok på glissen form.
#'
#' @param kb_glissen Kodebok på glissen form, det vil si en kodebok som er
#' formatert i henhold til fagsenterets standardformat.
#' @param sjekk_varnamn Skal variabelnavn også sjekkes for gyldighet?
#' Godkjente variabelnavn er i snake-case; små bokstaver og "_" som mellomrom.
#' @export
kb_er_gyldig = function(kb_glissen, sjekk_varnamn = TRUE, ...) {

  # Antar i utgangspunktet at kodeboken er gyldig
  gyldig = TRUE

  #------------------------------------------------Tester på glissen KB-------------------------------------------

  # Kjed saman tekststrengar og formater enkeltelement med '-teikn rundt seg
  lag_liste = function(x) {
    str_c("'", x, "'", collapse = ", ")
  }

  # Standard kolonnenamn som alle kodebøker skal ha
  std_namn = c(
    "skjema_id", "skjemanamn", "kategori", "innleiing", "variabel_id",
    "variabeletikett", "forklaring", "variabeltype", "eining", "unik",
    "obligatorisk", "verdi", "verditekst", "manglande", "desimalar",
    "min", "maks", "min_rimeleg", "maks_rimeleg", "kommentar_rimeleg",
    "utrekningsformel", "logikk", "kommentar"
  )

  # Sjekk først at alle standardkolonnane er med
  # (kodeboka kan ha andre kolonnar òg, men iallfall desse)
  kol_manglar = setdiff(std_namn, names(kb_glissen))
  if (length(kol_manglar) > 0) {
    warning(
      "Kodeboka manglar kolonnar:\n",
      paste0(kol_manglar, sep = "\n")
    )
    gyldig = FALSE
  }

  # Sjå på standardkolonnane, men i rekkefølgja dei finst på i kodeboka
  kb_namn = names(kb_glissen[names(kb_glissen) %in% std_namn])

  # Sjekk at kolonnane kjem i standard rekkefølgje
  # (men merk at me i *denne* testen godtek at
  # enkelte kolonnar manglar, sidan me testar det tidlegare).
  ind = match(kb_namn, std_namn)
  forste_feil = which(diff(ind) < 0)[1]
  if (!is.na(forste_feil)) {
    warning(paste0(
      "Feil rekkefølgje på kolonnar. Første feil:\n",
      "Kolonnen ",
      lag_liste(kb_namn[forste_feil]),
      " står *før* ",
      lag_liste(kb_namn[forste_feil + 1]),
      " men skal stå (ein eller annan plass) etter."
    ))
    gyldig = FALSE
  }

  # Ser vidare berre på standardkolonnane
  kb_stdkols = kb_glissen[std_namn]

  # Sjekk at variabelformatet (tal, tekst, dato osv.) er rett
  format_std = tibble::tribble(
    ~kol_namn, ~kol_klasse_std,
    "skjema_id", "character",
    "skjemanamn", "character",
    "kategori", "character",
    "innleiing", "character",
    "variabel_id", "character",
    "variabeletikett", "character",
    "forklaring", "character",
    "variabeltype", "character",
    "eining", "character",
    "unik", "character",
    "obligatorisk", "character",
    "verdi", "character", # Oftast numerisk, men av og til ikkje, eks. ICD-kodar, så må vera tekst
    "verditekst", "character",
    "manglande", "character",
    "desimalar", "integer",
    "min", "numeric",
    "maks", "numeric",
    "min_rimeleg", "numeric",
    "maks_rimeleg", "numeric",
    "kommentar_rimeleg", "character",
    "utrekningsformel", "character",
    "logikk", "character",
    "kommentar", "character"
  )
  format_kb = tibble(kol_namn = names(kb_stdkols), kol_klasse = purrr::map_chr(kb_stdkols, ~ class(.x)[1]))
  format = left_join(format_std, format_kb, by = "kol_namn")
  format_feil = format %>%
    filter(kol_klasse_std != kol_klasse)
  if (nrow(format_feil) > 0) {
    warning(
      "Feil format på kolonnar:\n",
      format_feil %>% as.data.frame() %>% capture.output() %>% paste0(sep = "\n")
    )
    gyldig = FALSE
  }

  #------------------------------------------------Objekter---------------------------------
  # I vidare testar føreset me at kodeboka er på ikkje-glissen form,
  # dvs. at skjema_id, variabel_id og sånt er gjentatt nedover.
  # Viss ho er ikkje på den forma, ordnar me det sjølv. :)
  kb = kb_til_kanonisk_form(kb_glissen)

  # lager objekt for numeriske/utrekna variabler
  kb_num = kb %>%
    filter(variabeltype == "numerisk" | variabeltype == "utrekna")

  # lager objekt for kategoriske variabler
  kb_kat = kb %>%
    filter(variabeltype == "kategorisk")

  # lager objekt for boolske variabler
  kb_bool = kb %>%
    filter(variabeltype == "boolsk")

  # lager objekt for de som har kommentar_rimeleg
  kb_kom_rimeleg = kb %>%
    filter(!is.na(kommentar_rimeleg))

  # mange av advarselene starter med samme teksten
  # er nynorsken helt på tryne kan den rettes her
  advar_tekst = paste0("Ein eller fleire variablar har")


  #------------------------------------------------Tester på ikkje-glissen KB-------------------------------------------

  # Sjekk at me ikkje har duplikate skjema-ID-ar, skjemanamn eller variabel-ID-ar,
  # dvs. at alle unike verdiar kjem samanhengande nedover, utan nokre hòl
  # (eks. «xxxyy» er OK, men «xxyyx» er det ikkje).
  sjekk_dup = function(kb, idkol) {
    idkol = quo_name(enquo(idkol))
    ids = rle(kb[[idkol]])$values
    if (any(duplicated(ids))) {
      warning(
        "Duplikate verdiar i ", lag_liste(idkol), ":\n",
        lag_liste(unique(ids[duplicated(ids)])),
        "\n(Men merk at seinare duplikatar kan vera følgjefeil av første.)"
      )
      gyldig <<- FALSE
    }
  }
  sjekk_dup(kb, skjema_id)
  sjekk_dup(kb, skjemanamn)
  kb_per_skjema = kb %>%
    split(.$skjema_id)
  kb_per_skjema %>%
    walk(~ sjekk_dup(., variabel_id)) # Skal vera unik, men det held at det berre er innanfor skjema

  # Sjekk at valt variabel berre har éin verdi innanfor kvar gruppe
  sjekk_ikkjevar = function(df, gruppe, varid) {
    gruppe_tekst = quo_name(enquo(gruppe))
    varid_tekst = quo_name(enquo(varid))
    nest_cols = setdiff(names(df), gruppe_tekst)
    df_grupper = df %>%
      tidyr::nest_(key_col = "data", nest_cols = nest_cols) # fixme: Byt til quasi-quotation, dvs. «-!!gruppe» når det er støtta i dplyr

    ikkjeunike = df_grupper$data %>%
      map_lgl(~ length(unique(.x[[varid_tekst]])) > 1)

    if (any(ikkjeunike)) {
      warning(
        "Varierande/inkonsistente '", varid_tekst, "'-verdiar for desse '", gruppe_tekst, "'-verdiane:\n",
        lag_liste(df_grupper[[gruppe_tekst]][ikkjeunike])
      )
      gyldig <<- FALSE
    }
  }

  # Sjekk at innanfor skjema er skjemanamn unikt
  sjekk_ikkjevar(kb, skjema_id, skjemanamn)

  # Sjekk at kvar variabel berre har éin (dvs. unik)
  # variabeltype, variabeletikett osv.
  # (Nokre av testane gjer me innnanfor skjema,
  # då det ikkje er *så* farleg om dei varierer på
  # tvers av skjema.)
  sjekk_ikkjevar(kb, variabel_id, variabeltype)
  kb_per_skjema %>%
    walk(~ sjekk_ikkjevar(., variabel_id, variabeletikett))
  kb_per_skjema %>%
    walk(~ sjekk_ikkjevar(., variabel_id, forklaring))
  sjekk_ikkjevar(kb, variabel_id, unik)
  kb_per_skjema %>%
    walk(~ sjekk_ikkjevar(., variabel_id, obligatorisk))
  sjekk_ikkjevar(kb, variabel_id, kategori) # Variablar kan ikkje kryssa kategori- eller skjemagrenser

  # Sjekk at alle verdiar for kategoriske variablar er unike og ingen er NA
  kb_kat_nest = kb_kat %>%
    nest(-variabel_id, -skjema_id)
  verdi_ok = kb_kat_nest$data %>%
    map_lgl(~ (!any(duplicated(.x$verdi) | is.na(.x$verdi))))
  if (any(!verdi_ok)) {
    warning(
      advar_tekst, " dupliserte 'verdi'-ar eller NA som 'verdi':\n",
      lag_liste(kb_kat_nest$variabel_id[!verdi_ok])
    )
    gyldig = FALSE
  }

  # Sjekk at kategoriske variablar har minst *to* svaralternativ
  verdi_ok = kb_kat_nest$data %>%
    map_lgl(~ nrow(.) >= 2)
  if (any(!verdi_ok)) {
    warning(
      advar_tekst, " færre enn to svaralternativ:\n",
      lag_liste(kb_kat_nest$variabel_id[!verdi_ok])
    )
    gyldig = FALSE
  }

  # *Viss* kodeboka brukar kategoriar (det er frivillig å bruka,
  # men viss ein brukar det, skal alle skjema ha minst éin kategori),
  # sjekk at alle skjema startar med ei kategorioverskrift
  # (me sjekkar tidlegare oppe at desse er unike innanfor variabel_id)
  if (any(!is.na(kb$kategori))) {
    kb_skjema = kb %>%
      nest(-skjema_id)
    har_kat = kb_skjema$data %>%
      map_lgl(~ (!is.na(.x$kategori[1])) & (.x$kategori[1] != ""))
    if (any(!har_kat)) {
      warning(
        "Nokre skjema manglar kategorioverskrift (i førsterader):\n",
        lag_liste(kb_skjema$skjema_id[!har_kat])
      )
      gyldig = FALSE
    }
  }

  # Tester at bare gyldige variabeltyper er med i kodeboka
  # Objekt med gyldige variabeltyper til kanonisk standardform av kodebok,
  # hentet fra dokumentasjon om standardformen. Kan utvides.
  gyldige_vartyper = c("numerisk", "kategorisk", "boolsk", "dato", "dato_kl", "kl", "utrekna", "tekst", "tekst*", "fritekst")
  if (any(!kb$variabeltype %in% gyldige_vartyper)) {
    ugyldig_vartyp = kb %>%
      filter(!variabeltype %in% gyldige_vartyper) %>%
      pull(variabel_id) %>%
      unique()
    warning(
      advar_tekst, " ugyldige variabeltypar:\n",
      lag_liste(ugyldig_vartyp)
    )
    gyldig = FALSE
  }

  # Test på eining. Eining kan ikkje vera tom ("") (men kan vera NA)
  if (any(kb$eining %in% "")) {
    tom_eining = kb %>%
      filter(eining == "") %>%
      pull(variabel_id) %>%
      unique()
    warning(
      advar_tekst, " ugyldig eining, kor ein eller fleire har tomme tekststrengar:\n",
      lag_liste(tom_eining)
    )
    gyldig = FALSE
  }

  #-----------------------------------------------------Sjekk Gyldig Vartype-----------------------------------------
  # Funksjon som sjekker om en eller flere variabler i kodeboka
  # har en variabeltype som er ugyldig gitt
  # at variabelen har en verdi for en kolonne som kun gjelder andre variabeltyper

  # Funksjonen tester for kolonnetyper som gjelder ikke-numeriske, ikke-utrekna variabler
  # - disse kan ikke ha eining, desimalar, min- eller maksverdi og/eller min_rimeleg- eller maks_rimeleg-verdi

  # Funksjonen tester tester for kolonnetyper som gjeder ikke-kategoriske variabler
  # - disse kan ikke ha verdi, verditekst eller manlande == "ja"

  # Funksjonen krever argument for dataramme (kb),
  # kolonnetype, som er hvilken kolonne
  # man ønsker å sjekke at har gyldig tilhørende variabeltype (f.eks verditekst),
  # og hvilken variabeltype som er den gyldige - "numerisk" eller "kategorisk".

  sjekk_gyldig_vartype = function(kb, kolonnetype, vartype) {

    # alle tester for om variabeltypen er gyldig mht. kolonnetypen
    # har omtrent den samme teksten. lager en funksjon
    # for tekst til numerisk og kategoriske tester
    lag_tekst = function(vartype, kolonnetype) {
      if (vartype == "numerisk") {
        num_tekst = paste0("", advar_tekst, " ", kolonnetype, ", men er verken numerisk eller utrekna:\n")
      } else if (vartype == "kategorisk") {
        num_tekst = paste0("", advar_tekst, " ", kolonnetype, ", men er ikke kategorisk:\n")
      }
      num_tekst
    }

    # lager en if-statement med 3 ulike tester.
    # 1 tester for kolonnetyper som gjelder ikke-numeriske variabler
    # 2 tester for kolonnetyper som gjeder ikke-kategoriske variabler
    # 3 tester spesifikt for ikke-kategoriske variabler og om manglande == "ja" (som ikke skal være mulig)

    # objekt for vartyper som ikke er numerisk eller utrekna
    ikke_num = (kb$variabeltype != "numerisk") & (kb$variabeltype != "utrekna")

    # objekt for vartyper som ikke er kategorisk
    ikke_kat = (kb$variabeltype != "kategorisk")

    if (vartype == "numerisk") {
      # ikke ok kolonnetype, gitt variabeltypen
      ikke_ok_num = ikke_num & (!is.na(kb[[kolonnetype]]))

      if (any(ikke_ok_num)) {
        ugyldig_var = kb %>%
          filter(ikke_ok_num) %>%
          pull(variabel_id) %>%
          unique()
        warning(
          lag_tekst(vartype = "numerisk", kolonnetype),
          lag_liste(ugyldig_var)
        )
        gyldig = FALSE
      }
    } else if (vartype == "kategorisk") {
      # ikke ok kolonnetype, gitt variabeltypen
      ikke_ok_kat = ikke_kat & (!is.na(kb[[kolonnetype]]))

      if (any(ikke_ok_kat)) {
        ugyldig_var = kb %>%
          filter(ikke_ok_kat) %>%
          pull(variabel_id) %>%
          unique()
        warning(
          lag_tekst(vartype = "kategorisk", kolonnetype),
          lag_liste(ugyldig_var)
        )
        gyldig = FALSE
      }
    } else if (vartype == "kategorisk" & kolonnetype == "manglande") {
      # ikke ok kolonnetype, gitt variabeltypen
      ikke_ok_mangl = ikke_kat & (!is.na(kb[[kolonnetype]]) & kb[[kolonnetype]] == "ja")

      if (any(ikke_ok_mangl)) {
        ugyldig_var = kb %>%
          filter(ikke_ok_mangl) %>%
          pull(variabel_id) %>%
          unique()
        warning(
          lag_tekst(vartype = "kategorisk", kolonnetype),
          lag_liste(ugyldig_var)
        )
        gyldig = FALSE
      }
    }
  }

  sjekk_gyldig_vartype(kb, "eining", "numerisk")
  sjekk_gyldig_vartype(kb, "desimalar", "numerisk")
  sjekk_gyldig_vartype(kb, "min", "numerisk")
  sjekk_gyldig_vartype(kb, "maks", "numerisk")
  sjekk_gyldig_vartype(kb, "min_rimeleg", "numerisk")
  sjekk_gyldig_vartype(kb, "maks_rimeleg", "numerisk")
  sjekk_gyldig_vartype(kb, "verdi", "kategorisk")
  sjekk_gyldig_vartype(kb, "verditekst", "kategorisk")
  sjekk_gyldig_vartype(kb, "manglande", "kategorisk")


  # Funksjon som tester om kodeboka har noe annet enn verdiene
  # "ja" og "nei" for kolonnetyper
  # som kan bare ha "ja" og "nei".
  # Funksjonen tar et argument for objektet (kodeboka)
  # som skal testes, og en for kolonnetypen
  # som ikke kan ha noen andre verdier enn "ja" og "nei".
  sjekk_ja_nei = function(kb, kolonnetype) {

    # objekt som tester at kolonnetypen er innenfor ja og nei
    er_ja_nei = kb[[kolonnetype]] %in% c("ja", "nei")

    # Tester at observasjoner fra objektet over er "False".
    # Gir advarsel hvis testen ikke oppfyller dette.
    if (any(!er_ja_nei)) {
      ugyldig_ja_nei = kb %>%
        filter(!er_ja_nei) %>%
        pull(variabel_id) %>%
        unique()
      warning(
        advar_tekst, " har ein verdi for ", kolonnetype, " som ikkje er ja eller nei:\n",
        lag_liste(ugyldig_ja_nei)
      )
      gyldig = FALSE
    }
  }

  # tester om unik, obligatorisk og manglande bare har "ja" og "nei".
  sjekk_ja_nei(kb, "unik")
  sjekk_ja_nei(kb, "obligatorisk")
  sjekk_ja_nei(kb_kat, "manglande")

  # tester om desimalar kun er 0, positive eller missing.
  kb_num = kb_num %>%
    mutate(des_ok = (desimalar >= 0) | is.na(desimalar))
  if (!all(kb_num$des_ok)) {
    ugyldig_des = kb_num %>%
      filter(!des_ok) %>%
      pull(variabel_id) %>%
      unique()
    warning(
      advar_tekst, "har ein desimal som ikke er 0 eller positiv:\n",
      lag_liste(ugyldig_ja_nei)
    )
    gyldig = FALSE
  }

  # tester størrelsen på relasjon til en annen kolonne
  # her kan man velge om man ønsker å teste om
  # kolonnen er >, =>,<, <= osv. men funksjonen er laget
  # for å teste om min, maks, min_rimleg og maks_rimeleg er større
  # eller mindre enn hverandre.
  # Trenger kb, op for operasjon, x som er første kolonnen man tester,
  # og y, for andre kolonne man tester
  sjekk_op = function(kb, op, x, y) {
    op_ok = op(kb[[x]], kb[[y]]) | is.na(kb[[x]]) | is.na(kb[[y]])
    if (!all(op_ok)) {
      ugyldig_op = kb %>%
        filter(!op_ok) %>%
        pull(variabel_id) %>%
        unique()
      warning(
        advar_tekst, " ", x, "-verdi som er større enn ", y, "-verdi:\n",
        lag_liste(ugyldig_op)
      )
      gyldig = FALSE
    }
  }

  sjekk_op(kb, op = `<`, x = "min", y = "maks")
  sjekk_op(kb, op = `<`, x = "min_rimeleg", y = "maks_rimeleg")
  sjekk_op(kb, op = `<=`, x = "min", y = "min_rimeleg")
  sjekk_op(kb, op = `<=`, x = "maks_rimeleg", y = "maks")

  # Tester at variabler som har en verdi for kommentar_rimeleg
  # har enten min_rimeleg eller maks_rimeleg
  ok_kom_rimeleg = (!is.na(kb_kom_rimeleg$min_rimeleg) | !is.na(kb_kom_rimeleg$maks_rimeleg))

  # gir advarsel hvis testen ikke er oppfylt
  if (!all(ok_kom_rimeleg)) {
    ugyldig_kom_rimeleg = kb_kom_rimeleg %>%
      filter(!ok_kom_rimeleg) %>%
      pull(variabel_id) %>%
      unique()
    warning(
      advar_tekst, " verdi for kommentar_rimeleg, men ingen verdi for min- eller maks_rimeleg:\n",
      lag_liste(ugyldig_kom_rimeleg)
    )
    gyldig = FALSE
  }

  # Tester at boolske variabler ikke har obligatorisk == "nei" eller unik == "ja"
  ok_boolsk = (kb_bool$obligatorisk != "nei" & kb_bool$unik != "ja")

  # gir advarsel hvis testen ikke er oppfylt
  if (!all(ok_boolsk)) {
    ugyldig_bool = kb_bool %>%
      filter(!ok_boolsk) %>%
      pull(variabel_id) %>%
      unique()
    warning(
      advar_tekst, " obligatorisk = nei eller unik = ja selv om variabelen er boolsk:\n",
      lag_liste(ugyldig_bool)
    )
    gyldig = FALSE
  }

  #--------------------------------------- valider variabelnamn-------------------------------------------------

  # Viser og returnerer dei ugyldige variabelnamna
  # Viss «skjemaprefiks» er sann, må òg namna vera
  # gyldige dersom me fjernar teksten fram til
  # og med første «_»-teikn.
  # Viss «fald_bokstavar» er sann, vert namna gjort
  # om til små bokstavar før testing, dvs. store bokstavar
  # og ei blanding av små og store bokstavar vert godtatt.
  varnamn_er_gyldig = function(varnamn, skjemaprefiks = FALSE, fald_bokstavar = FALSE) {
    # Reglar:
    #   Må starta med a-z (ikkje siffer)
    #   Kan elles innhalda bokstavane a-z, siffera 0-9 og
    #   teiknet _, men kan ikkje avsluttast med _.
    reg_ok = "^[a-z]([0-9a-z_]*[0-9a-z])?$"
    varnamn = varnamn[varnamn != ""] # Sjå vekk frå tomme namn ...

    # Gjer ev. om til små bokstavar (dersom ein
    # ikkje vil testa at dei alt er det)
    if (fald_bokstavar) {
      varnamn = str_to_lower(varnamn)
    }

    # Finn indeks til ugyldige variabelnamn
    ugyldig = !str_detect(varnamn, reg_ok)

    # Nokre variabelnamn har skjemaprefiks, og i dei
    # tilfelle må variabelnamna vera gyldige sjølv om
    # me strippar vekk skjemaprefiksa
    if (skjemaprefiks) {
      varnamn_slutt = str_split_fixed(varnamn, "_", n = 2)[, 2] # Delen av variabelnamnet etter første _ (dvs. utan skjemaprefiks)
      ugyldig = (ugyldig | !str_detect(varnamn_slutt, reg_ok))
    }

    # Hent ut dei ugyldige variabelnamna
    varnamn_ugyldig = varnamn[ugyldig]

    if (any(ugyldig)) {
      # Vis alle dei ugyldige namna, eitt per linje.
      warning(
        advar_tekst, " ugyldige varnamn:\n",
        lag_liste(varnamn_ugyldig)
      )
    }

    # Returner info om alle variabelnavnene er gyldige
    all(!ugyldig)
  }

  # Sjekk at alle variabelnavn er gyldig (dersom brukeren vil sjekke dette)
  if (sjekk_varnamn) {
    varnamn_ok = varnamn_er_gyldig(kb$variabel_id, ...)
    if (!varnamn_ok) {
      gyldig = FALSE
    }
  }
  gyldig
}



# Eksempel på bruk --------------------------------------------------------

# # Eksempeldata for testing
# mappe = "h:/kvalreg/ablasjonsregisteret/"
# kb_test = readxl::read_excel(paste0(mappe,"kodebok-utkast.xlsx"), sheet = 1)
#
# # Lesing frå Excel gjev ikkje automatisk rett variabeltype
# # til alle kolonnar, så fiks dette manuelt
# kb_test$verdi = as.character(kb_test$verdi)
# kb_test$desimalar = as.integer(kb_test$desimalar)
#
# kb_er_gyldig(kb_test)
