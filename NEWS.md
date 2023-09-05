# rapwhale versjon 0.5.0

## Samandrag

rapwhale-pakken er no eit eige git-depot,
og er tilgjengeleg på [GitHub](https://github.com/Rapporteket/rapwhale).


## Endringar i utviklingsnvå

Desse funksjonane er nye:

- `aggreger_ki_rate()`:
  Funksjon for utrekning av kvalitetsindikatorar for ratar.
  Sjå hjelpesida for funksjonen eller oppdatert vignett
  «Kvalitetsindikatorfunksjonar» for detaljar.


## Nytt som krev endringar i *din* kode

- Du må ha dplyr versjon 1.1.1 eller nyare installert.
  Denne har fleire endringar som kanskje krev endringar i koden din.
  Sjå for eksempel tekst lenger nede om endringar i `*_join()`-funksjonane.
  Du kan lesa meir om alle endringane i dplyr-pakken her:
  https://cran.r-project.org/web/packages/dplyr/news/news.html#dplyr-1.1.1
- Funksjonane `regn_ki_univar()` og `regn_ki_bin()` har bytt namn
  til `regn_konfint_univar()` og `regn_konfint_univar()`.
  Dette er gjort for å unngå forveksling med funksjonar
  der `ki` tyder kvalitetsindikator, eksempelvis `aggreger_ki_snitt()`.
  Frå no av vil `ki` i funksjonsnamn alltid tyda kvalitetsindikator.
- Som nemnt i versjon 0.4.0 vert argumentet `tabell` i funksjonane
  `num()` og `prosent()` fjerna i denne versjonen.
  Sjå versjonsloggen for v0.4.0 nedanfor for detaljar om endringa.
- Brukar du `bygg_og_installer_pakker()`, må du no eksplisitt
  oppgje pakkemappene. Det følgjer med eit eksempel som viser
  korleis du lett kan søka opp mapper som inneheld R-pakkar,
  for å automatisera dette.


## Ny funksjonalitet

- `skaar_datasett()` har fått støtte for konstantledd.
  For å leggja til eit konstantledd for ein sumskår legg ein til ei rad i
  `skaaringstabell`.
  I rada skal `delskala` vera sumskåren konstantleddet skal gjelda for,
  `variabel` og `verdi` begge vera `NA`,
  og `koeffisient` vera verdien til konstantleddet.
- `regn_konfint_univar()` har fått eit nytt argument `konf_niva` for val av konfidensnivå.
- I `normaliser_varnamn()` vart variabelnamn utan bokstavar gjorde om til
  tomme tekstvektorar.
  Funksjonen krev no variabelnamn med minst éin bokstav,
  og gjev feilmelding elles.
- `normaliser_varnamn()` gjev no åtvaring viss utdata får to eller fleire like
  variabelnamn.
- `les_kb_mrs()` tolkar no `"None"` og `"Unknown"` som manglande verdiar.


## Feil retta opp

- `num()` hadde ein feil som gjorde at `num(NA)` ikkje vart til LaTeX-kommandoen
  `\textendash`,
  men teksten «textendash» i staden.
- `regn_ki_univar()` kalla ein funksjon `simpleboot::boot.ci()` som ikkje fanst
  i den pakken.
  Funksjonen brukar no pakken `boot` i staden for `simpleboot`,
  og bootstrapping funkar igjen.
- Funksjonen `tid_til_tidslinje()` hadde en feil som gjorde at punkter ble 
  plassert mellom eksisterende datapunkt, og ikke på riktig plassering innen 
  hvert aktuelle år. Ved ikke-etterfølgende årstall i inndata kunne dermed 
  punktene bli plassert i feil år. 
  Har også fjernet omkoding av klokkeslett for datovariabler med klokkeslett 
  hvor registrert tid var '00:00:00'. Disse vil nå behandles som korrekte 
  klokkeslett. 


## Forbetringar

Det sett opp automatisk køyring av testane i rapwhale-pakken på GitHub.
Dette vil gje endå betre sikring mot at nye feil vert innført,
og oppdateringar i andre pakkar som fører til endringar i rapwhale,
vil oppdagast raskare.

Alle kall av `*_join()`-funksjonane frå dplyr-pakken brukar no det nye
argumentet `relationship`,
som vart introdusert i dplyr versjon 1.1.1.
I dette argumentet spesifiserer ein kva type matching ein ventar
(éin-til-éin, mange-til-éin, éin-til-mange eller mange-til-mange).
Dette gjer koden sikrare ved at ein får feilmelding ved forsøk på
joining med uventa tal på treff per rad.
Denne endringa gjer at rapwhale krev versjon 1.1.1 eller nyare av dplyr-pakken.

Ein del funksjonar frå styler-pakken som er brukt i rapwhale,
som tidlegare var interne og vart kalla ved bruk av `:::`-operatoren,
er gjort eksterne.
Desse er tekne i bruk,
og vert kalla på vanleg måte.

Tekniske endringar for importerte og føreslegne pakkar:
- Lagt til manglande pakkar i bruk i funksjonar under `Imports`.
- Lagt til manglande pakkar i bruk vignettar og testar under `Suggests`.
- Fjerna pakkar som ikkje var i bruk frå `Imports`.
- Flytta nokre pakkar frå `Imports` til `Suggests`
  (pakkar som ikkje er nødvendige for funksjonaliteten til rapwhale,
  t.d. pakkar berre brukt i testar).

Funksjonalitet som har vorte utdatert (*deprecated*) i andre pakkar,
er oppdatert:

- `all_of()` eller `any_of()` i staden for ekstern vektor i tidyselect.
- `linewidth` i staden for `size` i linjer i ggplot2.
- `as.symbol()` i staden for filtrering på éin-kolonne-matriser i `filter()`.
- `append(after = 0)` i staden for `purrr::prepend()`.
- `if()`/`else()` i staden for `purrr::when()`.
- `paste0()` i staden for `stringr::str_c()` ved resirkulering av vektor med
  anna lengd enn 1.
- Anonyme funksjonar i staden for `...` i `dplyr::across()`.

Fleire funksjonar har fått:

- Utbetra dokumentasjon,
  blant anna for nokre argument som mangla dette i nokre funksjonar.
- Fleire og betre testar.
  Dette vil ikkje vera synleg for brukaren,
  men det sikrar at funksjonane alltid skal fungera slik dei er meint.
  Feil/manglar som vart avdekte av dei nye testane,
  er sjølvsagt retta opp.
- Køyrbar kode i eksempla (t.d. ved manglande lasting av pakke).
- Mindre feilrettingar og utbetringar.



# rapwhale versjon 0.4.0

## Samandrag

I denne versjonen er det både nye funksjonar,
forbetra funksjonar og feilretting.
Generelt er det betre kodekvalitet, og mange funksjonar
har fått betre dokumentasjon, eksempel, feilmeldingar og testar.

For rapportering finn du nye og endra funksjonar for formatering
av figurar og tal samt funksjonalitet for berekning av komplettheit.

Me har òg fått våre eigne `styler`-innstillingar
for automatisk finformatering av R-kode. Bruk desse!
Sjå vignetten *Bruk av styler og lintr* for meir informasjon.

Og alle funksjonane i pakken har no informasjon om utviklingsnivå.
Sjå vignetten *Utviklingsnivå for funksjonar i rapwhale-pakken*
for meir informasjon om korleis denne er meint å brukast.
Førebels er mange funksjonar merkte som *eksperimentelle*,
og ved vidare arbeid med pakken vil me prioritera
å høgna utviklingsnivå til funksjonar basert på *brukarbehov*.
Meld derfor frå om kva funksjonar du vil ha mest nytte av,
slik at me kan prioritera dei.

Elles er det fleire endringar som krev endringar i *din* kode.
Sjå eige punkt om det nedanfor.


## Endringar i utviklingsnvå

Desse funksjonane er nye:

- `boy_enhet()`:
Formater automatisk tall med tilhørende entalls-/flertallsform.
Funksjonen kan være nyttig om man for eksempel skal skrive en rapport,
og man ikke er sikker på hvor mange det er av en spesifikk enhet.
Da kan man for eksempel bruke
`boy_enhet(x, entall = "pasient", flertall = "pasienter")`
i stedet for å skrive `paste(x, "pasient(er)")`.
Funksjonen kan også ta forskjellige formateringsargument,
som kan gjøre tallvektoren om til den formen man måtte ønske,
for eksempel med et gitt antall desimaler.

- `erstatt_0()`:
Erstatt 0 med en valgfri verdi.
Funksjonen er sannsynligvis mest nyttig sammen med `boy_enhet()`,
hvor den er standardvalg for `formatering`-argumentet.

- `beregn_kompletthet_datasett()` og `beregn_kompletthet_datasett_med_ukjent()`:
Forenkler arbeidet med å beregne kompletthet for registerdata. 
`beregn_kompletthet_datasett()` beregner antall og andel rader som er 
eksplisitt `NA` i et datasett.
I enkelte tilfeller ønsker vi også å inkludere 
verdier som representerer en manglende besvarelse for en variabel. For eksempel
har enkelte variabler kodet egne verdier for «Ukjent», «Vet ikke» og lignende. 
`beregn_kompletthet_datasett_med_ukjent()` konverterer disse 
*ukjente* besvarelsene til eksplisitt `NA` og regner ut kompletthet med og uten 
denne konverteringen. 

- `erstatt_ukjent()` `beregn_kompletthet()`,
`beregn_kompletthet_med_ukjent()` og `erstatt_ukjent_for_datasett()`:
Hjelpefunksjoner som kan brukes hvis en kun ønsker å se på enkeltvariabler
eller å erstatte alle *ukjente* verdier med `NA`.
Funksjonene er foreløpig å anse som eksperimentelle.  

- `rapwhale_style()`: Fagsenteret sin kodestilguide til bruk med
[`styler`](https://github.com/r-lib/styler).

- `aktiver_kvalregtema()`: Oppdater tema,
fargar og andre figurinstillingar til fagsenteret sine standardinnstillingar.

- `assignment_eq_linter()`: Til bruk med
[`lintr`](https://github.com/r-lib/lintr) og sjekkar at `=` vert brukt ved
tilordning i staden for `<-`.

- `kompiler_rnw()`: Kompiler ei .Rnw-fil til ei .tex-fil.
Gjev beskjed om kompileringa gjekk bra og slettar eventuelt .tex-fila ved feil
med kompilering, slik at ein då unngår forsøk på kompilering vidare til .pdf.

- `kompiler_tex()`: Kompiler ei .tex-fil til ei .pdf-fil.
Funksjonen gjentek kompilering til alle kryssreferansar og slikt er i orden,
men maks `maksiter` gonger.
Gjev beskjed om status for kompilering,
og gjev òg ut loggen viss `vis_feilmeldingar = TRUE`.

- `lag_fig_soyle_prosent()`: Lik `lag_fig_soyle()`,
men der y-aksen vert vist som prosent.


Desse funksjonane har endra utviklingsnivå:

- `boy_enhet()`: ny funksjon, no `stable`
- `erstatt_0()`: ny funksjon, no `stable`
- Fleire nye offentlege/eksporterte funksjonar (sjå ovanfor)
  og interne funksjonar som har utviklingsnivå `experimental`.


## Nytt som krev endringar i *din* kode

- Fjerna temainstillingar frå `lag_fig_histogram()` og `lag_fig_soyle()`.
Desse instillingane får ein no ved å bruka `aktiver_kvalregtema()` eller
`ggplot2::theme_set(tema_kvalreg())`.

- `lag_fig_soyle()` lagar no som standard horisontale søyler.
For vertikale søyler bruk `flip = FALSE` (nytt argument, sjå info under).

- I vignetten for ekstern validering fungerte ikkje eksempla som
  brukte `slice_sample(..., n = Inf)` med (nokre) nyare versjonar av
  dplyr-pakken.
  Me har endra eksempla til heller å bruka argumentet `prop = 1`
  (som strengt tatt òg er meir logisk).
  Viss du brukar tilsvarande kode,
  bør du òg endra denne.
  Sjå relatert feilrapport for dplyr:
  https://github.com/tidyverse/dplyr/issues/6185
  
- LaTeX-klassen kvalreg brukar no pakken numprint i staden
  for siunitx for automatisk formatering av tal
  (med tusenskiljeteikn og norsk komma).
  Det er fleire fordelar med dette:
  Kompileringa går litt raskare,
  og me treng ikkje lenger spesialhandtera tal
  som skal visast i tabellar.
  
  R-funksjonen `num()` er oppdatert til å bruka numprint,
  så all *automatisk* talformatering skal fungera akkurat som før.
  Men argumentet `tabell` til funksjonen
  er merkt som utdatert («deprecated») og vert fjerna heilt
  i *neste* versjon av rapwhale-pakken.
  Fjern derfor dette argumentet i alle kall til `num()`.
  Det same gjeld `prosent()`-funksjonen.
  
  Viss du formaterer tal *manuelt* med `\num`-makroen i LaTeX
  (ikkje anbefalt),
  må du no bruka `\numprint` (og syntaksen for denne) i staden for.
  
- I LaTeX-klassen kvalreg har me no gått tilbake til å bruka
  polyglossia-pakken i staden for babel-pakken for norsk språkstøtte.
  Me måtte mellombels bruka babel på grunn av ein feil
  i luaotfload i TeX Live 2021
  (sjå https://tex.stackexchange.com/q/594485).
  
  I utgangspunktet treng du ikkje gjera nokon endringar i koden din,
  men viss du har begynt å bruka babel-spesifikke makroar,
  må du endra desse til tilsvarande polyglossia-makroar.
  Brukar du for eksempel `\foreignlanguage{british}{English text}`
  for å skriva engelsk tekst (med automatisk engelsk orddeling),
  må du endra dette til `\textenglish{English text}`.

- Hvis du bruker funksjonen `les_kb_mrs()` må du passe på at ekstern
  tabell for skjemakobling har riktig kolonnenavn. kolonnenavnet 
  `skjema_id_kodebok_ny` er endret til `skjema_id_kodebok`. 

## Ny funksjonalitet

- `lag_fig_soyle()` har fått eit nytt argument `flip` som avgjer om søylene
skal vera horisontale eller vertikale.
Standardverdi er `TRUE` som gjev vertikale søyler.

- `les_kb_mrs()` har fått støtte for datovariablar utan klokkeslett.

- `utviklingsnivaa()` gjev no ut info om funksjonane er interne eller eksterne.


## Feil retta opp

- Bruk av `\kode{}`-makroen frå LaTeX-klassen kvalreg vil
  no *aldri* gje orddeling etter _-teikn.

- LaTeX-dokument som brukar kvalreg-klassen,
  får no igjen vassmerket «UTKAST» i bakgrunnen
  ved kompilering i utkastmodus.
  (Dette var mellombels fjerna på grunn av ein feil i TeX Live 2021.)
  

## Forbetringar

Mange funksjonar har fått:

- Nye eller forbetra (og meir realistiske) eksempel.
- Forbetra dokumentasjon,
  som skal vera lettare å lesa og meir presis,
  med nøyaktig informasjon om kva inndata ein funksjon tek
  (eksempelvis lengd og type/klasse)
  og kva utdata han gjev ut
  (òg ved spesialtilfelle der inndataa er NA eller har lengd 0).
- Meir presise feilmeldingar, og med betre språk.
- Fleire og betre testar.
  Dette vil ikkje vera synleg for brukaren,
  men det sikrar at funksjonane alltid skal fungera slik dei er meint.
  Feil/manglar som vart avdekte av dei nye testane,
  er sjølvsagt retta opp.
  


# rapwhale versjon 0.3.3 (og tidlegare)

Me har ikkje skrive endringslogg for rapwhale versjon 0.3.3
eller tidlegare versjonar.
Sjå derfor Git-loggen for detaljert informasjon
om alle tidlegare endringar.
Versjon 0.3.3 vart offisielt utgjeven 20. oktober 2021.
Alle endringar i versjon 0.4.0 og seinare
er altså gjorde *etter* denne datoen.
