# rapwhale

<!-- badges: start -->
[![R-CMD-check](https://github.com/Rapporteket/rapwhale/actions/workflows/check-standard.yaml/badge.svg)](https://github.com/Rapporteket/rapwhale/actions/workflows/check-standard.yaml)
[![Kodestil](https://github.com/Rapporteket/rapwhale/actions/workflows/style.yaml/badge.svg)](https://github.com/Rapporteket/rapwhale/actions/workflows/style.yaml)
[![Testdekning](https://codecov.io/gh/Rapporteket/rapwhale/branch/main/graph/badge.svg)](https://app.codecov.io/gh/Rapporteket/rapwhale)
[![Lisens: GPL v3](https://img.shields.io/badge/lisens-GPL--3-blue.svg)](LICENSE)
[![Dokumentasjon](https://github.com/Rapporteket/rapwhale/actions/workflows/pkgdown.yaml/badge.svg)](https://github.com/Rapporteket/rapwhale/actions/workflows/pkgdown.yaml)
<!-- badges: end -->

**Ymse verktøy for arbeid med kvalitetsregisterdata.**

rapwhale samlar rutinane som ein elles endar opp med å skriva på nytt
for kvart register og kvar årsrapport:
innlesing av datadumpar og kodebøker frå dei ulike innregistreringsløysingane,
kontroll av at dataa faktisk stemmer med kodeboka,
utrekning av kvalitetsindikatorar med konfidensintervall,
skåring av spørjeskjema, måling av komplettheit
og laging av figurar og tabellar i ein einsarta stil.

## Kvifor bruka pakken

- **Éi kodebokform for mange system.**
  Kvalitetsregistera i Noreg køyrer på ulike innregistreringsløysingar
  (i hovudsak OQR og MRS),
  og kvar av dei har sitt eige kodebok- og eksportformat.
  rapwhale les alle inn til éi standardisert kodebokform,
  slik at den vidare analysekoden ikkje treng vita
  kvar dataa opphavleg kjem frå.
- **Validering i staden for overraskingar.**
  Du kan sjekka om ei kodebok er internt konsistent,
  og om ein datadump stemmer med kodeboka si:
  variabeltypar, tillatne kategoriar, verdiområde, rimelege grenser,
  obligatoriske felt og unike verdiar.
- **Kvalitetsindikatorar rekna ut på same måten kvar gong.**
  Ved utrekning av kvalitetsindikatorar er det mange fallgruver å gå i.
  Indikatorane vert difor rekna ut frå eit fast inndataformat
  med eit lite sett funksjonar,
  og du får konfidensintervall på kjøpet.
- **Reproduserbare rapportar.**
  Felles fargar, eit felles tema for ggplot2 og qicharts2,
  ferdige figur- og LaTeX-tabellfunksjonar
  og formatering av tal til bruk i rapporttekst.

## Installasjon

rapwhale ligg ikkje på CRAN.
Du installerer utviklingsversjonen frå GitHub:

```r
# install.packages("remotes")
remotes::install_github("Rapporteket/rapwhale")
```

Viss du òg vil ha vignettane installerte:

```r
remotes::install_github("Rapporteket/rapwhale", build_vignettes = TRUE)
```

Du treng R versjon 4.4 eller nyare.

## Kom i gang

Kvalitetsindikatorar vert rekna ut frå eit datasett
med éi rad per eining (typisk per pasient eller per operasjon)
og logiske kolonnar som seier om rada høyrer med i nemnaren,
og om ho oppfyller kriteriet for teljaren:

```r
library(dplyr)
library(rapwhale)

d = tibble(
  pasid = 1:8,
  sykehus = rep(c("Haukeland", "Førde", "Voss"), times = c(3, 2, 3)),
  ki_krit_teller = c(TRUE, FALSE, TRUE, TRUE, FALSE, TRUE, TRUE, FALSE),
  ki_krit_nevner = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE)
)

# Indikatoren for heile datasettet, med 95 %-konfidensintervall
aggreger_ki_prop(d)

# Eller rekna ut på sjukehusnivå
d |>
  group_by(sykehus) |>
  aggreger_ki_prop()
```

## Kva pakken inneheld

| Område | Funksjonar |
| --- | --- |
| Kodebøker | `les_kb_oqr()`, `les_kb_mrs()`, `les_kb_checkware()`, `kb_oqr_base_til_std()`, `kb_til_kanonisk_form()`, `kb_fyll()`, `kb_er_gyldig()` |
| Datadumpar | `les_dd_oqr()`, `les_dd_mrs()`, `les_dd_checkware()`, `les_csv_oqr()`, `les_varnavn()`, `dd_er_gyldig()` |
| Kvalitetsindikatorar | `aggreger_ki_prop()` (andelar), `aggreger_ki_rate()` (rater), `aggreger_ki_snitt()` (gjennomsnitt), `regn_konfint_bin()`, `regn_konfint_univar()` |
| Ekstern validering | `lag_valideringsdatasett()`, `er_valideringsdatasett_gyldig()`, `analyser_valideringsdatasett()`, `samanlikn_identisk()` |
| Skåring av spørjeskjema | `skaar_datasett()` (vilkårleg skåringstabell), `skaar_rand12()` (RAND-12/SF-12) |
| Kompletthet | `beregn_kompletthet()`, `beregn_kompletthet_datasett()`, `erstatt_ukjent()` og `*_med_ukjent()`-variantane |
| Figurar og tabellar | `aktiver_kvalregtema()`, `tema_kvalreg()`, `farger_kvalreg()`, `lag_fig_soyle()`, `lag_fig_linje()`, `lag_fig_histogram()`, `lag_fig_shewhart()`, `lag_tab_latex()` |
| Rapporttekst og kompilering | `prosent()`, `num()`, `tab()`, `boy_enhet()`, `kjed_ord()`, `kompiler_rnw()`, `kompiler_tex()`, `kopier_latex_klassefil()` |
| Fødselsnummer og anonymisering | `anonymiser()`, `lag_anonymiseringsfunksjon()`, `fnr_er_gyldig()`, `finn_type_idnummer()`, `fnr_foresla()` |
| Kodestil | `rapwhale_style()` til bruk med styler, og eigne lintr-linterar |

Ei full oversikt over funksjonane får du i R med `help(package = "rapwhale")`.

## Dokumentasjon

Kvar funksjon har eit «skilt» på hjelpesida —
**stable**, **maturing** eller **experimental** —
som seier kva utviklingsnivå han er på,
altså kor mykje du kan lita på at han vert verande som han er.
Bruk helst stabile funksjonar i produksjonskode.
`utviklingsnivaa()` gjev ei oversikt over skilta i heile pakken.

Lengre bruksrettleiingar ligg som vignettar:

- [Kvalitetsindikatorfunksjonar](https://rapporteket.github.io/rapwhale/articles/ki-funksjonar.html) —
  rammeverket for utrekning av kvalitetsindikatorar, med gjennomgåtte eksempel
- [Ekstern validering](https://rapporteket.github.io/rapwhale/articles/ekstern-validering.html) —
  samanlikning av registerdata med ei ekstern datakjelde
- [Bruksdokumentasjon for skåring av RAND-12/SF-12](https://rapporteket.github.io/rapwhale/articles/dokumentasjon-rand-12.html)
- [Utviklingsnivå for funksjonar](https://rapporteket.github.io/rapwhale/articles/utviklingsniva.html) —
  kva skilta på hjelpesidene tyder
- [Bruk av styler og lintr](https://rapporteket.github.io/rapwhale/articles/bruk-av-styler-og-linter.html) —
  den felles kodestilen vår, og korleis du fylgjer han

Dei kan lesast i R med for eksempel
`vignette("ki-funksjonar", package = "rapwhale")`.

Endringar mellom versjonane,
inkludert dei som krev endringar i din eigen kode,
er lista i [endringsloggen](https://rapporteket.github.io/rapwhale/news/index.html).

## Hjelp og spørsmål

Opprett ei [sak](https://github.com/Rapporteket/rapwhale/issues)
for feil, spørsmål og ynskje om ny funksjonalitet.
Eit minimalt og reproduserbart eksempel
gjer det mykje lettare å gjera noko med ein feilrapport.
Merk at saker aldri må innehalda pasientdata —
lag konstruerte eksempeldata i staden.

## Bidra

Bidrag er velkomne.
Før du opnar ein pull request:

- Formater koden med `styler::style_pkg(style = rapwhale_style)`
  og sjekk han med `lintr::lint_package()`.
  Sjå vignetten om styler og lintr for detaljar.
- Skriv `testthat`-testar for ny funksjonalitet,
  og sjå til at `devtools::test()` går gjennom.
- Dokumenter eksporterte funksjonar med roxygen2,
  og gjev dei eit utviklingsnivå.
- Legg til eit punkt i `NEWS.md` dersom endringa har noko å seia for brukarane.

Kvar push køyrer `R CMD check`, linting, kodestilsjekk og testdekning
via GitHub Actions,
og alt dette må vera grønt før ein pull request kan flettast inn.
