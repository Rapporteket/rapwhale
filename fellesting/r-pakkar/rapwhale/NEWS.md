# Rapwhale versjon 0.4.0

## Nye funksjoner

- Ny funksjon,
`boy_enhet()`,
som automatisk formaterer tall med tilhørende entalls-/flertallsform.
Funksjonen kan være nyttig om man for eksempel skal skrive en rapport,
og man ikke er sikker på hvor mange det er av en spesifikk enhet.
Da kan man for eksempel bruke
`boy_enhet(x, entall = "pasient", flertall = "pasienter")`
i stedet for å skrive `paste(x, "pasient(er)")`.
Funksjonen kan også ta forskjellige formaterings-argument,
som kan gjøre tallvektoren om til den formen man måtte ønske,
for eksempel med et gitt antall desimaler.

- Ny funksjon,
`erstatt_0()`,
som erstatter 0 med en valgfri verdi.
Funksjonen er sannsynligvis mest nyttig sammen med `boy_enhet()`,
hvor den er standardvalg for `formatering`-argumentet.

- `rapwhale_style()`: Fagsenteret sin kodestilguide til bruk med
[`styler`](https://github.com/r-lib/styler).

- `aktiver_kvalregtema()`: Funksjon som oppdaterer tema,
fargar og andre figurinstillingar til fagsenteret sine standardinnstillingar.

- `assignment_eq_linter()`: Funksjon til bruk med
[`lintr`](https://github.com/r-lib/lintr) som sjekkar at `=` vert brukt ved
tilordning i staden for `<-`.

- `kompiler_rnw()`: Funksjon som kompilerer ei .Rnw-fil til ei .tex-fil.
Gjev beskjed om kompileringa gjekk bra og slettar eventuelt .tex-fila ved feil
med kompilering slik at ein då unngår forsøk på kompilering vidare til .pdf.

- `kompiler_tex()`: Funksjon som kompilerer ei .tex-fil til ei .pdf-fil.
Funksjonen gjentek kompilering til alle kryssreferansar og slikt er i orden,
men maks `maksiter` gonger.
Gjev beskjed om status for kompilering,
og gjev òg ut loggen viss `vis_feilmeldingar = TRUE`.

- `lag_fig_soyle_prosent()`: Lik `lag_fig_soyle()`,
men der y-aksen vert vist som prosent.

## Nytt som krev endringar i *din* kode

- Fjerna temainstillingar frå `lag_fig_histogram()` og `lag_fig_soyle()`.
Desse instillingane får ein no ved å bruka `aktiver_kvalregtema()` eller
`ggplot2::theme_set(tema_kvalreg())`.

- `lag_fig_soyle()` lagar no som standard horisontale søyler.
For vertikale søyler bruk `flip = FALSE` (nytt argument, sjå info under).

## Ny funksjonalitet

- `lag_fig_soyle()` har fått eit nytt argument `flip` som avgjer om søylene
skal vera horisontale eller vertikale.
Standardverdi er `TRUE` som gjev vertikale søyler.

- `les_kb_mrs()` har fått støtte for datovariablar utan klokkeslett.

- `utviklingsnivaa()` gjev no ut info om funksjonane er interne eller eksterne.

## Generelle utbetringar

Mange funksjonar har fått:

- Nye eller utbetra eksempel.
- Utbetra dokumentasjon.


# Rapwhale versjon 0.3.3 (og tidlegare)

Me har ikkje skrive endringslogg for rapwhale versjon 0.3.3
eller tidlegare versjonar.
Sjå derfor Git-loggen for detaljert informasjon
om alle tidlegare endringar.
Versjon 0.3.3 vart offisielt utgjeven 20. oktober 2021.
Alle endringar i versjon 0.4.0 og seinare
er altså gjorde *etter* denne datoen.
