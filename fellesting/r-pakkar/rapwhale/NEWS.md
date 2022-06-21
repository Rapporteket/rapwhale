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