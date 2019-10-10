
library(tidyverse)
library(testthat)

# Aggregeringsfunksjoner:

# ----Aggregering - Beregn andel:
context("Sjekker at inndata er på forventet format")

# Funksjonen skal ta inn et datasett på 101-format.
test_that("Inndata-format er etter 101-metodikken", {
  d = lag_d(1000)
  d = d %>%
    select(-ki_krit_nevner)
  d2 = d %>%
    select(-ki_krit_teller)
  expect_error(aggreger_ki_prop(d))
  expect_error(aggreger_ki_prop(d2))
  expect_error(aggreger_ki_prop(d, gruppering = "gruppe"))
  expect_error(aggreger_ki_prop(d2, gruppering = "gruppe"))
})

# Kriterier nevner og teller kan ikke inneholde andre verdier enn 1 og 0, og NA.
test_that("kriterier nevner og teller kun inneholder verdiene 1 og 0", {
  n = 1000
  d = lag_d(n)
  d2 = d3 = d4 = d
  ekstra_tall = sample(seq(0, 1, 1), size = n, replace = TRUE)
  d$ki_krit_nevner = d$ki_krit_nevner + ekstra_tall
  d2$ki_krit_teller = d2$ki_krit_teller - ekstra_tall
  d3$ki_krit_teller[151] = "c"
  expect_error(aggreger_ki_prop(d), "kriterie variablene inneholder ugyldige verdier")
  expect_error(aggreger_ki_prop(d2), "kriterie variablene inneholder ugyldige verdier")
  expect_error(aggreger_ki_prop(d3), "kriterie variablene inneholder ugyldige verdier")
  expect_error(aggreger_ki_prop(d, gruppering = "gruppe"), "kriterie variablene inneholder ugyldige verdier")
  expect_error(aggreger_ki_prop(d2, gruppering = "gruppe"), "kriterie variablene inneholder ugyldige verdier")
  expect_error(aggreger_ki_prop(d3, gruppering = "gruppe"), "kriterie variablene inneholder ugyldige verdier")
})

# Hvis nevner er 0 kan ikke teller være 1 for samme obs, da må teller være 0 eller NA.
test_that("teller ikke kan være 1 hvis nevner er 0", {
  n = 1000
  d = lag_d(n)
  d$ki_krit_nevner[156] = 0
  d$ki_krit_teller[156] = 1

  expect_error(aggreger_ki_prop(d))
  expect_error(aggreger_ki_prop(d, gruppering = "gruppe"))
})


context("Sjekker grensetilfeller i inndata")

# Funksjonen må tillate tilfeller hvor sum teller_krit er 0.
test_that("Funksjonen tillater tilfeller hvor ingen observasjoner oppfyller kriteriet for teller", {
  n = 1000
  d = lag_d(n)
  d$ki_krit_teller = c(rep.int(0, times = n))
  expect_equal(aggreger_ki_prop(d), tibble(est = 0, konfint_nedre = 0, konfint_ovre = 0, ki_teller = 0, ki_nevner = sum(d$ki_krit_nevner)))
})


# Funksjonen må ha en løsning for tilfeller hvor sum nevner_krit er 0. (Returnere 0 for NaN-verdier)
test_that("Funksjonen returnerer 0 og ikke NaN i de tilfellene hvor ingen observasjoner oppfyller kriterier for nevner", {
  n = 1000
  d = lag_d(n)
  d$ki_krit_nevner = c(rep.int(0, times = n))
  d$ki_krit_teller = c(rep.int(0, times = n))
  expect_equal(aggreger_ki_prop(d), tibble(est = 0, konfint_nedre = 0, konfint_ovre = 0, ki_teller = 0, ki_nevner = 0))
})

context("Sjekker at utdata er innenfor forventede verdier")

# Funksjonen kan kun gi ut en verdi for estimat som er mellom 0 og 1, inklusive endepunkt.
# Sjekker 100% dekning.
test_that("Funksjonen returnerer godkjent verdi når alle observasjoner oppfyller begge kriterier", {
  n = 1000
  d = lag_d(n)
  d$ki_krit_nevner = c(rep.int(1, times = n))
  d$ki_krit_teller = c(rep.int(1, times = n))
  expect_equal(aggreger_ki_prop(d), tibble(est = 1, konfint_nedre = 1, konfint_ovre = 1, ki_teller = n, ki_nevner = n))
})

# Hvordan skal funksjonen håndtere missing i grupperingsvariabel?
test_that("Funksjonen gir en advarsel når det finnes NA-verdier i grupperingsvariabel", {
  n = 1000
  d = lag_d(n)
  d$gruppe[1] = NA
  expect_warning(aggreger_ki_prop(d, gruppering = "gruppe"))
})

# Hvordan skal funksjonen håndtere tilfeller hvor ingen er i en av gruppene?
# Vil her at vi får ut en oppføring for *HVER* gruppe, inkludert de som ikke har noen observasjoner
# De må da bli 0 for alle variabler.
# Grupper som ikke har noen verdier kan eventuelt filtreres ut i etterkant hvis de ikke skal være med i figurer, etc.
test_that("Funksjonen returnerer verdier for alle grupper i inndata, selv de gruppene som ikke inneholder observasjoner", {
  n = 1000
  d = lag_d(n)
  expect_equal(nrow(aggreger_ki_prop(d, gruppering = "gruppe")), length(levels(d$gruppe)))
})


# Test datasett:
# https://stackoverflow.com/questions/42734547/generating-random-strings
