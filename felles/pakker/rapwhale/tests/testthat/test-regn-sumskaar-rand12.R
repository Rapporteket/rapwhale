# Testing av skåringsfunksjon for RAND-12

# Eksempel på inndata (inneholder besvarelser som gir max- og min
# sumskår + tilfeldige besvarelser)
d_inn_eks = tibble::tribble(
  ~rand_1, ~rand_2a, ~rand_2b, ~rand_3a, ~rand_3b, ~rand_4a, ~rand_4b, ~rand_5, ~rand_6a, ~rand_6b, ~rand_6c, ~rand_7,
  1, 3, 3, 2, 2, 2, 2, 1, 1, 1, 6, 5, # max PCS12
  5, 1, 1, 1, 1, 1, 1, 5, 6, 6, 1, 2, # min PCS12
  1, 1, 1, 2, 2, 2, 2, 1, 1, 1, 6, 5, # max MCS12
  5, 3, 3, 1, 1, 1, 1, 5, 6, 6, 1, 2, # min MCS12
  5, 1, 1, 1, 1, 1, 1, 5, 6, 6, 1, 1, # besvarelse med dårligst mulig helse på alle enkeltspørsmål
  5, 1, 3, 2, 1, 2, 2, 1, 6, 6, 1, 5, # tilfeldig
  1, 3, 3, 2, 2, 1, 1, 5, 2, 4, 1, 3, # tilfeldig
  3, 2, 1, 1, 2, 1, 2, 3, 3, 5, 2, 1, # tilfeldig
  5, 1, 2, 1, 1, 2, 2, 4, 3, 5, 5, 4, # tilfeldig
  2, 3, 1, NA, 2, 2, 2, 1, 2, 1, 1, 2 # tilfeldig med NA
)

# Sumskårer for d_inn_eks (regnet ut ved hjelp av SPSS-syntaksen)
d_sumskaarer_oblique = tibble::tribble(
  ~rand12_pcs, ~rand12_mcs,
  62.37966, 65.38813,
  17.67266, 10.81346,
  55.48371, 65.73092,
  24.56861, 10.47067,
  18.39445, 11.48746,
  39.46188, 25.88550,
  43.46585, 28.85581,
  37.3071, 31.57685,
  27.95627, 41.86409,
  NA, NA
)

# Kombinerer d_inn_eks og d_sumskaarer_oblique
d_inn_inkl_sumskaarer = tibble::tibble(d_inn_eks, d_sumskaarer_oblique)

test_that("skaar_rand12() gir ut det originale datasettet inkludert
          riktig utregnede sumskårer hvis sumskårer ikke finnes fra før", {
  expect_equal(
    skaar_rand12(d_inn_eks, godta_manglende = TRUE),
    d_inn_inkl_sumskaarer
  )
})

test_that("skaar_rand12() gir ut det originale datasettet inkludert
          riktig utregnede sumskårer hvis sumskårer finnes fra før", {
  d_inn_inkl_feil_sumskaarer = d_inn_inkl_sumskaarer
  d_inn_inkl_feil_sumskaarer$rand12_pcs = 1

  expect_equal(
    suppressWarnings(skaar_rand12(d_inn_inkl_feil_sumskaarer,
      godta_manglende = TRUE
    )),
    d_inn_inkl_sumskaarer
  )
})

test_that("skaar_rand12() fungerer hvis man oppgir variabelnavn", {
  d_inn_eks_feil_varnavn = d_inn_eks
  d_inn_eks_feil_varnavn = rename(d_inn_eks_feil_varnavn,
    g_h_1 = rand_1,
    r_e_3 = rand_4b
  )

  d_inn_eks_feil_varnavn_inkl_sumskaarer = tibble::tibble(
    d_inn_eks_feil_varnavn,
    d_sumskaarer_oblique
  )

  expect_equal(
    skaar_rand12(d_inn_eks_feil_varnavn,
      variabelnavn = c(
        rand_1 = "g_h_1",
        rand_4b = "r_e_3"
      ),
      godta_manglende = TRUE
    ),
    d_inn_eks_feil_varnavn_inkl_sumskaarer
  )
})

test_that("skaar_rand12() gir feilmelding ved manglende verdier hvis
          godta_manglende = FALSE", {
  expect_error(skaar_rand12(d_inn_eks, godta_manglende = FALSE))
})
