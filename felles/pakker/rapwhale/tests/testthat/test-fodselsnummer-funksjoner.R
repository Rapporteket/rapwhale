context("fnr_er_gyldig_v2()")

nummer = c(
  "15076500565", # Gyldig F-nummer
  "70019950032", # Gyldig D-nummer
  "01410199935", # Gyldig H-nummer
  "88888888831", # Gyldig FH-nummer
  "98019800546"
) # Ugyldig nummer generelt

test_that("fnr_er_gyldig_v2() gjev rette verdiar når «gyldige_typar» er sett til éin verdi", {
  expect_identical(
    fnr_er_gyldig_v2(nummer, gyldige_typar = "FNR"),
    er_gyldig_f_nummer(nummer)
  )
  expect_identical(
    fnr_er_gyldig_v2(nummer, gyldige_typar = "D"),
    er_gyldig_d_nummer(nummer)
  )
  expect_identical(
    fnr_er_gyldig_v2(nummer, gyldige_typar = "H"),
    er_gyldig_h_nummer(nummer)
  )
  expect_identical(
    fnr_er_gyldig_v2(nummer, gyldige_typar = "FH"),
    er_gyldig_fh_nummer(nummer)
  )
})

test_that("fnr_er_gyldig_v2() gjev rette verdiar for kombinasjonar av «gyldige_typar»", {
  expect_identical(
    fnr_er_gyldig_v2(nummer, gyldige_typar = c("FNR", "D")),
    er_gyldig_f_nummer(nummer) | er_gyldig_d_nummer(nummer)
  )
  expect_identical(
    fnr_er_gyldig_v2(nummer, gyldige_typar = c("D", "H")),
    er_gyldig_d_nummer(nummer) | er_gyldig_h_nummer(nummer)
  )
  expect_identical(
    fnr_er_gyldig_v2(nummer, gyldige_typar = c("H", "FH")),
    er_gyldig_h_nummer(nummer) | er_gyldig_fh_nummer(nummer)
  )
  expect_identical(
    fnr_er_gyldig_v2(nummer),
    er_gyldig_f_nummer(nummer) | er_gyldig_d_nummer(nummer) |
      er_gyldig_h_nummer(nummer) | er_gyldig_fh_nummer(nummer)
  )
})

test_that("er_gyldig_f_nummer() fungerer òg med vektorar av lengd 0", {
  expect_identical(er_gyldig_fnr_dato(character()), logical())
})

test_that("fnr_er_gyldig_v2() reknar alle inndata som ugyldige dersom «gyldige_typar» er tom", {
  alle_ugyldige = rep(FALSE, length(nummer))
  expect_identical(fnr_er_gyldig_v2(nummer, gyldige_typar = character()), alle_ugyldige)
})

test_that("fnr_er_gyldig_v2() gjev feilmelding viss inn-nummera ikkje er av typen tekst", {
  expect_error(fnr_er_gyldig_v2(as.numeric(nummer)))
})

test_that("fnr_er_gyldig_v2() gjev feilmelding viss ein oppgjev ukjende «gyldige_typar»", {
  expect_error(fnr_er_gyldig_v2(nummer, gyldige_typar = c("foo", "FNR")))
})



context("er_syntaktisk_fnr()")

test_that("er_syntaktisk_fnr() gjev forventa resultat", {
  nummer = c(
    "123456789", "123456789ab", "12345612345", "123456123456",
    "abcdefghijk"
  )
  forventa = c(FALSE, FALSE, TRUE, FALSE, FALSE)
  expect_identical(er_syntaktisk_fnr(nummer), forventa)
})



context("er_gyldig_fnr_dato()")

test_that("er_gyldig_fnr_dato() gjev forventa resultat", {
  datoar = c(
    "010101", "170527", "311299",
    "290200", # Skotårsdag, gyldig i år 2000, men ikkje i 1900 ...
    "320101", "011382", "999999",
    "290225", # «Skotårsdag» som aldri er gyldig ...
    "241224"
  )
  dato_gyldig = c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, TRUE)
  expect_identical(er_gyldig_fnr_dato(datoar), dato_gyldig)
})

test_that("er_gyldig_f_nummer() fungerer òg med vektorar av lengd 1", {
  expect_identical(er_gyldig_fnr_dato("010101"), TRUE)
  expect_identical(er_gyldig_fnr_dato("320101"), FALSE)
})

test_that("er_gyldig_f_nummer() fungerer òg med vektorar av lengd 0", {
  expect_identical(er_gyldig_fnr_dato(character()), logical())
})




context("er_gyldig_f_nummer()")

# Dei gyldige fødselsnummera o.l. er ikkje reelle,
# men henta frå dokumentet «Testaktører»
# versjon 3.7, datert 19.11.2013.

test_that("er_gyldig_f_nummer() gjev forventa resultat", {
  fnr_gyldige = c(
    "15076500565", "21016400952",
    "12057900499", "13116900216",
    "14019800513", "05073500186"
  )
  fnr_ugyldige = c(
    "15076500561", "21016400992",
    "13057900499", "13126900216",
    "98019800547"
  ) # Siste nummer har gyldige sjekksiffer
  expect_identical(
    er_gyldig_f_nummer(fnr_gyldige),
    rep(TRUE, length(fnr_gyldige))
  )
  expect_identical(
    er_gyldig_f_nummer(fnr_ugyldige),
    rep(FALSE, length(fnr_ugyldige))
  )
})

test_that("er_gyldig_f_nummer() fungerer òg med vektorar av lengd 1", {
  expect_identical(er_gyldig_f_nummer("15076500565"), TRUE)
  expect_identical(er_gyldig_f_nummer("15076500561"), FALSE)
})

test_that("er_gyldig_f_nummer() fungerer òg med vektorar av lengd 0", {
  expect_identical(er_gyldig_f_nummer(character()), logical())
})



context("er_gyldig_d_nummer()")

test_that("er_gyldig_d_nummer() gjev forventa resultat", {
  dnr_gyldige = c(
    "70019950032", # Henta frå testaktørar, resten er laga manuelt
    "41010199946", "51010199917",
    "61010199998", "71010199969"
  )
  dnr_ugyldige = c(
    "70019950033",
    "15076500565", # Gyldig F-nummer, men ugyldig som D-nummer
    "81030399951",
    "98019800547"
  )
  expect_identical(
    er_gyldig_d_nummer(dnr_gyldige),
    rep(TRUE, length(dnr_gyldige))
  )
  expect_identical(
    er_gyldig_d_nummer(dnr_ugyldige),
    rep(FALSE, length(dnr_ugyldige))
  )
})

test_that("er_gyldig_d_nummer() fungerer òg med vektorar av lengd 1", {
  expect_identical(er_gyldig_d_nummer("70019950032"), TRUE)
  expect_identical(er_gyldig_d_nummer("70019950033"), FALSE)
})

test_that("er_gyldig_d_nummer() fungerer òg med vektorar av lengd 0", {
  expect_identical(er_gyldig_d_nummer(character()), logical())
})



context("er_gyldig_h_nummer()")

test_that("er_gyldig_h_nummer() gjev forventa resultat", {
  hnr_gyldige = c("01410199935", "01510199983")
  hnr_ugyldige = c(
    "01410199936", "01410199945",
    "15076500565", # Gyldig F-nummer, men ugyldig som H-nummer
    "41010199946", # Gyldig D-nummer, men ugyldig som H-nummer
    "98019800547"
  )
  expect_identical(
    er_gyldig_h_nummer(hnr_gyldige),
    rep(TRUE, length(hnr_gyldige))
  )
  expect_identical(
    er_gyldig_h_nummer(hnr_ugyldige),
    rep(FALSE, length(hnr_ugyldige))
  )
})

test_that("er_gyldig_h_nummer() fungerer òg med vektorar av lengd 1", {
  expect_identical(er_gyldig_h_nummer("01410199935"), TRUE)
  expect_identical(er_gyldig_h_nummer("01410199936"), FALSE)
})

test_that("er_gyldig_h_nummer() fungerer òg med vektorar av lengd 0", {
  expect_identical(er_gyldig_h_nummer(character()), logical())
})



context("er_gyldig_fh_nummer()")

test_that("er_gyldig_fh_nummer() gjev forventa resultat", {
  fhnr_gyldige = c("88888888831", "99999999928", "80102030404")
  fhnr_ugyldige = c(
    "88888888832", "88888888841",
    "77777777745",
    "15076500565", # Gyldig F-nummer, men ugyldig som FH-nummer
    "41010199946", # Gyldig D-nummer, men ugyldig som FH-nummer
    "01410199935", # Gyldig H-nummer, men ugyldig som FH-nummer
    "98019800546"
  )
  expect_identical(
    er_gyldig_fh_nummer(fhnr_gyldige),
    rep(TRUE, length(fhnr_gyldige))
  )
  expect_identical(
    er_gyldig_fh_nummer(fhnr_ugyldige),
    rep(FALSE, length(fhnr_ugyldige))
  )
})

test_that("er_gyldig_fh_nummer() fungerer òg med vektorar av lengd 1", {
  expect_identical(er_gyldig_fh_nummer("88888888831"), TRUE)
  expect_identical(er_gyldig_fh_nummer("88888888832"), FALSE)
})

test_that("er_gyldig_fh_nummer() fungerer òg med vektorar av lengd 0", {
  expect_identical(er_gyldig_fh_nummer(character()), logical())
})



context("er_fnr_sjekksum_korrekt()")

test_that("er_fnr_sjekksum_korrekt() gjev forventa resultat", {
  nummer_gyldige = c("12345678911", "15076500565")
  nummer_ugyldige = c("12345678922", "15076500511")
  expect_identical(
    er_fnr_sjekksum_korrekt(nummer_gyldige),
    rep(TRUE, length(nummer_gyldige))
  )
  expect_identical(
    er_fnr_sjekksum_korrekt(nummer_ugyldige),
    rep(FALSE, length(nummer_ugyldige))
  )
})
