feilmelding = "Skriv FIXME med store bokstavar"

test_that("fixme_stil_linter() godtek FIXME med store bokstavar", {
  lintr::expect_no_lint("x = 1 # FIXME Rett dette", fixme_stil_linter())
})

test_that("fixme_stil_linter() godtek kommentarar utan FIXME-markør", {
  lintr::expect_no_lint(
    content = "x = 1 # Ein heilt vanleg kommentar",
    linters = fixme_stil_linter()
  )
  lintr::expect_no_lint("x = 1", fixme_stil_linter())
})

test_that("fixme_stil_linter() melder frå om små bokstavar", {
  lintr::expect_lint(
    content = "x = 1 # fixme: rett dette",
    checks = feilmelding,
    linters = fixme_stil_linter()
  )
  lintr::expect_lint(
    content = "x = 1 # Fixme: rett dette",
    checks = feilmelding,
    linters = fixme_stil_linter()
  )
  lintr::expect_lint(
    content = "x = 1 # FixMe rett dette",
    checks = feilmelding,
    linters = fixme_stil_linter()
  )
})

test_that("fixme_stil_linter() melder frå om markørar med utropsteikn", {
  lintr::expect_lint(
    content = "x = 1 # fixme! rett dette",
    checks = feilmelding,
    linters = fixme_stil_linter()
  )
  lintr::expect_lint(
    content = "x = 1 # !fixme rett dette",
    checks = feilmelding,
    linters = fixme_stil_linter()
  )
})

test_that("fixme_stil_linter() ser markørar midt i kommentaren", {
  lintr::expect_lint(
    content = "x = 1 # Bokstavane read_csv() brukar (fixme: utvid med fleire)",
    checks = feilmelding,
    linters = fixme_stil_linter()
  )
  lintr::expect_no_lint(
    content = "x = 1 # Bokstavane read_csv() brukar (FIXME: utvid med fleire)",
    linters = fixme_stil_linter()
  )
})

test_that("fixme_stil_linter() handterer bøygde former", {
  lintr::expect_lint(
    content = "x = 1 # Rett då òg fixme-en over",
    checks = feilmelding,
    linters = fixme_stil_linter()
  )
  lintr::expect_no_lint(
    content = "x = 1 # Rett då òg FIXME-en over",
    linters = fixme_stil_linter()
  )
})

# Ord som berre inneheld «fixme» skal ikkje gje feil
test_that("fixme_stil_linter() krev ordgrenser", {
  lintr::expect_no_lint(
    content = "x = 1 # Ein prefixmestring skal gå fint",
    linters = fixme_stil_linter()
  )
  lintr::expect_no_lint(
    content = "x = 1 # Ordet suffixme er heller ikkje ein markør",
    linters = fixme_stil_linter()
  )
})

# Norske bokstavar inntil markøren skal ikkje gje treff.
# Testen føreset eit UTF-8-lokale, slik resten av testpakka òg gjer:
# utan det skriv parsaren om «å» til <U+00E5>, og «>» lagar ei falsk
# ordgrense framfor markøren.
test_that("fixme_stil_linter() reknar norske bokstavar som ordteikn", {
  lintr::expect_no_lint(
    content = "x = 1 # åfixme er ikkje ein markør",
    linters = fixme_stil_linter()
  )
  lintr::expect_no_lint(
    content = "x = 1 # fixmeø er ikkje ein markør",
    linters = fixme_stil_linter()
  )
  lintr::expect_no_lint(
    content = "x = 1 # \u00e5fixme er ikkje ein mark\u00f8r",
    linters = fixme_stil_linter()
  )
  lintr::expect_no_lint(
    content = "x = 1 # fixme\u00f8 er ikkje ein mark\u00f8r",
    linters = fixme_stil_linter()
  )
})

test_that("fixme_stil_linter() ser ikkje på tekststrengar", {
  lintr::expect_no_lint('x = "fixme"', fixme_stil_linter())
})

test_that("fixme_stil_linter() melder frå éin gong per kommentar", {
  kode = "x = 1 # fixme: ein\ny = 2 # fixme: to"
  lintr::expect_lint(kode, list(feilmelding, feilmelding), fixme_stil_linter())
})

test_that("fixme_stil_linter() gjev rett linjenummer", {
  kode = "x = 1\ny = 2 # fixme: rett dette\nz = 3"
  lintr::expect_lint(
    content = kode,
    checks = list(list(feilmelding, line_number = 2L)),
    linters = fixme_stil_linter()
  )
})
