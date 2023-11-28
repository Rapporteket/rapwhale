# Testing av num()

test_that("num() gir ut tankestrek når «x»-argumentet inneholder NA", {
  expect_identical(num(NA_real_), "{\\textendash{}}")
})

test_that("num() gir ut riktig når inndata er av lengde 0", {
  expect_identical(num(numeric()), character())
})

test_that("num() gir ut riktig verdi når kun «x»-argumentet er gitt", {
  expect_identical(num(1234), "{\\numprint{1234}}")
  expect_identical(
    num(c(12.34, 3.1415)),
    c("{\\numprint{12.34}}", "{\\numprint{3.1415}}")
  )
})

test_that("num() gir ut riktig verdi med «desimalar»-argumentet", {
  expect_identical(
    num(c(12.34, 3.1415), desimalar = 3),
    c("{\\numprint{12.340}}", "{\\numprint{3.142}}")
  )
  expect_identical(
    num(c(12.34, 3.1415), desimalar = NULL),
    c("{\\numprint{12.34}}", "{\\numprint{3.1415}}")
  )
})

test_that("num() gir feilmelding om «x»-argument er i tekstform", {
  expect_error(
    num("12"),
    "Assertion on 'x' failed: Must be of type 'numeric', not 'character'."
  )
  expect_error(
    num(c(12, "3.14")),
    "Assertion on 'x' failed: Must be of type 'numeric', not 'character'."
  )
})

test_that("num() ikke konverterer store tall til 
          eksponentiell notasjon", {
  expect_identical(num(1.2e+08), "{\\numprint{120000000}}")
  expect_identical(num(120000000.34), "{\\numprint{120000000}}")
})

test_that("num() ikke konverterer små tall til
          eksponentiell notasjon", {
  expect_identical(num(1e-12), "{\\numprint{0.000000000001}}")
})

# Testing av akse_prosent_format()

test_that("akse_prosent_format() gir ut verdier på riktig format ved
           0, 1 og 2 desimaler", {
  prosent_format_0_desimaler = akse_prosent_format(antall_desimaler = 0)
  expect_identical(
    prosent_format_0_desimaler(c(0.0036, 0.0468, 0.78)),
    c("0 %", "5 %", "78 %")
  )

  prosent_format_1_desimal = akse_prosent_format()
  expect_identical(
    prosent_format_1_desimal(c(0.0036, 0.0468, 0.78)),
    c("0,4 %", "4,7 %", "78,0 %")
  )

  prosent_format_2_desimaler = akse_prosent_format(antall_desimaler = 2)
  expect_identical(
    prosent_format_2_desimaler(c(0.0036, 0.0468, 0.78)),
    c("0,36 %", "4,68 %", "78,00 %")
  )
})


test_that("akse_prosent_format() gir ut verdier på riktig format ved å
           oppgi punktum som desimalskilletegn", {
  prosent_format_punktum_skilletegn = akse_prosent_format(decimal.mark = ".")
  expect_identical(
    prosent_format_punktum_skilletegn(c(0.0036, 0.0468, 0.78)),
    c("0.4 %", "4.7 %", "78.0 %")
  )
})

test_that("akse_prosent_format() gir ut en tom tekstvektor hvis den tar
          inn en en tom tallvektor", {
  prosent_format = akse_prosent_format()
  expect_identical(prosent_format(integer()), character())
})

# Testing av akse_tall_format()

test_that("akse_tall_format() gir ut verdier på riktig format ved
           0, 1 og 2 desimaler", {
  tall_format_0_desimaler = akse_tall_format(antall_desimaler = 0)
  expect_identical(
    tall_format_0_desimaler(c(468.999, 7685.87, 65473.3, 256473)),
    c("469", "7 686", "65 473", "256 473")
  )

  tall_format_1_desimal = akse_tall_format(antall_desimaler = 1)
  expect_identical(
    tall_format_1_desimal(c(468.999, 7685.87, 65473.3, 256473)),
    c("469,0", "7 685,9", "65 473,3", "256 473,0")
  )

  tall_format_2_desimaler = akse_tall_format()
  expect_identical(
    tall_format_2_desimaler(c(468.999, 7685.87, 65473.3, 256473)),
    c("469,00", "7 685,87", "65 473,30", "256 473,00")
  )
})

test_that("akse_tall_format() gir ut verdier på riktig format ved å
           oppgi punktum som desimalskilletegn", {
  tall_format_punktum_skilletegn = akse_tall_format(decimal.mark = ".")
  expect_identical(
    tall_format_punktum_skilletegn(c(468.999, 7685.87, 65473.3, 256473)),
    c("469.00", "7 685.87", "65 473.30", "256 473.00")
  )
})

test_that("akse_tall_format() gir ut verdier på riktig format ved å
           oppgi XX som tusenskille", {
  tall_format_uten_tusenskille = akse_tall_format(big.mark = "XX")
  expect_identical(
    tall_format_uten_tusenskille(c(468.999, 7685.87, 65473.3, 256473)),
    c("469,00", "7XX685,87", "65XX473,30", "256XX473,00")
  )
})

test_that("akse_tall_format() gir ut en tom tekstvektor hvis den tar
          inn en en tom tallvektor", {
  tall_format = akse_tall_format()
  expect_identical(tall_format(integer()), character())
})

# Testing av prosent()

test_that("prosent() gir ut tankestrek når «x»-argumentet inneholder NA", {
  expect_identical(prosent(NA_real_), num(NA_real_))
  expect_identical(
    prosent(c(NA_real_, 0.12)),
    c(num(NA_real_), paste0(num(12), "\\prosent"))
  )
})

test_that("prosent() gir ut riktig når inndata er av lengde 0", {
  expect_identical(prosent(numeric()), character())
})

test_that("prosent() gir ut riktig verdi når kun «x»-argumentet er gitt", {
  expect_identical(
    prosent(0.1234),
    paste0(num(12), "\\prosent")
  )
  expect_identical(
    prosent(c(0.1234, 0.31415)),
    c(paste0(num(12), "\\prosent"), paste0(num(31), "\\prosent"))
  )
})

test_that("prosent() gir ut riktig verdi med «desimalar»-argumentet", {
  expect_identical(
    prosent(0.1234, desimalar = 1),
    paste0(num(12.34, desimalar = 1), "\\prosent")
  )
})

test_that("prosent() gir feilmelding om «x»-argument er i tekstform", {
  expect_error(
    prosent("0.12"),
    checkmate::check_numeric("0.12")
  )
})
