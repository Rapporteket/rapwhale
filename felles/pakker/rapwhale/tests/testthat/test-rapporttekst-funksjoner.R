# Testing av akse_prosent_format()

test_that("akse_prosent_format() gir ut verdier på riktig format ved
           0, 1 og 2 desimaler", {
  prosent_format_0_desimaler = akse_prosent_format(antall_desimaler = 0)
  expect_identical(prosent_format_0_desimaler(c(0.0036, 0.0468, 0.78)), c("0 %", "5 %", "78 %"))

  prosent_format_1_desimal = akse_prosent_format()
  expect_identical(prosent_format_1_desimal(c(0.0036, 0.0468, 0.78)), c("0,4 %", "4,7 %", "78,0 %"))

  prosent_format_2_desimaler = akse_prosent_format(antall_desimaler = 2)
  expect_identical(prosent_format_2_desimaler(c(0.0036, 0.0468, 0.78)), c("0,36 %", "4,68 %", "78,00 %"))
})


test_that("akse_prosent_format() gir ut verdier på riktig format ved å
           oppgi punktum som desimalskilletegn", {
  prosent_format_punktum_skilletegn = akse_prosent_format(decimal.mark = ".")
  expect_identical(prosent_format_punktum_skilletegn(c(0.0036, 0.0468, 0.78)), c("0.4 %", "4.7 %", "78.0 %"))
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
  expect_identical(tall_format_0_desimaler(c(468.999, 7685.87, 65473.3, 256473)), c("469", "7 686", "65 473", "256 473"))

  tall_format_1_desimal = akse_tall_format(antall_desimaler = 1)
  expect_identical(tall_format_1_desimal(c(468.999, 7685.87, 65473.3, 256473)), c("469,0", "7 685,9", "65 473,3", "256 473,0"))

  tall_format_2_desimaler = akse_tall_format()
  expect_identical(tall_format_2_desimaler(c(468.999, 7685.87, 65473.3, 256473)), c("469,00", "7 685,87", "65 473,30", "256 473,00"))
})

test_that("akse_tall_format() gir ut verdier på riktig format ved å
           oppgi punktum som desimalskilletegn", {
  tall_format_punktum_skilletegn = akse_tall_format(decimal.mark = ".")
  expect_identical(tall_format_punktum_skilletegn(c(468.999, 7685.87, 65473.3, 256473)), c("469.00", "7 685.87", "65 473.30", "256 473.00"))
})

test_that("akse_tall_format() gir ut verdier på riktig format ved å
           oppgi XX som tusenskille", {
  tall_format_uten_tusenskille = akse_tall_format(big.mark = "XX")
  expect_identical(tall_format_uten_tusenskille(c(468.999, 7685.87, 65473.3, 256473)), c("469,00", "7XX685,87", "65XX473,30", "256XX473,00"))
})

test_that("akse_tall_format() gir ut en tom tekstvektor hvis den tar
          inn en en tom tallvektor", {
  tall_format = akse_tall_format()
  expect_identical(tall_format(integer()), character())
})
