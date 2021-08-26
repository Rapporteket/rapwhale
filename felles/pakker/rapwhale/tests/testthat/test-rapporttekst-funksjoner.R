# Testing av akse_prosent_format()

test_that("akse_prosent_format() gir ut verdier på riktig format ved 0 desimaler", {
  prosent_format_0_desimaler = akse_prosent_format(antall_desimaler = 0)
  expect_identical(prosent_format_0_desimaler(0.0468), "5 %")
})

test_that("akse_prosent_format() gir ut verdier på riktig format ved 1 desimal", {
  prosent_format_1_desimal = akse_prosent_format()
  expect_identical(prosent_format_1_desimal(0.0468), "4,7 %")
})

test_that("akse_prosent_format() gir ut verdier på riktig format ved 2 desimaler", {
  prosent_format_2_desimaler = akse_prosent_format(antall_desimaler = 2)
  expect_identical(prosent_format_2_desimaler(0.0468), "4,68 %")
})

test_that("akse_prosent_format() gir ut verdier på riktig format ved å
           oppgi punktum som desimalskilletegn", {
  prosent_format_punktum_skilletegn = akse_prosent_format(decimal.mark = ".")
  expect_identical(prosent_format_punktum_skilletegn(0.0468), "4.7 %")
})

# Testing av akse_tall_format()

test_that("akse_tall_format() gir ut verdier på riktig format ved
           0, 1 og 2 desimaler", {
  tall_format_0_desimaler = akse_tall_format(antall_desimaler = 0)
  expect_identical(tall_format_0_desimaler(468.999), "469")

  tall_format_1_desimal = akse_tall_format(antall_desimaler = 1)
  expect_identical(tall_format_1_desimal(468.999), "469,0")

  tall_format_2_desimaler = akse_tall_format()
  expect_identical(tall_format_2_desimaler(468.999), "469,00")
})

test_that("akse_tall_format() gir ut verdier på riktig format ved å
           oppgi punktum som desimalskilletegn", {
  tall_format_punktum_skilletegn = akse_tall_format(decimal.mark = ".")
  expect_identical(tall_format_punktum_skilletegn(468.999), "469.00")
})

test_that("akse_tall_format() gir ut verdier på riktig format ved å
           oppgi XX som tusenskille", {
  tall_format_uten_tusenskille = akse_tall_format(big.mark = "XX")
  expect_identical(tall_format_uten_tusenskille(1000), "1XX000,00")
})

test_that("akse_tall_format() gir ut tusenskille på riktig sted for tall
           med ulikt antall siffer", {
  tall_format_2_desimaler = akse_tall_format()
  expect_identical(tall_format_2_desimaler(10000), "10 000,00")
  expect_identical(tall_format_2_desimaler(100000), "100 000,00")
})
