test_that("utviklingsnivaa() har utviklingsnivå experimental", {
  nivaa = utviklingsnivaa("eksempeldokumentasjon")$utviklingsnivaa
  expect_identical(nivaa, "experimental")
})
