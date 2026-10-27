library(testthat)
library(DNAr)

test_that("libSBML validates and imports the example case", {
    skip_if_not(sbml_available())

    path <- file.path("..", "..", "examples", "sbml", "cases", "00028.xml")
    validation <- validate_sbml(path)
    expect_true(validation$valid)
    expect_length(validation$errors, 0)

    crn <- read_sbml_to_dnar(path)
    expect_equal(crn$species, c("S1", "S2"))
    expect_equal(unname(crn$ci), c(1, 0))
    expect_equal(crn$reactions, "S1 -> S2")
    expect_equal(crn$ki, 0.96)
    expect_equal(crn$kinetic_laws[[1]]$formula,
                 "factorial(ceil(p1 * S1)) * p2^-1")
})

test_that("missing SBML files fail explicitly", {
    skip_if_not(sbml_available())
    expect_error(read_sbml_to_dnar("does-not-exist.xml"), "does not exist")
})
