context("gender")

test_that("gender", {
              g <- get_gender (c ("Henri", "Smith", "lewis")) # 77 32 77
              expect_identical (g, c (77L, 32L, 77L))
              g <- get_gender (c ("Stephanie", "Smith", "julie")) # 70 32 70
              expect_identical (g, c (70L, 32L, 70L))
              g <- get_gender (c ("Stephanie Smith", "julie", "andrea"), "it") # 70 70 63
              expect_identical (g, c (70L, 70L, 63L))
})
