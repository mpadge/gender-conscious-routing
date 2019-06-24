context("gender")

test_that("gender", {
              g <- get_gender (c ("Henri", "Smith", "lewis")) # 77 32 77
              expect_is (g, "data.frame")
              expect_equal (ncol (g), 2)
              expect_equal (nrow (g), 3)
              expect_identical (names (g), c ("text", "gender"))
              expect_identical (g$gender,
                                c ("IS_MALE", "NAME_NOT_FOUND", "IS_MALE"))

              g <- get_gender (c ("Stephanie", "Smith", "julie")) # 70 32 70
              expect_identical (g$gender,
                                c ("IS_FEMALE", "NAME_NOT_FOUND", "IS_FEMALE"))

              g <- get_gender (c ("Stephanie Smith", "julie", "andrea"), "it") # 70 70 63
              expect_identical (g$gender,
                                c ("IS_FEMALE", "IS_FEMALE", "IS_UNISEX_NAME"))
              g <- get_gender (c ("Stephanie Smith", "julie", "andrea"), "de")
              expect_identical (g$gender,
                                c ("IS_FEMALE", "IS_FEMALE", "IS_FEMALE"))
})
