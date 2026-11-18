context ("gender")

test_that ("gender", {
    g <- get_gender (c ("Henri", "Smith", "lewis")) # 77 32 77
    expect_is (g, "data.frame")
    expect_equal (ncol (g), 3)
    expect_equal (nrow (g), 3)
    expect_identical (names (g), c ("text", "gender", "rel_freq"))
    expect_identical (
        g$gender,
        c ("IS_MALE", "NAME_NOT_FOUND", "IS_MALE")
    )

    g <- get_gender (c ("Stephanie", "Smith", "julie")) # 70 32 70
    expect_identical (
        g$gender,
        c ("IS_FEMALE", "NAME_NOT_FOUND", "IS_FEMALE")
    )

    g <- get_gender (c ("Stephanie Smith", "julie", "andrea"), "it") # 70 70 63
    expect_identical (
        g$gender,
        c ("IS_FEMALE", "IS_FEMALE", "IS_UNISEX_NAME")
    )
    g <- get_gender (c ("Stephanie Smith", "julie", "andrea"), "de")
    expect_identical (
        g$gender,
        c ("IS_FEMALE", "IS_FEMALE", "IS_FEMALE")
    )
})

test_that ("relative frequency", {
    g <- get_gender (c ("Stephanie", "Smith", "Henri"))
    expect_true (all (g$rel_freq [c (1, 3)] >= 0.5 &
        g$rel_freq [c (1, 3)] <= 1))
    expect_true (is.na (g$rel_freq [2]))
})
