context ("route")

# Small local network with hand-assigned genders, to avoid Overpass and
# Wikidata calls which `gender_streetnet()` would require.
make_test_net <- function () {

    net <- dodgr::weight_streetnet (dodgr::hampi, wt_profile = "foot")
    way_ids <- unique (net$way_id)
    set.seed (1L)
    g <- sample (c ("IS_FEMALE", "IS_MALE", "NAME_NOT_FOUND"),
        size = length (way_ids), replace = TRUE
    )
    net$gender <- g [match (net$way_id, way_ids)]
    net$female <- net$gender == "IS_FEMALE"
    return (net)
}

test_that ("conscious_route input checks", {

    net <- make_test_net ()
    expect_error (conscious_route (net), "must be specified")
    expect_error (
        conscious_route (net,
            start = "a", stop = "b",
            gender_weight = 0.5
        ),
        "single numeric value"
    )
    expect_error (
        conscious_route (net,
            start = "a", stop = "b",
            gender_weight = "a"
        ),
        "single numeric value"
    )
})

test_that ("conscious_route output", {

    net <- make_test_net ()
    v <- dodgr::dodgr_vertices (net)
    v <- v [v$component == 1L, ]
    set.seed (2L)
    ids <- sample (v$id, size = 2L)

    res <- conscious_route (net,
        start = ids [1], stop = ids [2],
        gender_weight = 10
    )

    expect_type (res, "list")
    expect_identical (names (res), c ("p_stats", "paths", "points"))

    expect_s3_class (res$p_stats, "data.frame")
    expect_identical (rownames (res$p_stats), c ("default", "female"))
    expect_equal (res$p_stats$path_length_rel [1], 1)

    p <- res$paths
    expect_s3_class (p, "data.frame")
    expect_false (inherits (p, "sf"))
    expect_identical (names (p), c ("type", "order", "x", "y"))
    expect_type (p$type, "character")
    expect_setequal (unique (p$type), c ("default", "female"))
    expect_false (anyNA (p [, c ("x", "y")]))
    for (type in c ("default", "female")) {
        pt <- p [p$type == type, ]
        expect_identical (pt$order, seq_len (nrow (pt)))
    }

    pts <- res$points
    expect_equal (nrow (pts), 2L)
    expect_true (all (c ("x", "y", "colour") %in% names (pts)))
    # Both routes start and end at the specified points
    for (type in c ("default", "female")) {
        pt <- p [p$type == type, ]
        expect_equal (
            unname (unlist (pt [1, c ("x", "y")])),
            unname (unlist (pts [1, c ("x", "y")]))
        )
        expect_equal (
            unname (unlist (pt [nrow (pt), c ("x", "y")])),
            unname (unlist (pts [2, c ("x", "y")]))
        )
    }
})
