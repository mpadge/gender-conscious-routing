#' gcr_city
#'
#' Analyse entire properties of one city
#'
#' @param net Street network in `osmdata_sc` format.
#' @param wt_profile Type of weighting profile for street network routing,.
#' @param n Number of pints used to calculate pair-wise routes along which to
#' aggregate statistics. If no value specified, routes are calculated between
#' all pairs for the entire network.
#' @export
gcr_city <- function (net, wt_profile = "foot", n = NULL) {

    dodgr::dodgr_cache_off ()

    net <- readRDS (f) |>
        gender_streetnet (wt_profile = wt_profile) |>
        dodgr::dodgr_contract_graph ()
    net$edge_type <- net$gender
    v <- dodgr::dodgr_vertices (net)

    if (!is.null (n)) {
        from <- to <- sample (v$id, size = n)
    } else {
        from <- to <- v$id
    }

    d0 <- dodgr::dodgr_dists_categorical (net, from = from, to = to,
                                          proportions_only = TRUE)
    d0 <- c (
        d0,
        "IS_A_NAME" = sum (d0 [1:2]),
        "IS_FEMALE_RAW" =
            length (which (net$edge_type == "IS_FEMALE")) / nrow (net),
        "IS_MALE_RAW" =
            length (which (net$edge_type == "IS_MALE")) / nrow (net))
    names (d0) [names (d0) == "NAME_NOT_FOUND"] <- "NOT_A_NAME"
    d0 <- c (
        d0,
        "NOT_A_NAME_RAW" = 1 - sum (d0 [5:6]),
        "IS_A_NAME_RAW" = sum (d0 [5:6]))

    net$edge_type <- net$highway

    if (wt_profile == "foot") {
        types <- c (
            "bridleway",
            "track",
            "path",
            "living_street",
            "steps",
            "pedestrian")
        this_category <- "pedestrian"
    } else {
        types <- c (
            "bridleway",
            "track",
            "path",
            "living_street",
            "pedestrian",
            "cycleway")
        this_category <- "bicycle"
    }

    types <- paste0 ("^", paste0 (types, collapse = "|^"))
    net$edge_type [grep (types, net$edge_type)] <- this_category
    net$edge_type [net$edge_type == "unclassified"] <- NA_character_
    net$edge_type [net$edge_type != this_category &
                   !is.na (net$edge_type)] <- "vehicular"

    d1 <- dodgr::dodgr_dists_categorical (net, from = from, to = to,
                                          proportions_only = TRUE)
    d1 <- c (d1,
             length (which (net$edge_type == this_category)) / nrow (net),
             length (which (net$edge_type == "vehicular")) / nrow (net))
    names (d1) [3:4] <- paste0 (c (this_category, "vehicular"), "_RAW")

    names (d1) <- gsub ("vehicular",
                        paste0 ("vehicular_", wt_profile), names (d1))

    ret <- c (d0, d1)
    data.frame (wt_profile = wt_profile,
                category = names (ret),
                proportion = as.numeric (ret))
}
