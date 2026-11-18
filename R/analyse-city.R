#' gcr_city
#'
#' Analyse entire properties of one city
#'
#' @param net Street network in `osmdata_sc` format.
#' @param wt_profile Type of weighting profile for street network routing,.
#' @param n Number of pints used to calculate pair-wise routes along which to
#' aggregate statistics. If no value specified, routes are calculated between
#' all pairs for the entire network. If value exceeds total number of network
#' nodes, that number is used instead.
#' @param quiet if `FALSE`, display progress information on screen.
#' @export
gcr_city <- function (net, wt_profile = "foot", n = NULL, quiet = FALSE) {

    if (!methods::is (net, "dodgr_streetnet")) {
        stop ("'net' must be a 'dodgr_streetnet' object constructed with ",
              "the 'gender_streetnet' function")
    }

    if (!quiet) {
        message (cli::symbol$play,
                 cli::col_green (" Contracting street network ... "))
    }
    net$edge_type <- net$gender
    v <- dodgr::dodgr_vertices (net)

    if (!is.null (n)) {
        if (n > nrow (v)) {
            from <- to <- v$id
        } else {
            from <- to <- sample (v$id, size = n)
        }
    } else {
        from <- to <- v$id
    }

    if (!quiet) {
        message (cli::col_green (cli::symbol$tick,
                                 " Contracted street network"))
        message (cli::symbol$play,
                 cli::col_green (" Calculating routes (1/2) ..."))
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
    } else if (wt_profile == "bicycle") {
        types <- c (
            "bridleway",
            "track",
            "path",
            "living_street",
            "pedestrian",
            "cycleway")
        this_category <- "bicycle"
    } else if (wt_profile == "motorcar") {
        types <- c (
            "motorway",
            "primary",
            "secondary",
            "tertiary",
            "service",
            "residential",
            "trunk",
            "unclassified")
        this_category <- "motorcar"
    }

    types <- paste0 ("^", paste0 (types, collapse = "|^"))
    net$edge_type [grep (types, net$edge_type)] <- this_category
    net$edge_type [net$edge_type == "unclassified"] <- NA_character_
    vtype <- ifelse (wt_profile == "motorcar",
                     "motorcar", "vehicular")
    net$edge_type [net$edge_type != this_category &
                   !is.na (net$edge_type)] <- vtype

    if (!quiet) {
        message (cli::symbol$tick,
                 cli::col_green (" Calculated routes (1/2)"))
    }

    ret <- d0

    if (this_category != "motorcar") {

        if (!quiet) {
            message (cli::col_green (cli::symbol$play,
                                     " Calculating routes (2/2) ..."))
        }

        d1 <- dodgr::dodgr_dists_categorical (net, from = from, to = to,
                                              proportions_only = TRUE)
        if (!quiet) {
            message (cli::symbol$tick,
                     cli::col_green (" Calculated routes (2/2)"))
        }

        d1 <- c (d1,
                 length (which (net$edge_type == this_category)) / nrow (net),
                 length (which (net$edge_type == vtype)) / nrow (net))
        names (d1) [3:4] <- paste0 (c (this_category, "vehicular"), "_RAW")

        names (d1) <- gsub ("vehicular",
                            paste0 ("vehicular_", wt_profile), names (d1))

        ret <- c (ret, d1)
    }

    data.frame (wt_profile = wt_profile,
                category = names (ret),
                proportion = as.numeric (ret))
}
