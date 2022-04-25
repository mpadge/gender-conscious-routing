#' conscious_route
#'
#' Route between nominated start and end points along greatest proportional
#' length of streets named after women rather than men.
#'
#' @param net Street network with gender column, returned by
#' \link{gender_streetnet} function.
#' @param start Start point of route.
#' @param stop End point of route.
#' @param gender_weight A numeric factor >= 1 by which to preferentially route
#' along streets named after women. Larger values will search larger distances 
#' relative to the default (shortest) route to find routes with greatest
#' proportional distances along streets named after women. Arbitrarily large
#' values may be specified.
#' @export
conscious_route <- function (net, start = NULL, stop = NULL,
                             gender_weight = 10) {

    if (is.null (start) | is.null (stop)) {
        stop ("[start] and [stop] must be specified.")
    }
    if (!is.numeric (gender_weight) | length (gender_weight) != 1L) {
        stop ("[gender_weight] must be a single numeric value >= 1")
    } else if (gender_weight < 1.0) {
        stop ("[gender_weight] must be a single numeric value >= 1")
    }

    net_f <- net
    index <- which (net_f$female)
    net_f$d_weighted [index] <- net_f$d_weighted [index] / gender_weight

    v <- dodgr::dodgr_vertices (net)
    pmat <- dodgr::dodgr_paths (net, from = start, to = stop)
    pmat <- v [match (pmat [[1]] [[1]], v$id), ]
    start_end <- data.frame (rbind (pmat [1, c ("x", "y")],
                                    pmat [nrow (pmat), c ("x", "y")]))
    start_end$colour <- c ("#44FF22FF", "#FF4422FF")

    p0_stats <- path_stats (pmat, net)

    pmat_f <- dodgr::dodgr_paths (net_f, from = start, to = stop)
    pmat_f <- v [match (pmat_f [[1]] [[1]], v$id), ]
    pf_stats <- path_stats (pmat_f, net_f)

    p_stats <- data.frame (rbind (p0_stats, pf_stats))
    rownames (p_stats) <- c ("default", "female")

    p_stats$path_length <- rowSums (p_stats [, c ("d_female", "d_male", "d_non")])
    p_stats$path_length_rel <- p_stats$path_length / p_stats$path_length [1]

    p0 <- as.matrix (pmat [, c ("x", "y")])
    p0 <- sf::st_linestring (p0)
    p0 <- sf::st_sfc (p0, crs = 4326)
    p0 <- sf::st_sf (geometry = p0)

    pf <- as.matrix (pmat_f [, c ("x", "y")])
    pf <- sf::st_linestring (pf)
    pf <- sf::st_sfc (pf, crs = 4326)
    pf <- sf::st_sf (geometry = pf)

    psf <- rbind (p0, pf)
    psf <- data.frame (type = c ("default", "female"),
                       psf,
                       stringsAsFactors = FALSE)


    list (p_stats = p_stats, paths = psf, points = start_end)
}

path_stats <- function (pmat, net) {

    if (all (c ("from_id", "to_id") %in% names (net))) {
        from_id <- net$from_id
        to_id <- net$to_id
    } else if (all (c (".vx0", ".vx1") %in% names (net))) {
        from_id <- net$.vx0
        to_id <- net$.vx1
    }

    # get edge IDs for the path
    from <- pmat$id [-nrow (pmat)]
    to <- pmat$id [-1]
    ft_net <- paste0 (from_id, "-", to_id)
    ft_path <- paste0 (from, "-", to)
    index <- match (ft_path, ft_net)

    index_f <- which (net$gender [index] == "IS_FEMALE")
    index_m <- which (net$gender [index] == "IS_MALE")
    index_non <- which (net$gender [index] == "NAME_NOT_FOUND")

    d_f <- sum (net$d [index] [index_f])
    d_m <- sum (net$d [index] [index_m])
    d_non <- sum (net$d [index] [index_non])
    d_tot <- sum (net$d [index])

    res <- c (d_f, d_m, d_non, d_f / d_tot, d_m / d_tot, d_non / d_tot)
    names (res) <- c ("d_female", "d_male", "d_non",  "p_female", "p_male", "p_non")

    return (res)
}
