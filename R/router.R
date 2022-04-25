#' conscious_route
#'
#' Route between nominated start and end points along greatest proportional
#' length of streets named after women rather than men.
#'
#' @param net Street network with gender column, returned by
#' \link{gender_streetnet} function.
#' @param start Start point of route.
#' @param stop End point of route.
#' @export
conscious_route <- function (net, start, stop) {

    net_f <- net
    index <- which (net_f$female)
    net_f$d_weighted [index] <- net_f$d_weighted [index] / 10

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
    index_out <- which (!net$female [index])
    index_in <- which (net$female [index])

    d_in <- sum (net$d [index] [index_in])
    d_out <- sum (net$d [index] [index_out])
    d_res <- c (d_in, d_out, d_in / d_out)
    names (d_res) <- c ("d_female", "d_non", "ratio")
    return (d_res)
}
