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
conscious_route <- function (net, start, stop)
{
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

    p0 <- as.matrix (pmat [, c ("x", "y")]) %>%
        sf::st_linestring () %>%
        sf::st_sfc (crs = 4326) %>%
        sf::st_sf (geometry = .)
    pf <- as.matrix (pmat_f [, c ("x", "y")]) %>%
        sf::st_linestring () %>%
        sf::st_sfc (crs = 4326) %>%
        sf::st_sf (geometry = .)
    psf <- rbind (p0, pf)
    psf <- data.frame (type = c ("default", "female"),
                       psf,
                       stringsAsFactors = FALSE)


    list (p_stats = p_stats, paths = psf, points = start_end)
}

path_stats <- function (pmat, net)
{
    # get edge IDs for the path
    from <- pmat$id [-nrow (pmat)]
    to <- pmat$id [-1]
    ft_net <- paste0 (net$from_id, "-", net$to_id)
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

#' gender_streetnet
#' 
#' Weight a street network via \pkg{dodgr} function `weight_streetnet`, adding
#' additional information on gender of street names
#' 
#' @inheritParams get_gender
#' @param net Street network in \pkg{sf} format
#' @param wt_profile Type of weighting to be used; see `dodgr::weight_streetnet`
#' for details.
#' @return A weighted street network in \pkg{dodgr} format. 
#' @export
gender_streetnet <- function (net, wt_profile = "foot", country = "de")
{
    g <- get_gender (net$name)
    g$gender [is.na (g$text)] <- NA_character_

    if (tolower (country) == "de")
    {
        # These can not be personal names:
        index <- which (substring (g$text, 1, 6) == "Alter" |
                        substring (g$text, 1, 3) == "Am" |
                        substring (g$text, 1, 3) == "An " |
                        substring (g$text, 1, 4) == "Auf " |
                        substring (g$text, 1, 3) == "Im " |
                        substring (g$text, 1, 3) == "In " |
                        substring (g$text, 1, 4) == "Zum " |
                        substring (g$text, 1, 4) == "Zur " |
                        substring (g$text, 1, 3) == "Zu " |
                        !grepl ("-|\\s", g$text) |
                        grepl ("[0-9]", g$text) |
                        grepl ("Heide$", g$text))
        g$gender [index] <- "NAME_NOT_FOUND"
    }
    net$gender <- g$gender

    netw <- dodgr::weight_streetnet (net, wt_profile = wt_profile)
    netw$gender <- "NAME_NOT_FOUND"
    id_m <- net$osm_id [which (net$gender == "IS_MALE" |
                                  net$gender == "IS_MOSTLY_MALE")]
    id_f <- net$osm_id [which (net$gender == "IS_FEMALE" |
                                  net$gender == "IS_MOSTLY_FEMALE")]
    netw$gender [which (netw$way_id %in% id_m)] <- "IS_MALE"
    netw$gender [which (netw$way_id %in% id_f)] <- "IS_FEMALE"

    netw$female <- FALSE
    netw$female [which (netw$gender == "IS_FEMALE")] <- TRUE

    return (netw)
}
