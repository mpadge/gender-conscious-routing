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
gender_streetnet <- function (net, wt_profile = "foot", country = "de") {

    g <- get_gender (net$name)
    g$gender [is.na (g$text)] <- NA_character_

    if (tolower (country) == "de") {

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
