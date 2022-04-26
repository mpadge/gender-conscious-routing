#' gender_streetnet
#'
#' Weight a street network via \pkg{dodgr} function `weight_streetnet`, adding
#' additional information on gender of street names
#'
#' @inheritParams get_gender
#' @param net Street network in \pkg{sf} or \pkg{sc} format
#' @param wt_profile Type of weighting to be used; see `dodgr::weight_streetnet`
#' for details.
#' @return A weighted street network in \pkg{dodgr} format.
#' @export
gender_streetnet <- function (net, wt_profile = "foot", country = "de") {

    netw <- dodgr::weight_streetnet (net, wt_profile = wt_profile,
                                     keep_cols = c ("name",
                                                    "name:etymology",
                                                    "name:etymology:wikidata"))

    netw <- wikidata_gender (netw)

    index <- which (is.na (netw$gender)) # non-wikidata entries
    g <- get_gender (netw$name [index])
    g$gender [is.na (g$text)] <- NA_character_

    if (tolower (country) == "de") {

        # These can not be personal names:
        index2 <- which (substring (g$text, 1, 6) == "Alter" |
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
        g$gender [index2] <- "NAME_NOT_FOUND"
    }

    netw$gender [index] <- "NAME_NOT_FOUND"
    netw$gender [index] [which (g$gender == "IS_MALE" |
                                g$gender == "IS_MOSTLY_MALE")] <- "IS_MALE"
    netw$gender [index] [which (g$gender == "IS_FEMALE" |
                                g$gender == "IS_MOSTLY_FEMALE")] <- "IS_FEMALE"

    netw$female <- FALSE
    netw$female [which (netw$gender == "IS_FEMALE")] <- TRUE

    return (netw)
}

wikidata_gender <- function (net) {

    if (!"gender" %in% names (net)) {
        net$gender <- NA_character_
    }

    wiki <- unique (net$`name:etymology:wikidata`)
    wiki <- wiki [which (!is.na (wiki))]
    # some have multiple wiki tags, for which choose the first
    wiki <- gsub (";.*$", "", wiki)

    wd <- paste0 (paste0 ("wd:", wiki), collapse = " ")
    qry <- paste0 ('
        SELECT DISTINCT ?person ?personLabel ?genderLabel WHERE {
          VALUES ?person {', wd, '
          }
        ?person wdt:P21 ?gender;
        SERVICE wikibase:label {
            bd:serviceParam wikibase:language "[AUTO_LANGUAGE]". }
    }')
    qry <- gsub ("\\s+", "%20", gsub ("^\\s?", "", qry))

    u <- sprintf ("https://query.wikidata.org/sparql?format=json&query=%s", qry)
    # calling fromJSON directly sometimes errors on wikidata calls:
    #res <- jsonlite::fromJSON (u)
    res <- httr::GET (u)
    if (res$status != 200L) {
        return (net)
    }
    res <- jsonlite::fromJSON (httr::content (res, as = "text"))
    res <- res$results$bindings$genderLabel$value

    # https://www.wikidata.org/wiki/Property:P21
    gender_map <- rbind (
        c ("male", "Q6581097"),
        c ("female", "Q6581072"),
        c ("intersex", "Q1097630"),
        c ("transgender female", "Q1052281"),
        c ("transgender male", "Q2449503")
        )

    gender <- gender_map [match (res, gender_map [, 2]), 1]
    gender <- paste0 ("IS_", toupper (gender))

    index <- which (net$`name:etymology:wikidata` %in% wiki)
    index2 <- match (net$`name:etymology:wikidata` [index], wiki)
    net$gender [index] <- gender [index2]

    attr (net, "num_wikidata_entries") <- length (index)

    return (net)
}
