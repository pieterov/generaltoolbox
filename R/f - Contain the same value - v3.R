##############################################################
# FUNCTION - CONTAIN THE SAME VALUE
#
# VERSION:      3.
# DATE:         July 27, 2021.
# NAME:         Pieter Overdevest.
#
# VERSIONS:     v1 -    start
#               v2 -    ...
#               v3 -    Uitbreiding van vergelijking/analyse.
#
##############################################################

        f_contain_the_same_value <- function(v1, v2) {


##############################################################################
# TEST ONLY!!
##############################################################################

        # v1  <- c(1,2,1); v2 <- c(1,2,1,1)
        # v1  <- c(1,2,1); v2 <- as.factor(c(1,1,2))
        # v1  <- c(1,1,2); v2 <- c(1,2,1,3,2)

        # v1 <- c("1a6edc03-3dfc-4d0f-adb1-238dd042fe3c", "1a6edc03-3dfc-4d0f-adb1-238dd042fe3c", "1a6edc03-3dfc-4d0f-adb1-238dd042fe3c", "1a6edc03-3dfc-4d0f-adb1-238dd042fe3c", "1a6edc03-3dfc-4d0f-adb1-238dd042fe3c")
        # v2 <- c("1a6edc03-3dfc-4d0f-adb1-238dd042fe3c", "1a6edc03-3dfc-4d0f-adb1-238dd042fe3c", "1a6edc03-3dfc-4d0f-adb1-238dd042fe3c", "1a6edc03-3dfc-4d0f-adb1-238dd042fe3c", "1a6edc03-3dfc-4d0f-adb1-238dd042fe3c")

        # v1 <- df.bord.d01.rotonde$polygon.id
        # v2 <- df.rotonde.polygon$group.id


##############################################################################
# Initialize // Error checking.
##############################################################################

        v1        <- as.character(v1)
        v2        <- as.character(v2)

        v1.sorted <- sort(v1)
        v2.sorted <- sort(v2)

        v1.unique <- unique(v1.sorted)
        v2.unique <- unique(v2.sorted)

        b.identical        <- identical(v1,        v2)
        b.identical.sorted <- identical(v1.sorted, v2.sorted)
        b.identical.unique <- identical(v1.unique, v2.unique)

        v.v1.not.in.v2     <- setdiff(v1.unique, v2.unique)
        v.v2.not.in.v1     <- setdiff(v2.unique, v1.unique)


##############################################################################
# Communicatie naar de gebruiker.
##############################################################################

        if(b.identical) {

                cat("v1 en v2 zijn identiek; ze bevatten dezelfde waarden op dezelfde posities.\n\n")

                f_info(v1)


        } else if(b.identical.sorted) {

                cat("v1 en v2 bevatten dezelfde waarden, maar niet op dezelfde posities.\n\n")

                f_info(v1)


        } else if(b.identical.unique) {

                cat("v1 en v2 bevatten dezelfde unieke waarden, maar niet in gelijke aantallen.\n\n")

                f_info(v1)

                f_info(v2)


        } else {

                cat("v1 en v2 bevatten niet dezelfde unieke waarden.")

                if(!identical(v.v1.not.in.v2, character(0))) { cat("\n\nv1 bevat waarden, die niet in v2 voorkomen:", f_paste(v.v1.not.in.v2), "\n") }
                if(!identical(v.v2.not.in.v1, character(0))) { cat("\n\nv2 bevat waarden, die niet in v1 voorkomen:", f_paste(v.v2.not.in.v1), "\n") }

                f_info(v1)

                f_info(v2)

                }


##############################################################################
# Return.
##############################################################################

        #return()

        }
