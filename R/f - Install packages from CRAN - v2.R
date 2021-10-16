##############################################################
# FUNCTION - Install latest packages from CRAN, and 'library' them.
#
# NAME:         Pieter Overdevest.
# DATE:         Feb 12, 2021.
# VERSION:      1.
#
# VERSIONS:     v1 - Start-up.
#
##############################################################

        # TESTING
        # v.package <- c("tmaptools")

        f_install_packages_from_cran <- function(v.package) {

                # Install (if needed) and load libraries.
                v.loaded <- lapply(v.package,

                                   function(c.package) {

                                           # 'require' returns (invisibly) a logical indicating whether the
                                           # required package is available.
                                           if (!require(package        = c.package,
                                                        quietly        = TRUE,
                                                        character.only = TRUE)
                                           ) {

                                                   install.packages(c.package)
                                           }

                                           # Geeft error met 'tidyverse'
                                           suppressPackageStartupMessages(

                                                   do.call(
                                                           what = "library",
                                                           args = list(c.package)
                                                   )
                                           )
                                   }
                )
        }

