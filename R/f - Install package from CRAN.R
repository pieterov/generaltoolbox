#' @title Install latest packages from CRAN
#'
#' @description Install latest packages from CRAN, and 'library' them.
#'
#' @author Pieter Overdevest
#'
#' @param v.package Vector with package names to install
#'
#' @returns Nothing.
#'
#' @details -
#'
#' @export
#'
#' @examples
#' f_install_package_from_cran(
#'
#'     v.package = c("tmaptools")
#' )

        #################################################################################
        # FUNCTION.
        #################################################################################

        f_install_package_from_cran <- function(

                v.package
        ) {

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

