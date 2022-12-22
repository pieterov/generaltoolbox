#' @title Install last version of package on GitHub
#'
#' @description Install last version of package on GitHub.
#'
#' @author Pieter Overdevest
#'
#' @param c.package Package name.
#' @param c.auth.token Authentication token to access private repo. (default: NULL)
#'
#' @returns Nothing.
#'
#' @details Note - Never insert a token in the script. GitHub recognizes these tokens when committing and
#' revokes the concerned tokens. Great for security, bit unhandy in use!
#'
#' @export
#'
#' @examples
#' f_install_package_from_github(
#'
#'     c.package    = "generaltoolbox"
#'     c.auth.token = NULL
#' )

        #################################################################################
        # FUNCTION.
        #################################################################################

        f_install_package_from_github <- function(

                c.package,
                c.auth.token = NULL
        ) {


        ##############################################################################################
        # ERROR CHECK
        ##############################################################################################

        # Warning when
        # if( ) {
        #
        #         warning("")
        # }


        # Error when
        if(!c.package %in% c("generaltoolbox", "ipsmtoolbox")) {

                stop("Note, the value you gave for c.package ('", c,package,
                     "') must be one of, 'generaltoolbox' or 'ipsmtoolbox'!")
        }


        # Error when
        if(c.package == "ipsmtoolbox" & is.null(c.auth.token)) {

                stop("Note, c.auth.token is mandatory for 'ipsmtoollbox'!")
        }


        ##############################################################################################
        # INITIALIZE
        ##############################################################################################

        if(c.package == "generaltoolbox") {

                c.repo <- "pieterov"

                if("generaltoolbox" %in% (.packages())){

                        detach("package:generaltoolbox", unload=TRUE)
                }

        }

        if(c.package == "ipsmtoolbox") {

                c.repo <- "hr-groep"
        }


        ##############################################################################################
        # MAIN BODY
        ##############################################################################################

        # Get local hash
        # https://stackoverflow.com/questions/67842907/find-installed-package-sha-hash-in-r
        c.temp       <- devtools::package_info(c.package)$source
        c.hash.local <- substr(c.temp, nchar(c.temp) - 7, nchar(c.temp) - 1)

        # Get GitHub hash
        # https://stackoverflow.com/questions/14135233/get-sha1-of-latest-remote-commit
        c.temp        <- system(paste0("git ls-remote git@github.com:", c.repo, "/", c.package, ".git HEAD"), intern = TRUE)
        c.hash.github <- substr(c.temp, 1, 7)

        # Install if hash' are different
        if(c.hash.local != c.hash.github) {

                # Update the toolbox by downloading the latest version from GitHub.
                devtools::install_github(

                        repo         = paste0(c.repo, "/", c.package),
                        auth_token   = c.auth.token#,
                        #INSTALL_opts = c("--no-multiarch")
                )

                # Comms to user.
                cat(paste0(
                        "A new version of '", c.package, "' was installed (", packageVersion(c.package), ")!"
                ))
        }

        # Zet toolbox in memory.
        do.call(what = "library", args = list(c.package))


        ##############################################################################################
        # RETURN
        ##############################################################################################

}
