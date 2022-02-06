##############################################################################################
# FUNCITON NAME:  INSTALL PACKAGE FROM GITHUB
# AUTHOR:         Pieter Overdevest
# DESCRIPTION:    Installeer laatste versie van github en zet in memory.
##############################################################################################

        f_install_package_from_github <- function(

                c.package,            # name of package
                c.auth.token = NULL   # authentication token to access private repo.
        ) {


##############################################################################################
# TESTING ONLY!!
##############################################################################################

        # c.package    <- "generaltoolbox"
        # c.package    <- "ipsmtoolbox"
        # c.auth.token <- "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"

        # f_install_package_from_github("generaltoolbox")
        # f_install_package_from_github("ipsmtoolbox", "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")


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

                        repo         = "hr-groep/ipsmtoolbox",
                        auth_token   = "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",
                        INSTALL_opts = c("--no-multiarch")
                )

                # Comms to user.
                cat(paste0(
                        "A new version of '", c.package, "' was installed (", packageVersion("ipsmtoolbox"), ")!"
                ))
        }

        # Zet toolbox in memory.
        do.call(what = "library", args = list(c.package))


##############################################################################################
# RETURN
##############################################################################################

}
