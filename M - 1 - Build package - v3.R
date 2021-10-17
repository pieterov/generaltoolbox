##############################################################################################
# BUILD PACKAGE - GENERALTOOLBOX
#
# NAME:         Pieter Overdevest.
# DATE:         Mar 23, 2021.
# VERSION:      2.
#
# DESCRIPTION:  Packaging functions in General Toolbox.
#
# VERSION:      v1 - Start-up.
#               v2 - ...
#               v3 - Code toegevoegd om oudere versies te archiveren.
#
# COMMENTS: Ik kreeg de error: Error: Failed to install 'generaltoolbox' from local:
# (converted from warning) installation of package
# ‘/var/folders/m9/89f2y1bn2_qbfv4yn75zlw140000gn/T//RtmpCqi7Z7/file3b3761f3dcef/generaltoolbox_1.1.11.tar.gz’
# had non-zero exit status.
#
# Toen devtools::install() gedraaid. RStudio herstarten hielp ook.
#
##############################################################################################

######################################################################################
# INITIALZIE
######################################################################################

        library(stringr)
        library(dplyr)
        library(lubridate)
        library(openxlsx)

        c.path.source      <- paste0(path.iwd, "Packages/generaltoolbox/R/")
        c.path.destination <- paste0(path.iwd, "Packages/generaltoolbox/Archive/")


######################################################################################
# ARCHIVE OLDER VERSIONS
######################################################################################

        # All function files in toolbox folder.
        v.file.to.move <- f_get_filenames_in_folder(c.path = c.path.source) %>%

                mutate(function.name.and.version = gsub("^f - |\\.R$", "", file.name),
                       function.version           = as.numeric(gsub("v", "", str_extract(function.name.and.version, "v[0-9]+$"))),
                       function.name              = gsub(" - v[0-9]+$", "", function.name.and.version)
                ) %>%

                arrange(function.name, desc(function.version)) %>%

                # Verwijder hoogste versie per file.name.
                group_by(function.name) %>%

                slice(-1) %>%

                ungroup() %>%

                pull(file.name)


        # Check!
        if(length(v.file.to.move) == 0) { warning("Er zijn geen nieuwe functies!") }


        # Move oudere versies.
        df.moved.files <- f_move_files(

                c.path.source      = c.path.source,
                c.path.destination = c.path.destination,
                v.file.to.move     = v.file.to.move
                )


######################################################################################
# PACKAGE LATEST VERSIONS
######################################################################################

        # Build latest version of 'generaltoolbox' using the info in DESCRIPTION:
        c.path.latest.version                  <- devtools::build()

        # Get basename of built package.
        c.basename                             <- basename(c.path.latest.version)

        # Extract version number of built package
        c.latest.built.version.generaltoolbox  <- str_extract(c.basename, "[0-9]+\\.[0-9]+\\.[0-9]+")

        # Comms.
        cat(paste0("\nVersie General Toolbox", c.latest.built.version.generaltoolbox,
                   " is succesvol gebouwd en lokaal geplaatst in folder '", path.iwd, "Packages'.\n"))


        # Copy package to HR Groep's Package folder op DB.
        b.file.copy <- file.copy(

                from = c.path.latest.version,

                to   = paste0(path.ipsm.dropbox.packages,
                              basename(c.path.latest.version)
                              ),

                overwrite = FALSE
                )


        # Comms.
        if(b.file.copy) {

                cat(paste0("\nVersie General Toolbox", c.latest.built.version.generaltoolbox,
                           " is succesvol gekopieerd naar '", path.ipsm.dropbox.packages, "'."))

        } else {

                warning("\nVersie General Toolbox ", c.latest.built.version.generaltoolbox,
                        " was eerder al gekopieerd naar ", path.ipsm.dropbox.packages,
                        ", en heeft de reeds aanwezige file NIET overschreven.")
        }


###########################################################################################
# INSTALL PACKAGE
###########################################################################################

        # Zie .Rprofile middels 'usethis::edit_r_profile()' voor functies.
        # f_update_package_from_dropbox("generaltoolbox")
