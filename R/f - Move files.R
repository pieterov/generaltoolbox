#################################################################################
# NAME:         FUNCTION - Move files.
# AUTHOR:       Pieter Overdevest.
# DESCRIPTION:  Move files.
#################################################################################

        f_move_files = function(

                c.path.source,
                c.path.destination,
                v.file.to.move              = NULL,
                c.file.to.move.filter.type  = "regex", # 'regex' is default, alternative is 'file.name.ext'
                c.date.treshold             = NULL,
                b.check.md5                 = TRUE,
                b.delete.from.source        = TRUE,
                b.overwrite                 = TRUE
                ) {


#################################################################################
# TEST!
#################################################################################

        # ALTIJD
        # v.file.to.move              = NULL
        # c.date.treshold             = NULL
        # b.check.md5                 = TRUE
        # b.delete.from.source        = TRUE
        # b.overwrite                 = TRUE

        # # TESTEN
        # v.file.to.move     = df.files.temp$file.name.ext
        # c.path.source      = c.path
        # c.path.destination = paste0(c.path, "Archive")

        # # TESTEN
        # c.path.source      <- paste0(path.prive, "Bieb/")
        # c.path.destination <- paste0(path.prive, "Bieb/_Archive/")
        # c.date.treshold    <- (today() - 100)

        # c.path.source      = path.data.rds
        # c.path.destination = paste0(path.data.source, "RDS/")
        # c.date.treshold    = (today() - 70)

        # Set 3
        # c.path.source      = paste0(path.prive, "Bieb/")
        # c.path.destination = paste0(path.prive, "Bieb/_Archive/")
        # c.date.treshold    = "2021-05-01"

        # Set 4
        # c.path.source      = path.data.rds
        # c.path.destination = paste0(path.data.source, "RDS/")
        # c.date.treshold    = (today() - 50)
        # b.check.md5        = FALSE

        #  Move JPGs
        # c.path.source        = paste0("/Users/home/TEMP/", c.folder)
        # c.path.destination   = paste0("/Users/home/TEMP/", c.folder, " - ", i)
        # v.file.to.move       = df.file %>% filter(subbatch == i) %>% pull(file.name.ext)
        # c.date.treshold      = NULL
        # b.check.md5          = FALSE
        # b.delete.from.source = FALSE
        # b.overwrite          = TRUE


#################################################################################
# ERROR CHECK
#################################################################################

        cat("\n##############\nINTRODUCTION:\n##############\n\n")

        if(!dir.exists(c.path.source)) {

                stop("The source folder '", c.path.source, "' does not exist!\n")
        }


        if(!dir.exists(c.path.destination)) {

                # Create 'archive' folder when it does not already exist.
                dir.create(c.path.destination, showWarnings = FALSE)

                warning("The destination folder '", c.path.destination, "' does not exist, so it has been created.\n")
        }



#################################################################################
# INITIALIZE
#################################################################################

        # Determine filename for df.file.to.move that will be saved in path.data,
        # for reference.
        f_get_last_two_folders_in_path <- function(c.path) {

                return(
                        c.path %>%
                                gsub("^/|/$", "", .) %>%
                                str_split(pattern = "/") %>%
                                unlist() %>%
                                tail(2) %>%
                                paste(collapse = "_")
                )
        }


        c.file.name <- paste(

                f_get_last_two_folders_in_path(c.path.source),
                "-",
                f_get_last_two_folders_in_path(c.path.destination)
                )


        # Location where c.file.name will be saved.
        c.folder.name <- path.data


#################################################################################
# MAIN BODY
#################################################################################

        # Files move from the source folder to the destination folder.

        #############################
        # SOURCE FOLDER
        #############################

        df.file.source.before <- f_get_filenames_in_folder(c.path       = c.path.source,
                                                           b.return.md5 = b.check.md5) %>%

                rename(full.path.source = full.path)


        if(b.check.md5) {

                df.file.source.before <- df.file.source.before %>%

                        rename(file.md5.source  = file.md5) %>%

                        add_count(file.md5.source, name = "n.md5.source")
                }


        #############################
        # DESTINATION FOLDER
        #############################

        # Files in the destination folder.
        df.file.destination.before <- f_get_filenames_in_folder(c.path       = c.path.destination,
                                                                b.return.md5 = b.check.md5) %>%

                rename(full.path.destination = full.path)


        if(b.check.md5) {

                df.file.destination.before <- df.file.destination.before %>%

                        rename(file.md5.destination  = file.md5) %>%

                        add_count(file.md5.destination, name = "n.md5.destination")
                }


        #############################
        # DF.FILE.TO.MOVE
        #############################

        # Starting list of files to move; equal to all files in the source folder.
        df.file.to.move <- df.file.source.before

        # Filter by DATE.
        if(!is.null(c.date.treshold)) {

                df.file.to.move <- df.file.to.move %>%

                        filter(
                                date.in.file.name < as_date(c.date.treshold)
                        )
                }


        # Filter by optional FILENAMES.
        if(!is.null(v.file.to.move)) {

                df.file.to.move <- df.file.to.move %>%

                        purrr::when(

                                # Filter based on regex, though without ^ amd $.
                                c.file.to.move.filter.type == "regex" ~

                                        filter(.,

                                                grepl(
                                                        f_paste_regex(

                                                                v.file.to.move,
                                                                c.pre = "",
                                                                c.post = ""
                                                        ),

                                                        file.name.ext
                                                )
                                        ),

                                # Filter based on file.name.ext
                                c.file.to.move.filter.type == "file.name.ext" ~

                                        filter(.,

                                               file.name.ext %in% v.file.to.move
                                        )
                        )
                }


        # Filter by optional FILENAMES - in het geval v.file.to.move leeg is.
        if(!is.null(v.file.to.move) & length(v.file.to.move) == 0) {

                df.file.to.move <- df.file.to.move %>%

                        head(0)

        }


        # Flag files are already in the destination folder.
        df.file.to.move <- df.file.to.move %>%

                mutate(in.destination.before = {file.name.ext %in%

                                df.file.destination.before$file.name.ext})


        # Als een of meer bestanden al voorkomen in de destination folder
        # dan worden deze gevlagd in df.file.to.move, middels 'in.destination.before'.
        if(any(df.file.to.move$in.destination.before)) {

                cat(
                        "The following",
                        sum(df.file.to.move$in.destination.before),
                        "files already exist in the destination folder:\n\n",
                        c.path.destination, "\n\n",
                        paste(df.file.to.move %>% filter(in.destination.before) %>% pull(file.name.ext),
                              collapse = "\n")
                        )

                if(b.overwrite) {

                        cat(
                                "\n\nSince, 'b.overwrite' is TRUE, these will overwrite those in the",
                                "destination folder and be removed from the source folder.\n\n"
                                )

                } else {

                        cat(
                                "\n\nSince, 'b.overwrite' is FALSE, these will not move to the",
                                "destination folder and remain in the source folder.\n\n"
                                )
                        }
                }


        # Check of er bestanden met zelfde MD5 voorkomen in source en source folders.
        if(b.check.md5) {

                if(any(df.file.source.before$n.md5.source > 1)) {

                        df.temp <- df.file.source.before %>%

                                filter(n.md5.source > 1) %>%

                                select(file.name.ext, file.md5.source) %>%

                                arrange(file.md5.source) %>%

                                as.data.frame()


                        # Sla data op.
                        f_write_data_to_file(

                                x             = df.temp,
                                v.path        = c.folder.name,
                                c.file.string = paste0(c.file.name, " - dubbel md5 - source"),
                                v.add.time    = TRUE
                                )


                        print(df.temp)


                        stop("The files listed above in the source folder:\n\n",
                             c.path.source, "\n\nhave the same MD5 value!")
                }


                if(any(df.file.destination.before$n.md5.destination > 1)) {

                        df.temp <- df.file.destination.before %>%

                                filter(n.md5.destination > 1) %>%

                                select(file.name.ext, file.md5.destination) %>%

                                arrange(file.md5.destination) %>%

                                as.data.frame()


                        # Sla data op.
                        f_write_data_to_file(

                                x             = df.temp,
                                v.path        = c.folder.name,
                                c.file.string = paste0(c.file.name, " - dubbel md5 - destination"),
                                )


                        print(df.temp)


                        stop("The files listed above in the destination folder:\n\n",
                             c.path.destination, "\n\nhave the same MD5 value!")
                }
        }


        #############################################################################################
        # WE GAAN FILES KOPIEREN NAAR DE DESTINATION FOLDER!
        #############################################################################################

        cat("\n##############\nCOPYING FILES:\n##############\n\n")

        # Als een of meer files NIET in de destination folder voorkomen.
        if(any(!df.file.to.move$in.destination.before)) {

                dummy1 <- 1

                df.temp <- df.file.to.move %>% filter(!in.destination.before)

                cat(
                        "The following", nrow(df.temp),
                        "file(s) - not present in the destination folder - were copied from:\n\n",
                        c.path.source, "\n\nto:\n\n", c.path.destination, "\n"
                        )

                for (i in seq(nrow(df.temp))) { # i <- 1

                        cat("\nCopying (", i, "of", nrow(df.temp), "): ", df.temp$file.name.ext[i])

                        # Het maakt niet zo veel uit of we overwrite op TRUE/FALSE zetten.
                        # We kopieren bestanden die niet in de destination folder voorkomen.
                        file.copy(from      = df.temp$full.path.source[i],
                                  to        = paste0(c.path.destination, "/", df.temp$file.name.ext[i]),
                                  copy.mode = TRUE,
                                  copy.date = TRUE)
                }

                cat("\n\n")
        }

        # Als een of meer files WEL in de destination folder voorkomen en b.overwrite is TRUE.
        if(any(df.file.to.move$in.destination.before) & b.overwrite) {

                dummy1 <- 1

                df.temp <- df.file.to.move %>% filter(in.destination.before)

                cat(
                        "The following", nrow(df.temp),
                        "files - also present in the destination folder - were copied from:\n\n",
                        c.path.source, "\n\nto:\n\n", c.path.destination, "\n"
                        )

                for (i in seq(nrow(df.temp))) { # i <- 1

                        cat("\nCopying (", i, "of", nrow(df.temp), "): ", df.temp$file.name.ext[i])

                        # We overschrijven altijd, omdat we in het geval we dubbele files niet willen overschrijven, we deze
                        # hierboven verwijderen, zie b.overwrite.
                        file.copy(from      = df.temp$full.path.source[i],
                                  to        = paste0(c.path.destination, "/", df.temp$file.name.ext[i]),
                                  copy.mode = TRUE,
                                  copy.date = TRUE,
                                  recursive = FALSE,
                                  overwrite = TRUE)
                }

                cat("\n\n")
        }

        # In case there are no files to copy.
        if(!exists("dummy1")) {

                cat("There are no files to copy, because:\n",
                    "(1) v.file.to.move was empty to begin with.\n",
                    "(2) v.file.to.move has no matches with the files in the source folder.\n",
                    "(3) c.date.treshold is too stringent.\n",
                    "(4) b.overwrite is equal to FALSE and all files already exist in the destination folder.\n",
                    "In all cases, NO files are removed from the source folder.\n\n")
        }


        #############################
        # DESTINATION FOLDER
        #############################

        # Bepaal bestanden in de destination folder after copying.
        df.file.destination.after <- f_get_filenames_in_folder(c.path       = c.path.destination,
                                                               b.return.md5 = b.check.md5) %>%

                rename(full.path.destination = full.path)


        if(b.check.md5) {


                df.file.destination.after <- df.file.destination.after %>%

                        rename(file.md5.destination  = file.md5) %>%

                        add_count(file.md5.destination, name = "n.md5.destination")
                }


        #############################
        # DF.FILE.TO.MOVE
        #############################

        # Bepaal bestanden toegevoegd aan destination folder.
        if(b.check.md5) {

                # Op basis van md5.
                df.file.to.move <- df.file.to.move %>%

                        mutate(
                                in.destination.after = {file.md5.source %in%

                                        (df.file.destination.after %>% pull(file.md5.destination))}
                                )
        } else {

                # Op basis van file.name.ext
                df.file.to.move <- df.file.to.move %>%

                        mutate(
                                in.destination.after = {file.name.ext %in%

                                        (df.file.destination.after %>% pull(file.name.ext))}
                                )
        }


        # Error check
        if(b.overwrite & !all(df.file.to.move$in.destination.after)) {

                warning("We verwachten dat alle bestanden naar de destination folder gekopieerd zou ",
                        "worden (b.overwrite = TRUE). Echter, de volgende bestanden zijn niet naar de ",
                        "destination folder gekopieerd:\n\n",
                        paste(
                                df.file.to.move %>%
                                        filter(!in.destination.after) %>%
                                        pull(file.name.ext),

                                collapse = "\n")
                        )
        }

        if(!b.overwrite & !all(df.file.to.move %>% filter(!in.destination.before) %>% pull(in.destination.after))) {

                warning("We verwachten niet dat alle bestanden naar de destination folder gekopieerd zou",
                        "worden (b.overwrite = FALSE). Echter, de volgende bestanden - die vooraf niet",
                        "voorkwamen in de destination folder - zijn niet naar de",
                        "destination folder gekopieerd:\n\n",
                        paste(
                                df.file.to.move %>%
                                        filter(!in.destination.before & !in.destination.after) %>%
                                        pull(file.name.ext),

                                collapse = "\n")
                        )
        }


        #############################################################################################
        # WE GAAN FILES VERWIJDEREN UIT DE SOURCE FOLDER!
        #############################################################################################

        cat("\n##############\nREMOVING FILES:\n##############\n\n")


        # Alleen als b.delete.from.source gelijk is aan TRUE, dan verwijderen we de verplaatste files
        # uit de source folder. Zijn er bestanden om te verwijderen?
        if(b.delete.from.source & nrow(df.file.to.move) > 0) {

                # Scenario 'b.overwrite = TRUE': We verwijderen alle bestanden in df.file.to.move.
                if(b.overwrite) {

                        cat(
                                "The following", nrow(df.file.to.move), "file(s) were succesfully",
                                "copied to the destination folder. Now, all these files are removed",
                                "from the source folder (b.overwrite = TRUE):\n\n",
                                c.path.source, "\n")

                        for (i in seq(nrow(df.file.to.move))) { # i <- 1

                                cat("\nRemoving (", i, "of", nrow(df.file.to.move), "): ",
                                    df.file.to.move$file.name.ext[i])

                                file.remove(
                                        from = df.file.to.move$full.path.source[i]
                                )
                        }

                        cat("\n\n")
                }

                # Scenario 'b.overwrite = FALSE': We verwijderen alleen bestanden in df.file.to.move,
                # die 'in.destination.before' op FALSE hadden staan.
                if(!b.overwrite) {

                        # Files that were not in the destination folder before.
                        df.temp <- df.file.to.move %>% filter(!in.destination.before)

                        if(nrow(df.temp) > 0) {

                                cat(
                                        "The following", nrow(df.temp), "file(s) were not in the",
                                        "destination folder before, and were succesfully copied from",
                                        "the source folder to the destination folder. Now, these files",
                                        "are removed from the source folder (b.overwrite = FALSE):\n\n",
                                        c.path.source, "\n")

                                for (i in seq(nrow(df.temp))) { # i <- 1

                                        cat("\nRemoving (", i, "of", nrow(df.temp), "): ",
                                            df.temp$file.name.ext[i])

                                        file.remove(
                                                from = df.temp$full.path.source[i]
                                        )
                                }

                                cat("\n\n")
                        }

                        # Files that were in the destination folder before.
                        df.temp <- df.file.to.move %>% filter(in.destination.before)

                        if(nrow(df.temp) > 0) {

                                cat(
                                        "The following", nrow(df.temp), "file(s) were already in the",
                                        "destination folder before, and were not copied from",
                                        "the source folder to the destination folder. These files",
                                        "are not removed from the source folder (b.overwrite = FALSE):\n\n",
                                        c.path.source, "\n")

                                for (i in seq(nrow(df.temp))) { # i <- 1

                                        cat("\nNot removed (", i, "of", nrow(df.temp), "): ",
                                            df.temp$file.name.ext[i])
                                }

                                cat("\n\n")
                        }
                }
        }

        #############################################################################################
        # ZIJN DE BESTANDEN GOED VERWIJDERD?
        #############################################################################################

        cat("\n##############\nCHECKING FILES:\n##############\n\n")


        #############################
        # SOURCE FOLDER
        #############################

        # Bepaal bestanden in de source folder.
        df.file.source.after <- f_get_filenames_in_folder(c.path       = c.path.source,
                                                          b.return.md5 = b.check.md5) %>%

                rename(full.path.source = full.path)


        if(b.check.md5) {

                df.file.source.after <- df.file.source.after %>%

                        rename(file.md5.source  = file.md5) %>%

                        add_count(file.md5.source, name = "n.md5.source")
                }


        #############################
        # DF.FILE.TO.MOVE
        #############################

        # Bepaal bestanden toegevoegd aan source folder. Mogelijk zijn er alleen bestanden
        # overschreven, dan is dit data frame ook leeg.
        if(b.check.md5) {

                # Bepaal bestanden toegevoegd aan source folder, op basis van md5.
                df.file.to.move <- df.file.to.move %>%

                        mutate(
                                in.source.after = {file.md5.source %in%

                                                (df.file.source.after %>% pull(file.md5.source))}
                        )
        } else {

                # Op basis van file.name.ext
                df.file.to.move <- df.file.to.move %>%

                        mutate(
                                in.source.after = {file.name.ext %in%

                                                (df.file.source.after %>% pull(file.name.ext))}
                        )
        }


        # Onterecht niet verwijderd - scenario 1.
        if(b.delete.from.source & b.overwrite &
           any(df.file.to.move %>% pull(in.source.after))) {

                dummy2 <- 1

                warning("We verwachten dat alle bestanden uit de source folder verwijderd zouden ",
                        "worden (b.delete.from.source = TRUE, b.overwrite = TRUE). Echter, de volgende ",
                        "bestanden zijn niet uit de source folder verwijderd:\n\n",
                        paste(
                                df.file.to.move %>%
                                        filter(in.source.after) %>%
                                        pull(file.name.ext),

                                collapse = "\n\n")
                )
        }


        # Onterecht niet verwijderd - scenario 2.
        if(b.delete.from.source & !b.overwrite &
           any(df.file.to.move %>% filter(!in.destination.before) %>% pull(in.source.after))) {

                dummy2 <- 1

                warning("We verwachten dat de bestanden uit de source folder - die niet in de ",
                        "destination folder voorkomen - verwijderd zouden ",
                        "worden (b.delete.from.source = TRUE, b.overwrite = FALSE). Echter, de volgende ",
                        "bestanden zijn niet uit de source folder verwijderd:\n\n",
                        paste(
                                df.file.to.move %>%
                                        filter(!in.destination.before,
                                               in.source.after) %>%
                                        pull(file.name.ext),

                                collapse = "\n\n")
                )
        }


        # Onterecht verwijderd - scenario 1.
        if(!b.delete.from.source &
           any(!df.file.to.move %>% pull(in.source.after))) {

                dummy2 <- 1

                warning("We verwachten dat er geen bestanden uit de source folder zouden worden verwijderd ",
                        "(b.delete.from.source = FALSE). Echter, de volgende ",
                        "bestanden zijn uit de source folder verwijderd:\n\n",
                        paste(
                                df.file.to.move %>%
                                        filter(!in.source.after) %>%
                                        pull(file.name.ext),

                                collapse = "\n\n")
                )
        }


        # Onterecht verwijderd - scenario 2.
        if(b.delete.from.source & !b.overwrite &
           any(!df.file.to.move %>% filter(in.destination.before) %>% pull(in.source.after))) {

                dummy2 <- 1

                warning("We verwachten dat de bestanden uit de source folder - die in de ",
                        "destination folder voorkomen - niet verwijderd zouden ",
                        "worden (b.delete.from.source = TRUE, b.overwrite = FALSE). Echter, de volgende ",
                        "bestanden zijn uit de source folder verwijderd:\n\n",
                        paste(
                                df.file.to.move %>%
                                        filter(in.destination.before,
                                               !in.source.after) %>%
                                        pull(file.name.ext),

                                collapse = "\n\n")
                )
        }


        if(!exists("dummy2") & nrow(df.file.to.move) > 0) {

                cat(paste0("Alle bestanden zijn - waar van toepassing - correct verwijderd uit de source folder:\n\n",
                    c.path.source , "\n\nDe tabel met verplaatste bestanden is opgeslagen in Excel file '",
                    c.file.name, "' in folder:\n\n", c.folder.name, "\n"))
        }


#################################################################################
# Save data
#################################################################################

        if(nrow(df.file.to.move) > 0) {

                # Sla data op.
                f_write_data_to_file(

                        x             = df.file.to.move,
                        v.path        = c.folder.name,
                        c.file.string = c.file.name,
                        v.add.time    = TRUE
                )
        }

        # Print
        cat("\n\n")
        print(df.file.to.move %>% select(file.name.ext, in.destination.after, in.source.after) %>% as.data.frame())


#################################################################################
# RETURN
#################################################################################

        return(df.file.to.move)

}
