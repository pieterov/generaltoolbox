#' @title Move (or copy) files between folders
#'
#' @description This function can be used to move (or copy) files from one folder (source) to another folder (destination).
#'
#' @author Pieter Overdevest
#'
#' @param c.path.source Full path to folder that holds files to be moved/copied.
#' @param c.path.destination Full path to folder where files should be moved/copied to. In case it does not exist,
#' it will be created. Though, the parent of the new folder must exist, else an error will be thrown.
#' @param v.file.to.move List of files in c.path.source to be moved/copied. Each value should be the filename plus its
#' extension. By default the value is NULL, resulting in the files to be moved/copied.
#' @param c.date.treshold In case given - e.g., 'today() - 60' - files older than this date will be moved/copied. The date
#' is derived from what is given in the filename. By default the value is NULL, resulting in the files to be moved/copied.
#' @param b.delete.from.source Should we move or copy the files? By default the value is TRUE, effectively moving the
#' files, and when set to FALSE, effectively copying the files.
#' @param b.overwrite Should we overwrite files in case they are already present in the destination folder? By default
#' the value is TRUE. In case, the value is set to FALSE, files will not be overwritten, if applicable.
#' @param b.save.file.list Should we save the list of moved/copied files to an Excel file? The default is set to FALSE.
#'
#' @returns A data frame listing the moved/copied files.
#'
#' @details In case files from the source folder already occur in the destination folder, they will not be removed from
#' the source folder when b.overwrite has been set to FALSE and b.delete.from.source has been set to TRUE. This is to
#' prevent loosing the concerned files.
#'
#' @export
#'
#' @examples
#' df.move <- f_move_files(c.path.source = "...", c.path.destination = "...")

#################################################################################
# FUNCTION.
#################################################################################

        f_move_files = function(

                c.path.source,
                c.path.destination,
                v.file.to.move              = NULL,
                c.date.treshold             = NULL,
                b.delete.from.source        = TRUE,
                b.overwrite                 = TRUE,
                b.save.file.list            = FALSE
        ) {


#################################################################################
# TEST!
#################################################################################

        # ALTIJD
        # v.file.to.move              = NULL
        # c.date.treshold             = NULL
        # b.delete.from.source        = TRUE
        # b.overwrite                 = TRUE
        # b.save.file.list            = FALSE

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

        #  Move JPGs
        # c.path.source        = paste0("/Users/home/TEMP/", c.folder)
        # c.path.destination   = paste0("/Users/home/TEMP/", c.folder, " - ", i)
        # v.file.to.move       = df.file %>% filter(subbatch == i) %>% pull(file.name.ext)
        # c.date.treshold      = NULL
        # b.delete.from.source = FALSE
        # b.overwrite          = TRUE

        # Set 5
        # c.path.source       = "/Users/macstudio/Downloads/test_from"
        # c.path.destination  = "/Users/macstudio/Downloads/test_to"
        # v.file.to.move      = c("file_abc.png", "file_def.png")


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

        c.file.name <- paste(

                c.path.source      %>% gsub("^/|/$", "", .) %>% str_split(pattern = "/") %>% unlist() %>% tail(2) %>% paste(collapse = "_"),

                "-",

                c.path.destination %>% gsub("^/|/$", "", .) %>% str_split(pattern = "/") %>% unlist() %>% tail(2) %>% paste(collapse = "_")
        )


#################################################################################
# MAIN BODY
#################################################################################

        # Files move from the source folder to the destination folder.

        #############################
        # SOURCE FOLDER
        #############################

        df.file.source.before <- f_get_filenames_in_folder(c.path = c.path.source) %>%

                rename(full.path.source = full.path)


        #############################
        # DESTINATION FOLDER
        #############################

        # Files in the destination folder.
        df.file.destination.before <- f_get_filenames_in_folder(c.path = c.path.destination) %>%

                rename(full.path.destination = full.path)


        #############################
        # DF.FILE.TO.MOVE
        #############################

        # Starting list of files to move; equal to all files in the source folder.
        df.file.to.move <- df.file.source.before

        # Filter by DATE - if applicable.
        if(!is.null(c.date.treshold)) {

                df.file.to.move <- df.file.to.move %>%

                        filter(
                                date.in.file.name < as_date(c.date.treshold)
                        )
                }


        # Filter by FILENAMES - if applicable.
        if(!is.null(v.file.to.move)) {

                df.file.to.move <- df.file.to.move %>%

                        filter(

                               file.name.ext %in% v.file.to.move
                        )
                }


        # Flag files are already in the destination folder.
        df.file.to.move <- df.file.to.move %>%

                mutate(in.destination.before = {file.name.ext %in%

                                df.file.destination.before$file.name.ext})


        # Als een of meer bestanden al voorkomen in de destination folder
        # dan worden deze gevlagd in df.file.to.move, middels 'in.destination.before'.
        if(any(df.file.to.move$in.destination.before)) {

                cat(paste0(

                        "The following ",

                        sum(df.file.to.move$in.destination.before),

                        " files already exist in the destination folder:\n\n",

                        c.path.destination, "\n\n",

                        paste(
                                df.file.to.move %>% filter(in.destination.before) %>% pull(file.name.ext),
                                collapse = "\n"
                        ),

                        "\n\nSince, 'b.overwrite' is ", b.overwrite, ", these files in the destination folder ",
                        "will ", ifelse(!b.overwrite, "NOT", ""), "be overwritten. ",

                        ifelse(
                                b.delete.from.source,

                                "They will not be removed from the source folder, even though 'b.delete.from.source' was set to TRUE.\n\n",

                                "\n\n"
                        )

                ))


        }


        #############################################################################################
        # WE GAAN FILES KOPIEREN NAAR DE DESTINATION FOLDER!
        #############################################################################################

        cat("\n##############\nCOPYING FILES:\n##############\n\n")

        # Eerst kopieren we de bestanden die vooraf NIET in de destination folder voorkwamen.
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
                        file.copy(

                                from      = df.temp$full.path.source[i],
                                to        = paste0(c.path.destination, "/", df.temp$file.name.ext[i]),
                                copy.mode = TRUE,
                                copy.date = TRUE
                        )
                }

                cat("\n\n")
        }

        # Vervolgens, indien van toepassing, kopieren we alleen bestanden naar de destination die
        # vooraf WEL in de destination voorkwamen, als b.overwrite is TRUE.
        if(any(df.file.to.move$in.destination.before) & b.overwrite) {

                dummy1 <- 1

                df.temp <- df.file.to.move %>% filter(in.destination.before)

                cat(
                        "The following", nrow(df.temp),
                        "files - also present in the destination folder - were copied (overwritten) from:\n\n",
                        c.path.source, "\n\nto:\n\n", c.path.destination, "\n"
                        )

                for (i in seq(nrow(df.temp))) { # i <- 1

                        cat("\nCopying (", i, "of", nrow(df.temp), "): ", df.temp$file.name.ext[i])

                        # We overschrijven altijd, omdat we in het geval we dubbele files niet willen overschrijven, we deze
                        # hierboven verwijderen, zie b.overwrite.
                        file.copy(

                                from      = df.temp$full.path.source[i],
                                to        = paste0(c.path.destination, "/", df.temp$file.name.ext[i]),
                                copy.mode = TRUE,
                                copy.date = TRUE,
                                recursive = FALSE,
                                overwrite = TRUE
                        )
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
        df.file.destination.after <- f_get_filenames_in_folder(c.path = c.path.destination) %>%

                rename(full.path.destination = full.path)


        #############################
        # DF.FILE.TO.MOVE
        #############################

        # Bepaal van de bestanden in df.file.to.move of ze aangetroffen zijn in de destination folder.
        df.file.to.move <- df.file.to.move %>%

                mutate(
                        in.destination.after = {file.name.ext %in%

                                (df.file.destination.after %>% pull(file.name.ext))}
                        )



        # Error check
        if(!all(df.file.to.move$in.destination.after)) {

                warning("After copying, the following file(s) were not observed in the destination folder:\n\n",

                        paste(
                                df.file.to.move %>%
                                        filter(!in.destination.after) %>%
                                        pull(file.name.ext),

                                collapse = "\n")
                        )
        }



        #############################################################################################
        # WE GAAN FILES VERWIJDEREN UIT DE SOURCE FOLDER!
        #############################################################################################

        cat("\n##############\nREMOVING FILES:\n##############\n\n")

        # Initialize
        n.row.temp <- ifelse(

                b.overwrite,

                # Indien overwrite is TRUE, verwijderen we alle bestanden genoemd in df.file.to.move.
                df.file.to.move %>% nrow(),

                # Indien overwrite is FALSE, verwijderen we alleen de bestanden genoemd in df.file.to.move,
                # die vooraf nog niet in de destination folder voorkwamen, anders zouden we die verliezen.
                # We hebben ze immers niet naar de destination gekopieerd.
                df.file.to.move %>% filter(!in.destination.before) %>% nrow()
        )


        # Alleen als b.delete.from.source gelijk is aan TRUE, dan verwijderen we eventuele bestanden
        # uit de source folder.
        if(b.delete.from.source & n.row.temp > 0) {

                # Scenario 'b.overwrite = TRUE':
                # We verwijderen alle bestanden in df.file.to.move.
                if(b.overwrite) {

                        cat(
                                "The following", nrow(df.file.to.move), "file(s) were succesfully copied to",
                                "the destination folder (b.overwrite = TRUE). Now, all these files are removed",
                                "from the source folder:\n\n",
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

                # Scenario 'b.overwrite = FALSE':
                # We verwijderen alleen bestanden in df.file.to.move, die 'in.destination.before'
                # op FALSE hadden staan.
                if(!b.overwrite) {

                        # Files that were not in the destination folder before.
                        df.temp <- df.file.to.move %>% filter(!in.destination.before)

                        if(nrow(df.temp) > 0) {

                                cat(
                                        "The following", nrow(df.temp), "file(s) were not in the",
                                        "destination folder before, and were succesfully copied from",
                                        "the source folder to the destination folder. Now, these files",
                                        "are removed from the source folder:\n\n",
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
                                        "the source folder to the destination folder (b.overwrite = FALSE).",
                                        "These files are not removed from the source folder:\n\n",
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
        df.file.source.after <- f_get_filenames_in_folder(c.path = c.path.source) %>%

                rename(full.path.source = full.path)


        #############################
        # DF.FILE.TO.MOVE
        #############################

        # Bepaal bestanden toegevoegd aan source folder. Mogelijk zijn er alleen bestanden
        # overschreven, dan is dit data frame ook leeg.
        df.file.to.move <- df.file.to.move %>%

                mutate(
                        in.source.after = {file.name.ext %in%

                                        (df.file.source.after %>% pull(file.name.ext))},

                        irregularity = FALSE
                )


        #############################
        # CHECKS
        #############################

        f_update_irregularity <- function(df.file.to.move, df.temp) {

                return(df.file.to.move %>%

                        mutate(
                                irregularity = ifelse(

                                        file.name.ext %in% df.temp$file.name.ext,
                                        TRUE,
                                        irregularity
                                )
                        )
                )
        }


        # Do the check
        if(b.delete.from.source) {

                if(b.overwrite) {

                        # ALL SHOULD HAVE BEEN REMOVED.
                        df.temp <- df.file.to.move %>% filter(in.source.after)

                        if(nrow(df.temp) > 0) {

                                df.file.to.move <- f_update_irregularity(df.file.to.move, df.temp)

                                warning(

                                        "We expect ALL file(s) to have been removed from the source folder ",
                                        "(b.delete.from.source = TRUE, b.overwrite = TRUE). However, the following ",
                                        "files have NOT been removed from the source folder:\n\n",

                                        paste(df.temp %>% pull(file.name.ext), collapse = "\n\n")
                                )
                        }

                } else {

                        # Onterecht niet verwijderd uit SOURCE folder.
                        df.temp <- df.file.to.move %>% filter(!in.destination.before, in.source.after)

                        if(nrow(df.temp) > 0) {

                                df.file.to.move <- f_update_irregularity(df.file.to.move, df.temp)

                                warning(
                                        "We expect THOSE files(s) to have been removed from the source folder that ",
                                        "were NOT in the destination folder before (b.delete.from.source = TRUE, ",
                                        "b.overwrite = FALSE). However, the following ",
                                        "files have NOT been removed from the source folder:\n\n",

                                        paste(df.temp %>% pull(file.name.ext), collapse = "\n\n")
                                )
                        }


                        # Onterecht verwijderd uit SOURCE folder.
                        df.temp <- df.file.to.move %>% filter(in.destination.before, !in.source.after)

                        if(nrow(df.temp) > 0) {

                                df.file.to.move <- f_update_irregularity(df.file.to.move, df.temp)

                                warning(
                                        "We expect THOSE files(s) to have NOT been removed from the source folder that ",
                                        "were in the destination folder before (b.delete.from.source = TRUE, ",
                                        "b.overwrite = FALSE). However, the following ",
                                        "files have been removed from the source folder:\n\n",

                                        paste(df.temp %>% pull(file.name.ext), collapse = "\n\n")
                                )
                        }
                }

        } else {

                # NONE SHOULD HAVE BEEN REMOVED.
                df.temp <- df.file.to.move %>% filter(!in.source.after)

                if(nrow(df.temp) > 0) {

                        df.file.to.move <- f_update_irregularity(df.file.to.move, df.temp)

                        warning(
                                "We expect NONE of the file(s) to have been removed from the source folder ",
                                "(b.delete.from.source = FALSE). However, the following ",
                                "files have been removed from the source folder:\n\n",

                                paste(df.temp %>% pull(file.name.ext), collapse = "\n\n")
                        )
                }
        }


        # Further comms.
        if(nrow(df.file.to.move) > 0) {

                cat(paste0(

                        "We observed ",

                        ifelse(
                                any(df.file.to.move$irregularity),
                                "",
                                "NO "
                        ),

                        "irregulaties while ", ifelse(b.delete.from.source, "moving", "copying"),
                        " file(s) from the source folder:\n\n'",
                        c.path.source, "'\n\n into the destination folder:\n\n'", c.path.destination, "'\n\n",

                        ifelse(
                                any(df.file.to.move$irregularity),

                                paste0(
                                        "This concerns the following files: ",
                                        df.file.to.move %>% filter(irregularity) %>% pull(file.name.ext) %>% f_paste(b.quotation = TRUE),
                                        ".\n\n"
                                ),

                                ""
                        ),

                        ifelse(
                                b.save.file.list,

                                paste0(
                                        "An Excel table named '", c.file.name, ".xlsx' listing all ",
                                        ifelse(b.delete.from.source, "moved", "copied"), " file(s) will be ",
                                        "saved into folder",

                                        ifelse(
                                                any(df.file.to.move$irregularity),
                                                " (see colum 'irregularity')",
                                                ""
                                        ),

                                        ":\n\n'", path.data, "'\n\n",

                                        "The same list is returned from this function.\n\n"
                                ),

                                paste0(
                                        "A list with all ", ifelse(b.delete.from.source, "moved", "copied"),
                                        " file(s) is returned from this function.\n\n"
                                )
                        )
                ))
        }


#################################################################################
# Save data
#################################################################################

        if(nrow(df.file.to.move) > 0 & b.save.file.list) {

                # Sla data op.
                f_write_data_to_file(

                        x             = df.file.to.move,
                        v.path        = path.data,
                        c.file.string = c.file.name,
                        v.add.time    = TRUE
                )
        }


#################################################################################
# RETURN
#################################################################################

        return(df.file.to.move)

}
