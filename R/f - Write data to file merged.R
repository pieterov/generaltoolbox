#' @title Reads files, appends data and writes merged file
#'
#' @description Reads, appends and writes data to merged file. Function looks for files with same core name in folder -
#' usually only the date (and time) differs. The files can be filtered on their age (number of days). Only files older
#' than n.day.max are merged and written to a new file. New data can be appended through df.input. The files that were
#' merged are moved to an Archive\ folder.
#'
#' @author Pieter Overdevest
#'
#' @param df.input Data frame with data to append to merged data,
#' @param c.path Path where file(s) are searched for.
#' @param c.file.string Core of the filename of the files to be merged. The same is used for the file that is written.
#' @param c.file.type File type of files to merge. The merged file will also be saved in this format.
#' @param c.sheet.name Sheetname containing the data to be merged. Is relevant in case of c.file.type being xls
#' (default: "Sheet1").
#' @param v.add.date,v.add.time Vector of booleans to specify whether data and/or time should be added.
#' This should be as long as x (default: TRUE and FALSE, resp.).
#' @param n.day.max Files newer than the number of days given in n.day.max are merged. The high default value essentially
#' means that all files are merged (default: 1000000).
#'
#' @returns Boolean whether the vector contains unique values.
#'
#' @details -
#'
#' @export
#'
#' @examples
#' f_write_data_to_file_merged(
#'
#'      df.input       = mtcars,
#'      c.path         = path.data,
#'      c.file.string  = "dummy file",
#'      c.sheet.name   = "Sheet1",
#'      c.file.type    = "xls",
#'      v.add.date     = TRUE,
#'      v.add.time     = FALSE,
#'      n.day.max      = 1000000
#' )

        #################################################################################
        # FUNCTION.
        #################################################################################

        f_write_data_to_file_merged <- function(

                  df.input,
                  c.path,
                  c.file.string,
                  c.file.type,
                  c.sheet.name   = "Sheet1",
                  v.add.date     = TRUE,
                  v.add.time     = FALSE,
                  n.day.max      = 1000000
                  ) {


##############################################################################################
# TEST
##############################################################################################

        # TEST
        # df.input       = data.frame(x = c(11,11), y = c(11, 11))
        # c.path         = path.data
        # c.file.string  = "Test"
        # c.sheet.name   = "Sheet1"
        # c.file.type    = "xls"
        # v.add.date     = TRUE
        # v.add.time     = FALSE
        # n.day.max      = 100

        # df.input      = df.datachamp.increment.change
        # c.file.string = "Increment Changes"
        # c.path        = paste0(path.datachamp.dropbox, "Increment Changes - ", str_to_title(c.update.type), "/")
        # c.file.type   = "xls"
        # n.day.max     = 7

        # df.input      = df.datachamp.change.log
        # c.file.string = "Change Log"
        # c.path        = paste0(path.datachamp.dropbox, "Change Log - ", str_to_title(c.update.type), "/")
        # c.file.type   = "xls"
        # n.day.max     = 7


##############################################################################################
# ERROR CHECK!
##############################################################################################

        if(length(v.add.date) != length(v.add.time)) {

                stop("Note, 'v.add.date' and 'v.add.time' must have the same length!")
        }


##############################################################################################
# PROCESS
##############################################################################################

        # Determine files that need merging
        df.files.temp <- f_get_filenames_in_folder(

                        c.path       = c.path,
                        c.file.type  = c.file.type
                ) %>%

                filter(grepl(paste0("- ", c.file.string), file.name)) %>%

                mutate(
                        # Add timestamp.
                        timestamp.file = file.name %>%

                                str_extract("[0-9 ]* - [0-9 ]*") %>%

                                ymd_hms(),

                        n.day         = difftime(

                                now(), timestamp.file, units = "days"
                        )
                ) %>%

                filter(
                        n.day < n.day.max
                )


        # Read concerned files
        df.data.temp <- f_read_data_from_file(

                v.file.string            = df.files.temp$file.name,
                c.file.type              = c.file.type,
                c.sheet.name             = c.sheet.name,
                c.path                   = c.path,
                b.exact.match            = TRUE,
                b.clean.up.header.names  = FALSE
        ) %>%

                rbind(df.input)


        # Create 'Archive' folder when it does not already exist.
        dir.create(paste0(c.path, "Archive"), showWarnings = FALSE)

        # Move concerned source files to Archive.
        f_move_files(
                v.file.to.move     = df.files.temp$file.name.ext,
                c.path.source      = c.path,
                c.path.destination = paste0(c.path, "Archive")
        )


        # Initialize in preparation for writing merged data.
        v.xls <- v.csv <- v.txt <- v.delim <- v.rds <- v.fst <- FALSE

        if(c.file.type == "xls")     v.xls     = TRUE
        if(c.file.type == "csv")     v.csv     = TRUE
        if(c.file.type == "txt")     v.txt     = TRUE
        if(c.file.type == "delim")   v.delim   = TRUE
        if(c.file.type == "rds")     v.rds     = TRUE
        if(c.file.type == "fst")     v.fst     = TRUE


##############################################################################################
# WRITE DATA
##############################################################################################

        for(i in seq_along(v.add.date)) {

                f_write_data_to_file(

                        x             = df.data.temp,
                        c.file.string = c.file.string,
                        v.path        = c.path,
                        v.sheet.name  = c.sheet.name,
                        v.add.date    = v.add.date[i],
                        v.add.time    = v.add.time[i],

                        # Determine where to save the data to.
                        v.xls         = v.xls,
                        v.csv         = v.csv,
                        v.txt         = v.txt,
                        v.delim       = v.delim,
                        v.rds         = v.rds,
                        v.fst         = v.fst
                )
        }

        }

