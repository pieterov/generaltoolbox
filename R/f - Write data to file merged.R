##############################################################################################
# NAME:         FUNCTION - WRITE DATA TO FILE MERGED
# AUTHOR:       Pieter Overdevest
##############################################################################################

        f_write_data_to_file_merged <- function(

                  df.input,
                  c.path,
                  c.file.string,
                  c.sheet.name   = "Sheet1",
                  c.file.type,
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

                        interval = difftime(

                                now(), timestamp.file, units = "days"
                        )
                ) %>%

                filter(
                        interval < n.day.max
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

