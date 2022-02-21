##############################################################################################
# NAME:         FUNCTION - WRITE DATA TO FILE MERGED
# AUTHOR:       Pieter Overdevest
##############################################################################################

        f_write_data_to_file_merged <- function(

                  df.input,
                  c.file.string,
                  c.path,
                  c.file.type,
                  n.day.max = 100000
                  ) {


##############################################################################################
# TEST
##############################################################################################

        # TEST
        # df.input       = data.frame(x =  c(11,11), y = c(11, 11))
        # c.file.string  = "Test"
        # c.path         = path.data
        # c.file.type    = "xls"
        # n.day.max = 2


##############################################################################################
# PROCESS
##############################################################################################

        # Determine files that need merging
        df.files.temp <- f_get_filenames_in_folder(

                        c.path       = c.path,
                        c.file.type  = "xls"
                ) %>%

                filter(grepl("Test", file.name)) %>%

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
                c.path                   = c.path,
                b.exact.match            = TRUE,
                b.clean.up.header.names  = FALSE
        ) %>%

                rbind(df.input)


        # Create 'archive' folder when it does not already exist.
        dir.create(paste0(c.path, "Archive"), showWarnings = FALSE)

        # Move concerned source files to Archive.
        f_move_files(
                v.file.to.move     = df.files.temp$file.name,
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

        f_write_data_to_file(

                x             = df.data.temp,
                v.path        = c.path,
                c.file.string = c.file.string,
                v.add.time    = TRUE,

                # Determine where to save the data to.
                v.xls         = v.xls,
                v.csv         = v.csv,
                v.txt         = v.txt,
                v.delim       = v.delim,
                v.rds         = v.rds,
                v.fst         = v.fst
        )

        }

