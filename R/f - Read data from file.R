#################################################################################
# NAME:         FUNCTION - Read data from file
# AUTHOR:       Pieter Overdevest.
# DESCRIPTION:  Read data from file.
#################################################################################

        f_read_data_from_file <- function(

                # String to search for in the filename. In case of Google Sheet this can be kept NULL.
                v.file.string            = NULL,

                # Options: 'xls', 'csv', 'tsv', 'txt', 'delim', 'rds', 'fst', 'xml', 'parquet', 'sqlite', 'shp', 'dbf', 'gs'.
                c.file.type,

                # Path where file should be searched for.
                c.path,

                # Exact match or not.
                b.exact.match            = FALSE,
                c.file.string.exclude    = NULL,

                # Show header names
                b.show.header.names      = FALSE,

                # In case of Excel file.
                c.sheet.name             = NULL, # was "Sheet1"
                n.slice.rows             = 0,

                # Needed in case c.file.type is equal to 'delim'.
                c.delim                  = NULL,

                # Do the data contain header names?
                b.col.names              = TRUE,

                # In case of a Excel or delimited file.
                # c = character, i = integer, n = number, d = double,
                # l = logical, D = date, T = date time, t = time,
                # ? = guess, or _/- to skip the column.
                #
                # Or put everything on character, using: cols(.default = "c")
                l.col.type               = NULL,

                # In case of SQLite file.
                c.table.name             = NULL,

                # Options: 'none', 'minimal', 'all'
                c.show.report            = "all",

                # Add auxiliry fields (mod.date, path.file)
                b.add.mod.date.path.file = FALSE,

                # Clean up header names?
                b.clean.up.header.names  = TRUE
                ) {


                ##############################################################################
                # TEST ONLY!!
                ##############################################################################

                # DEFAULT WAARDEN - HOUDEN!!
                # v.file.string            = NULL
                # b.exact.match            = FALSE
                # c.file.string.exclude    = NULL
                # b.show.header.names      = FALSE
                # c.sheet.name             = NULL
                # n.slice.rows             = 0
                # c.delim                  = ","
                # b.col.names              = TRUE
                # l.col.type               = NULL
                # c.table.name             = NULL
                # c.show.report            = "all"
                # b.add.mod.date.path.file = FALSE
                # b.clean.up.header.names  = TRUE

                # # SHAPEFILES
                # v.file.string       = "D1"
                # c.file.type         = "shp"
                # c.path              = paste0(path.data, "2021 06 24 - Data van Eric vd Ster - D01 borden/")
                # b.exact.match       = TRUE
                # b.show.header.names = TRUE
                # c.show.report       = "all"

                # v.file.string = y %>%
                #
                #         gsub("\\(", "\\\\(", .) %>%
                #         gsub("\\)", "\\\\)", .)
                #
                # c.file.type         = "sqlite"
                # c.path              = paste0(c.rootfolder.read, x, "/")
                # b.exact.match       = TRUE
                # c.table.name        = "dragers"
                # b.show.header.names = b.show.header.names
                # c.show.report       = c.show.report

                # Google Sheet
                # c.file.type             = "gs"
                # c.path                  = "https://docs.google.com/spreadsheets/d/1bqRGItDJUloEgkQtuOINOzM4Z45M7io99mykbPx5-D8"
                # c.sheet.name            = "FeatureDictionary"
                # b.clean.up.header.names = FALSE

                # delim
                # c.path      = c.url.source
                # c.file.type = "delim"
                # l.col.type  = cols(.default = "c")

                # DataChamp
                # v.file.string           = "2022 02 09 - DataChamp - Oletti Productfeed"
                # c.file.type             = "delim"
                # c.path                  = path.data
                # b.clean.up.header.names = FALSE

                # Developing f_read_data in case of xls and unknown sheet name.
                # v.file.string            = "10699885"
                # c.file.type              = "xls"
                # c.path                   = path.data
                # c.sheet.name             = "data"


                ##############################################################################
                # ERRROR CHECK
                ##############################################################################

                # Does c.file.type have a valid value?
                if(!c.file.type %in% c("xls", "csv", "tsv", "txt", "delim", "rds", "fst", "xml", "parquet", "sqlite", "shp", "dbf", "gs")) {

                        stop(paste0(

                                "Note, the value for c.file.type ('", c.file.type, "') is invalid. It must be one of, ",
                                "'xls', 'csv', 'tsv', 'txt', 'delim', 'rds', 'fst', 'xml', 'parquet', 'sqlite', 'shp', 'dbf', or 'gs'!"
                        ))
                }


                # Does c.delim have a value when needed?
                if(c.file.type == "delim" & is.null(c.delim)) {

                        stop(paste0(

                                "Note, you must provide a value for 'c.delim' in case you have chosen 'c.file.type' to be 'delim'!"
                        ))
                }


                ##############################################################################
                # INITIALIZE
                ##############################################################################

                # Define c.file.category and c.delim in case of c.file.type to be 'csv' or 'txt'.
                if(c.file.type %in% c("csv", "tsv", "txt")) {

                        c.file.category <- "delim"

                        c.delim <- case_when(

                                c.file.type == "csv" ~ ",",
                                c.file.type == "tsv" ~ "\t",
                                c.file.type == "txt" ~ " "
                        )

                } else {

                        c.file.category <- c.file.type
                }


                # Get latest file in case other file than Google Sheet is read.
                if(!is.null(v.file.string)) {

                        l.path.file <- lapply(v.file.string, function(c.file.string) {# c.file.string <- v.file.string[1]

                                f_get_latest_file(

                                        c.file.string         = c.file.string,
                                        c.file.type           = c.file.type,
                                        c.path                = c.path,
                                        b.exact.match         = b.exact.match,
                                        c.file.string.exclude = c.file.string.exclude,
                                        c.sheet.name          = c.sheet.name,
                                        c.show.report         = c.show.report
                                )
                        })

                } else {

                        l.path.file <- list(c.path)
                }


                # Define l.col.type in case c.file.type is 'gs' and l.col.type is NULL.
                if(c.file.type == "gs" & is.null(l.col.type)) {

                        l.col.type <- strrep("c", f_gs_col_number(c.path, c.sheet.name))
                }


                ##############################################################################
                # Read data based on file type.
                ##############################################################################

                # Get data.
                switch(c.file.category,

                       "xls" = {

                               # Read data from Excel file. Suppress warnings for not being able to match the right format.
                               suppressWarnings(

                                       l.data.object <- lapply(l.path.file,

                                               function(c.path.file) { # c.path.file <- l.path.file[[1]]

                                                       v.sheet.name <- readxl::excel_sheets(c.path.file)

                                                       if(is.null(c.sheet.name)) {

                                                                c.sheet.name <- v.sheet.name[1]

                                                       } else {
                                                                c.sheet.name <- v.sheet.name[grepl(c.sheet.name, v.sheet.name)]
                                                       }


                                                       df.temp <- readxl::read_xlsx(

                                                               path      = c.path.file,
                                                               sheet     = c.sheet.name,
                                                               skip      = n.slice.rows,
                                                               col_names = b.col.names,
                                                               col_types = l.col.type)


                                                       # Clean up header names when requested.
                                                       if(b.clean.up.header.names) {

                                                               df.temp <- df.temp %>%

                                                                       # Clean-up header names.
                                                                       f_clean_up_header_names()
                                                       }


                                                       # Add column with path name and date of modification.
                                                       if(b.add.mod.date.path.file) {

                                                               df.temp <- df.temp %>%

                                                                       mutate(
                                                                                path.file = c.path.file,
                                                                                mod.date  = file.mtime(c.path.file)
                                                                               )
                                                       }


                                                       # Show header names, if needed.
                                                       if (b.show.header.names) {

                                                               cat(basename(c.path.file), "\n")
                                                               cat(names(df.temp), "\n\n")

                                                       }


                                                       return(df.temp)
                                               })
                                       )
                       },


                       "delim" = {

                               # Read data from delimited file. Suppress warnings for not being able to match the right format.
                               suppressWarnings(

                                       l.data.object <- lapply(l.path.file,

                                                               function(c.path.file) { # c.path.file <- l.path.file[[1]]

                                                                       df.temp <- readr::read_delim(

                                                                               file      = c.path.file,
                                                                               delim     = c.delim,
                                                                               trim_ws   = TRUE,
                                                                               col_names = b.col.names,
                                                                               col_types = l.col.type
                                                                               )


                                                                       # Clean up header names when requested.
                                                                       if(b.clean.up.header.names) {

                                                                               df.temp <- df.temp %>%

                                                                                       # Clean-up header names.
                                                                                       f_clean_up_header_names()
                                                                       }

                                                                       # Add column with path name and date of modification.
                                                                       if(b.add.mod.date.path.file) {

                                                                               df.temp <- df.temp %>%

                                                                                       mutate(
                                                                                               path.file = c.path.file,
                                                                                               mod.date  = file.mtime(c.path.file)
                                                                                       )
                                                                       }


                                                                       # Show header names, if needed.
                                                                       if (b.show.header.names) {

                                                                               cat(basename(c.path.file), "\n")
                                                                               cat(names(df.temp), "\n\n")

                                                                       }

                                                                       return(df.temp)
                                                               })
                                       )
                       },


                       "rds" = {

                               # Read data from RDS file.
                               l.data.object <- lapply(l.path.file,

                                                       function(c.path.file) { # c.path.file <- l.path.file[[1]]

                                                               df.temp <- readRDS(file = c.path.file)


                                                               # Clean up header names when requested.
                                                               if(b.clean.up.header.names) {

                                                                       df.temp <- df.temp %>%

                                                                               # Clean-up header names.
                                                                               f_clean_up_header_names()
                                                               }


                                                               # Add column with path name and date of modification.
                                                               if(b.add.mod.date.path.file) {

                                                                       df.temp <- df.temp %>%

                                                                               mutate(
                                                                                       path.file = c.path.file,
                                                                                       mod.date  = file.mtime(c.path.file)
                                                                               )
                                                               }


                                                               # Show header names, if needed.
                                                               if (b.show.header.names) {

                                                                       cat(basename(c.path.file), "\n")
                                                                       cat(names(df.temp), "\n\n")

                                                               }

                                                               return(df.temp)
                                                       }
                               )
                       },

                       "fst" = {

                               # Read data from FST file.
                               l.data.object <- lapply(l.path.file,

                                                       function(c.path.file) { # c.path.file <- l.path.file[[1]]

                                                               df.temp <- read_fst(path = c.path.file)


                                                               # Clean up header names when requested.
                                                               if(b.clean.up.header.names) {

                                                                       df.temp <- df.temp %>%

                                                                               # Clean-up header names.
                                                                               f_clean_up_header_names()
                                                               }


                                                               # Add column with path name and date of modification.
                                                               if(b.add.mod.date.path.file) {

                                                                       df.temp <- df.temp %>%

                                                                               mutate(
                                                                                       path.file = c.path.file,
                                                                                       mod.date  = file.mtime(c.path.file)
                                                                               )
                                                               }


                                                               # Show header names, if needed.
                                                               if (b.show.header.names) {

                                                                       cat(basename(c.path.file), "\n")
                                                                       cat(names(df.temp), "\n\n")

                                                               }

                                                               return(df.temp)
                                                       }
                               )
                       },

                       "xml" = {

                               # Read data from XML file.
                               l.data.object <- lapply(l.path.file,

                                                       function(c.path.file) { # c.path.file <- l.path.file[[1]]

                                                               df.temp <- xmlParse(file = c.path.file) %>% xmlToDataFrame()


                                                               # Clean up header names when requested.
                                                               if(b.clean.up.header.names) {

                                                                       df.temp <- df.temp %>%

                                                                               # Clean-up header names.
                                                                               f_clean_up_header_names()
                                                               }


                                                               # Add column with path name and date of modification.
                                                               if(b.add.mod.date.path.file) {

                                                                       df.temp <- df.temp %>%

                                                                               mutate(
                                                                                       path.file = c.path.file,
                                                                                       mod.date  = file.mtime(c.path.file)
                                                                               )
                                                               }


                                                               # Show header names, if needed.
                                                               if (b.show.header.names) {

                                                                       cat(basename(c.path.file), "\n")
                                                                       cat(names(df.temp), "\n\n")

                                                               }

                                                               return(df.temp)
                                                       }
                               )
                       },


                       "parquet" = {

                               # Read data from PARQUET file.
                               l.data.object <- lapply(l.path.file,

                                                       function(c.path.file) { # c.path.file <- l.path.file[[1]]

                                                               df.temp <- read_parquet(file = c.path.file)


                                                               # Clean up header names when requested.
                                                               if(b.clean.up.header.names) {

                                                                       df.temp <- df.temp %>%

                                                                               # Clean-up header names.
                                                                               f_clean_up_header_names()
                                                               }


                                                               # Add column with path name and date of modification.
                                                               if(b.add.mod.date.path.file) {

                                                                       df.temp <- df.temp %>%

                                                                               mutate(
                                                                                       path.file = c.path.file,
                                                                                       mod.date  = file.mtime(c.path.file)
                                                                               )
                                                               }


                                                               # Show header names, if needed.
                                                               if (b.show.header.names) {

                                                                       cat(basename(c.path.file), "\n")
                                                                       cat(names(df.temp), "\n\n")

                                                               }

                                                               return(df.temp)
                                                       }
                               )
                       },


                       "sqlite" = {

                               # Check whether a table name is provided.
                               if (is.null(c.table.name)) stop(paste0("Missing c.table.name in function input."))

                               l.data.object <- lapply(l.path.file,

                                                       function(c.path.file) {

                                                               # c.path.file <- l.path.file[[2]]
                                                               #c.table.name <- "borden"
                                                               #c.path.file <- "D:/SQLITE MIN1/Wassenaar.sqlite"
                                                               #print(c.path.file)

                                                               # Connect to sqlite file.
                                                               con <- dbConnect(drv    = RSQLite::SQLite(),
                                                                                dbname = c.path.file)

                                                               # Check whether c.table.name is in the list of tables.
                                                               if (!c.table.name %in% dbListTables(con)) {

                                                                       # Disconnect from database.
                                                                       dbDisconnect(con)

                                                                       df.temp <- NULL

                                                                       warning(paste0(

                                                                               c.path.file," does not contain table '",
                                                                               c.table.name, "', the rest was read."
                                                                               ))

                                                               } else {

                                                                       # Extract table c.table.name and add column with path name and date
                                                                       # of modification.
                                                                       df.temp <- dbGetQuery(con, paste("select * from", c.table.name))


                                                                       # Clean up header names when requested.
                                                                       if(b.clean.up.header.names) {

                                                                               df.temp <- df.temp %>%

                                                                                       # Clean-up header names.
                                                                                       f_clean_up_header_names()
                                                                       }


                                                                       # Add column with path name and date of modification.
                                                                       if(b.add.mod.date.path.file) {

                                                                               df.temp <- df.temp %>%

                                                                                       mutate(
                                                                                               path.file = c.path.file,
                                                                                               mod.date  = file.mtime(c.path.file)
                                                                                       )
                                                                       }


                                                                       # Disconnect from database.
                                                                       dbDisconnect(con)

                                                                       }


                                                               # Show header names, if needed.
                                                               if (b.show.header.names) {

                                                                       cat(basename(c.path.file), "\n")
                                                                       cat(names(df.temp), "\n\n")

                                                               }

                                                               return(df.temp)

                                                               }
                                                       )
                       },


                       "shp" = {

                               # Read data from SHAPE file.
                               l.data.object <- lapply(l.path.file,

                                                       function(c.path.file) { # c.path.file <- l.path.file[[1]]

                                                               # Open shapefile.
                                                               sf.temp <- readOGR(

                                                                       dsn   = dirname(c.path.file),
                                                                       layer = gsub("\\.shp$", "", basename(c.path.file)
                                                                                    )
                                                                       )


                                                               # Open shapefile.
                                                               df.temp <- sf.temp %>%

                                                                       # Converteer shapefile in dataframe.
                                                                       as.data.frame(.)


                                                               # Clean up header names when requested.
                                                               if(b.clean.up.header.names) {

                                                                       df.temp <- df.temp %>%

                                                                               # Clean-up header names.
                                                                               f_clean_up_header_names()
                                                               }


                                                               # Add column with path name and date of modification.
                                                               if(b.add.mod.date.path.file) {

                                                                       df.temp <- df.temp %>%

                                                                               mutate(
                                                                                       path.file = c.path.file,
                                                                                       mod.date  = file.mtime(c.path.file)
                                                                               )
                                                               }


                                                               # Show header names, if needed.
                                                               if (b.show.header.names) {

                                                                       cat(basename(c.path.file), "\n")
                                                                       cat(names(df.temp), "\n\n")

                                                               }

                                                               return(df.temp)
                                                       }
                               )
                       },


                       "dbf" = {

                               # Read data from DBF file.
                               l.data.object <- lapply(l.path.file,

                                                       function(c.path.file) { # c.path.file <- l.path.file[[1]]

                                                               # Open dbf file.
                                                               df.temp <- read.dbf(

                                                                       file = c.path.file,
                                                                       as.is = TRUE)


                                                               # Clean up header names when requested.
                                                               if(b.clean.up.header.names) {

                                                                       df.temp <- df.temp %>%

                                                                               # Clean-up header names.
                                                                               f_clean_up_header_names()
                                                               }


                                                               # Add column with path name and date of modification.
                                                               if(b.add.mod.date.path.file) {

                                                                       df.temp <- df.temp %>%

                                                                               mutate(
                                                                                       path.file = c.path.file,
                                                                                       mod.date  = file.mtime(c.path.file)
                                                                               )
                                                               }


                                                               # Show header names, if needed.
                                                               if(b.show.header.names) {

                                                                       cat(basename(c.path.file), "\n")
                                                                       cat(names(df.temp), "\n\n")

                                                               }

                                                               return(df.temp)
                                                       }
                               )
                       },


                       "gs" = {

                               # Read data from Google Sheet.
                               l.data.object <- lapply(l.path.file,

                                                       function(c.path.file) { # c.path.file <- l.path.file[[1]]

                                                               # Open dbf file.
                                                               df.temp <- read_sheet(

                                                                       ss        = c.path.file,
                                                                       sheet     = c.sheet.name,
                                                                       col_names = b.col.names,
                                                                       col_types = l.col.type
                                                                )

                                                               # Clean up header names when requested.
                                                               if(b.clean.up.header.names) {

                                                                       df.temp <- df.temp %>%

                                                                               # Clean-up header names.
                                                                               f_clean_up_header_names()
                                                               }

                                                               # Add column with path name and date of modification.
                                                               if(b.add.mod.date.path.file) {

                                                                       df.temp <- df.temp %>%

                                                                               mutate(
                                                                                       path.file = c.path.file,
                                                                                       mod.date  = file.mtime(c.path.file)
                                                                               )
                                                               }


                                                               # Show header names, if needed.
                                                               if(b.show.header.names) {

                                                                       cat(basename(c.path.file), "\n")
                                                                       cat(names(df.temp), "\n\n")

                                                               }

                                                               return(df.temp)
                                                       }
                               )
                       })



                # Bind list of data frames in one data frame.
                df.data.object <- suppressWarnings(

                        bind_rows(l.data.object))


                # Convert to tibble.
                # df.data.object <- as_tibble(df.data.object)

                # Add break lines.
                cat("\n\n")

                # Return data.object.
                return(df.data.object)

                }
