#' @title Read data from file
#'
#' @description Reads data from file.
#'
#' @author Pieter Overdevest
#'
#' @param v.file.string String to search for in the filename. In case of Google Sheet this can be kept NULL (default: NULL).
#' @param c.file.type Options: 'xls', 'csv', 'tsv', 'txt', 'delim', 'rds', 'fst', 'xml', 'parquet', 'sqlite', 'shp', 'dbf', 'gs'
#' @param c.path Path where file should be searched for (default: NULL).
#' @param b.exact.match Exact match or not (default: FALSE).
#' @param c.file.string.exclude Means to filter out files by providing a string that should not exist in the filename
#' (default: NULL).
#' @param b.show.info Should we show info? (default: FALSE).
#' @param c.sheet.name In case of xls (default: NULL).
#' @param n.skip.rows In case of xls (default: 0).
#' @param c.delim In case of txt, csv, delim (default: NULL).
#' @param c.decimal.mark In case of txt, csv, delim (default: ',').
#' @param b.col.names Do the data contain header names? (default: TRUE).
#' @param l.col.type List with column types, see also Details (default: NULL).
#' @param n.guess.max Number of rows to check on data type (default: 1000).
#' @param c.table.name In case of SQLITE (default: NULL).
#' @param c.show.report Options: "none", "minimal", "all" (default: "all").
#' @param b.add.mod.date.path.file Should we add auxiliry features: mod.date, path.file? (default: FALSE).
#' @param b.clean.up.header.names Should we clean up the header names? (default: TRUE).
#'
#' @returns Data frame containing the data read from the file.
#'
#' @details In case of Excel file, l.col.type should consist of list of: "skip", "guess", "logical", "numeric", "date",
#' "text" or "list". In case of delimited file, l.col.type should consist of list of: c = character, i = integer,
#' n = number, d = double, l = logical, D = date, T = date time, t = time, ? = guess, or _/- to skip the column.
#'
#' @export
#'
#' @examples
#' f_read_data_from_file(
#'
#'      v.file.string            = "bord.allocation",
#'      c.file.type              = "xls",
#'      c.path                   = path.data,
#'      b.exact.match            = FALSE,
#'      c.file.string.exclude    = NULL,
#'      b.show.info              = FALSE,
#'      c.sheet.name             = NULL,
#'      n.skip.rows              = 0,
#'      c.delim                  = NULL,
#'      c.decimal.mark           = ",",
#'      b.col.names              = TRUE,
#'      l.col.type               = NULL,
#'      n.guess.max              = 1000,
#'      c.table.name             = NULL,
#'      c.show.report            = "all",
#'      b.add.mod.date.path.file = FALSE,
#'      b.clean.up.header.names  = TRUE
#' )


        #################################################################################
        # FUNCTION.
        #################################################################################

        f_read_data_from_file <- function(

                # String to search for in the filename. In case of Google Sheet this can be kept NULL.
                v.file.string            = NULL,

                # Options: 'xls', 'csv', 'tsv', 'txt', 'delim', 'rds', 'fst', 'xml', 'parquet', 'sqlite', 'shp', 'dbf', 'gs'.
                c.file.type,

                # Path where file should be searched for.
                c.path                   = NULL,

                # Exact match or not.
                b.exact.match            = FALSE,
                c.file.string.exclude    = NULL,

                # Show info
                b.show.info              = FALSE,

                # In case of Excel file.
                c.sheet.name             = NULL, # was "Sheet1"
                n.skip.rows              = 0,

                # Set to NULL, so we know when set by user.
                c.delim                  = NULL,

                # Set standard decimal mark.
                c.decimal.mark           = ",",

                # Do the data contain header names?
                b.col.names              = TRUE,

                # In case of Excel file, l.col.type should consist of list of:
                # "skip", "guess", "logical", "numeric", "date", "text" or "list".

                # In case of delimited file, l.col.type should consist of list of:
                # c = character, i = integer, n = number, d = double,
                # l = logical, D = date, T = date time, t = time,
                # ? = guess, or _/- to skip the column.

                # In the past I used 'cols(.default = "c")' (no longer needed).

                # Or put everything on character.
                l.col.type               = NULL,
                n.guess.max              = 1000,

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
                # c.path                   = NULL
                # b.exact.match            = FALSE
                # c.file.string.exclude    = NULL
                # b.show.info              = FALSE
                # c.sheet.name             = NULL
                # n.skip.rows              = 0
                # c.delim                  = NULL
                # b.col.names              = TRUE
                # l.col.type               = NULL
                # n.guess.max              = 1000
                # c.table.name             = NULL
                # c.show.report            = "all"
                # b.add.mod.date.path.file = FALSE
                # b.clean.up.header.names  = TRUE

                # BLC
                # v.file.string = "DPCOMP with sort test multivariate v1 case 4"
                # c.file.type   = "xls"
                # c.path        = path.data
                # c.sheet.name  = "model"

                # v.file.string            = "Binary Mixtures - Parts I II III - FINAL"
                # c.file.type              = "xls"
                # c.path                   = path.deliverables
                # c.sheet.name             = "Solids"
                # n.guess.max              = 20000

                # c.path        = paste0(path.data, "AI")
                # v.file.string = paste0(c.filename, "_", c.type)
                # c.file.type   = "sqlite"
                # c.table.name  = "ts"

                # SQLite / AI Infrasolutions
                # v.file.string = paste0(c.filename, "_", c.type)
                # c.file.type   = "sqlite"
                # c.table.name  = "bord"
                # c.path        = paste0(path.data, "AI")


                ##############################################################################
                # ERRROR CHECK
                ##############################################################################

                # Does c.file.type have a valid value?
                if(!c.file.type %in% c("xls", "csv", "tsv", "txt", "rds", "fst", "xml", "parquet", "sqlite", "shp", "dbf", "gs")) {

                        stop(paste0(

                                "Note, the value for c.file.type ('", c.file.type, "') is invalid. It must be one of, ",
                                "'xls', 'csv', 'tsv', 'txt', 'rds', 'fst', 'xml', 'parquet', 'sqlite', 'shp', 'dbf', or 'gs'!"
                        ))
                }


                ##############################################################################
                # INITIALIZE
                ##############################################################################

                # Define c.file.category and c.delim in case of c.file.type to be 'csv' or 'txt'.
                if(c.file.type %in% c("csv", "tsv", "txt")) {

                        c.file.category <- "delim"

                        c.delim <- ifelse(

                                is.null(c.delim),

                                case_when(

                                        c.file.type == "csv" ~ ",",
                                        c.file.type == "tsv" ~ "\t",
                                        c.file.type == "txt" ~ " "
                                ),

                                c.delim
                        )


                } else {

                        c.file.category <- c.file.type
                }


                # Update c.path when it is not NULL.
                if(!is.null(c.path)) {

                        #  Append '/' if not there and it is not an url.
                        if(!grepl("/$", c.path) & !grepl("^http", c.path)) {

                                c.path <- paste0(c.path, "/")
                        }

                        # Get latest file in case the file is not read by URL (like Channable).
                        if(!grepl("^http", c.path)) {

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
                }


                ########################################################################################################
                # Functions
                ########################################################################################################

                f_post_processing <- function(df.input, c.path.file, c.delim, b.add.mod.date.path.file, b.show.info) {


                        # Add column with path name and date of modification.
                        if(b.add.mod.date.path.file) {

                                df.input <- df.input %>%

                                        mutate(
                                                path.file = c.path.file,
                                                mod.date  = file.mtime(c.path.file)
                                        )
                        }


                        # Show header names, if needed.
                        if (b.show.info) {

                                cat(paste0("\nFile name:    '", basename(c.path.file), "'"), "\n")
                                cat(paste0("Column names: ",    f_paste(names(df.input), b.quotation = TRUE)), "\n")

                                if(!is.null(c.delim)) {

                                        cat(paste0("Delimiter:    '", c.delim, "'"))
                                }
                        }

                        # Return updated input
                        return(df.input)
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

                                                       # Get all sheet names in the workbook.
                                                       v.sheet.name <- readxl::excel_sheets(c.path.file)

                                                       if(is.null(c.sheet.name)) {

                                                                c.sheet.name <- v.sheet.name[1]

                                                       } else {
                                                                c.sheet.name <- v.sheet.name[grepl(c.sheet.name, v.sheet.name)]
                                                       }


                                                       df.temp <-
                                                               readxl::read_xlsx(

                                                                       path      = c.path.file,
                                                                       sheet     = c.sheet.name,
                                                                       skip      = n.skip.rows,
                                                                       col_names = b.col.names,
                                                                       col_types = l.col.type,
                                                                       guess_max = n.guess.max
                                                               ) %>%

                                                               f_post_processing(

                                                                       c.path.file,
                                                                       c.delim,
                                                                       b.add.mod.date.path.file,
                                                                       b.show.info
                                                               )


                                                       # Check whether one or more of the columns is empty. If so,
                                                       # advice to increase 'guess_max'
                                                       walk(names(df.temp), function(x) {

                                                               if(all(is.na(df.temp[x]))) {

                                                                        cat(paste0(
                                                                               "\nAll cells in column '", x, "' are empty. ",
                                                                               "Consider increasing 'n.guess.max' to a ",
                                                                               "number close to the number of rows in the ",
                                                                               "data frame (",
                                                                               format(nrow(df.temp), big.mark = ","), ").\n"
                                                                        ))
                                                               }
                                                       })

                                                       return(df.temp)
                                               })
                                       )
                       },


                       "delim" = {

                               # Read data from delimited file. Suppress warnings for not being able to match the right format.
                               suppressWarnings(

                                       l.data.object <- lapply(l.path.file,

                                               function(c.path.file) { # c.path.file <- l.path.file[[1]]

                                                       df.temp <-

                                                               readr::read_delim(

                                                                       file      = c.path.file,
                                                                       delim     = c.delim,
                                                                       locale    = locale(decimal_mark = c_decimal_mark),
                                                                       trim_ws   = TRUE,
                                                                       col_names = b.col.names,
                                                                       col_types = l.col.type
                                                               )  %>%

                                                               f_post_processing(

                                                                       c.path.file,
                                                                       c.delim,
                                                                       b.add.mod.date.path.file,
                                                                       b.show.info
                                                               )


                                                       return(df.temp)
                                               })
                                       )
                       },


                       "rds" = {

                               # Read data from RDS file.
                               l.data.object <- lapply(l.path.file,

                                       function(c.path.file) { # c.path.file <- l.path.file[[1]]

                                               df.temp <-

                                                       readRDS(

                                                               file = c.path.file

                                                       )  %>%

                                                       f_post_processing(

                                                               c.path.file,
                                                               c.delim,
                                                               b.add.mod.date.path.file,
                                                               b.show.info
                                                       )


                                               return(df.temp)
                                       }
                               )
                       },

                       "fst" = {

                               # Read data from FST file.
                               l.data.object <- lapply(l.path.file,

                                       function(c.path.file) { # c.path.file <- l.path.file[[1]]

                                               df.temp <-

                                                       read_fst(

                                                               path = c.path.file
                                                       )  %>%

                                                       f_post_processing(

                                                               c.path.file,
                                                               c.delim,
                                                               b.add.mod.date.path.file,
                                                               b.show.info
                                                       )


                                               return(df.temp)
                                       }
                               )
                       },

                       "xml" = {

                               # Read data from XML file.
                               l.data.object <- lapply(l.path.file,

                                       function(c.path.file) { # c.path.file <- l.path.file[[1]]

                                               df.temp <-

                                                       xmlParse(

                                                               file = c.path.file
                                                       ) %>%

                                                       xmlToDataFrame()  %>%

                                                       f_post_processing(

                                                               c.path.file,
                                                               c.delim,
                                                               b.add.mod.date.path.file,
                                                               b.show.info
                                                       )


                                               return(df.temp)
                                       }
                               )
                       },


                       "parquet" = {

                               # Read data from PARQUET file.
                               l.data.object <- lapply(l.path.file,

                                       function(c.path.file) { # c.path.file <- l.path.file[[1]]

                                               df.temp <-

                                                       read_parquet(

                                                               file = c.path.file
                                                       )  %>%

                                                       f_post_processing(

                                                               c.path.file,
                                                               c.delim,
                                                               b.add.mod.date.path.file,
                                                               b.show.info
                                                       )


                                               return(df.temp)
                                       }
                               )
                       },


                       "sqlite" = {

                               # Check whether a table name is provided.
                               if (is.null(c.table.name)) stop(paste0("Missing c.table.name in function input."))

                               l.data.object <- lapply(

                                       l.path.file,

                                       function(c.path.file) {

                                               # c.path.file <- l.path.file[[2]]
                                               #c.table.name <- "borden"
                                               #c.path.file <- l.path.file[[1]]
                                               #print(c.path.file)

                                               # Connect to sqlite file.
                                               con <- dbConnect(

                                                       drv    = RSQLite::SQLite(),
                                                       dbname = c.path.file
                                               )

                                               # Check whether c.table.name is in the list of tables.
                                               if (!c.table.name %in% dbListTables(con)) {

                                                       # Give warning to user.
                                                       warning(
                                                               paste0(
                                                                       c.path.file," does not contain table '",
                                                                       c.table.name, "', The SQLite does contain the ",
                                                                       "following tables: ", f_paste(dbListTables(con))
                                                               )
                                                       )

                                                       # Disconnect from database.
                                                       dbDisconnect(con)

                                                       df.temp <- NULL


                                               } else {

                                                       # Extract table c.table.name and add column with path name and date
                                                       # of modification.
                                                       df.temp <-

                                                               dbGetQuery(

                                                                       con,
                                                                       paste("select * from", c.table.name)
                                                               )  %>%

                                                               f_post_processing(

                                                                       c.path.file,
                                                                       c.delim,
                                                                       b.add.mod.date.path.file,
                                                                       b.show.info
                                                               )


                                                       # Disconnect from database.
                                                       dbDisconnect(con)

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
                                                       layer = gsub("\\.shp$", "", basename(c.path.file))
                                               )


                                               # Open shapefile.
                                               df.temp <-

                                                       sf.temp %>%

                                                       # Converteer shapefile in dataframe.
                                                       as.data.frame(.)  %>%

                                                       f_post_processing(

                                                               c.path.file,
                                                               c.delim,
                                                               b.add.mod.date.path.file,
                                                               b.show.info
                                                       )


                                               return(df.temp)
                                       }
                               )
                       },


                       "dbf" = {

                               # Read data from DBF file.
                               l.data.object <- lapply(l.path.file,

                                       function(c.path.file) { # c.path.file <- l.path.file[[1]]

                                               # Open dbf file.
                                               df.temp <-

                                                       read.dbf(

                                                               file  = c.path.file,
                                                               as.is = TRUE
                                                       )  %>%

                                                       f_post_processing(

                                                               c.path.file,
                                                               c.delim,
                                                               b.add.mod.date.path.file,
                                                               b.show.info
                                                       )


                                               return(df.temp)
                                       }
                               )
                       },


                       "gs" = {

                               # Read data from Google Sheet.
                               l.data.object <- lapply(v.file.string,

                                       function(c.file.string) { # c.file.string <- v.file.string[[1]]

                                               # Open dbf file.
                                               df.temp <-

                                                       read_sheet(

                                                               ss        = c.file.string,
                                                               sheet     = c.sheet.name,
                                                               col_names = b.col.names,
                                                               col_types = l.col.type
                                                        )  %>%

                                                       f_post_processing(

                                                               c.path.file,
                                                               c.delim,
                                                               b.add.mod.date.path.file,
                                                               b.show.info
                                                       )


                                               return(df.temp)
                                       }
                               )
                       })



                # Bind list of data frames in one data frame.
                df.data.object <- suppressWarnings(bind_rows(l.data.object))

                # Clean up header names when requested.
                if(b.clean.up.header.names) {

                        df.data.object <- df.data.object %>%

                                # Clean-up header names.
                                f_clean_up_header_names()
                }

                # Convert to tibble.
                # df.data.object <- as_tibble(df.data.object)

                # Add break lines.
                cat("\n\n")

                # Return data.object.
                return(df.data.object)

                }
