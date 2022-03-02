#################################################################################
# NAME:         FUNCTION - Write data to file.
# AUTHOR:       Pieter Overdevest
# DESCRIPTION:  Write data to file
#################################################################################

f_write_data_to_file <- function(

                        x,

                        # File name without date and without extension, like xlsx.
                        c.file.string                 = "Data Export",

                        # Vector of paths where file should be stored. Default saved in Downloads.
                        v.path,

                        # Vector of sheet names, in case of save to Excel.
                        v.sheet.name                  = NULL, # In case of XLS, CSV, TXT.
                        v.table.name                  = NULL, # In case of SQLITE

                        # Logical to confirm whether date should be added to the filename
                        # v.add.date must be as long as v.path, allowing to set add.date per
                        # file location. If length is one, it will be used for all. Default
                        # is true.
                        v.add.date                   = TRUE,
                        v.add.time                   = FALSE,

                        # Determine where to save the data to.
                        v.xls                        = FALSE,
                        v.csv                        = FALSE,
                        v.txt                        = FALSE,
                        v.delim                      = FALSE,
                        v.rds                        = FALSE,
                        v.fst                        = FALSE,
                        v.sqlite                     = FALSE,
                        v.parquet                    = FALSE,
                        v.xml                        = FALSE,

                        # Needed in case v.delim is equal to TRUE.
                        c.delim                     = NULL,

                        # Should we write header names in first row?
                        b.col.names                 = TRUE,

                        # Should data be appended?
                        b.append                    = FALSE,

                        # Used for xls (Excel).

                        # Number of rows and columns to freeze.
                        # Default is first row and first column.
                        v.freeze.row                = NULL,
                        v.freeze.col                = NULL,

                        # Column numbers of headers to color.
                        v.col.dark.blue             = NULL,
                        v.col.light.blue            = NULL,
                        v.col.green                 = NULL,
                        v.col.purple                = NULL,
                        v.col.lila                  = NULL,
                        v.col.orange                = NULL,
                        v.col.red                   = NULL,

                        c.conditional.eval          = NULL, # bijv. "$I2==0"
                        c.conditional.color         = "#ABB2B9",

                        b.banded.rows               = TRUE
                        )

                        {


        # WEGSCHRIJVEN ALS SHAPEFILE / WISH-LIST.
        # # Convert data frame to shapefile and write to disk.
        # # https://gis.stackexchange.com/questions/214062/create-a-shapefile-from-dataframe-in-r-keeping-attribute-table
        # wgs              <-  df.bord.allocation.amsterdam.gemeente
        # coordinates(wgs) <-~ bord.lon + bord.lat
        # proj4string(wgs) <- CRS("+proj=longlat +datum=WGS84")
        # sp.bord.allocation.amsterdam.gemeente <- spTransform(wgs, CRS("+proj=longlat"))
        # raster::shapefile(sp.bord.allocation.amsterdam.gemeente,
        #                   paste0(path.deliverables, "HR Groep - Amsterdam - Borden.shp"))
        #
        #
        # wgs              <-  df.drager.allocation.amsterdam.gemeente
        # coordinates(wgs) <-~ drager.lon + drager.lat
        # proj4string(wgs) <- CRS("+proj=longlat +datum=WGS84")
        # sp.drager.allocation.amsterdam.gemeente <- spTransform(wgs, CRS("+proj=longlat"))
        # raster::shapefile(sp.drager.allocation.amsterdam.gemeente,
        #                   paste0(path.deliverables, "HR Groep - Amsterdam - Drager.shp"))



##############################################################################
# TEST ONLY!!
##############################################################################

        # ALTIJD
        # v.sheet.name        = NULL
        # v.table.name        = NULL
        # c.file.string       = "Data Export"
        # v.add.date          = TRUE
        # v.add.time          = FALSE
        # v.freeze.row        = NULL
        # v.freeze.col        = NULL
        # v.xls               = FALSE
        # v.csv               = FALSE
        # v.txt               = FALSE
        # v.delim             = FALSE
        # v.rds               = FALSE
        # v.fst               = FALSE
        # v.sqlite            = FALSE
        # v.parquet           = FALSE
        # v.xml               = FALSE
        # c.delim             = NULL
        # b.col.names         = TRUE
        # b.append            = FALSE
        # v.col.dark.blue     = NULL
        # v.col.light.blue    = NULL
        # v.col.green         = NULL
        # v.col.purple        = NULL
        # v.col.lila          = NULL
        # v.col.orange        = NULL
        # v.col.red           = NULL
        # c.conditional.eval  = NULL
        # c.conditional.color = "#ABB2B9"
        # b.banded.rows       = TRUE

        # Set 1
        # x             = df.nwb.look.up
        # c.file.string = paste0(c.time, " - df.nwb.look.up - Analyse - Wide2 - ", c.init)
        # v.path        = path.data.rds
        # v.csv         = TRUE
        # v.add.date    = TRUE

        # Testing Parquet files
        # x             = df.bord.ipsm
        # c.file.string = "test_parquet2"
        # v.parquet     = TRUE
        # v.path        = path.data

        # Test
        # x               = data.frame(x=seq(5),y=LETTERS[1:5])
        # v.path          = path.data
        # v.delim         = TRUE
        # c.delim         = ";"
        # b.append        = TRUE

        # Sondeeronderzoek (wb 74)
        # x             = l.sondeeronderzoek
        # v.sheet.name  = v.sonderonderzoek.tabellen
        # v.path        = path.deliverables
        # c.file.string = "Sondeerdata van PDOK - sample"

        # x             = data.frame(x=seq(10),y=LETTERS[1:10])
        # v.path        = path.root.local
        # c.file.string = "Data Export"
        # v.add.time    = FALSE

        # x             = df.datachamp.baseline.updated
        # c.file.string = "Oletti Productfeed - Baseline"
        # v.path        = rep(paste0(path.datachamp.dropbox, "Baseline - ", str_to_title(c.update.type), "/"), 2)
        # v.add.date    = c(TRUE, FALSE)
        # v.add.time    = c(TRUE, FALSE)


##############################################################################
# ERROR CHECK
##############################################################################

        # Data cannot be saved to CSV and TXT files within the same function call.
        if(any(v.csv) & any(v.txt)) {

                stop(paste0(

                        "The function 'f_write_data_to_file' can only write to one delimited ",
                        "file type, for example, 'csv' OR 'txt', and not to 'csv' and 'txt' at ",
                        "the same time. This is because the data is written using fwrite by ",
                        "data.table - a general purpose and fast writing function!"
                ))
        }


        # Does c.delim have a value when needed?
        if(any(v.delim) & is.null(c.delim)) {

                stop(paste0(

                        "Note, you must provide a value for 'c.delim' in case you have chosen 'v.delim' to be TRUE!"
                ))
        }


        # Check that v.path, v.sheet.name, v.table.name, v.add.date, v.add.time,
        # v.xls, v.csv, v.txt, v.delim, v.rds, v.fst, v.sqlite, v.parquet, v.xml, all have same length
        v.input <- c("v.add.date", "v.add.time", "v.xls", "v.csv", "v.txt", "v.delim",
                     "v.rds", "v.fst", "v.sqlite", "v.parquet", "v.xml")

        v.temp1 <- v.input %>%

                lapply(function(x) length(get(x))) %>%

                unlist()

        v.temp2 <- v.input[which(v.temp1 > length(v.path))]
        v.temp3 <- v.temp1[which(v.temp1 > length(v.path))]

        if(length(v.temp2) > 0) {

                stop(paste0(

                        "Note,", f_paste(paste(paste0("'", v.temp2, "'"), paste0("(", v.temp3, ")"))),
                        ", cannot have more elements than v.path (", length(v.path), ")!"
                ))
        }


##############################################################################
# INITIALIZE
##############################################################################

        # Update filename in case the default is chosen.
        if (c.file.string == "Data Export") {

                c.file.string = paste("Data Export -", deparse(substitute(x)))
        }


        # Maak v.xls gelijk aan TRUE, indien alle FALSE zijn.
        if( all(!v.csv) & all(!v.txt) & all(!v.delim) & all(!v.rds) & all(!v.fst) &
            all(!v.xls) & all(!v.sqlite) & all(!v.parquet) & all(!v.xml) ) {

                v.xls = TRUE
        }


        # Define c.extension, in case of v.delim.
        if(any(v.delim)) {

                c.extension <- "txt"
        }


        # Define v.delim, c.delim, and c.extension, in case of v.csv.
        if(any(v.csv)) {

                v.delim     <- v.csv

                c.delim     <- ","

                c.extension <- "csv"
        }


        # Define v.delim, c.delim, and c.extension, in case of v.txt.
        if(any(v.txt)) {

                v.delim     <- v.txt

                c.delim     <- " "

                c.extension <- "txt"
        }


        # Initialize.
        v.file <- NULL


        # Define Excel style when needed
        excel.style.center    <- openxlsx::createStyle(

                halign = "center")

        excel.style.header.default    <- openxlsx::createStyle(

                halign = "center",  valign = "bottom",
                textRotation = 90,  wrapText = TRUE, fontColour = "white",
                fgFill = "#2E86C1")


        excel.style.header.dark.blue <- openxlsx::createStyle(

                halign = "center",  valign = "bottom",
                textRotation = 90,  wrapText = TRUE, fontColour = "white",
                fgFill = "#000861")

        excel.style.header.light.blue <- openxlsx::createStyle(

                halign = "center",  valign = "bottom",
                textRotation = 90,  wrapText = TRUE, fontColour = "white",
                fgFill = "#52b2f2")

        excel.style.header.green <- openxlsx::createStyle(

                halign = "center",  valign = "bottom",
                textRotation = 90,  wrapText = TRUE, fontColour = "white",
                fgFill = "#1E8449")

        excel.style.header.purple <- openxlsx::createStyle(

                halign = "center",  valign = "bottom",
                textRotation = 90,  wrapText = TRUE, fontColour = "white",
                fgFill = "#8E44AD")

        excel.style.header.lila <- openxlsx::createStyle(

                halign = "center",  valign = "bottom",
                textRotation = 90,  wrapText = TRUE, fontColour = "white",
                fgFill = "#AF7AC5")

        excel.style.header.orange <- openxlsx::createStyle(

                halign = "center",  valign = "bottom",
                textRotation = 90,  wrapText = TRUE, fontColour = "white",
                fgFill = "#E67E22")

        excel.style.header.red <- openxlsx::createStyle(

                halign = "center",  valign = "bottom",
                textRotation = 90,  wrapText = TRUE, fontColour = "white",
                fgFill = "#960000")

        excel.style.conditional <- openxlsx::createStyle(

                bgFill = c.conditional.color)


##############################################################################
# Error check.
##############################################################################

        # Append can only be used in combination with csv, txt and delim.
        if( b.append & (any(v.rds) | any(v.fst) | any(v.xls) | any(v.sqlite) | any(v.parquet) | any(v.xml)) ) {

                stop("Note, data can only be appended in case of csv, txt, and delim!")
        }


##############################################################################
# Save data.
##############################################################################

        # Save data based on extension.
        for (i in seq_along(v.path)) { # i <- 1

                # Initialize.
                v.add.date.i <- ifelse(length(v.add.date) == 1, v.add.date, v.add.date[i])
                v.add.time.i <- ifelse(length(v.add.time) == 1, v.add.time, v.add.time[i])
                v.xls.i      <- ifelse(length(v.xls)      == 1, v.xls,      v.xls[i])
                v.delim.i    <- ifelse(length(v.delim)    == 1, v.delim,    v.delim[i])
                v.rds.i      <- ifelse(length(v.rds)      == 1, v.rds,      v.rds[i])
                v.fst.i      <- ifelse(length(v.fst)      == 1, v.fst,      v.fst[i])
                v.sqlite.i   <- ifelse(length(v.sqlite)   == 1, v.sqlite,   v.sqlite[i])
                v.parquet.i  <- ifelse(length(v.parquet)  == 1, v.parquet,  v.parquet[i])
                v.xml.i      <- ifelse(length(v.xml)      == 1, v.xml,      v.xml[i])


                c.file         <- paste0(

                        ifelse(v.add.date.i, paste0(format(Sys.time(), "%Y %m %d"), " - "), ""),

                        ifelse(v.add.time.i, paste0(format(Sys.time(), "%H %M %S"), " - "), ""),

                        c.file.string
                )

                # Add dataframe to Excel workbook.
                if (v.xls.i) {

                        # Create workbook
                        wb <- openxlsx::createWorkbook()

                        # Turn the object into a list, if not already.
                        if (!any(class(x) == "list")) {x.object <- list(x)} else {x.object <- x}

                        # Add dataframe(s) to separate worksheet(s).
                        for (j in seq_along(x.object)) { # j <- 1

                                # Convert object to dataframe if not already.
                                if (!any(class(x.object[[j]]) == "data.frame")) {

                                        x.object.j <- as.data.frame(x = x.object[[j]])

                                        } else {

                                        x.object.j <- x.object[[j]]

                                        }

                                # Check that all columns are of acceptable class.
                                v.class.observed          <- lapply(lapply(x.object.j, class), function(xx) {paste(xx, collapse = "|")}) %>% unlist()
                                v.class.scope             <- c("character", "factor", "numeric", "integer", "logical", "Date", "POSIXct", "POSIXt", "difftime", "list")
                                v.class.observed.in.scope <- lapply(v.class.observed, function(xx) {any(grepl(xx, v.class.scope))}) %>% unlist()

                                if(!all(v.class.observed.in.scope)) {

                                        stop(paste0(

                                                "Note, the following features in '", deparse(substitute(x)),
                                                "' do not have an acceptable class: ",
                                                f_paste(names(x.object.j)[!v.class.observed.in.scope], b.quotation = TRUE),
                                                ". They are of class: ",
                                                f_paste(v.class.observed[!v.class.observed.in.scope], b.quotation = TRUE),
                                                ", resp. Features must be one of, ",
                                                f_paste(v.class.scope, b.quotation = TRUE, c.and = "or"), "!"
                                        ))
                                }

                                # Determine numeric columns.
                                v.col.numeric <- seq(ncol(x.object.j))[unlist(lapply(x.object.j, is.numeric))]

                                # Determine number of rows and columns to freeze.
                                if (any(is.null(v.freeze.row))) {v.freeze.row.j <- 1} else
                                        {v.freeze.row.j <- v.freeze.row[j]}

                                if (any(is.null(v.freeze.col))) {v.freeze.col.j <- 1} else
                                        {v.freeze.col.j <- v.freeze.col[j]}

                                # Determine sheet names.
                                if (any(is.null(v.sheet.name))) {

                                        v.sheet.name.j <- paste0("Sheet", j)

                                } else {

                                        v.sheet.name.j <- v.sheet.name[j]

                                        if ((length(v.sheet.name) != length(x.object)) & !any(is.null(v.sheet.name)))

                                                stop(paste("The number of sheet names must equal the number of objects.",
                                                           "Or v.sheet.name should equal NULL, and the sheets will be",
                                                           "named 'Sheet_[seq]'."))
                                        }

                                # Add worksheet.
                                openxlsx::addWorksheet(

                                        wb           = wb,
                                        sheetName    = v.sheet.name.j,
                                        gridLines    = FALSE)

                                openxlsx::freezePane(

                                        wb             = wb,
                                        sheet          = v.sheet.name.j,
                                        firstActiveRow = v.freeze.row.j + 1,
                                        firstActiveCol = v.freeze.col.j + 1)

                                openxlsx::writeDataTable(

                                        wb         = wb,
                                        sheet      = v.sheet.name.j,
                                        x          = x.object.j,
                                        tableStyle = "TableStyleMedium9",
                                        withFilter = TRUE,
                                        bandedRows = b.banded.rows)

                                openxlsx::setColWidths(

                                        wb           = wb,
                                        sheet        = v.sheet.name.j,
                                        cols         = seq(ncol(x.object.j)),
                                        widths       = 20)

                                openxlsx::addStyle(

                                        wb    = wb,
                                        sheet = v.sheet.name.j,
                                        style = excel.style.center,
                                        rows  = seq(nrow(x.object.j)) + 1,
                                        cols  = v.col.numeric,
                                        gridExpand = TRUE
                                        )

                                # Update header row.
                                setRowHeights(

                                        wb      = wb,
                                        sheet   = v.sheet.name.j,
                                        rows    = 1,
                                        heights = 220)

                                openxlsx::addStyle(

                                        wb    = wb,
                                        sheet = v.sheet.name.j,
                                        style = excel.style.header.default,
                                        rows  = 1,
                                        cols  = seq(ncol(x.object.j))
                                        )

                                # Dark blue cells.
                                if(!is.null(v.col.dark.blue)) {

                                        openxlsx::addStyle(

                                                wb    = wb,
                                                sheet = v.sheet.name.j,
                                                style = excel.style.header.dark.blue,
                                                rows  = 1,
                                                cols  = v.col.dark.blue,
                                        )
                                }

                                # Light blue cells.
                                if(!is.null(v.col.light.blue)) {

                                        openxlsx::addStyle(

                                                wb    = wb,
                                                sheet = v.sheet.name.j,
                                                style = excel.style.header.light.blue,
                                                rows  = 1,
                                                cols  = v.col.light.blue
                                        )
                                }

                                # Green cells.
                                if(!is.null(v.col.green)) {

                                        openxlsx::addStyle(

                                                wb    = wb,
                                                sheet = v.sheet.name.j,
                                                style = excel.style.header.green,
                                                rows  = 1,
                                                cols  = v.col.green
                                        )
                                }

                                # Purple cells.
                                if(!is.null(v.col.purple)) {

                                        openxlsx::addStyle(

                                                wb    = wb,
                                                sheet = v.sheet.name.j,
                                                style = excel.style.header.purple,
                                                rows  = 1,
                                                cols  = v.col.purple
                                                )
                                        }

                                # Lila cells.
                                if(!is.null(v.col.lila)) {

                                        openxlsx::addStyle(

                                                wb    = wb,
                                                sheet = v.sheet.name.j,
                                                style = excel.style.header.lila,
                                                rows  = 1,
                                                cols  = v.col.lila
                                        )
                                }

                                # Orange cells.
                                if(!is.null(v.col.orange)) {

                                        openxlsx::addStyle(

                                                wb    = wb,
                                                sheet = v.sheet.name.j,
                                                style = excel.style.header.orange,
                                                rows  = 1,
                                                cols  = v.col.orange
                                        )
                                }

                                # Red cells.
                                if(!is.null(v.col.red)) {

                                        openxlsx::addStyle(

                                                wb    = wb,
                                                sheet = v.sheet.name.j,
                                                style = excel.style.header.red,
                                                rows  = 1,
                                                cols  = v.col.red
                                        )
                                }


                                # Conditional formating
                                if(!is.null(c.conditional.eval)) {

                                        openxlsx::conditionalFormatting(

                                                wb    = wb,
                                                sheet = v.sheet.name.j,
                                                cols = seq(ncol(x.object.j)),
                                                rows = seq(nrow(x.object.j)) + 1,
                                                rule = c.conditional.eval,
                                                style = excel.style.conditional
                                        )
                                }

                        }

                        # Create filename, and append to vector.
                        c.file.i <- paste0(c.file, ".xlsx")
                        v.file <- c(v.file, c.file.i)

                        # Save workbook to disc.
                        openxlsx::saveWorkbook(

                                wb           = wb,
                                file         = paste0(v.path[i], c.file.i),
                                overwrite    = TRUE
                        )
                }


                # Add data frame to RDS file.
                if (v.rds.i) {

                        # Create filename.
                        c.file.i <- paste0(c.file, ".rds")

                        # Append filename to vector.
                        v.file <- c(v.file, c.file.i)

                        # Write data to rds file.
                        saveRDS(
                                object = x,
                                file   = paste0(v.path[i], c.file.i)
                        )
                }


                # Add data frame to FST file.
                if (v.fst.i) {

                        # Create filename.
                        c.file.i <- paste0(c.file, ".fst")

                        # Append filename to vector.
                        v.file <- c(v.file, c.file.i)

                        # Write data to rds file.
                        write_fst(
                                x    = x,
                                path = paste0(v.path[i], c.file.i)
                        )
                }



                # Add data frame to PARQUET file.
                if (v.parquet.i) {

                        # Create filename.
                        c.file.i <- paste0(c.file, ".parquet")

                        # Append filename to vector.
                        v.file <- c(v.file, c.file.i)

                        # Write data to rds file.
                        write_parquet(
                                x    = x,
                                sink = paste0(v.path[i], c.file.i)
                        )
                }



                # Add dataframe to delimited file.
                if (v.delim.i) {

                        # Turn the object into a list, if not already.
                        if (!any(class(x) == "list")) {x.object <- list(x)} else {x.object <- x}

                        # Add data to delimited file(s).
                        for (k in seq_along(x.object)) { # k <- 1

                                # Convert object to dataframe if not already.
                                if (!any(class(x.object[[k]]) == "data.frame")) {

                                        x.object.k <- data.frame(x.object = x.object[[k]])

                                } else {

                                        x.object.k <- x.object[[k]]

                                }

                                # Determine sheet names.
                                if (any(is.null(v.sheet.name))) {

                                        v.sheet.name.k <- paste0(c.extension, k)

                                        } else {

                                                if ((length(v.sheet.name) != length(x.object)) & !any(is.null(v.sheet.name)))

                                                        stop(paste("The number of sheet names must equal the number of objects.",
                                                                   "Or v.sheet.name should equal NULL, and the sheets will be",
                                                                   "named 'Sheet_[seq]'."))

                                                v.sheet.name.k <- v.sheet.name[k]
                                        }


                                # Create filename.
                                c.file.k <- paste0(

                                        c.file,
                                        ifelse(length(x.object) == 1, "", paste(" -", v.sheet.name.k)),
                                        ".", c.extension
                                )


                                # Check whether file already exists. In case b.append is TRUE, then set b.col.names to FALSE.
                                # This is to prevent writing the column names again. POINT FOR IMPROVEMENT: TEST NUMBER OF COLUMNS
                                # IN THE FILE THAT IS ALREADY THERE AND COMPARE TO THE NUMBER OF COLUMNS IN THE DATA FRAME TO
                                # BE WRITTEN.
                                if(file.exists(paste0(v.path[i], c.file.k)) & b.append) {

                                        b.col.names <- FALSE
                                }


                                # Append filename to vector.
                                v.file <- c(v.file, c.file.k)


                                # Write data to csv file. Eerder had ik BOM op TRUE gezet ivm diakritische tekens. Nu weer op FALSE omdat er probleem
                                # is met inlezen van allocatie tabel. Diakritische tekens heeft Rian intern opgelost.
                                # "If TRUE a BOM (Byte Order Mark) sequence (EF BB BF) is added at the beginning of the file; format 'UTF-8 with BOM'."
                                data.table::fwrite(x         = data.table::as.data.table(x.object.k),
                                                   file      = paste0(v.path[i], c.file.k),
                                                   bom       = FALSE,
                                                   append    = b.append,
                                                   sep       = c.delim,
                                                   dec       = ".",
                                                   row.names = FALSE,
                                                   col.names = b.col.names
                                )
                        }
                }


                # Add dataframe to SQLITE file(s).
                if (v.sqlite.i) {

                        # Turn the object into a list, if not already.
                        if (!any(class(x) == "list")) {x.object <- list(x)} else {x.object <- x}

                        # Create filename.
                        c.file.i <- paste0(c.file, ".sqlite")

                        # Create SQLITE file connection.
                        db <- dbConnect(

                                drv    = SQLite(),
                                dbname = paste0(v.path[i], c.file.i)
                        )

                        # Append filename to vector.
                        v.file <- c(v.file, c.file.i)


                        # Add data to sqlite file(s).
                        for (k in seq_along(x.object)) { # k <- 2

                                # Convert object to dataframe if not already.
                                if (!any(class(x.object[[k]]) == "data.frame")) {

                                        x.object.k <- data.frame(x.object = x.object[[k]])

                                } else {

                                        x.object.k <- x.object[[k]]

                                }


                                # Determine dataframe names.
                                if (any(is.null(v.table.name))) {

                                        v.table.name.k <- paste0("table_", k)

                                } else {

                                        if ((length(v.table.name) != length(x.object)) & !any(is.null(v.table.name)))

                                                stop(paste("The number of table names must equal the number of objects.",
                                                           "Or v.table.name should equal NULL, and the tables will be",
                                                           "named 'table_[seq]'."))

                                        v.table.name.k <- v.table.name[k]
                                }


                                dbWriteTable(conn       = db,
                                             name       = v.table.name.k,
                                             value      = x.object.k,
                                             overwrite  = T,
                                             row.names  = FALSE)
                        }

                        # Close connection.
                        dbDisconnect(db)
                }


                # Add dataframe to XML file(s).
                if (v.xml.i) {

                        # Create filename.
                        c.file.i <- paste0(c.file, ".xml")

                        # Append filename to vector.
                        v.file <- c(v.file, c.file.i)

                        # Function to add node data.
                        convertToXML <- function(df.input) {

                                # Create empty XML Tree.
                                xml <- suppressWarnings( xmlTree(tag = "data_frame") )

                                # Iterate through each data point.
                                for (i in 1:nrow(df.input)) {

                                        xml$addNode("record", close=FALSE)

                                        for (j in names(df.input)) {

                                                xml$addNode(j, df.input[i, j])
                                        }

                                        xml$closeTag()
                                }

                                xml$closeTag()

                                return(xml)
                        }

                        saveXML(
                                doc  = convertToXML(x),
                                file = paste0(v.path[i], c.file.i)
                        )
                }
        }


##############################################################################
# Communicate stats to the user.
##############################################################################

        cat(paste0("\nWrite    : ", deparse(substitute(x))))

        cat(paste0("\nAs       : ", paste(c("xls", "csv", "rds", "txt", "fst", "parquet", "xml")[c(any(v.xls), any(v.csv), any(v.rds), any(v.txt), any(v.fst), any(v.parquet), any(v.xml))], collapse = ", ") ))

        cat(paste0("\nName     : ", v.file))

        cat(paste0("\n\nPath     : ", v.path))

        cat("\n")

        cat("\n==========================\n")


}
