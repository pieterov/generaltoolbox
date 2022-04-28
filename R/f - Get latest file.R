#################################################################################
# NAME:         FUNCTION - Get latest file
# AUTHOR:       Pieter Overdevest.
# DESCRIPTION:  Get latest file with given c.file.string and of type c.file.type.
#################################################################################

        f_get_latest_file <- function(

                # String to search for in the filename.
                c.file.string,

                # File type.
                c.file.type,

                # Path where file should be searched for.
                c.path,

                # Exact match or not.
                b.exact.match         = FALSE,
                c.file.string.exclude = NULL,

                # In case of Excel file.
                c.sheet.name          = NULL,

                # Show all feedback.
                c.show.report         = "all" # alternative: 'none', 'minimal', 'all'

                ) {


##############################################################################
# Initialize data.
##############################################################################

        # Add escape characters before '(' and ')', if present.
        if(grepl("\\(", c.file.string)) {

                c.file.string <- gsub("\\(", "\\\\(", c.file.string)
        }

        if(grepl("\\)", c.file.string)) {

                c.file.string <- gsub("\\)", "\\\\)", c.file.string)
        }


        # Determine c.pattern based on whether the exact string/filename needs to be found.
        c.pattern <- ifelse(b.exact.match,

                            paste0("^", c.file.string, "\\.", c.file.type, "[xm]?$"),

                            paste0(c.file.string, ".*\\.", c.file.type, "[xm]?$"))


##############################################################################
# Get latest file.
##############################################################################

        # Get all files in the path folder that match c.pattern.
        df.file <- tibble(

                files = list.files(

                        path        = c.path,
                        recursive   = FALSE,
                        full.names  = FALSE,
                        ignore.case = TRUE,
                        pattern     = c.pattern
                        )
                )


        # Remove files that contain string in c.file.string.exclude.
        if(!is.null(c.file.string.exclude)) {

                df.file <- df.file %>%

                        filter(!grepl(c.file.string.exclude, files))
        }


        # Check whether a file was found.
        if (nrow(df.file) == 0) {

                stop(paste0("There was no file found with string '", c.file.string,
                            "' and of type '", c.file.type, "' in path '", c.path, ".'"))
        }

        # Clean up the file names and get the concerned file name.
        df.type <- df.file %>%

                # Check version number in the filename.
                mutate(

                        # Define full path.
                        path.file = paste0(c.path, files),

                        # Get the version number from the filename (v.. followed by number). In case,
                        # there is no version number present in the filename, assign 'v1.'
                        version.number = str_extract(paste(files, "v1"), "[vV][0-9]+"),
                        version.number = gsub("[vV]", "", version.number),
                        version.number = as.numeric(version.number),

                        # Get boolean to express if file is highest version.
                        highest.version = ifelse(version.number == max(version.number), TRUE, FALSE)) %>%

                # Filter the file(s) with the highest version number.
                filter(highest.version) %>%

                # Check date of modification of the file.
                mutate(

                        # Get the last modified date for file.
                        date.last.mod = file.info(path.file)$mtime,

                        # Get boolean to express if file is the most recently modifed version.
                        most.recent.mod.version = ifelse(date.last.mod == max(date.last.mod), TRUE, FALSE)) %>%

                # Filter the file with the most recent date of modification.
                filter(most.recent.mod.version == TRUE)


        # Communicate stats to the user.
        c.path.file <- as.character(df.type$path.file)

        # If filetype is 'xls''
        if(c.file.type == "xls") {

                v.sheet.name <- readxl::excel_sheets(c.path.file)

                if(is.null(c.sheet.name)) {

                        c.sheet.name.read <- v.sheet.name[1]

                        if(length(v.sheet.name) > 1) {

                                warning(paste0(

                                        "Note, you did not supply a value for 'c.sheet.name' and we found more than ",
                                        "one sheet in the workbook: ", f_paste(v.sheet.name, b.quotation = TRUE),
                                        ". We read the data from the first sheet: '", c.sheet.name.read, "'."
                                ))
                        }

                } else {

                        b.sheet.name <- grepl(c.sheet.name, v.sheet.name)

                        if(sum(b.sheet.name) == 0) {

                                stop(paste0(

                                        "Note, we found no sheet that contains the string: '",
                                        c.sheet.name, "'. We did find the following sheets in the workbook: ",
                                        f_paste(v.sheet.name, b.quotation = TRUE), "!"
                                ))
                        }

                        if(sum(b.sheet.name) > 1) {

                                stop(paste0(

                                        "Note, we found more than one sheet that contains the string: '",
                                        c.sheet.name, "', namely: ",
                                        f_paste(v.sheet.name[b.sheet.name], b.quotation = TRUE),
                                        ". Please better specify c.sheet.name and/or use regex ('^', '$')!"
                                ))
                        }

                        c.sheet.name.read <- v.sheet.name[b.sheet.name]
                }
        }


        if (c.show.report != "none") {

                if(c.show.report == "all") {

                        cat(paste0("\nRead as  : ", c.file.type))

                        cat(paste0("\nString   : ", c.file.string))

                        cat(paste0("\nFile     : ", df.type$files))

                        if (c.file.type == "xls") {

                                cat(paste0("\nSheet    : '", c.sheet.name, "' (requested), '", c.sheet.name.read, "' (read)"))
                        }

                        cat(paste0("\nPath     : ", c.path))

                        cat(paste0("\nLast mod : ", df.type$date.last.mod))

                        cat("\n\n")

                        print(round(difftime(Sys.time(), df.type$date.last.mod, units = "auto"), 1))

                        cat("\n==========================\n")

                } else {

                        cat(paste0(df.type$files, " - ", df.type$date.last.mod, " - "))
                } }

        # Return file, incl path.
        return(c.path.file)

        }
