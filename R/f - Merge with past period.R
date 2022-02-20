##############################################################################################
# NAME:         FUNCTION - MERGE WITH PAST PERIOD
# AUTHOR:       Pieter Overdevest
##############################################################################################

##############################################################################################
# INITIALIZATION.
##############################################################################################

##############################################################################################
# INITIALIZATION.
##############################################################################################


        f_merge_with_past_period <- function(
          
                  df.input,
                  c.file.string,
                  c.path,
                  c.file.type,
                  n.interval.max = 100000
                  ) {
          
          # TEST
          # df.input       = data.frame(x =  c(11,11), y = c(11, 11))
          # c.file.string  = "Test"
          # c.path         = path.data
          # c.file.type    = "xls"
          # n.interval.max = 2
          
          
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
                          interval < n.interval.max
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
    
          # Write merged data.
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
        
        
        f_check_cols_present <- function(df.input, v.col) {
        
                # Test
                # df.input <- tibble(ID = letters[1:6], dummy1 = c(seq(4), 4, 4), dummy2 = seq(6))
                # v.col    <- c("dummy1", "dummy2", "dummy3", "dummy4")
                # f_check_cols_present(df.input, v.col)
          
                v.col.not.present <- v.col[!v.col %in% names(df.input)]
                
                if(length(v.col.not.present) > 0) {
                  
                        stop(paste0(
                          
                          "Note, ", f_paste(v.col.not.present, b.quotation = TRUE),
                          " are not present in '", deparse(substitute(df.input)), "'!"
                        ))
                }
        }
        

        f_check_col_unique <- function(df.input, c.col) {
          
                # Test
                # df.input <- tibble(ID = letters[1:6], dummy1 = c(seq(4), 4, 4), dummy2 = seq(6))
                # c.col    <- "dummy1"
                # f_check_col_unique(df.input, c.col)
                
                # Check c.col in present in df.input.
                f_check_cols_present(df.input, c.col)
                
                # Check c.col does not contain empty cells.
                f_check_col_not_empty(df.input, c.col)
                
                # Check that c.col does not contain NA.
                if(!f_is_unique(df.input[[c.col]])) {
                  
                        stop(paste0(
                          
                                "Note, '", c.col, "' (c.col) in '", deparse(substitute(df.input)),
                                "' (df.input) must contain unique values!"
                        ))
                }
        }
        
        
        f_check_cols_unique <- function(df.input, v.unique, c.id) {
              
                # Test!
                # df.input <- df.datachamp.baseline.source
                # v.unique <- v.feature.must.be.unique
                # c.id     <- "ID"
                
                # df.input <- tibble(ID = letters[1:6], `Variants: SKU` = c(seq(5), 5), dummy1 = c(seq(4), 4, 4), dummy2 = seq(6))
                # v.unique <- c("Variants: SKU", "dummy1", "dummy2")
                # c.id     <- "ID"
                # f_check_cols_unique(df.input, v.unique, c.id)
                
                # Remove c.id from v.unique, if present.
                v.unique <- v.unique[v.unique != c.id]
    
                
                # Check presence of columns in df.input.
                f_check_cols_present(df.input, c.id)
                f_check_cols_present(df.input, v.unique)
              
                # Check that c.id does not contain NA and is unique.
                f_check_col_not_empty(df.input, c.id)
                f_check_col_unique(df.input, c.id)
                  
          
                # Determine features with NA.
                df.temp <- df.input %>%
                  
                        # Select concerned columns
                        select(all_of(v.unique)) %>%
                        
                        # Determine number of NA per column
                        f_info_per_column() %>%
                        
                        # Select columns that are not unique
                        filter(n.unique != n.tot) %>%
                  
                        # Sort by non-uniqueness.
                        arrange(desc(n.unique)) %>%
                        
                        # Create label.
                        mutate(n.label = paste0("'", feature, "' (", n.unique, ")"))

                                
                # Error check!
                if(nrow(df.temp) > 0) {
                  
                        v.temp <- lapply(df.temp$feature, function(c.unique) { # c.unique <- df.temp$feature[1]
                        
                                df.input %>%
                                  
                                    # Select concerned columns
                                    select(all_of(c(c.id, c.unique))) %>%
                                    
                                    add_count(get(c.unique)) %>%
                                    
                                    filter(n > 1) %>%
                                  
                                    pull(c.id) %>%
                                  
                                    f_paste(., b.quotation = TRUE) %>%
                        
                                    paste0("'", c.unique, "' (", ., ")")
                          
                        }) %>% unlist()
                
                        
                        stop(paste0(
                          
                                  "Note, the following features do not contain unique values (", c.id, "):\n",
                                  f_paste(v.temp, b.sort = FALSE), ".\n\n"
                        ))
                }
        }
        
        
        f_check_col_not_empty <- function(df.input, c.col) {
          
                # Test
                # df.input <- tibble(ID = letters[1:6], dummy1 = c(seq(4), NA, 4), dummy2 = seq(6))
                # c.col    <- "dummy1"
                # f_check_col_not_empty(df.input, c.col)
                
                # Check c.col in present in df.input.
                f_check_cols_present(df.input, c.col)
                
                # Check that c.col does not contain NA.
                if(any(is.na(df.input[[c.col]]))) {
                  
                        stop(paste0(
                          
                                "Note, '", c.col, "' (c.col) in '", deparse(substitute(df.input)),
                                "' (df.input) cannot contain NAs!"
                        ))
                }
        }
        
        f_check_cols_not_empty <- function(df.input, v.not.empty, c.id) {
                
                # Test.
                # df.input    = data.frame(ID = c(1,2,3,4), pieter = c(1,2,NA,NA), bart = c(NA,NA,NA,4))
                # df.input    = df.datachamp.baseline.source %>% filter(Status == "ACTIVE")
                # v.not.empty = v.feature.cannot.be.empty
                # v.not.empty = c("pieter", "bart")
                # c.id        = "ID"
                # f_check_cols_empty_cells(df.input, v.not.empty, c.id)
              
                
                # Check presence of columns in df.input.
                f_check_cols_present(df.input, c.id)
                f_check_cols_present(df.input, v.unique)
                
                # Check that c.id does not contain NA and is unique.
                f_check_col_not_empty(df.input, c.id)
                f_check_col_unique(df.input, c.id)
          
                
                # Determine features with NA.
                df.temp <- df.input %>%
                
                        # Select concerned columns
                        select(all_of(v.not.empty)) %>%
                                
                        # Determine number of NA per column
                        f_info_per_column() %>%
                        
                        # Select columns with NA
                        filter(n.na > 0) %>%
                        
                        # Create label.
                        mutate(n.label = paste0(feature, " (", n.na, ")"))
                
                
                # Create label.
                v.temp <- lapply(df.temp$feature, function(c.temp) { # c.temp = df.temp$feature[1]
                        
                        paste0(
                                "'", c.temp, "' (",
                               
                                f_paste(
                        
                                        df.input %>%
                                                
                                                select(all_of(c(c.id, c.temp))) %>%
                                                
                                                filter(is.na(get(c.temp))) %>%
                                                
                                                pull(all_of(c.id))
                                ), ")"
                        )
                        
                }) %>% unlist()
                
                
                # Evaluation.
                if(length(v.temp) > 0) {
                        
                        stop(paste0(
                                
                                "Note, the following features contain one ore more NAs, see '", c.id, "': ",
                                f_paste(v.temp)
                        ))
                }
        } 
        
        
##############################################################################################
# GET BASELINE
##############################################################################################

        # Latest version of baseline file.
        df.datachamp.baseline.source <- f_read_data_from_file(

                v.file.string           = "Oletti Productfeed - Baseline - Original",
                c.file.type             = "csv",
                c.path                  = paste0(path.datachamp.dropbox, "Baseline - ", str_to_title(c.update.type), "/"),
                b.clean.up.header.names = FALSE,
                l.col.type              = cols(.default = "c")
                )


        # Indien bovenstaande file een original file betreft dan kolom 'time.stamp.increment' toevoegen en lege quantityvelden vervangen door 0.
        f_write_data_to_file(

                x             = df.datachamp.baseline.source %>%

                        mutate(
                                # Replace empty cells by 0.
                                `Variants: Inventory quantity` = ifelse(is.na(`Variants: Inventory quantity`), 0, `Variants: Inventory quantity`),

                                # Add timestamp
                                timestamp.increment            = paste0(format(Sys.time(), "%Y %m %d"), " - ", format(Sys.time(), "%H %M %S")),
                        ) %>%

                        # Put timestamp first.
                        select(timestamp.increment, everything()),

                v.path        = paste0(path.datachamp.dropbox, "Baseline - ", str_to_title(c.update.type), "/"),
                c.file.string = "Oletti Productfeed - Baseline",
                v.xls         = TRUE,
                v.add.time    = TRUE
        )
        

        # Latest version of baseline file.
        df.datachamp.baseline.source <- f_read_data_from_file(
                
                v.file.string           = "Oletti Productfeed - Baseline",
                c.file.type             = "xls",
                c.path                  = paste0(path.datachamp.dropbox, "Baseline - ", str_to_title(c.update.type), "/"),
                b.clean.up.header.names = FALSE
        )
        
        # Define object with feature names in baseline file.
        v.datachamp.baseline.names <- names(df.datachamp.baseline.source)
        
        
        #'####################################################################################
        # ERROR CHECK
        #'####################################################################################
        
        # Error check!
        f_check_col_uniqueness(df.datachamp.baseline.source,                                "ID")
        f_check_col_uniqueness(df.datachamp.baseline.source,                                "Variants: SKU")
        f_check_col_uniqueness(df.datachamp.baseline.source %>% filter(Status == "ACTIVE"), "Variants: SKU")
        
        f_check_cols_empty_cells(
                
                df.input    = df.datachamp.baseline.source %>% filter(Status == "ACTIVE"),
                v.not.empty = v.feature.cannot.be.empty,
                c.id        = "ID" 
        )
        
        
##############################################################################################
# GET AND PROCESS LATEST INCREMENTS
##############################################################################################
        
        # Determine increment files on Dropbox, that need processing.
        df.datachamp.increment.files <- f_get_filenames_in_folder(
                
                c.path       = path.datachamp.dropbox,
                c.file.type  = "csv")

        
        # Latest exported increment data from Oletti.nl via Shopify.
        df.datachamp.increment.latest <- f_read_data_from_file(

                v.file.string            = df.datachamp.increment.files$file.name,
                c.file.type              = "csv",
                c.path                   = path.datachamp.dropbox,
                b.clean.up.header.names  = FALSE,
                b.add.mod.date.path.file = TRUE
                ) %>% 
                
                mutate(
                        
                        # Replace empty cells by 0.
                        `Variants: Inventory quantity` = ifelse(is.na(`Variants: Inventory quantity`), 0, `Variants: Inventory quantity`),
                        
                        # Add timestamp.
                        timestamp.increment = path.file %>%
                               
                               basename() %>%
                               
                               str_extract("[0-9 ]* - [0-9 ]*") %>%
                               
                               trimws()
                ) %>%
                
                # Verwijder kolommen die zijn toegevoegd tgv f_read_dat_from_file.
                select(-mod.date, -path.file) %>%

                # Select laatste update per ID.
                group_by(ID) %>%

                        arrange(desc(`Updated at`)) %>%

                        slice(1) %>%

                ungroup() %>%
                
                # Put timestamp first.
                select(timestamp.increment, everything())
        
        
        # Define object with feature names in baseline file.
        v.datachamp.increment.names <- names(df.datachamp.increment.latest)
        
        
        #'####################################################################################
        # ERROR CHECK
        #'####################################################################################
        
        f_check_col_uniqueness(df.datachamp.increment.latest, "ID")
        f_check_col_uniqueness(df.datachamp.increment.latest, "Variants: SKU")
        
        
        # df.datachamp.increment.latest cannot be empty in v.feature.cannot.be.empty columns.
        f_check_cols_empty_cells(
                
                df.input    = df.datachamp.increment.latest %>% filter(Status == "ACTIVE"),
                v.not.empty = v.feature.cannot.be.empty,
                c.id        = "ID" 
        )
        

        # Are names identical?
        if(!identical(v.datachamp.baseline.names, v.datachamp.increment.names)) {
                
                stop("Note, baseline and increments consist of different features names!")
        }
        
        
##############################################################################################
# PROCESS UPDATED BASELINE
##############################################################################################
        
        # Update baseline.
        df.datachamp.baseline.updated <- rbind(
                
                a = df.datachamp.baseline.source %>%
                        
                        anti_join(
                                
                                y  = df.datachamp.increment.latest,
                                by = "ID"
                        ),
                
                b = df.datachamp.increment.latest
        )
        
        # Comms toi user
        nrow(df.datachamp.baseline.source)
        nrow(df.datachamp.baseline.updated)
        
        nrow(df.datachamp.baseline.source %>% filter(Status == "ACTIVE"))
        nrow(df.datachamp.baseline.updated %>% filter(Status == "ACTIVE"))
        
        
        #'####################################################################################
        # ERROR CHECK
        #'####################################################################################
        
        f_check_col_uniqueness(df.datachamp.baseline.updated,                                "ID")
        f_check_col_uniqueness(df.datachamp.baseline.updated,                                "Variants: SKU")
        f_check_col_uniqueness(df.datachamp.baseline.updated %>% filter(Status == "ACTIVE"), "Variants: SKU")
        

##############################################################################################
# PROCESS INCREMENT CHANGES.
##############################################################################################
        
        # Records in df.datachamp.baseline.updated die verwijderd moeten worden.
        df.datachamp.baseline.to.remove <- df.datachamp.baseline.source %>%
                
                inner_join(
                        
                        y  = df.datachamp.increment.latest %>% select(ID),
                        by = "ID"
                )
        
        
        # Consolidate increment changes.
        df.datachamp.increment.change <- rbind(
                
                a = df.datachamp.increment.latest %>%
                        
                        mutate(
                                status.increment = "added"
                        ),
                
                b = df.datachamp.baseline.to.remove %>%
                        
                        mutate(
                                status.increment    = "removed"
                        )
        ) %>%
                
                arrange(ID, desc(status.increment)) %>%
                
                # Put timestamp first.
                select(timestamp.increment, status.increment, everything())
        
        
        #'####################################################################################
        # ERROR CHECK
        #'####################################################################################
        
        # Do number of rows match?
        if(
                nrow(df.datachamp.baseline.updated) != nrow(df.datachamp.baseline.source) -
                
                nrow(df.datachamp.baseline.to.remove) +
                
                nrow(df.datachamp.increment.latest)
        ) {
                
                stop("Note, something went wrong with wrangling the df.datachamp data frames!")
        }
        
        
##############################################################################################
# PROCESS CHANGE LOG
##############################################################################################
        
        # Generate change log.
        df.datachamp.change.log.source <- df.datachamp.increment.change %>%
                
                # TEST ONLY!!
                # filter(!(ID == "gid://shopify/Product/7085501776035" & status.increment == "removed")) %>%
                
                group_by(ID) %>%
                
                        mutate(n.row = n()) %>%
                
                ungroup() %>%
                
                # Convert all columns to character.
                mutate(across(everything(), as.character))
                
                #select(timestamp.increment, status.increment, ID, Title, `Variants: Price`, n.row)
        
        
        # Process 'new' products.
        df.datachamp.change.log.new <- df.datachamp.change.log.source %>%
                
                # Filter on products that were already in the baseline data.
                filter(n.row == 1) %>%
                
                # Remove unneeded columns.
                select(-n.row) %>%
                
                #pivot_longer(cols = c("timestamp.increment", "Title", "Variants: Price")) %>%
                pivot_longer(
                        
                        cols = v.datachamp.baseline.names[
                                
                                !v.datachamp.baseline.names %in% c("ID", "status.increment", "timestamp.increment")],
                        
                        names_to = "feature.name"
                ) %>%
                
                # Add empty cells.
                mutate(
                        
                        timestamp.increment_from = NA,
                        value_from               = NA
                ) %>%
                
                # Voeg SKU toe. Dit doen we niet bij de pivot, want SKU zou in theorie verandert kunnen zijn. Door te joinen met
                # de updated baseline is er altijd een unieke waarde voor SKU (anders zou er error in pivot plaatsvinden).
                left_join(
                        
                        y  = df.datachamp.baseline.updated %>% select(ID, `Variants: SKU`),
                        by = "ID"
                ) %>%
                
                # Order columns
                select(
                        ID,
                        `Variants: SKU`,
                        feature.name,
                        timestamp.increment_from,
                        timestamp.increment_to   = timestamp.increment,
                        value_from,
                        value_to                 = value
                )
        
        
        # Process 'updated' products.
        df.datachamp.change.log.updated.source <- df.datachamp.change.log.source %>%
                
                # Filter on products that were already in the baseline data.
                filter(n.row == 2) %>%
        
                # Remove unneeded columns.
                select(-n.row) %>%
                
                #pivot_longer(cols = c("timestamp.increment", "Title", "Variants: Price")) %>%
                pivot_longer(
                        
                        cols = v.datachamp.baseline.names[
                                
                                !v.datachamp.baseline.names %in% c("ID", "status.increment", "timestamp.increment")],
                        
                        names_to = "feature.name"
                ) %>%
                
                mutate(
                        from.to = rep(
                                
                                x     = rep(
                                        
                                        x    = c("from", "to"),
                                        
                                        each = ncol(df.datachamp.change.log.source) - 4
                                ),
                                
                                times = nrow(df.datachamp.change.log.source) / 2
                        )
                       
                ) %>%
                
                # Remove unneeded columns.
                select(-status.increment) %>%
                
                #filter(ID == "gid://shopify/Product/7170057568419") %>%
        
                # Put from and to data next to each other.
                pivot_wider(names_from = "from.to", values_from = c("value", "timestamp.increment")) %>%

                mutate(
                        same.from.to = ifelse(
                                
                                is.na(value_from) & is.na(value_to) |
                                value_from %in% c("false", "FALSE") & value_to %in% c("false", "FALSE") |
                                value_from %in% c("true", "TRUE")   & value_to %in% c("true", "TRUE"),
                                
                                TRUE,
                                
                                ifelse(
                                        !is.na(value_from) &  is.na(value_to) |
                                         is.na(value_from) & !is.na(value_to),
                                        
                                        FALSE,
                                        
                                        value_from == value_to
                                )
                        )
                ) %>%
                
                # Voeg SKU toe. Dit doen we niet bij de pivot, want SKU zou in theorie verandert kunnen zijn. Door te joinen met
                # de updated baseline is er altijd een unieke waarde voor SKU (anders zou er error in pivot plaatsvinden).
                left_join(
                        
                        y  = df.datachamp.baseline.updated %>% select(ID, `Variants: SKU`),
                        by = "ID"
                ) %>%
                
                # Order columns
                select(
                        ID,
                        `Variants: SKU`,
                        feature.name,
                        timestamp.increment_from,
                        timestamp.increment_to,
                        value_from,
                        value_to,
                        same.from.to
                ) #%>%
                
                # # Remove rows that are not of interest. Strange that 'Created at' is different... TBD with Max!!
                # filter(
                #         !feature.name %in% c(
                #                 
                #                 "Created at", "Variants: Created at",                            "Variants: Inventory item: Created at",
                #                 "Published at",
                #                 "Updated at", "Variants: Updated at", "Collections: Updated at", "Variants: Inventory item: Updated at")
                # )
        
        
        # Score per ID
        df.datachamp.change.log.updated.score <- df.datachamp.change.log.updated.source %>%
                
                group_by(ID) %>%
                
                        summarise(n.not.same = sum(!same.from.to)) %>%
                
                ungroup()
        
        
        # Process 'updated' products - CONTINUED
        df.datachamp.change.log.updated <- df.datachamp.change.log.updated.source %>%
                
                filter(!same.from.to) %>%
                
                # Remove unneeded columns.
                select(-same.from.to)
        
        
        # Consolidate.
        df.datachamp.change.log <- rbind(
          
                a = df.datachamp.change.log.new,
                b = df.datachamp.change.log.updated
        ) %>%
          
                mutate(timestamp = t.sys.time)
        
        
        #'####################################################################################
        # ERROR CHECK
        #'####################################################################################
        
        # Are increment and baseline the same for some products?
        df.temp <- df.datachamp.change.log.updated.score %>% filter(n.not.same == 0)
        
        if(nrow(df.temp) > 0) {
                
                warning(paste0(
                        
                        "Note, the following ID's occur in the increment files, however, ",
                        "we do not observe any differences with the baseline:\n",
                        f_paste(df.temp$ID)
                ))
                
                df.temp <- df.datachamp.change.log.updated %>% filter(ID %in% df.temp$ID)
                
                # Sla data op.
                f_write_data_to_file(
                
                        x             = df.temp,
                        v.path        = path.deliverables,
                        c.file.string = "Only changes in dates"
                )
        }
        
        
        # Does 'same.from.to' in df.change.log.updated.source contain NAs?
        if(any(is.na(df.datachamp.change.log.updated.source$same.from.to))) {
        
                stop("Note, we observe NAs in 'same.from.to' in 'df.change.log.updated.source'!")        
        }
        
        
##############################################################################################
# WRITE FILES
##############################################################################################

        ######################################################################################
        # UPDATED BASELINE
        ######################################################################################
        
        # Sla data frame op.
        f_write_data_to_file(
                
                x             = df.datachamp.baseline.updated,
                c.file.string = "Oletti Productfeed - Baseline",
                v.path        = paste0(path.datachamp.dropbox, "Baseline - ", str_to_title(c.update.type), "/"),
                v.add.time    = TRUE 
        )

        
        ######################################################################################
        # INCREMENT CHANGES
        ######################################################################################
        
        # Sla data op.
        f_write_data_to_file(

                x             = df.datachamp.increment.change,
                v.path        = paste0(path.datachamp.dropbox, "Increment Changes - ", str_to_title(c.update.type), "/"),
                c.file.string = "Increment Changes",
                v.sheet.name  = "Increment Changes",
                v.xls         = TRUE,
                v.add.time    = TRUE
        )
        
        # PER WEEK EEN NIEUW BESTAND - XLS

  
        
        ######################################################################################
        # CHANGE LOG
        ######################################################################################
        
        # Sla data op.
        f_write_data_to_file(
                
                x             = df.datachamp.change.log,
                v.path        = paste0(path.datachamp.dropbox, "Change Log - ", str_to_title(c.update.type), "/"),
                c.file.string = "Change Log",
                v.sheet.name  = "Change Log",
                v.xls         = TRUE,
                v.add.time    = TRUE
        )
        
        
##############################################################################################
# MOVE INCREMENT FILES FROM ROOT FOLDER TO PROCESSED FOLDER - ONLY IN PRODUCTION
##############################################################################################

        if(c.update.type == "production") {
                
                df.moved.files <- f_move_files(
        
                        c.path.source      = paste0(path.datachamp.dropbox),
                        c.path.destination = paste0(path.datachamp.dropbox, "Processed/")
                )
        }
        
        
   