##############################################################
# FUNCTION Give basic info of vector.
#
# Name:    Pieter Overdevest.
# Date:    Mei 8, 2020.
# Version: 7.
#
##############################################################

    f_vector_info <- function(v,
                              name,
                              n.top,
                              show.freq) {

    ##############################################################################
    # Error check.
    ##############################################################################
    
    # v = df.dictionary$bord.type
    # name = "df.dictionary$bord.type"
    # n.top = 3
    # show.freq = TRUE
        
        
    ##############################################################################
    # Error check.
    ##############################################################################
        
    if(!is.numeric(n.top) & n.top != "all")
        stop("n.top moet valide waarde bevatten: integer of 'all'")
        
        
    ##############################################################################
    # Analyse data.
    ##############################################################################
    
    
        # Calculate basic info.
        df.basic.info <- data.frame(x  = c("Total elements:",
                                           "Unique elements:",
                                           "NA:",
                                           "0:",
                                           "Empty:"),
                                    
                                    y = c(length(v),
                                          length(unique(v)),
                                          sum(is.na(v)),
                                          sum(v == 0, na.rm = TRUE),
                                          sum(v == "", na.rm = TRUE)),
                                    
                                    z = c("",
                                          "",
                                          paste(round(
                                                  sum(is.na(v)) / length(v) * 100,
                                                  digits = 1),
                                                "%"),
                                          
                                          paste(round(
                                                  sum(v == 0, na.rm = TRUE) / length(v) * 100,
                                                  digits = 1),
                                                "%"),
                                          
                                          paste(round(
                                                  sum(v == "", na.rm = TRUE) / length(v) * 100,
                                                  digits = 1),
                                                "%")
                                          )) 
        
    
        names(df.basic.info) <- c("============================", "======", "======")
         
        # Print header.
        cat(paste0("\n ", name, "\n"))
        
        cat(paste0(rep(" ", 29), collapse = ""), "n      perc\n")
        
        # Show in console, left align.
        print(x         = df.basic.info,
              row.names = FALSE,
              right     = FALSE)
        
        
        # Show frequency table.
        if (show.freq) {
                
                # Replace any NA by "NA"
                v[is.na(v)] <- "NA"
                
                # Calculate frequency of levels in vector.
                df.freq.source <- as.data.frame(table(v)) %>%
                        
                        arrange(desc(Freq), v) %>%
                        
                        mutate(perc = Freq / sum(Freq) * 100)

                
                df.dots <- data.frame(v    = "...",
                                      Freq = "...",
                                      perc = "..."
                                      )                            
                        
                
                df.total <- data.frame(v    = c("----------------", "TOTAL"),
                                       Freq = c("------", sum(df.freq.source$Freq)),
                                       perc = c("------", paste(round(sum(df.freq.source$perc), digits = 1), "%"))
                                       )
                
                
                df.freq <- df.freq.source %>%
                        
                        mutate(perc = paste(round(df.freq.source$perc, digits = 1), "%")) %>%
                    
                        head(n.top)
                
                # Puntjes toevoegen als n.top een getal is.
                if(is.numeric(n.top))
                    df.freq <- rbind(df.freq, df.dots)
                
                # Total toevoegen.
                df.freq <- rbind(df.freq, df.total)
                
                names(df.freq) <- c("============================", "======", "======")
                
                
                
                # Header frequency table.
                cat(paste0("\n Frequency table ",
                           
                           if(is.numeric(n.top)) {
                               
                                if(nrow(df.freq.source) < n.top)
                                       
                                    {"(all items): "} else {paste0("(Top-" , n.top, "):    ")}
                                   
                                } else {"(all items): "},
                           
                           "n      perc\n"))
                
                
                
                print(x         = df.freq,
                      row.names = FALSE,
                      right     = FALSE)
                
                }
        }