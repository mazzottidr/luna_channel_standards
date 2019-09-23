# Find channels that need reference, reference and copy


library(dplyr)

reference_channels_luna <- function(header_file, project, keepch, rm.original=F, dir="edfs_part02/", tag="part02", smplst=paste0(project,"_part02.lst")) {
        
        std_headers <- read.table(header_file, header = T, sep = "\t", stringsAsFactors = F, fill = T)[,1:2]
        std_headers_list <- split(std_headers$CH, std_headers$ID)
        
        # Identify which samples do not have EEG_C3_M2, EEG_C4_M1, EEG_F3_M2, EEG_F4_M1, EEG_O1_M2, EEG_O2_M1, EMG_CHIN, EOG_E1_M, EOG_E2_M, but have individual components
        sample_status_df <-  data.frame(smp_id=names(std_headers_list), stringsAsFactors = F)
        
        ## EEG
        
        sample_status_df$hasEEG_C3_M2 <- grepl("\\bEEG_C3_M2\\b", std_headers_list)
        sample_status_df$hasEEG_C3_and_M2 <- grepl("\\bEEG_C3\\b", std_headers_list) &  grepl("\\bEEG_M2\\b", std_headers_list) & !grepl("\\bEEG_C3_M2\\b", std_headers_list)
        sample_status_df$hasEEG_C3_only <- grepl("\\bEEG_C3\\b", std_headers_list) &  !grepl("\\bEEG_M2\\b", std_headers_list) & !grepl("\\bEEG_C3_M2\\b", std_headers_list)
        sample_status_df$hasEEG_C3_M2_solution <- sample_status_df$hasEEG_C3_M2 | sample_status_df$hasEEG_C3_and_M2 | sample_status_df$hasEEG_C3_only
        sample_status_df$EEG_C3_M2_action <- "skip"
        sample_status_df$EEG_C3_M2_action[sample_status_df$hasEEG_C3_M2] <- "nothing"
        sample_status_df$EEG_C3_M2_action[sample_status_df$hasEEG_C3_and_M2] <- "reference"
        sample_status_df$EEG_C3_M2_action[sample_status_df$hasEEG_C3_only] <- "use C3"
        
        sample_status_df$hasEEG_C4_M1 <- grepl("\\bEEG_C4_M1\\b", std_headers_list)
        sample_status_df$hasEEG_C4_and_M1 <- grepl("\\bEEG_C4\\b", std_headers_list) &  grepl("\\bEEG_M1\\b", std_headers_list) & !grepl("\\bEEG_C4_M1\\b", std_headers_list)
        sample_status_df$hasEEG_C4_only <- grepl("\\bEEG_C4\\b", std_headers_list) &  !grepl("\\bEEG_M1\\b", std_headers_list) & !grepl("\\bEEG_C4_M1\\b", std_headers_list)
        sample_status_df$hasEEG_C4_M1_solution <- sample_status_df$hasEEG_C4_M1 | sample_status_df$hasEEG_C4_and_M1 | sample_status_df$hasEEG_C4_only
        sample_status_df$EEG_C4_M1_action <- "skip"
        sample_status_df$EEG_C4_M1_action[sample_status_df$hasEEG_C4_M1] <- "nothing"
        sample_status_df$EEG_C4_M1_action[sample_status_df$hasEEG_C4_and_M1] <- "reference"
        sample_status_df$EEG_C4_M1_action[sample_status_df$hasEEG_C4_only] <- "use C4"
        
        sample_status_df$hasEEG_F3_M2 <- grepl("\\bEEG_F3_M2\\b", std_headers_list)
        sample_status_df$hasEEG_F3_and_M2 <- grepl("\\bEEG_F3\\b", std_headers_list) &  grepl("\\bEEG_M2\\b", std_headers_list) & !grepl("\\bEEG_F3_M2\\b", std_headers_list)
        sample_status_df$hasEEG_F3_only <- grepl("\\bEEG_F3\\b", std_headers_list) &  !grepl("\\bEEG_M2\\b", std_headers_list) & !grepl("\\bEEG_F3_M2\\b", std_headers_list)
        sample_status_df$hasEEG_F3_M2_solution <- sample_status_df$hasEEG_F3_M2 | sample_status_df$hasEEG_F3_and_M2 | sample_status_df$hasEEG_F3_only
        sample_status_df$EEG_F3_M2_action <- "skip"
        sample_status_df$EEG_F3_M2_action[sample_status_df$hasEEG_F3_M2] <- "nothing"
        sample_status_df$EEG_F3_M2_action[sample_status_df$hasEEG_F3_and_M2] <- "reference"
        sample_status_df$EEG_F3_M2_action[sample_status_df$hasEEG_F3_only] <- "use F3"
        
        sample_status_df$hasEEG_F4_M1 <- grepl("\\bEEG_F4_M1\\b", std_headers_list)
        sample_status_df$hasEEG_F4_and_M1 <- grepl("\\bEEG_F4\\b", std_headers_list) &  grepl("\\bEEG_M1\\b", std_headers_list) & !grepl("\\bEEG_F4_M1\\b", std_headers_list)
        sample_status_df$hasEEG_F4_only <- grepl("\\bEEG_F4\\b", std_headers_list) &  !grepl("\\bEEG_M1\\b", std_headers_list) & !grepl("\\bEEG_F4_M1\\b", std_headers_list)
        sample_status_df$hasEEG_F4_M1_solution <- sample_status_df$hasEEG_F4_M1 | sample_status_df$hasEEG_F4_and_M1 | sample_status_df$hasEEG_F4_only
        sample_status_df$EEG_F4_M1_action <- "skip"
        sample_status_df$EEG_F4_M1_action[sample_status_df$hasEEG_F4_M1] <- "nothing"
        sample_status_df$EEG_F4_M1_action[sample_status_df$hasEEG_F4_and_M1] <- "reference"
        sample_status_df$EEG_F4_M1_action[sample_status_df$hasEEG_F4_only] <- "use F4"
        
        sample_status_df$hasEEG_O1_M2 <- grepl("\\bEEG_O1_M2\\b", std_headers_list)
        sample_status_df$hasEEG_O1_and_M2 <- grepl("\\bEEG_O1\\b", std_headers_list) &  grepl("\\bEEG_M2\\b", std_headers_list) & !grepl("\\bEEG_O1_M2\\b", std_headers_list)
        sample_status_df$hasEEG_O1_only <- grepl("\\bEEG_O1\\b", std_headers_list) &  !grepl("\\bEEG_M2\\b", std_headers_list) & !grepl("\\bEEG_O1_M2\\b", std_headers_list)
        sample_status_df$hasEEG_O1_M2_solution <- sample_status_df$hasEEG_O1_M2 | sample_status_df$hasEEG_O1_and_M2 | sample_status_df$hasEEG_O1_only
        sample_status_df$EEG_O1_M2_action <- "skip"
        sample_status_df$EEG_O1_M2_action[sample_status_df$hasEEG_O1_M2] <- "nothing"
        sample_status_df$EEG_O1_M2_action[sample_status_df$hasEEG_O1_and_M2] <- "reference"
        sample_status_df$EEG_O1_M2_action[sample_status_df$hasEEG_O1_only] <- "use O1"
        
        sample_status_df$hasEEG_O2_M1 <- grepl("\\bEEG_O2_M1\\b", std_headers_list)
        sample_status_df$hasEEG_O2_and_M1 <- grepl("\\bEEG_O2\\b", std_headers_list) &  grepl("\\bEEG_M1\\b", std_headers_list) & !grepl("\\bEEG_O2_M1\\b", std_headers_list)
        sample_status_df$hasEEG_O2_only <- grepl("\\bEEG_O2\\b", std_headers_list) &  !grepl("\\bEEG_M1\\b", std_headers_list) & !grepl("\\bEEG_O2_M1\\b", std_headers_list)
        sample_status_df$hasEEG_O2_M1_solution <- sample_status_df$hasEEG_O2_M1 | sample_status_df$hasEEG_O2_and_M1 | sample_status_df$hasEEG_O2_only
        sample_status_df$EEG_O2_M1_action <- "skip"
        sample_status_df$EEG_O2_M1_action[sample_status_df$hasEEG_O2_M1] <- "nothing"
        sample_status_df$EEG_O2_M1_action[sample_status_df$hasEEG_O2_and_M1] <- "reference"
        sample_status_df$EEG_O2_M1_action[sample_status_df$hasEEG_O2_only] <- "use O2"
        
        
        ## EOG
        sample_status_df$hasEOG_E1_M <- grepl("\\bEOG_E1_M\\b", std_headers_list)
        sample_status_df$hasEOG_E1_and_M <- grepl("\\bEOG_E1\\b", std_headers_list) &  (grepl("\\bEEG_M1\\b", std_headers_list) | grepl("\\bEEG_M2\\b", std_headers_list)) & !grepl("\\bEOG_E1_M\\b", std_headers_list)
        sample_status_df$hasEOG_E1_only <- grepl("\\bEOG_E1\\b", std_headers_list) &  !grepl("\\bEEG_M1\\b", std_headers_list) & !grepl("\\bEEG_M2\\b", std_headers_list) & !grepl("\\bEOG_E1_M\\b", std_headers_list)
        sample_status_df$hasEOG_E1_M_solution <- sample_status_df$hasEOG_E1_M | sample_status_df$hasEOG_E1_and_M | sample_status_df$hasEOG_E1_only
        sample_status_df$EOG_E1_M_action <- "skip"
        sample_status_df$EOG_E1_M_action[sample_status_df$hasEOG_E1_M] <- "nothing"
        sample_status_df$EOG_E1_M_action[sample_status_df$hasEOG_E1_and_M] <- "reference"
        sample_status_df$EOG_E1_M_action[sample_status_df$hasEOG_E1_only] <- "use E1"
        
        sample_status_df$hasEOG_E2_M <- grepl("\\bEOG_E2_M\\b", std_headers_list)
        sample_status_df$hasEOG_E2_and_M <- grepl("\\bEOG_E2\\b", std_headers_list) &  (grepl("\\bEEG_M1\\b", std_headers_list) | grepl("\\bEEG_M2\\b", std_headers_list)) & !grepl("\\bEOG_E2_M\\b", std_headers_list)
        sample_status_df$hasEOG_E2_only <- grepl("\\bEOG_E2\\b", std_headers_list) &  !grepl("\\bEEG_M1\\b", std_headers_list) & !grepl("\\bEEG_M2\\b", std_headers_list) & !grepl("\\bEOG_E2_M\\b", std_headers_list)
        sample_status_df$hasEOG_E2_M_solution <- sample_status_df$hasEOG_E1_M | sample_status_df$hasEOG_E1_and_M | sample_status_df$hasEOG_E1_only
        sample_status_df$EOG_E2_M_action <- "skip"
        sample_status_df$EOG_E2_M_action[sample_status_df$hasEOG_E2_M] <- "nothing"
        sample_status_df$EOG_E2_M_action[sample_status_df$hasEOG_E2_and_M] <- "reference"
        sample_status_df$EOG_E2_M_action[sample_status_df$hasEOG_E2_only] <- "use E2"
        
        ## EMG
        sample_status_df$hasEMG_CHIN <- grepl("\\bEMG_CHIN\\b", std_headers_list)
        sample_status_df$hasEMG_CHIN1_and_EMG_CHIN2 <- grepl("\\bEMG_CHIN1\\b", std_headers_list) &  grepl("\\bEMG_CHIN2\\b", std_headers_list) & !grepl("\\bEMG_CHIN\\b", std_headers_list)
        sample_status_df$hasEMG_CHIN1_only <- grepl("\\bEMG_CHIN1\\b", std_headers_list) &  !grepl("\\bEMG_CHIN2\\b", std_headers_list) & !grepl("\\bEMG_CHIN\\b", std_headers_list)
        sample_status_df$hasEMG_CHIN2_only <- grepl("\\bEMG_CHIN2\\b", std_headers_list) &  !grepl("\\bEMG_CHIN1\\b", std_headers_list) & !grepl("\\bEMG_CHIN\\b", std_headers_list)
        sample_status_df$hasEMG_CHIN_solution <- sample_status_df$hasEMG_CHIN | sample_status_df$hasEMG_CHIN1_and_EMG_CHIN2 | sample_status_df$hasEMG_CHIN1_only | sample_status_df$hasEMG_CHIN2_only
        sample_status_df$EMG_CHIN_action <- "skip"
        sample_status_df$EMG_CHIN_action[sample_status_df$hasEMG_CHIN] <- "nothing"
        sample_status_df$EMG_CHIN_action[sample_status_df$hasEMG_CHIN1_and_EMG_CHIN2] <- "reference"
        sample_status_df$EMG_CHIN_action[sample_status_df$hasEMG_CHIN1_only] <- "use EMG_CHIN1"
        sample_status_df$EMG_CHIN_action[sample_status_df$hasEMG_CHIN2_only] <- "use EMG_CHIN2"
        
        # for each sample, add corresponding command
        
        for (smp_i in 1:length(sample_status_df$smp_id)) {
                
                
                rule <- sample_status_df %>%
                        select(smp_id,contains("action")) %>%
                        slice(smp_i)
                
                
                
                # Write REFERENCE commands
                
                cmd <- ""
                
                if (rule$EEG_C3_M2_action=="reference") {
                        
                        cmd <- c(cmd, "REFERENCE sig=EEG_C3 ref=EEG_M2")
                        
                } 
                
                if (rule$EEG_C4_M1_action=="reference") {
                        
                        cmd <- c(cmd, "REFERENCE sig=EEG_C4 ref=EEG_M1")
                        
                } 
                
                if (rule$EEG_F3_M2_action=="reference") {
                        
                        cmd <- c(cmd, "REFERENCE sig=EEG_F3 ref=EEG_M2")
                        
                } 
                
                if (rule$EEG_F4_M1_action=="reference") {
                        
                        cmd <- c(cmd, "REFERENCE sig=EEG_F4 ref=EEG_M1")
                        
                } 
                
                if (rule$EEG_O1_M2_action=="reference") {
                        
                        cmd <- c(cmd, "REFERENCE sig=EEG_O1 ref=EEG_M2")
                        
                } 
                
                if (rule$EEG_O2_M1_action=="reference") {
                        
                        cmd <- c(cmd, "REFERENCE sig=EEG_O2 ref=EEG_M1")
                        
                } 
                
                if (rule$EOG_E1_M_action=="reference") {
                        
                        cmd <- c(cmd, "REFERENCE sig=EOG_E1 ref=EEG_M1") # EOG will always be referenced to M1
                        
                } 
                
                if (rule$EOG_E2_M_action=="reference") {
                        
                        cmd <- c(cmd, "REFERENCE sig=EOG_E2 ref=EEG_M1") # EOG will always be referenced to M1
                        
                } 
                
                if (rule$EMG_CHIN_action=="reference") {
                        
                        cmd <- c(cmd, "REFERENCE sig=EMG_CHIN2 ref=EMG_CHIN1")
                        
                } 
                
                ### add WRITE
                cmd <- c(cmd, paste0("WRITE edf-dir=",dir, " edf-tag=",tag, " sample-list=", smplst))
                
                
                # write alias file
                
                alias_text <- ""
                
                if (rule$EEG_C3_M2_action=="reference") {
                        
                        alias_text <- c(alias_text, "EEG_C3_M2|EEG_C3")
                        
                } else if (rule$EEG_C3_M2_action=="use C3") {
                        
                        alias_text <- c(alias_text, "EEG_C3_M2|EEG_C3")
                }
                
                if (rule$EEG_C4_M1_action=="reference") {
                        
                        alias_text <- c(alias_text, "EEG_C4_M1|EEG_C4")
                        
                } else if (rule$EEG_C4_M1_action=="use C4") {
                        
                        alias_text <- c(alias_text, "EEG_C4_M1|EEG_C4")
                }
                
                
                
                if (rule$EEG_F3_M2_action=="reference") {
                        
                        alias_text <- c(alias_text, "EEG_F3_M2|EEG_F3")
                        
                } else if (rule$EEG_F3_M2_action=="use F3") {
                        
                        alias_text <- c(alias_text, "EEG_F3_M2|EEG_F3")
                }
                
                
                
                if (rule$EEG_F4_M1_action=="reference") {
                        
                        alias_text <- c(alias_text, "EEG_F4_M1|EEG_F4")
                        
                } else if (rule$EEG_F4_M1_action=="use F4") {
                        
                        alias_text <- c(alias_text, "EEG_F4_M1|EEG_F4")
                }
                
                
                
                if (rule$EEG_O1_M2_action=="reference") {
                        
                        alias_text <- c(alias_text, "EEG_O1_M2|EEG_O1")
                        
                } else if (rule$EEG_O1_M2_action=="use O1") {
                        
                        alias_text <- c(alias_text, "EEG_O1_M2|EEG_O1")
                }
                
                
                
                if (rule$EEG_O2_M1_action=="reference") {
                        
                        alias_text <- c(alias_text, "EEG_O2_M1|EEG_O2")
                        
                } else if (rule$EEG_O2_M1_action=="use O2") {
                        
                        alias_text <- c(alias_text, "EEG_O2_M1|EEG_O2")
                }
                
                
                
                # EOG
                if (rule$EOG_E1_M_action=="reference") {
                        
                        alias_text <- c(alias_text, "EOG_E1_M|EOG_E1")
                        
                } else if (rule$EOG_E1_M_action=="use E1") {
                        
                        alias_text <- c(alias_text, "EOG_E1_M|EOG_E1")
                }
                
                if (rule$EOG_E2_M_action=="reference") {
                        
                        alias_text <- c(alias_text, "EOG_E2_M|EOG_E2")
                        
                } else if (rule$EOG_E2_M_action=="use E2") {
                        
                        alias_text <- c(alias_text, "EOG_E2_M|EOG_E2")
                }
                
                # EMG
                
                if (rule$EMG_CHIN_action=="reference") {
                        
                        alias_text <- c(alias_text, "EMG_CHIN|EMG_CHIN2")
                        
                } else if (rule$EMG_CHIN_action=="use EMG_CHIN1") {
                        
                        alias_text <- c(alias_text, "EMG_CHIN|EMG_CHIN1")
                        
                } else if (rule$EMG_CHIN_action=="use EMG_CHIN2") {
                        
                        alias_text <- c(alias_text, "EMG_CHIN|EMG_CHIN2")
                        
                }
                
                
                
                
                
                # Create alias file
                alias_df <- data.frame(alias="alias",alias_text)[-1,]
                
                
                # Save current alias and current cmd
                write.table(alias_df, "current_part02_alias.txt", sep = "\t", row.names = F, col.names = F, quote = F)
                write.table(cmd, "current_part02_cmd.txt", sep = "\t", row.names = F, col.names = F, quote = F)
                
                #Fix EOL alias
                system("tr -d '\r' < current_part02_alias.txt > tmp")
                system("mv tmp current_part02_alias.txt")
                system("tr -d '\r' < current_part02_cmd.txt > tmp")
                system("mv tmp current_part02_cmd.txt")
                
                # Run luna
                
                luna_cmd <- c("#!/bin/bash",
                                paste0('luna ', project,'_part01.lst ', sample_status_df$smp_id[smp_i],' ',ex_part,' sig="',keepch,'" @current_part02_alias.txt < current_part02_cmd.txt'))
                
                
                
                
                if (rm.original) {
                        
                        luna_cmd <- c(luna_cmd, paste0("grep -w ", sample_status_df$smp_id[smp_i]," ", project,"_part01.lst | cut -f 2 | xargs rm"))
                }
                
                write(luna_cmd, "current_luna_part02.sh")
                
                system("bash current_luna_part02.sh")
                
                
        }
        
        if (rm.original) {
                
                system("rm -R edfs_part01/")
        }
        
        
        
        
}

