#!/bin/Rscript


# Script to determine if channel names are appropriately mapped to alias


# Load packages
suppressPackageStartupMessages(library(argparse))
suppressPackageStartupMessages(library(dplyr))


# Argument parsing
parser <- ArgumentParser()

# specify our desired options 
# by default ArgumentParser will add an help option 
parser$add_argument("-p", "--project", default="MyProject",
                    help="Project name")
parser$add_argument("-s", "--sample-list", 
                    help="File with sample list (two-column tab-delimited file - sample ID and file path)")
parser$add_argument("-c", "--current-alias", help="Path to current alias file")
parser$add_argument("-b", "--bad-samples", help = "List of sample IDs to exclude", default=NULL)

# get command line options, if help option encountered print help and exit,
# otherwise if options not found on command line then set defaults, 
args <- parser$parse_args()

# Define inputs
project <- args$project
universal_alias <- args$current_alias #"~/luna/libs/fixed_alias_SAGIC.txt"
sample_list <- args$sample_list #"SAGICTEST.lst"
bad <- args$bad_samples #"bad_samples.txt"


check_aliases <- function(universal_alias, project, sample_list, bad=NULL) {
        
        # Helper functions
        
        stopQuietly <- function(...) {
                blankMsg <- sprintf("\r%s\r", paste(rep(" ", getOption("width")-1L), collapse=" "));
                stop(simpleError(blankMsg));
        } # stopQuietly()
        
        
        # Load most current alias
        #system("module load luna-base/0.2")
        #universal_alias <- "../libs/fixed_alias_SAGIC.txt"
        #sample_list  <- "BOSA.lst"
        #bad <- NULL
        #project <- "BOSA"
        
        current_alias <- read.table(universal_alias, sep = "\t", header = F, stringsAsFactors = F)
        unique_alias <- unlist(strsplit(paste(current_alias$V2, collapse="|"), "\\|"))
        
        #exclude part
        ex_part <- paste0(' exclude=',bad)
        if (is.null(bad)) {ex_part <- ""}
        
        # Run luna to get the channel names from headers
        luna_get_header_db <- c("#!/bin/bash",
                                paste0('luna ',sample_list, ex_part,' -o ',project,'_original_headers.db -s HEADERS'))
        write(luna_get_header_db, "luna_get_header_db.sh")
        system("bash luna_get_header_db.sh")
        
        destrat_get_header_db <- c("#!/bin/bash",
                                   paste0('destrat ',project,'_original_headers.db +HEADERS -r CH -v SR > ',project,'_original_headers.txt'))
        write(destrat_get_header_db, "destrat_get_header_db.sh")
        system("bash destrat_get_header_db.sh")
        
        # Read header in R and get channels not present in universal alias
        original_headers <- unique(read.table(paste0(project,'_original_headers.txt'), header = T, sep = "\t", stringsAsFactors = F)[,2])
        ch_not_present <- original_headers[!original_headers %in% unique_alias]
        
        candidate_list <- NULL
        for (ch in ch_not_present) {
                
                if(length(agrep(ch,current_alias$V2, value = T))==0) {
                        
                        candidate_df <- data.frame(candidates_full=NA,
                                                   candidates_short=NA,
                                                   assigned=F, stringsAsFactors = F)
                        
                        
                        message(paste0("No suggested macthes for channel: ", ch))
                        cat("\n")
                        candidate_list[[ch]] <- candidate_df
                        next()
                        
                }
                
                candidate_df <- data.frame(candidates_full=agrep(ch,current_alias$V2, value = T),
                                           candidates_short=sapply(strsplit(agrep(ch,current_alias$V2, value = T), "\\|"), "[[",1),
                                           assigned=F, stringsAsFactors = F)
                
                
                
                
                for (cand in candidate_df$candidates_short) {
                        skip=F
                        message(paste0("Channel name: ", ch))
                        message(paste0("All potential matches: ", paste(candidate_df$candidates_short, collapse = ",")))
                        message(paste0("Currently assessing: ", cand))
                        
                        ok=F
                        while (ok==F) {
                                
                                
                                
                                cat("Is this correct? (1-yes, 2-No, 3-cancel)\n")
                                #n <- readLines('stdin', n = -1)
                                #print(n)

                                read_user <- function()
                                { 
                                        n <- readline(prompt="Answer: ")
                                        return(n)
                                }
                                
                                print(paste("You answered:", read_user()))
                                
                                
                                if (n=="1") {
                                        # Add ch to corresponding row in current_alias
                                        candidate_df[candidate_df$candidates_short==cand,"assigned"] <- T
                                        message("Good!")
                                        cat("\n")
                                        ok=T
                                        skip=T
                                        break()
                                } else if (n=="2") {
                                        # keep ch as non-resolved
                                        message("No problem.")
                                        cat("\n")
                                        ok=T
                                } else if (n=="3") {
                                        stop("check_aliases cancelled.")
                                } else {
                                        message("Must provide valid answer.")
                                        cat("\n")
                                }
                                
                        }
                        if(skip==T) {break()}
                        
                        
                }
                
                candidate_list[[ch]] <- candidate_df
                
        }
        
        assigned_df <- bind_rows(candidate_list, .id = "original_channels") %>% filter(assigned)
        matches_df <- data.frame(original_channels=names(candidate_list), stringsAsFactors = F)
        matches_df <- merge(matches_df, select(assigned_df, original_channels, candidates_short), by="original_channels", all.x=T)
        colnames(matches_df)[2] <- c("standard_name")
        
        
        # Give the chance to match the nonMatched
        cat("Would you like assistance to assign non matched channels? (1-Yes, 2-No, 3-cancel)\n")
        try_manual <- readLines(con='stdin', n = 1L)
        
        if (try_manual=="3") { stop("check_aliases cancelled.") }
        
        if (try_manual=="2") {
                
                # Save data frame to be completed by user
                write.csv(data.frame(original_channel=notMatched, standard_name=NA, stringsAsFactors = F), paste0(project, "_ChannelNamesToComplete.csv"), row.names = F, quote = F)
                cat("\n")
                message(paste0("Please complete the file ", project, "_ChannelNamesToComplete.csv with standard channel names."))
                message("Use this file to update current alias definition for your project.")
                stopQuietly()
        }
        
        if (try_manual=="1") {
                
                message("Here are all the possible standard channel names:")
                message(paste(paste0(1:length(current_alias$V2), ". ", sapply(strsplit(current_alias$V2, "\\|"), "[[",1)), collapse = "\n"))
                cat("\n")
                
                for (ch in matches_df$original_channels[is.na(matches_df$standard_name)]) {
                        
                        cat("\n")
                        message(paste0("Currently assessing: ", ch))
                        cat("Which standard name represents this channel? (select number above, or 0 to skip, or c to cancel) ")
                        n_ch <- readLines(con='stdin', n = 1L)
                        
                        if (n_ch=="c") { stop("check_aliases cancelled.") }
                        
                        if(!(n_ch %in% as.character(0:length(current_alias$V2)))) {stop("invalid option.")}
                        
                        if (n_ch=="0") { next() }
                        
                        std_name <- sapply(strsplit(current_alias$V2, "\\|"), "[[",1)[as.numeric(n_ch)]
                        if(length(std_name)==0) {std_name=NA}
                        
                        matches_df$standard_name[matches_df$original_channels==ch] <- std_name
                        
                }
                
                write.csv(matches_df, paste0(project, "_SuccesfulChannelMatches.csv"), row.names = F, quote = F)
                cat("\n")
                message(paste0("The file ", project, "_SuccesfulChannelMatches.csv was created!"))
                message("Use this file to update current alias definition for your project.")
                message("Please note that channels without a match are represented as NA in the 'standard_name' column.")
                
        }

}

check_aliases(universal_alias = universal_alias, project = project, sample_list = sample_list, bad = bad)





