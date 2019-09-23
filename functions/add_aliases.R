### Add new aliases, from result of check_aliases or manually entered.
### Manual entry: file with 2 columns, csv, first column with original channel names, and second column with standardized name

#current_alias <- "../libs/fixed_alias_SAGIC.txt"
#new_alias <- "BOSA_SuccesfulChannelMatches.csv"

# Load packages
suppressPackageStartupMessages(library(argparse))

# Argument parsing
parser <- ArgumentParser()

# specify our desired options 
# by default ArgumentParser will add an help option 
parser$add_argument("-n", "--new-aliases", 
                    help="File with new aliases (result of check_aliases.R)")
parser$add_argument("-c", "--current-alias", help="Path to current alias file")
parser$add_argument("-d", "--destination-folder", help="Path to destination folder", default="/home/diegomaz/luna/libs/")

# get command line options, if help option encountered print help and exit,
# otherwise if options not found on command line then set defaults, 
args <- parser$parse_args()

# Define inputs
current_alias <- args$current_alias
new_alias <- args$new_alias
destination_folder <- args$destination_folder


add_aliases <- function(current_alias, new_alias, dest="/home/diegomaz/luna/libs/") {
        
        # Load current alias
        current_alias_df <- read.table(current_alias, sep = "\t", header = F, stringsAsFactors = F)
        
        # Load new alias
        new_alias_df <- read.csv(new_alias,  header = T, stringsAsFactors = F)

        #Get non NA matches
        new_alias_df <- na.exclude(new_alias_df)
        rownames(new_alias_df) <- NULL
        
        updated_alias_df <- current_alias_df
        
        # For each standard name, add original channel to the list of alias
        for (i in 1:nrow(new_alias_df)) {
                
                id <- grep(new_alias_df$standard_name[i], updated_alias_df$V2)
                updated_alias_df$V2[id] <- paste0(updated_alias_df$V2[id], "|", new_alias_df$original_channels[i])
                
        }
        
        
        # Split the | and combine
        # Fix quotes
        pasteQuotesandCollapsePipe <- function(vec) {
                
                return(paste(paste0('"', vec, '"'), collapse = "|"))
                
        }
        
        updated_alias_df$V2 <- sapply(strsplit(updated_alias_df$V2, "\\|"), pasteQuotesandCollapsePipe)
        
        # Save to file
        fname <- paste0("updated_alias_",gsub(":", "", gsub(" ", "", gsub("-", "", Sys.time()))), ".txt")
        pname <- paste0(dest,fname)
        
       write.table(updated_alias_df, pname, quote = F, sep = "\t", row.names = F, col.names = F)
        
        # Fix EOL
        system(paste0("tr -d '\r' < ", pname, " > ", dest, "fixed_", fname))
        
        message(paste0("Adding new aliases from ", new_alias, " to fixed_", pname))
        message("Done!")
        
}

add_aliases(current_alias, new_alias, dest=destination_folder)
