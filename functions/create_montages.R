##### This script needs to be included in the main pipeline!!!


# Create Montages and Montages Summary per sample

library(dplyr)

create_montages <- function(project, magdy_std_path="~/luna_channel_standards/libs/Magdy_Montages_Standards.csv") {
        
        final_headers <- read.table(paste0(project, "_post_std_headers.txt"), header = T, sep = "\t", stringsAsFactors = F, fill = T)
        final_headers_list <- split(final_headers$CH, final_headers$ID)
        
        # Get max channel number and create data frame with NA
        max_ch_n <- max(sapply(final_headers_list, length))
        montage_df <- data.frame(matrix(NA, ncol =max_ch_n+1, nrow=length(final_headers_list)), stringsAsFactors = F)
        montage_df$X1 <- names(final_headers_list)
        colnames(montage_df) <- c("SampleID", paste0("Ch",1:max_ch_n))
        
        for(smp_id in 1:nrow(montage_df)) {
                
                l <- length(final_headers_list[[smp_id]])
                montage_df[smp_id,2:(l+1)] <- final_headers_list[[smp_id]]
                
        }
        
        # Find unique montages
        
        unique_montages <- distinct(montage_df[,2:ncol(montage_df)], .keep_all=T)
        unique_montages$String <-apply(unique_montages,1,paste,collapse="|")
        unique_montages$Montage <- paste("Montage", 1:nrow(unique_montages))
        
        # Create string for all individual montages
        montage_df$String <- apply(montage_df[,2:ncol(montage_df)],1,paste,collapse="|")
        
        # Merge
        montage_df <- select(merge(montage_df, select(unique_montages, Montage, String), by="String", all.x = T), Montage, everything(), -String)
        
        
        montage_t <- data.frame(t(select(unique_montages, Montage, everything(), -String)), stringsAsFactors = F)
        colnames(montage_t) <- montage_t[1,]
        montage_t <- slice(montage_t, -1)
        
        #Load Magdy's standards
        magdy_std <- read.csv(magdy_std_path, stringsAsFactors = F)
        magdy_std$Order <- 1:nrow(magdy_std)
        
        mdata <- magdy_std 
        
        for (col_id in 1:ncol(montage_t)) {
                
                montage_name <- colnames(montage_t)[col_id]
                
                vec <- montage_t[,col_id]
                
                current_montage_df <- data.frame(Montage=vec, Montage_num=c(paste0(1:sum(!is.na(vec)), ". ", vec[!is.na(vec)]),  vec[is.na(vec)]), stringsAsFactors = F)
                
                mdata <- merge(mdata, current_montage_df, by.x="Standards", by.y="Montage", sort=F, all.x=T)
                
                mdata <- arrange(select(mdata, Montages, Order, everything()), Order)
                
                colnames(mdata)[which(colnames(mdata)=="Montage_num")] <- montage_name
                
        }
        
        # Save Magdy montage file
        montage_file <- select(mdata, Montages, everything(), -Order, -Standards)
        write.csv(montage_file, paste0(project, "_MontageMapMagdy.csv"), row.names = F, quote = F, na = "")
        
        
        # Save individual montage map
        # Load final sample list
        luna_sample_list <- read.table(paste0(project,"_processed.lst"), header = F, sep = "\t", stringsAsFactors = F)
        colnames(luna_sample_list) <- c("SampleID", "Path")
        luna_sample_list$Filename <- sapply(strsplit(luna_sample_list$Path, "/"), "[[", length(strsplit(luna_sample_list$Path, "/")[[1]]))
        
        montage_map <- select(merge(luna_sample_list, select(montage_df, SampleID, Montage), by = "SampleID", all.x=T), SampleID, Filename, Montage)
        write.csv(montage_map, paste0(project, "_IndividualMontages.csv"), row.names = F, quote = F, na = "")
        
        # Save generic montage map
        generic_map <- select(montage_df, SampleID, Montage, everything()) %>% arrange(SampleID)
        write.csv(generic_map, paste0(project, "_GenericMontageMap.csv"), row.names = F, quote = F, na = "")
}




