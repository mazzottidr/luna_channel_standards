keep_channels <- function(project, keepch, rm.original=F, dir="edfs_processed/", tag="part03", smplst=paste0(project,"_processed.lst")) {
        
        smp.list <- read.table(paste0(project,'_part02.lst'), header = F, sep = "\t", stringsAsFactors = F, fill = T)
        for (smp.i in 1:nrow(smp.list)) {
                
                
                luna_cmd_03 <- paste0("WRITE edf-dir=",dir," edf-tag=",tag," sample-list=",smplst)
                write(luna_cmd_03, "luna_cmd_03.txt")
                system("tr -d '\r' < luna_cmd_03.txt > tmp")
                system("mv tmp luna_cmd_03.txt")
                
                luna_part03 <- c("#!/bin/bash",
                                 paste0('luna ', project,'_part02.lst ', smp.list$V1[smp.i],' sig="',keepch,'" < luna_cmd_03.txt'))
                
                if (rm.original) {
                        
                        luna_part03 <- c(luna_part03, paste0("rm ", smp.list$V2[smp.i]))
                }
                
                write(luna_part03, "current_luna_part03.sh")
                system("bash current_luna_part03.sh")
        }
        
        if (rm.original) {
                
                system("rm -R edfs_part02/")
        }
        
        
}