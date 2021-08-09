#### Raw PSG data Processing Pipeline
### Developed by Diego Mazzotti
### April 2019
### Requires: luna (http://zzz.bwh.harvard.edu/luna/)

### Library files for luna: fixed_alias_SAGIC.txt (this file contains all possible annotated alias for SAGIC. New study sites might require adding new aliases)

### Input files:
        ### Project name
        ### EDF files (can be organized in a folder)
        ### Sample list (two-column tab-delimited file - sample ID and file location)
        ### alias file (~/luna/libs/fixed_alias_SAGIC.txt)
        ### bad samples list (bad_samples.txt) - one columns, with ID of samples to remove

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
parser$add_argument("-a", "--alias", help="Path to alias file")
parser$add_argument("-b", "--bad-samples", help = "List of sample IDs to exclude", default=NULL)

# get command line options, if help option encountered print help and exit,
# otherwise if options not found on command line then set defaults, 
args <- parser$parse_args()

# Define inputs
project <- args$project
universal_alias <- args$alias #"~/luna/libs/fixed_alias_SAGIC.txt"
sample_list <- args$sample_list #"SAGICTEST.lst"
bad <- args$bad_samples #"bad_samples.txt"

#print(project)
#print(universal_alias)
#print(sample_list)
#print(bad)

#exclude part
ex_part <- paste0(' exclude=',bad)
if (is.null(bad)) {ex_part <- ""}


#### Time tracker
start_time <- Sys.time()
message(paste0("Run started at ", start_time))

# Standard string of channel names
std_channel <- "ABDOMEN,EEG_C3,EEG_C4,EMG_CHIN1,EMG_CHIN2,EOG_E1,EOG_E2,ECG,EEG_F3,EEG_F4,AIRFLOW_NC,HR,LAT,EEG_M1,EEG_M2,EEG_O1,EEG_O2,RAT,BODY_POSITION,EXPIRATORY_PRESSURE,SNORE,SPO2,AIRFLOW_TH,CHEST,EEG_C3_M2,EEG_C4_M1,EMG_CHIN,EEG_F3_M2,EEG_F4_M1,EOG_E1_M,EEG_O1_M2,EEG_O2_M1,EOG_E2_M"
std_channel_referenced <- "ABDOMEN,ECG,AIRFLOW_NC,HR,LAT,RAT,BODY_POSITION,EXPIRATORY_PRESSURE,SNORE,SPO2,AIRFLOW_TH,CHEST,EEG_C3_M2,EEG_C4_M1,EMG_CHIN,EEG_F3_M2,EEG_F4_M1,EOG_E1_M,EEG_O1_M2,EEG_O2_M1,EOG_E2_M"

# Load luna
# system("module load luna-base/0.2") # running on docker

# Load accessory function
source("~/luna_channel_standards/functions/reference_channels.R")
source("~/luna_channel_standards/functions/keep_channels.R")
source("~/luna_channel_standards/functions/create_montages.R")



# Part 01. From EDF files, de-identify an create a copy with standardized label names derived from aliases

# Create project specific luna_cmd_01.txt
luna_cmd_01 <- c("ANON", paste0("WRITE edf-dir=edfs_part01/ edf-tag=part01 sample-list=",project,"_part01.lst"))
write(luna_cmd_01, "luna_cmd_01.txt")
system("tr -d '\r' < luna_cmd_01.txt > tmp")
system("mv tmp luna_cmd_01.txt")

luna_part_01 <- c("#!/bin/bash",
                 paste0('luna ',sample_list,' ',ex_part,' sig="',std_channel,'" @', universal_alias, ' < luna_cmd_01.txt'))
write(luna_part_01, "luna_part01.sh")
system("bash luna_part01.sh")

# Create header to figure out which samples need to be referenced and which not
luna_header_01 <- c("#!/bin/bash",
                 paste0('luna ', project,'_part01.lst',' -o ', project, '_pre_std_headers.db -s HEADERS'))
write(luna_header_01, "luna_header01.sh")
system("bash luna_header01.sh")

destrat_01 <- c("#!/bin/bash",
                    paste0('destrat ',project, '_pre_std_headers.db +HEADERS -r CH -v SR > ', project, '_pre_std_headers.txt'))
write(destrat_01, "destrat_part01.sh")
system("bash destrat_part01.sh")

# Part 02. Reference channels that need to be referenced and write in a new file
reference_channels_luna(header_file=paste0(project, '_pre_std_headers.txt'), project=project, keepch=std_channel, rm.original=F)

# Part 03. Create version only with referenced channels
keep_channels(project=project, keepch=std_channel_referenced, rm.original=T)

# Finally, create montage maps (Magdy and Generic)

# Re-run and load final headers
luna_header_02 <- c("#!/bin/bash",
                    paste0('luna ', project,'_processed.lst',' -o ', project, '_post_std_headers.db -s HEADERS'))
write(luna_header_02, "luna_header02.sh")
system("bash luna_header02.sh")

destrat_02 <- c("#!/bin/bash",
                paste0('destrat ',project, '_post_std_headers.db +HEADERS -r CH -v SR > ', project, '_post_std_headers.txt'))
write(destrat_02, "destrat_part02.sh")
system("bash destrat_part02.sh")
create_montages(project=project)

# Rename filenames
system(paste0("sed 's/part01-part02-part03/processed/g' ",project,"_processed.lst > tmp"))
system(paste0("mv tmp ", project,"_processed.lst"))

system("for i in edfs_processed/*.edf; do mv $i $(echo $i | sed 's/part01-part02-part03/processed/g'); done")

#### Time tracker
end_time <- Sys.time()
message(paste0("Run finished at ", end_time))
print(end_time - start_time)

############ Next Steps: add info per individual, such as lights off and on, total time of recording, and incorporate annotations

