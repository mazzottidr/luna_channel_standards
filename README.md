# Standardization of channel labels for EDF files

Contributors: Diego Mazzotti, Shaun Purcell

This repository is an initial effort to help standardize channel names from previously collected and recorded EDF files. It contains R wrappers and function that are used in conjunction with the software luna, developed by Shaun Purcell, to process and manipulate EDF files.

### Requirements
- R (packages: argparse, dplyr)
- [luna](http://zzz.bwh.harvard.edu/luna/)

### Functions
- `functions/add_aliases.R`: add new aliases, from result of check_aliases or manually entered
- `functions/check_aliases.R`: check whether other alias exisit and provide input for add_aliases.R
- `functions/create_montages.R`: create montage maps (list with order of channels in EDF)
- `functions/keep_channels.R`: accessory function to help finding channels that does not need reference
- `functions/PSG_Processing_Pipeline_P3.R`: main script calling other functions to extract, map, rename labels and export standardized copy of EDF.
- `functions/PSG_Processing_Pipeline_P3.R`: accesory function to reference channels that are unipolar. This is currently based on the channel label included in the universal alias mapping file.

### Resources (libs)
- `universal_alias_map.txt`: current universal map created from a heterogeneous sample of clinical sleep studies as part of the Sleep Apnea Global Interdisciplinary Consortium (SAGIC). It encompasses a standard-label mapping that was validated with the sleep laboratories of each SAGIC site.


### Usage
TO DO

### License
TO DO