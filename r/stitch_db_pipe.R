##pipeline script for stitching dB from wrngl_raw scripts
##JHMoxley, May 2022

library(tidyverse)
library(here)
source(here('r', 'fxn_library.R'))

#sweep-in ancillary data
#bruv <- read_csv(here('data/stitch', 'src_BRUV_westernAtl.csv'))
#srvy <- read_csv(here('data, 'lkup_srvy_params.csv'))

##raw input
dF.f <- data.frame(NULL)
blz <- source(here('r', 'wrngl_uvc_belize.R'))
colo <- source(here('r', 'wrngl_uvc_colombia.R'))
caicos <- source(here('r', 'wrngl_uvc_caicos.R'))
fla <- source(here('r', 'wrngl_uvc_florida.R'))
pr <- source(here('r', 'wrngl_uvc_pr.R'))
             

##stich'n'store
