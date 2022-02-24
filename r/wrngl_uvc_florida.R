#wrngl of florida data
#JHMoxley Feb 22

source(here('r', 'fxn_library.R'))
ctry <- "florida"
message(paste("sourcing raw uvc counts from", ctry))
# src <- c('src_Fish_Ab_Rosario.csv',
#          'src_Fish_Ab_San_Bernardo.csv')
n.src <- length(unique(src))
message(paste(n.src,"src files found in stitch"))
message(paste(src, "frm", ctry))
