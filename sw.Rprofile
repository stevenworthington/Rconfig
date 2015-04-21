
# =======================================================================
# .Rprofile options

# options
Sys.setlocale(category = "LC_ALL", locale = "en_US.UTF-8")

# rm(list=ls())
.First <- function() {
    options(
        width = 220,
        useFancyQuotes = FALSE,
        show.signif.stars = FALSE,
        stringsAsFactors = FALSE, # turn off factors
        scipen = 12,
        digits = 6,
        save.defaults = list(ascii = TRUE), # or compress = TRUE
        device = "quartz"
    )
}

# set.seed default
set.seed(1234)

# Sweave
Sys.setenv("SWEAVE_STYLEPATH_DEFAULT" = "TRUE")

# so the mac gui can find latex
Sys.setenv("PATH" = paste(Sys.getenv("PATH"),"/usr/texbin",sep=":"))

# setHook
setHook(packageEvent("grDevices", "onLoad"),
	function(...) grDevices::ps.options(horizontal=FALSE)) # set defaults for postscript graphics device
setHook(packageEvent("grDevices", "onLoad"),
	function(...) grDevices::quartz.options(width=7, height=7, pointsize=10)) # set defaults for quartz graphics device


# =======================================================================
# .Rprofile packages

# Make default library path a directory not included in the R installation.
# This makes /Users/stevenworthington/Documents/R_library the default path without losing the other paths. 

.libPaths(c("~/Documents/R_library", .libPaths()))

# Then add two paths to that one, say /Users/stevenworthington/R/Library/Work and /Users/stevenworthington/R/Library/Test. 
# The one that's put in the first position is the default one used if you don't specify lib in install.packages().
# Then you can assign two variables in your .Rprofile.site. 
# These ones are assigned in the base namespace, and hence always accessible and not removed by ls().
#.libwork <- '/Users/sworthin/Documents/R_library/Work'
#.libtest <- '/Users/sworthin/Documents/R_library/Test'
# which allows you to install packages like:
# install.packages('aPackage', lib=.libwork)

local({
    # add list of packages to the default packages, set a CRAN mirror
    base <- getOption("defaultPackages") # "datasets", "utils", "grDevices", "graphics", "stats", "methods"
    r <- getOption("repos") # URL of the repositories for use by update.packages.
    r["CRAN"] <- "http://cran.mtu.edu/" 
    extra <- c("devtools", "ggplot2", "plyr", "reshape2", "MASS", "RColorBrewer", "lme4") 
    options(defaultPackages = c(base, extra), repos = r)
    # install personal package
    install_github(stevenworthington/smisc)
    library(smisc)
    # suppressPackageStartupMessages(new) # does not work
    # invisible(capture.output(new))
})

(.packages()) # list loaded packages

# mysql databases
#library("RMySQL")
#library("foreign")
#answer <- readline("What database would you like to connect to? ")
#con <- dbConnect(MySQL(), user="root", password="mypass", dbname=answer)
#dbs <- dbGetQuery(con, "show databases;")

