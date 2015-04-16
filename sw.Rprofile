
# .Rprofile options
options(
    stringsAsFactors = FALSE,
    show.signif.stars = FALSE,
    width = 220,
    scipen = 12
    )

# options
Sys.setlocale(category = "LC_ALL", locale = "en_US.UTF-8")

# rm(list=ls())
.First <- function() {
  options(width = 120)
  options(useFancyQuotes = FALSE)
  options(show.signif.stars = FALSE)
  options(scipen = 10)
  options(digits = 3)
  options(save.defaults = list(ascii = TRUE)) # or compress = TRUE
  options(device = "quartz")
  options(stringsAsFactors = FALSE) # turn off factors
}

# Sweave
Sys.setenv("SWEAVE_STYLEPATH_DEFAULT" = "TRUE")

# so the mac gui can find latex
Sys.setenv("PATH" = paste(Sys.getenv("PATH"),"/usr/texbin",sep=":"))

# graphics devices
#goquartz = function() { # running in terminal -- it gives a quartz display #instead of X11
#  library("grDevices")
#  library("CarbonEL")
#  options(device = "quartz")
#  Sys.unsetenv("DISPLAY")
#}

#if (.Platform$GUI == "X11") {
#  goquartz()
#}


# setHook
setHook(packageEvent("grDevices", "onLoad"),
	function(...) grDevices::ps.options(horizontal=FALSE)) # set defaults for postscript graphics device
setHook(packageEvent("grDevices", "onLoad"),
	function(...) grDevices::quartz.options(width=7, height=7, pointsize=10)) # set defaults for quartz graphics device

# set.seed default
set.seed(1234)

# first and last messages
#.First <- function() cat("\n Rrrr! The statistics program for Pirates!\n\n")
#.Last <- function() cat("\n Rrrr! Avast Ye, YO HO!\n\n")




# =======================================================================





# .Rprofile packages
local({
  # add list of packages to the default packages, set a CRAN mirror
  old <- getOption("defaultPackages") # "datasets", "utils", "grDevices", "graphics", "stats", "methods"
  r <- getOption("repos") # URLs of the repositories for use by update.packages.
  r["CRAN"] <- "http://cran.mtu.edu/" # "http://cran.cnr.berkeley.edu/"
  # new <- c("ggplot2", "plyr", "reshape2", "lattice", "MASS",  "RColorBrewer", "lme4") 
  # "ape", "beanplot", "caper", "geiger", "ouch", "picante",  "phangorn", "robustbase" "apTreeshape",
  # "tikzDevice"
  options(defaultPackages = c(old), repos = r) # use 'old' to automatically load only the default packages
  # suppressPackageStartupMessages(new) # does not work
  # invisible(capture.output(new))
})

# (.packages()) # list loaded packages


# mysql databases
#library("RMySQL")
#library("foreign")
#answer <- readline("What database would you like to connect to? ")
#con <- dbConnect(MySQL(), user="root", password="mypass", dbname=answer)
#dbs <- dbGetQuery(con, "show databases;")


# Make default library path a directory not included in the R installation.
# This makes /Users/stevenworthington/Library/R_library the default path without losing the other paths. 

.libPaths(c("~/Documents/Analysis/Programs/R_Project/R_workspace/R_library", .libPaths()))


# Then add two paths to that one, say /Users/stevenworthington/R/Library/Work and /Users/stevenworthington/R/Library/Test. 
# The one that's put in the first position is the default one used if you don't specify lib in install.packages().
# Then you can assign two variables in your .Rprofile.site. 
# These ones are assigned in the base namespace, and hence always accessible and not removed by ls().
#.libwork <- '/Users/sworthin/Documents/Analysis/Programs/R_Project/R_workspace/R_library/Work'
#.libtest <- '/Users/sworthin/Documents/Analysis/Programs/R_Project/R_workspace/R_library/Test'
# which allows you to install packages like:
# install.packages('aPackage', lib=.libwork)




# =======================================================================





# functions in regular use

# lambda (also called H^2)
lambda.values <- function(model) {
    mcmc(rowSums(model$VCV[, grep("phylo", colnames(model$VCV))]) / rowSums(model$VCV))
}

# source an R script that is hosted over a HTTPS connection (like github)
source.https <- function(url, ...) {
  # load package
  require(RCurl)
  # parse and evaluate each .R script
  sapply(c(url, ...), function(u) {
      eval(parse(text = getURL(u, followlocation = TRUE,
           cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))),
           envir = .GlobalEnv)
  })
}


# merge multiple data frames
merge.all <- function(dflist, by.var) {
    Reduce(function(...) merge(..., by = by.var, all = TRUE), dflist)
}

# returns string w/o leading whitespace
trim.leading <- function(x) sub("^\\s+", "", x)

# returns string w/o trailing whitespace
trim.trailing <- function(x) sub("\\s+$", "", x)

# returns string w/o leading or trailing whitespace
trim <- function(x) gsub("^\\s+|\\s+$", "", x)

# redefine round function to "round up" instead of "round to even"
round2 <- function(x) trunc(x + 0.5)

# robust standard errors for OLS models
robSE <- function(model) {
    require(sandwich)
    cov_model <- vcovHC(model, type = "HC4m")
    std_err <- sqrt(diag(cov_model))
    r_est <- data.frame(
        estimate = coef(model),
        robust_SE = std_err,
        p_value = 2 * pnorm(abs(coef(model)/std_err), lower.tail = FALSE),
        lcl = coef(model) - 1.96 * std_err,
        ucl = coef(model) + 1.96 * std_err)
    return(r_est)
}

# improved list of objects
.ls.objects <- function (pos = 1, pattern, order.by,
                        decreasing=FALSE, head=FALSE, n=5) {
    napply <- function(names, fn) sapply(names, function(x)
                                         fn(get(x, pos = pos)))
    names <- ls(pos = pos, pattern = pattern)
    obj.class <- napply(names, function(x) as.character(class(x))[1])
    obj.mode <- napply(names, mode)
    obj.type <- ifelse(is.na(obj.class), obj.mode, obj.class)
    obj.prettysize <- napply(names, function(x) {
                           capture.output(print(object.size(x), units = "auto")) })
    obj.size <- napply(names, object.size)
    obj.dim <- t(napply(names, function(x)
                        as.numeric(dim(x))[1:2]))
    vec <- is.na(obj.dim)[, 1] & (obj.type != "function")
    obj.dim[vec, 1] <- napply(names, length)[vec]
    out <- data.frame(obj.type, obj.size, obj.prettysize, obj.dim)
    names(out) <- c("Type", "Size", "PrettySize", "Rows", "Columns")
    if (!missing(order.by))
        out <- out[order(out[[order.by]], decreasing=decreasing), ]
    if (head)
        out <- head(out, n)
    out
}

# shorthand
lsos <- function(..., n=10) {
    .ls.objects(..., order.by="Size", decreasing=TRUE, head=TRUE, n=n)
}

# use last(x) instead of x[length(x)], works on matrices too
last <- function(x) tail(x, n = 1)

# randomly sample several lines from a dataframe
sample_df <- function(df, n=3, ordered=TRUE) {
    if(ordered) {
            df[sort(sample(nrow(df), min(nrow(df), n))),]
    } else {
            df[sample(nrow(df), min(nrow(df), n)),]
    }
}

# summary statistics
summary_text <- function(d) {
  do.call(rbind, lapply( d, function(u)
    data.frame(
      Type    = class(u)[1],
      Min     = if(is.numeric(u)) min(   u, na.rm=TRUE) else NA,
      Mean    = if(is.numeric(u)) mean(  u, na.rm=TRUE) else NA,
      Median  = if(is.numeric(u)) median(u, na.rm=TRUE) else NA,
      Max     = if(is.numeric(u)) max(   u, na.rm=TRUE) else NA,
      Missing = sum(is.na(u))
    )    
  ) )
}

# summary plots
summary_plot <- function(d, aspect=1) {
  # Split the screen: find the optimal number of columns 
  # and rows to be as close as possible from the desired aspect ratio.
  n <- ncol(d)
  dx <- par()$din[1]
  dy <- par()$din[2]
  f <- function(u,v) {
    if( u*v >= n && (u-1)*v < n && u*(v-1) < n ) {
      abs(log((dx/u)/(dy/v)) - log(aspect))
    } else { 
      NA 
    }
  }
  f <- Vectorize(f)
  r <- outer( 1:n, 1:n, f )
  r <- which( r == min(r,na.rm=TRUE), arr.ind=TRUE )
  r <- r[1,2:1]

  op <- par(mfrow=c(1,1),mar=c(2,2,2,2))
  plot.new()
  if( is.null( names(d) ) ) { names(d) <- 1:ncol(d) }
  ij <- matrix(seq_len(prod(r)), nr=r[1], nc=r[2], byrow=TRUE)
  for(k in seq_len(ncol(d))) {
    i <- which(ij==k, arr.ind=TRUE)[1]
    j <- which(ij==k, arr.ind=TRUE)[2]
    i <- r[1] - i + 1
    f <- c(j-1,j,i-1,i) / c(r[2], r[2], r[1], r[1] )
    par(fig=f, new=TRUE)
    if(is.numeric(d[,k])) { 
      hist(d[,k], las=1, col="grey", main=names(d)[k], xlab="", ylab="")
      o <- par(fig=c(
          f[1]*.4  + f[2]*.6,
          f[1]*.15 + f[2]*.85,
          f[3]*.4  + f[4]*.6,
          f[3]*.15 + f[4]*.85
        ), 
        new=TRUE,
        mar=c(0,0,0,0)
      )
      qqnorm(d[,k],axes=FALSE,xlab="",ylab="",main="")
      qqline(d[,k])
      box()
      par(o)
    } else {
      o <- par(mar=c(2,5,2,2))
      barplot(table(d[,k]), horiz=TRUE, las=1, main=names(d)[k])
      par(o)
    }
  }
  par(op)
}


## function to summarize data
## provides count, mean, standard deviation, standard error of the mean,
## and confidence interval (default 95%).
##     data: a data frame.
##     measurevar: the name of a column that contains the variable to be summariezed
##     groupvars: a vector containing names of columns that contain grouping variables
##     na.remove: a boolean that indicates whether to ignore NA's
##     conf.interval: the percent range of the confidence interval (default is 95%)
summarySE <- function(data = NULL, measurevar, groupvars = NULL, na.remove = FALSE,
                      conf.interval = .95, .drop = TRUE) {
    require(plyr)
    # new version of length which can handle NA's: if na.rm == TRUE, don't count them
    len <- function (x, na.rm = FALSE) {
        if (na.rm) sum(!is.na(x))
        else       length(x)
    }
    # this does the summary
    datac <- ddply(data, groupvars, .drop = .drop,
                   .fun = function(x, col, na.rm) {
                           c( N    = len(x[, col], na.rm = na.remove),
                              mean = mean(x[, col], na.rm = na.remove),
                              sd   = sd(x[, col], na.rm = na.remove)
                             )
                          },
                       measurevar,
                       na.rm
                  )
    datac <- rename(datac, c("mean" = measurevar)) # rename the "mean" column
    datac$se <- datac$sd / sqrt(datac$N)  # calculate standard error of the mean
    # confidence interval multiplier for standard error
    # calculate t-statistic for confidence interval: 
    # e.g., if conf.interval is .95, use .975 (above/below), and use df = N-1
    ciMult <- qt(conf.interval / 2 + .5, datac$N - 1)
    datac$ci <- datac$se * ciMult
    return(datac)
}


# function to install new packages and load them
# pkg: a vector of package names in quotes
ipak <- function(pkg){
    new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
    if(length(new.pkg)) install.packages(new.pkg, dependencies = TRUE)
    sapply(pkg, require, character.only = TRUE)
}

# function to list installed packages and their versions
packinfo <- function(){
    packinfo <- installed.packages(fields = c ("Package", "Version", "Built"))
    versions <- packinfo[, c("Package", "Version", "Built")]
    versions[order(rownames(versions)), ]
}

# kernal density plot
dens <- function (variable) {
    d <- density(variable, na.rm = TRUE)
    plot(d, type = "n", main = deparse( substitute(variable) ) )
    polygon(d, col = "cornflowerblue", border = NA) 
    rug(variable)
}

# function to do arcsine transformation
arcsineTran <- function(y, n){
    ratio <- (y + (3/8)) / (n + (3/4))
    arc_p <- asin( sqrt(ratio) )
    return(arc_p)
}

# remove multiple elements from a dataframe
'%notin%' <- function(x, y) !(x %in% y)

# also provided by the Hmisc package
'%nin%' <- Negate('%in%')
