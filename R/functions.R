################################################################################
### collection of functions used in data processing and modeling
################################################################################


## function for maximum-minimum standardization
scale01 <- function(x, ...)
{
    (x - min(x)) / diff(range(x))
}


## function determining the major occupational group (MOG) from emp_title
## reference U.S. Standard Occupational Classification System
## reference url: http://www.job-analysis.net/G010.htm
grepMog <- function()
{
    ## MOG urls
    mogUrls <- paste0("https://www.bls.gov/ncs/ocs/ocsm/comMog",
                      letters[c(seq_len(8L), 11L)], ".Htm")
    grep1mog <- function(mogUrl) {
        tx <- readLines(mogUrl)
        ## extract contents between all <dl> tags
        ## begin with "<dl>" and end with "</dl>"
        idx1 <- grep("<dl>", tx)
        idx2 <- grep("<\\/dl>", tx)
        idx <- paste(idx1, idx2, sep = ":")
        idx <- do.call(c, lapply(idx, function(a) {
            eval(parse(text = a))
        }))
        tx0 <- paste0(tx[idx], collapse = "")
        ## pattern: "<code> </code>[^A-z0-9_][^A-z0-9_]*<br><a|</dd>"
        startVec <- grepRaw("<code> <\\/code>", tx0, all = TRUE)
        stopVec <- grepRaw("<br><a|<\\/dd>", tx0, all = TRUE)
        out <- sapply(seq_along(startVec), function(a) {
            substr(tx0, startVec[a] + nchar("<code> </code>"), stopVec[a] - 1L)
        })
        ## remove extra whitespace
        tolower(gsub("[ ][ ]*", " ", out))
    }
    lapply(mogUrls, grep1mog)
}


## function that helps matching emp_title with nine mog's
mogScore <- function(empTitleList, mogList)
{
    ## grep each word in empTitleList and count the matchs from each mog
    ## sum up the score of each word as the final score of emp_title for mog's
    ## pick up the mog with the largest score
    ## if maximum does not exist, put it to be No.10 group, "others"
    nMog <- length(mogList)
    nDoc <- sapply(mogList, length)
    grepOne <- function(oneVec) {
        if (! length(oneVec))
            return(10L)
        tf <- sapply(oneVec, function(oneTitle) {
            sapply(mogList, function(a) {
                length(grep(oneTitle, a))
            })
        })
        idfMat <- apply(tf, 2L, function(a) {
            ifelse(a > 0, log(nDoc) - log(a), 0)
        })
        tfMat <- tf / nDoc
        tf_idf <- rowSums(tfMat * idfMat)
        idx <- which(tf_idf == max(tf_idf))
        if (length(idx) > 1)
            return(10L)
        idx
    }
    sapply(empTitleList, grepOne)
}


## function generating design matrix for glmnet from specified model formula
glmnet2 <- function(formula, data, subset, na.action,
                    contrasts = NULL, predOnly = FALSE, ...)
{
    ## arguments check
    if (missing(formula))
        stop("Argument 'formula' is required.")
    if (missing(data))
        data <- environment(formula)

    mcall <- match.call(expand.dots = FALSE)
    mmcall <- match(c("formula", "data", "subset", "na.action"),
                    names(mcall), 0L)
    mcall <- mcall[c(1L, mmcall)]
    ## drop unused levels in factors
    mcall$drop.unused.levels <- TRUE
    mcall[[1L]] <- quote(stats::model.frame)
    mf <- eval(mcall, parent.frame())
    xMat <- stats::model.matrix(formula, data = mf, contrasts.arg = contrasts)
    xMat <- xMat[, - 1L, drop = FALSE]
    y <- stats::model.extract(mf, "response")
    if (predOnly)
        return(list(y = y, xMat = xMat))
    inputs <- c(list(x = xMat, y = y), list(...))
    out2 <- do.call(glmnet::cv.glmnet, inputs)
    list(xMat = xMat, object = out2)
}


## function that makes prediction
predict2 <- function(object, glmnetObj, newx, ...)
{
    if (missing(newx))
        newx <- object[[1L]]
    if (missing(glmnetObj))
        glmnetObj <- object[[2L]]
    out <- predict(glmnetObj, newx, ...)
    as.numeric(out)
}


## hue function
gg_color_hue <- function(n)
{
    hues <- seq(15, 375, length = n + 1)
    grDevices::hcl(h = hues, l = 65, c = 100)[seq_len(n)]
}


## Multiple plot function
## http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_%28ggplot2%29/
## ggplot objects can be passed in ..., or to plotlist
## (as a list of ggplot objects)
## - cols:   Number of columns in layout
## - layout: A matrix specifying the layout. If present, 'cols' is ignored.
##
## If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
## then plot 1 will go in the upper left, 2 will go in the upper right, and
## 3 will go all the way across the bottom.
##
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL)
{
    library(grid)

    ## Make a list from the ... arguments and plotlist
    plots <- c(list(...), plotlist)

    numPlots = length(plots)

    ## If layout is NULL, then use 'cols' to determine layout
    if (is.null(layout)) {
        ## Make the panel
        ## ncol: Number of columns of plots
        ## nrow: Number of rows needed, calculated from # of cols
        layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                         ncol = cols, nrow = ceiling(numPlots/cols))
    }

    if (numPlots==1) {
        print(plots[[1]])

    } else {
        ## Set up the page
        grid.newpage()
        pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

        ## Make each plot, in the correct location
        for (i in 1:numPlots) {
            ## Get the i,j matrix positions of the regions
            ## that contain this subplot
            matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

            print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                            layout.pos.col = matchidx$col))
        }
    }
}
