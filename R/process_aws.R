#' Process CAMPBELL data.
#'
#' Get the data from CAMPBELL database, parse and insert into ADT database.
#' 
#' @param dirFTP full path to the directory containing the Campbell data.\cr
#' @param dirAWS full path to the directory containing the AWS_DATA folder on Campbell server.\cr
#' @param dirUP full path to the directory containing the AWS_DATA folder on ADT server.
#'              Default NULL, must be provided if \code{upload} is \code{TRUE}.\cr
#' @param upload logical, if TRUE the data will be uploaded to ADT server.
#' 
#' @export

process.campbell <- function(dirFTP, dirAWS, dirUP = NULL, upload = TRUE){
    dirLOG <- file.path(dirAWS, "AWS_DATA", "LOG", "CAMPBELL")
    if(!dir.exists(dirLOG))
        dir.create(dirLOG, showWarnings = FALSE, recursive = TRUE)
    logPROC <- file.path(dirLOG, "processing_campbell.txt")

    ret <- try(get.campbell.data(dirFTP, dirAWS, dirUP, upload), silent = TRUE)
    if(inherits(ret, "try-error")){ 
        mserr <- gsub('[\r\n]', '', ret[1])
        msg <- paste(ret, "Getting CAMPBELL data failed")
        format.out.msg(paste(mserr, '\n', msg), logPROC)
        return(2)
    }

    return(0)
}

#' Process SEBA data.
#'
#' Get the data from SEBA database, parse and insert into ADT database.
#' 
#' @param dirFTP full path to the directory containing the Seba data.\cr
#' @param dirAWS full path to the directory containing the AWS_DATA folder on Seba server.\cr
#' @param dirUP full path to the directory containing the AWS_DATA folder on ADT server.
#'              Default NULL, must be provided if \code{upload} is \code{TRUE}.\cr
#' @param upload logical, if TRUE the data will be uploaded to ADT server.
#' 
#' @export

process.seba <- function(dirFTP, dirAWS, dirUP = NULL, upload = TRUE){
    dirLOG <- file.path(dirAWS, "AWS_DATA", "LOG", "SEBA")
    if(!dir.exists(dirLOG))
        dir.create(dirLOG, showWarnings = FALSE, recursive = TRUE)
    logPROC <- file.path(dirLOG, "processing_seba.txt")

    ret <- try(get.seba.data(dirFTP, dirAWS, dirUP, upload), silent = TRUE)
    if(inherits(ret, "try-error")){ 
        mserr <- gsub('[\r\n]', '', ret[1])
        msg <- paste(ret, "Getting SEBA data failed")
        format.out.msg(paste(mserr, '\n', msg), logPROC)
        return(2)
    }

    return(0)
}

#' Process SUTRON data.
#'
#' Get the data from SUTRON database, parse and insert into ADT database.
#' 
#' @param dirAWS full path to the directory containing the AWS_DATA folder on Sutron server.\cr
#' @param dirUP full path to the directory containing the AWS_DATA folder on ADT server.
#'              Default NULL, must be provided if \code{upload} is \code{TRUE}.\cr
#' @param upload logical, if TRUE the data will be uploaded to ADT server.
#' 
#' @export

process.sutron <- function(dirAWS, dirUP = NULL, upload = TRUE){
    on.exit(DBI::dbDisconnect(conn))
    dirLOG <- file.path(dirAWS, "AWS_DATA", "LOG", "SUTRON")
    if(!dir.exists(dirLOG))
        dir.create(dirLOG, showWarnings = FALSE, recursive = TRUE)
    logPROC <- file.path(dirLOG, "processing_sutron.txt")

    conn <- connect.sutron(dirAWS)
    if(is.null(conn)){
        msg <- "An error occurred when connecting to Sutron database"
        format.out.msg(msg, logPROC)
        return(1)
    }

    ret <- try(get.sutron.data(conn, dirAWS, dirUP, upload), silent = TRUE)
    if(inherits(ret, "try-error")){ 
        mserr <- gsub('[\r\n]', '', ret[1])
        msg <- paste(ret, "Getting SUTRON data failed")
        format.out.msg(paste(mserr, '\n', msg), logPROC)
        return(2)
    }

    return(0)
}

#' Process MICROSTEP data.
#'
#' Get the data from MICROSTEP database, parse and insert into ADT database.
#' 
#' @param dirAWS full path to the directory containing the AWS_DATA folder on Microstep server.\cr
#' @param dirUP full path to the directory containing the AWS_DATA folder on ADT server.
#'              Default NULL, must be provided if \code{upload} is \code{TRUE}.\cr
#' @param upload logical, if TRUE the data will be uploaded to ADT server.
#' 
#' @export

process.microstep <- function(dirAWS, dirUP = NULL, upload = TRUE){
    on.exit(DBI::dbDisconnect(conn))
    dirLOG <- file.path(dirAWS, "AWS_DATA", "LOG", "MICROSTEP")
    if(!dir.exists(dirLOG))
        dir.create(dirLOG, showWarnings = FALSE, recursive = TRUE)
    logPROC <- file.path(dirLOG, "processing_microstep.txt")

    conn <- connect.sutron(dirAWS)
    if(is.null(conn)){
        msg <- "An error occurred when connecting to Microstep database"
        format.out.msg(msg, logPROC)
        return(1)
    }

    ret <- try(get.microstep.data(conn, dirAWS, dirUP, upload), silent = TRUE)
    if(inherits(ret, "try-error")){ 
        mserr <- gsub('[\r\n]', '', ret[1])
        msg <- paste(ret, "Getting MICROSTEP data failed")
        format.out.msg(paste(mserr, '\n', msg), logPROC)
        return(2)
    }

    return(0)
}

