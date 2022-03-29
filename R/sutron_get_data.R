
get.sutron.data <- function(conn, dirAWS, dirUP = NULL, upload = TRUE){
    on.exit({
        if(upload) ssh::ssh_disconnect(session)
    })
    tz <- Sys.getenv("TZ")
    origin <- "1970-01-01"

    awsFile <- file.path(dirAWS, "AWS_DATA", "CSV", "sutron_aws_list.csv")
    varFile <- file.path(dirAWS, "AWS_DATA", "CSV", "sutron_parameters_table.csv")
    dirOUT <- file.path(dirAWS, "AWS_DATA", "DATA", "SUTRON")
    if(!dir.exists(dirOUT))
        dir.create(dirOUT, showWarnings = FALSE, recursive = TRUE)
    dirLOG <- file.path(dirAWS, "AWS_DATA", "LOG", "SUTRON")
    if(!dir.exists(dirLOG))
        dir.create(dirLOG, showWarnings = FALSE, recursive = TRUE)
    awsLOG <- file.path(dirLOG, "AWS_LOG.txt")

    if(upload){
        ssh <- readRDS(file.path(dirAWS, "AWS_DATA", "AUTH", "adt.cred"))
        session <- try(do.call(ssh::ssh_connect, ssh$cred), silent = TRUE)
        if(inherits(session, "try-error")){
            logUpload <- file.path(dirAWS, "AWS_DATA", "LOG", "SUTRON", "processing_sutron.txt")
            msg <- paste(session, "Unable to connect to ADT server\n")
            format.out.msg(msg, logUpload)

            upload <- FALSE
        }

        dirADT <- file.path(dirUP, "AWS_DATA", "DATA", "SUTRON")
        ssh::ssh_exec_wait(session, command = c(
            paste0('if [ ! -d ', dirADT, ' ] ; then mkdir -p ', dirADT, ' ; fi')
        ))
    }

    awsInfo <- utils::read.table(awsFile, header = TRUE, sep = ",", na.strings = "",
                                 stringsAsFactors = FALSE, quote = "\"")

    lastDate <- as.POSIXct(as.integer(awsInfo$last), origin = origin, tz = tz)
    lastDate <- time_local2utc_char(lastDate, "%Y-%m-%d %H:%M:%S")

    awsID <- awsInfo$id
    awsTable <- awsInfo$STATION_ID
    varTable <- var.network.table(varFile)

    for(j in seq_along(awsTable)){
        if(is.na(lastDate[j])){
            query <- paste0("SELECT * FROM xc_data1 WHERE STATION_ID='", awsTable[j], "'")
        }else{
            query <- paste0("SELECT * FROM xc_data1 WHERE STATION_ID='", awsTable[j], "' AND TIME_TAG > '", lastDate[j], "'")
        }
        qres <- try(DBI::dbGetQuery(conn, query), silent = TRUE)
        if(inherits(qres, "try-error")){
            msg <- paste("Unable to get data for", awsID[j])
            format.out.msg(msg, awsLOG)
            next
        }

        if(nrow(qres) == 0) next

        #### remove wrong dates (2022-12-31 21:00:00 - 2022-12-31 23:50:00)
        timeNow <- as.POSIXlt(Sys.time(), tz = "UTC")
        thres1 <- as.POSIXlt("2022-12-31 21:00:00", tz = "UTC")
        thres2 <- as.POSIXlt("2022-12-31 23:50:00", tz = "UTC")
        if(timeNow < thres1 | timeNow > thres2){
            tt <- as.POSIXlt(qres$TIME_TAG, tz = "UTC")
            it <- !(tt >= thres1 & tt <= thres2)
            qres <- qres[it, , drop = FALSE]
            if(nrow(qres) == 0) next
        }

        out <- parse.sutron.data(qres, awsID[j], varTable)
        if(is.null(out)) next

        awsInfo$last[j] <- max(out$obs_time)

        locFile <- paste0(awsID[j], "_", paste(range(out$obs_time), collapse = "_"))
        locFile <- file.path(dirOUT, locFile)
        saveRDS(out, locFile)

        if(upload){
            adtFile <- file.path(dirADT, basename(locFile))
            ssh::scp_upload(session, locFile, to = adtFile, verbose = FALSE)
        }
    }

    utils::write.table(awsInfo, awsFile, sep = ",", na = "", col.names = TRUE,
                       row.names = FALSE, quote = FALSE)
    if(upload){
        adtFile <- file.path(dirUP, "AWS_DATA", "CSV", basename(awsFile))
        ssh::scp_upload(session, awsFile, to = adtFile, verbose = FALSE)
    }

    return(0)
}
