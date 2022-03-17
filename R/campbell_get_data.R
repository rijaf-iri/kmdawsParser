get.campbell.data <- function(dirFTP, dirAWS, dirUP = NULL, upload = TRUE){
    tz <- Sys.getenv("TZ")
    origin <- "1970-01-01"

    awsFile <- file.path(dirAWS, "AWS_DATA", "CSV", "campbell_aws_list.csv")
    varFile <- file.path(dirAWS, "AWS_DATA", "CSV", "campbell_parameters_table.csv")
    dirOUT <- file.path(dirAWS, "AWS_DATA", "DATA", "CAMPBELL")
    if(!dir.exists(dirOUT))
        dir.create(dirOUT, showWarnings = FALSE, recursive = TRUE)
    dirLOG <- file.path(dirAWS, "AWS_DATA", "LOG", "CAMPBELL")
    if(!dir.exists(dirLOG))
        dir.create(dirLOG, showWarnings = FALSE, recursive = TRUE)
    awsLOG <- file.path(dirLOG, "AWS_LOG.txt")

    if(upload){
        ssh <- readRDS(file.path(dirAWS, "AWS_DATA", "AUTH", "adt.cred"))
        session <- try(do.call(ssh::ssh_connect, ssh$cred), silent = TRUE)
        if(inherits(session, "try-error")){
            logUpload <- file.path(dirAWS, "AWS_DATA", "LOG", "CAMPBELL", "processing_campbell.txt")
            msg <- paste(session, "Unable to connect to ADT server\n")
            format.out.msg(msg, logUpload)

            upload <- FALSE
        }

        dirADT <- file.path(dirUP, "AWS_DATA", "DATA", "CAMPBELL")
        ssh::ssh_exec_wait(session, command = c(
            paste0('if [ ! -d ', dirADT, ' ] ; then mkdir -p ', dirADT, ' ; fi')
        ))
    }

    awsInfo <- utils::read.table(awsFile, header = TRUE, sep = ",", na.strings = "",
                                 stringsAsFactors = FALSE, quote = "\"")

    lastDate <- as.integer(awsInfo$last)
    startDate <- as.Date(as.POSIXct(lastDate, origin = origin, tz = tz))
    endDate <- as.Date(Sys.time())

    awsID <- awsInfo$id
    awsDIR <- awsInfo$dirName
    awsFILES <- paste(awsInfo$stID, awsInfo$fileName, awsInfo$tableName, sep = "_")

    varTable <- var.network.table(varFile)

    for(j in seq_along(awsDIR)){
        dates <- seq(startDate[j], endDate, "day")
        daty <- format(dates, '%Y%m%d')

        aws_pth <- file.path(dirFTP, awsDIR[j])
        allfiles <- paste0(awsFILES[j], '_', daty, '.csv')
        idate <- file.exists(file.path(aws_pth, allfiles))

        if(!any(idate)) next

        allfiles <- allfiles[idate]
        dates <- dates[idate]

        out <- lapply(seq_along(allfiles), function(i){
            file_csv <- file.path(aws_pth, allfiles[i])
            qres <- try(read.campbell.table(file_csv), silent = TRUE)
            if(inherits(qres, "try-error")){
                mserr <- gsub('[\r\n]', '', qres[1])
                msg <- paste("Unable to read data for", awsID[j], ":", dates[i])
                format.out.msg(paste(mserr, '\n', msg), awsLOG)
                return(NULL)
            }

            if(nrow(qres) == 0) return(NULL)

            aws.dat <- try(parse.campbell.data(qres, awsID[j], varTable), silent = TRUE)
            if(inherits(aws.dat, "try-error")){
                mserr <- gsub('[\r\n]', '', aws.dat[1])
                msg <- paste("Unable to parse data for", awsID[j], ":", dates[i])
                format.out.msg(paste(mserr, '\n', msg), awsLOG)
                return(NULL)
            }

            aws.dat
        })

        out <- do.call(rbind, out)
        if(is.null(out)) next

        ilast <- out$obs_time > lastDate[j]
        out <- out[ilast, , drop = FALSE]
        if(nrow(out) == 0) next

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
        adtFile <- file.path(dirUP, "AWS_DATA", "CSV", basename(adtFile))
        ssh::scp_upload(session, awsFile, to = adtFile, verbose = FALSE)
    }

    return(0)
}
