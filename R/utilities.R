
.onLoad <- function(libname, pkgname){
    Sys.setenv(TZ = "Africa/Nairobi")
}

char_utc2local_time <- function(dates, format, tz){
    x <- strptime(dates, format, tz = "UTC")
    x <- as.POSIXct(x)
    x <- format(x, format, tz = tz)
    x <- strptime(x, format, tz = tz)
    as.POSIXct(x)
}

time_utc2local_char <- function(dates, format, tz){
    x <- as.POSIXct(dates)
    x <- format(x, format, tz = tz)
    x
}

char_local2utc_time <- function(dates, format, tz){
    x <- strptime(dates, format, tz = tz)
    x <- as.POSIXct(x)
    x <- format(x, format, tz = "UTC")
    x <- strptime(x, format, tz = "UTC")
    as.POSIXct(x)
}

time_local2utc_char <- function(dates, format){
    x <- as.POSIXct(dates)
    x <- format(x, format, tz = "UTC")
    x
}

time_local2utc_time <- function(dates){
    format <- "%Y-%m-%d %H:%M:%S"
    x <- time_local2utc_char(dates, format)
    x <- strptime(x, format, tz = "UTC")
    as.POSIXct(x)
}

time_utc2time_local <- function(dates, tz){
    format <- "%Y-%m-%d %H:%M:%S"
    x <- time_utc2local_char(dates, format, tz)
    x <- strptime(x, format, tz = tz)
    as.POSIXct(x)
}

round.time.minutes10 <- function(times){
    mn <- format(times, "%M")
    mn <- as.integer(paste0(substr(mn, 1, 1), 0))
    # trunc.POSIXt  
    # times <- round.POSIXt(times, units = "mins")
    times$min[] <- mn
    times$sec[] <- 0
    times
}

getObsId <- function(qres){
    paste(qres$network, qres$id, qres$height,
          qres$obs_time, qres$var_code,
          qres$stat_code, sep = "_")
}

var.network.table <- function(varFile){
    varTable <- utils::read.table(varFile, sep = ",", na.strings = "",
                                  header = TRUE, stringsAsFactors = FALSE)
    varTable <- varTable[!is.na(varTable$parameter_code), , drop = FALSE]
    varTable <- lapply(seq_along(varTable$parameter_code), function(i){
        vr <- strsplit(varTable$parameter_code[i], "\\|")[[1]]
        x <- varTable[i, , drop = FALSE]
        if(length(vr) > 1){
            x <- x[rep(1, length(vr)), ]
            x$parameter_code <- vr
        }
        x
    })
    varTable <- do.call(rbind, varTable)

    return(varTable)
}

format.out.msg <- function(msg, logfile, append = TRUE){
    ret <- c(paste("Time:", Sys.time(), "\n"), msg, "\n",
             "*********************************\n")
    cat(ret, file = logfile, append = append)
}

connect.DBI <- function(con_args, drv){
    args <- c(list(drv = drv), con_args)
    con <- try(do.call(DBI::dbConnect, args), silent = TRUE)
    if(inherits(con, "try-error")) return(NULL)
    con
}

connect.RODBC <- function(con_args){
    args <- paste0(names(con_args), '=', unlist(con_args))
    args <- paste(args, collapse = ";")
    args <- list(connection = args, readOnlyOptimize = TRUE)
    con <- try(do.call(RODBC::odbcDriverConnect, args), silent = TRUE)
    if(inherits(con, "try-error")) return(NULL)
    con
}

connect.sutron <- function(dirAWS){
    ff <- file.path(dirAWS, "AWS_DATA", "AUTH", "adt_sutron.con")
    sutron <- readRDS(ff)
    conn <- connect.DBI(sutron$connection, RMySQL::MySQL())
    if(is.null(conn)){
        Sys.sleep(3)
        conn <- connect.DBI(sutron$connection, RMySQL::MySQL())
        if(is.null(conn)) return(NULL)
    }

    return(conn)
}

