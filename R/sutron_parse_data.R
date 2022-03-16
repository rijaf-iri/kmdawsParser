
parse.sutron.data <- function(qres, awsID, varTable){
    tz <- Sys.getenv("TZ")
    vcols <- c('SENSORNAME', 'TIME_TAG', 'ORIG_VALUE', 'ED_VALUE')
    qres <- qres[qres$SENSORNAME %in% varTable$parameter_code, vcols, drop = FALSE]

    temps <- qres$TIME_TAG
    temps <- strptime(temps, "%Y-%m-%d %H:%M:%S", tz = "UTC")
    ina <- is.na(temps)
    qres <- qres[!ina, , drop = FALSE]

    if(nrow(qres) == 0) return(NULL)

    temps <- temps[!ina]
    temps <- time_utc2time_local(temps, tz)
    qres$time <- temps

    ix <- match(qres$SENSORNAME, varTable$parameter_code)
    var_nm <- c("var_height", "var_code", "stat_code", "min_val", "max_val")
    var_dat <- varTable[ix, var_nm, drop = FALSE]
    tmp <- cbind(qres, var_dat)

    #########
    ## convert wind speed units
    ## aws LUGARI
    if(awsID == "SUTRON09-LUGARI"){
        ## knots to m/s
        fac <- 0.514444
        # km/h to m/s
        # fac <- 0.2777778

        wsp <- tmp$var_code == 10
        tmp$ORIG_VALUE[wsp] <- tmp$ORIG_VALUE[wsp] * fac
        tmp$ED_VALUE[wsp] <- tmp$ED_VALUE[wsp] * fac

        wspg <- tmp$var_code == 11
        tmp$ORIG_VALUE[wspg] <- tmp$ORIG_VALUE[wspg] * fac
        tmp$ED_VALUE[wspg] <- tmp$ED_VALUE[wspg] * fac
    }
    
    # convert sunshine from to hour into sum in minutes
    sun <- tmp$var_code == 15
    tmp$ORIG_VALUE[sun] <- tmp$ORIG_VALUE[sun] * 60/10
    tmp$ED_VALUE[sun] <- tmp$ED_VALUE[sun] * 60/10

    #########

    tmp$min_val[is.na(tmp$min_val)] <- -Inf
    tmp$max_val[is.na(tmp$max_val)] <- Inf

    tmp$limit_check <- NA
    tmp$network <- 3
    tmp$id <- awsID

    ## limit check
    tmp$limit_check[tmp$ED_VALUE < tmp$min_val] <- 1
    tmp$limit_check[tmp$ED_VALUE > tmp$max_val] <- 2

    tmp <- tmp[, c("network", "id", "var_height", "time", "var_code",
                   "stat_code", "ED_VALUE", "ORIG_VALUE", "limit_check")]

    fun_format <- list(as.integer, as.character, as.numeric, as.integer,
                       as.integer, as.integer, as.numeric, as.numeric, as.integer)

    tmp <- lapply(seq_along(fun_format), function(j) fun_format[[j]](tmp[[j]]))
    tmp <- as.data.frame(tmp)

    names(tmp) <- c("network", "id", "height", "obs_time", "var_code",
                    "stat_code", "value", "raw_value", "limit_check")

    tmp$obs_id <- getObsId(tmp)

    tmp <- tmp[!duplicated(tmp$obs_id), , drop = FALSE]
    tmp <- tmp[order(tmp$obs_time), , drop = FALSE]

    return(tmp)
}
