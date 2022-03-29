
read.seba.table <- function(file_csv){
    x <- readLines(file_csv, skipNul = TRUE, warn = FALSE)
    missval1 <- "-99.90|-999.90|-99.9|-999.9"
    missval2 <- "-9999.90|-99999.90|-9999.9|-99999.9"
    missval3 <- "-99|-999|-9999|-99999"
    missval <- paste(missval1, missval2, missval3, sep = "|")

    row0 <- data.frame(matrix(NA, 0, 1))
    if(length(x) == 0) return(row0)
    h <- utils::read.table(text = x[1], header = TRUE, sep = ";",
                           colClasses = "character", na.strings = "",
                           stringsAsFactors = FALSE, quote = "\"")
    lenh <- length(names(h))
    x <- x[-1]
    x <- strsplit(x, ";")
    len <- sapply(x, length)
    il <- len == lenh
    if(!any(il)) return(row0)

    x <- lapply(x[il], function(v) gsub(missval, "", v))
    x <- lapply(x, function(v) gsub("\\\"", "", v))
    x <- do.call(rbind, x)
    x[x == ""] <- NA
    x <- as.data.frame(x, stringsAsFactors = FALSE)
    names(x) <- names(h)

    return(x)
}

parse.seba.data <- function(qres, awsID, varTable){
    tz <- Sys.getenv("TZ")
    temps <- qres$Datum.Zeit
    temps <- strptime(temps, "%d.%m.%Y %H:%M:%S", tz = tz)
    ina <- is.na(temps)
    qres <- qres[!ina, , drop = FALSE]

    if(nrow(qres) == 0) return(NULL)

    temps <- temps[!ina]
    temps <- round.time.minutes10(temps)

    tmp <- qres[, names(qres) %in% varTable$parameter_code, drop = FALSE]
    nom_col <- names(tmp)

    tmp <- lapply(seq_along(nom_col), function(l){
        x <- data.frame(time = temps, par = nom_col[l], data = trimws(tmp[, l]))
        x$data <- suppressWarnings(as.numeric(as.character(x$data)))
        x[!is.na(x$data), , drop = FALSE]
    })
    tmp <- do.call(rbind, tmp)

    if(nrow(tmp) == 0) return(NULL)

    ix <- match(tmp$par, varTable$parameter_code)
    var_nm <- c("var_height", "var_code", "stat_code", "min_val", "max_val")
    var_dat <- varTable[ix, var_nm, drop = FALSE]
    tmp <- cbind(tmp, var_dat)

    # ## convert soil moisture m^3/m^3 to %
    # sm <- tmp$var_code == 7
    # tmp$data[sm] <- tmp$data[sm] * 100
    ##

    tmp$min_val[is.na(tmp$min_val)] <- -Inf
    tmp$max_val[is.na(tmp$max_val)] <- Inf

    tmp$limit_check <- NA
    tmp$network <- 4
    tmp$id <- awsID
    tmp$raw_value <- tmp$data

    ## limit check
    tmp$limit_check[tmp$data < tmp$min_val] <- 1
    tmp$limit_check[tmp$data > tmp$max_val] <- 2

    tmp <- tmp[, c("network", "id", "var_height", "time", "var_code",
                   "stat_code", "data", "raw_value", "limit_check")]

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
