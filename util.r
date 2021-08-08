### util functions for final project proposal



## datetime
str_to_datetime <- function(df, cols, formats = "%b %d, %Y %I:%M %p", utc = "", cvt_tz = "") #utc (GMT)
{

    dt_df <- as.data.frame(lapply(df[ , cols], function(x) as.POSIXlt(x, format = formats, tz = utc)))
    if(nchar(cvt_tz) > 0)
    {
        #sapply(dt_df, function(x) attr(x, "tzone") <- cvt_tz)
        dt_df <- as.data.frame(sapply(dt_df, function(x) format(x, tz = cvt_tz, usetz = TRUE)))
        return(cbind(df[ , !(names(df) %in% cols)], dt_df))
    }
    return(cbind(df[ , !(names(df) %in% cols)], dt_df))
}

datetime_extract <- function(df, cols, extract_vec)
{
    # extract_vec support: 
    # year-month-day
    # year
    # month
    # day
    # week number: 1 - 53
    # hour: 0-23
    # day of week: 1(Monday)-7(Sunday)
    for(e in extract_vec)
    {
        if(e == "ymd")
        {
            ymd_df <- as.data.frame(lapply(df[, cols], function(x) as.Date(x, format = "%Y-%m-%d")))
            names(ymd_df) <- paste0(cols, "_ymd")
            df <- cbind(df, ymd_df)
        }
        else if(e == "year")
        {
            year_df <- as.data.frame(lapply(df[, cols], function(x) as.numeric(format(as.Date(x, format="%Y-%m-%d"), "%Y"))))
            names(year_df) <- paste0(cols, "_year")
            df <- cbind(df, year_df)
        }
        else if(e == "month")
        {
            month_df <- as.data.frame(lapply(df[, cols], function(x) as.numeric(format(as.Date(x, format="%Y-%m-%d"), "%m"))))
            names(month_df) <- paste0(cols, "_month")
            df <- cbind(df, month_df)
        }
        else if(e == "woy")
        {
            # use function(x) strftime(x, format = "%V") works the same
            wn_df <- as.data.frame(lapply(df[, cols], function(x) as.numeric(format(x, "%V"))))
            names(wn_df) <- paste0(cols, "_weeknum")
            df <- cbind(df, wn_df)
        }
        else if(e == "day")
        {
            day_df <- as.data.frame(lapply(df[, cols], function(x) as.numeric(format(x, "%d"))))
            names(day_df) <- paste0(cols, "_day")
            df <- cbind(df, day_df)
        }
        else if(e == "hour")
        {
            hour_df <- as.data.frame(lapply(df[, cols], function(x) as.numeric(format(x, "%H"))))
            names(hour_df) <- paste0(cols, "_hourly")
            df <- cbind(df, hour_df)
        }
        else if(e == "dow")
        {
            dow_df <- as.data.frame(lapply(df[, cols], function(x) as.numeric(format(x, "%u"))))
            names(dow_df) <- paste0(cols, "_dow")
            df <- cbind(df, dow_df)
        }
    }
    return(df)
}

sample_dataframe <- function(df, sample_r = 1, sample_c = ncol(df))
{
    return(df[sample(nrow(df), sample_r), sample(ncol(df), sample_c)])
}
