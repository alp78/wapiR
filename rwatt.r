
`%notin%` <- Negate(`%in%`)

as_simple_dt <- function(tb, tz = NULL) {

    if (is.null(tz)) {
        tz <- 'CET'
    }

    if ('issue_date' %in% names(tb)) {
        for (i in 1:length(tb$issue_date)) {
            if (nchar(tb$issue_date[i]) > 1) {
                dt <- suppressMessages(lubridate::ymd_hms(tb$issue_date[i], tz = tz))
                tb$issue_date[i] <- strftime(dt, "%Y-%m-%d %H:%M")
            }
        }   
    }

    if ('created' %in% names(tb)) {
        for (i in 1:length(tb$created)) {
            if (nchar(tb$created[i]) > 1) {
                dt <- suppressMessages(lubridate::ymd_hms(tb$created[i], tz = tz))
                tb$created[i] <- strftime(dt, "%Y-%m-%d %H:%M")
            }
        }    
    }

    if ('modified' %in% names(tb)) {
        for (i in 1:length(tb$modified)) {
            if (nchar(tb$modified[i]) > 1) {
                dt <- suppressMessages(lubridate::ymd_hms(tb$modified[i], tz = tz))
                tb$modified[i] <- strftime(dt, "%Y-%m-%d %H:%M")
            }
        }       
    }

    if ('data_from' %in% names(tb)) {
        for (i in 1:length(tb$data_from)) {
            if (nchar(tb$data_from[i]) > 1) {
                dt <- suppressMessages(lubridate::ymd_hms(tb$data_from[i], tz = tz))
                tb$data_from[i] <- strftime(dt, "%Y-%m-%d %H:%M")
            }
        }   
    }

    if ('data_to' %in% names(tb)) {
        for (i in 1:length(tb$data_to)) {
            if (nchar(tb$data_to[i]) > 1) {
                dt <- suppressMessages(lubridate::ymd_hms(tb$data_to[i], tz = tz))
                tb$data_to[i] <- strftime(dt, "%Y-%m-%d %H:%M")
            }
        }   
    }

    if ('begin' %in% names(tb)) {
        for (i in 1:length(tb$begin)) {
            if (nchar(tb$begin[i]) > 1) {
                dt <- suppressMessages(lubridate::ymd_hms(tb$begin[i], tz = tz))
                tb$begin[i] <- strftime(dt, "%Y-%m-%d %H:%M")
            }
        }     
    }

    if ('end' %in% names(tb)) {
        for (i in 1:length(tb$end)) {
            if (nchar(tb$end[i]) > 1) {
                dt <- suppressMessages(lubridate::ymd_hms(tb$end[i], tz = tz))
                tb$end[i] <- strftime(dt, "%Y-%m-%d %H:%M")
            }
        }    
    }

    if ('limit' %in% names(tb)) {
        for (i in 1:length(tb$limit)) {
            if (nchar(tb$limit[i]) > 1) {
                dt <- suppressMessages(lubridate::ymd_hms(tb$limit[i], tz = tz))
                tb$limit[i] <- strftime(dt, "%Y-%m-%d %H:%M")
            }
        }
    }  

    print(names(tb))
    if ('accessRange' %in% names(tb)) {
        
        for (i in 1:length(tb$accessRange)) {
            if (nchar(tb$accessRange[i]) > 1) {
                dt <- suppressMessages(lubridate::ymd_hms(tb$accessRange[i], tz = tz))
                tb$accessRange[i] <- strftime(dt, "%Y-%m-%d %H:%M")
            }
        }   
    } 

    dt <- suppressWarnings(DT::datatable(tb, 
                                         options = list(info = FALSE, 
                                         dom  = 't', 
                                         ordering = FALSE,
                                         columnDefs = list(list(className = 'dt-left', targets = "_all")),
                                         headerCallback = DT::JS("function(thead) {", "  $(thead).css('font-size', '10pt');", "}"),
                                         autoWidth = FALSE,
                                         scrollX = TRUE
                                         ),
                                         rownames = FALSE) %>%

                           DT::formatStyle(columns = names(tb),
                                           target = 'row',
                                           lineHeight = '70%',
                                           fontFamily = 'Helvetica',
                                           fontSize = '10pt',
                                           backgroundColor = 'white'))
  return(dt)
}


as_ts_dt <- function(tb) {

    suppressMessages(tb$times <- strftime(tb$times, "%Y-%m-%d %H:%M"))

    dt <- suppressWarnings(DT::datatable(tb, 
                                         options = list(info = FALSE, 
                                         dom  = 't', 
                                         columnDefs = list(list(className = 'dt-left', targets = "_all")),
                                         ordering = FALSE,
                                         headerCallback = DT::JS("function(thead) {", "  $(thead).css('font-size', '10pt');", "}"),
                                         autoWidth = FALSE,
                                         scrollX = TRUE
                                         ),
                                         rownames = FALSE) %>%

                           DT::formatStyle(columns = names(tb),
                                           target = 'row',
                                           lineHeight = '70%',
                                           fontFamily = 'Helvetica',
                                           fontSize = '10pt',
                                           backgroundColor = 'white'))
  return(dt)
}

as_tss_dt <- function(tb, tz = NULL) {

    if (is.null(tz)) {
        tz <- 'CET'
    }

    tb$times <- suppressMessages(lubridate::ymd_hms(tb$times, tz = tz))
    
    tb$times <- suppressMessages(strftime(tb$times, "%Y-%m-%d %H:%M"))

    dt <- suppressWarnings(DT::datatable(tb, 
                                         options = list(info = FALSE, 
                                         dom  = 't', 
                                         columnDefs = list(list(className = 'dt-left', targets = "_all")),
                                         ordering = FALSE,
                                         headerCallback = DT::JS("function(thead) {", "  $(thead).css('font-size', '10pt');", "}"),
                                         autoWidth = FALSE,
                                         scrollX = TRUE
                                         ),
                                         rownames = FALSE) %>%

                           DT::formatStyle(columns = names(tb),
                                           target = 'row',
                                           lineHeight = '70%',
                                           fontFamily = 'Helvetica',
                                           fontSize = '10pt',
                                           backgroundColor = 'white'))
  return(dt)
}

WriteAsUTF8 <- function(x, path) {
    con <- file(path, "wb")
    writeBin(charToRaw(x), con, endian = "little")
    close(con)    
}

ConvertMilToPos <- function (mil, tz){
    as.POSIXct(mil/1000, origin = "1970-01-01", tz=tz)
}

ConvertPosToChar <- function (pos) {
    strftime(pos, "%Y-%m-%dT%H:%M:%S%z")
}

ConvertMilToChar <- function (mil, tz) {
    d <- as.POSIXct(mil/1000, origin = "1970-01-01", tz = tz) %>%
    strftime("%Y-%m-%dT%H:%M:%S%z")
    last <- substr(d, nchar(d)-1, nchar(d))
    d <- stringr::str_replace(d, '\\d{2}$', as.character(stringr::str_glue(':{last}')))
    d
}

IsIso8601 <- function(dt_string) {
    if ((nchar(dt_string) == 25) & (substr(dt_string, 11, nchar(dt_string) - 14) == 'T') & (substr(dt_string, 20, nchar(dt_string) - 5) == '+')) {
        return(TRUE)
    } else {
        return(FALSE)
    }
}


FormatNextDay <- function(tz) {
  
  issue_date <- suppressMessages(lubridate::floor_date(lubridate::with_tz(lubridate::today(tzone = tz) + lubridate::ddays(1), tzone = tz), 'day') %>% lubridate::date())
  issue_date <- as.character(issue_date)
  issue_date
}

FormatDateTimeStringIso8601 <- function(dt_string, tz) {

    if (nchar(dt_string) == 10) {

        dt <- suppressMessages(lubridate::ymd(dt_string, tz = tz))
        dt <- strftime(dt, "%Y-%m-%dT%H:%M:%S%z")
        last <- substr(dt, nchar(dt)-1, nchar(dt))
        dt <- stringr::str_replace(dt, '\\d{2}$', as.character(stringr::str_glue(':{last}')))


    } else if (nchar(dt_string) == 16) {

        dt <- suppressMessages(lubridate::ymd_hm(dt_string, tz = tz))
        dt <- strftime(dt, "%Y-%m-%dT%H:%M:%S%z")
        last <- substr(dt, nchar(dt)-1, nchar(dt))
        dt <- stringr::str_replace(dt, '\\d{2}$', as.character(stringr::str_glue(':{last}')))

    } else {
        message('Wrong datetime format! Must be in [YYYY-MM-DD] or [YYYY-MM-DD HH:MM]')
        return(NA)
    }
    return(dt)
}

FormatNextHourIso8601 <- function(tz) {
  
    dt <- suppressMessages(lubridate::floor_date(lubridate::with_tz(lubridate::now(tzone = tz)) + lubridate::dhours(1), 'hour') %>% lubridate::ymd_hms(tz = tz))
    dt <- strftime(dt, "%Y-%m-%dT%H:%M:%S%z")
    last <- substr(dt, nchar(dt)-1, nchar(dt))
    dt <- stringr::str_replace(dt, '\\d{2}$', as.character(stringr::str_glue(':{last}')))
    dt
}

FormatEndOfdayIso8601 <- function(tz) {
  
    dt <- suppressMessages(lubridate::with_tz(lubridate::now(tzone = tz)) + lubridate::ddays(1))
    dt <- lubridate::floor_date(dt, 'day')
    dt <- strftime(dt, "%Y-%m-%dT%H:%M:%S%z")
    last <- substr(dt, nchar(dt)-1, nchar(dt))
    dt <- stringr::str_replace(dt, '\\d{2}$', as.character(stringr::str_glue(':{last}')))

    return(dt)
}

FormatTimePointIso8601 <- function(tp, tz) {

    tp <- suppressMessages(lubridate::ymd_hms(tp, tz = tz))
    tp <- strftime(tp, "%Y-%m-%dT%H:%M:%S%z")
    last <- substr(tp, nchar(tp)-1, nchar(tp)) 
    tp <- stringr::str_replace(tp, '\\d{2}$', as.character(stringr::str_glue(':{last}')))

    return(tp)
}


FormatDateTimeIso8601 <- function(tp, tz) {

    tp <- strftime(tp, "%Y-%m-%dT%H:%M:%S%z")
    last <- substr(tp, nchar(tp)-1, nchar(tp)) 
    tp <- stringr::str_replace(tp, '\\d{2}$', as.character(stringr::str_glue(':{last}')))

    return(tp)
}


SqlGetDbMinTo <- function(db_path) {
  
    con <- DBI::dbConnect(RSQLite::SQLite(), db_path)

    query <- as.character(stringr::str_glue("SELECT MIN(data_to) FROM metadata;"))
    res <- DBI::dbGetQuery(con, query)
    min_to <- res$`MIN(data_to)`

    DBI::dbDisconnect(con)
    
    return(min_to)
}

SqlGetDbMaxTo <- function(db_path, tz) {
  
    con <- DBI::dbConnect(RSQLite::SQLite(), db_path)

    query <- as.character(stringr::str_glue("SELECT MAX(data_to) FROM metadata;"))
    res <- DBI::dbGetQuery(con, query)
    max_to <- res$`MAX(data_to)`

    DBI::dbDisconnect(con)

    return(max_to)
}

SqlGetDbMinFrom <- function(db_path) {
  
    con <- DBI::dbConnect(RSQLite::SQLite(), db_path)

    query <- as.character(stringr::str_glue("SELECT MIN(data_from) FROM metadata;"))
    res <- DBI::dbGetQuery(con, query)
    min_from <- res$`MIN(data_from)`

    DBI::dbDisconnect(con)

    return(min_from)
}

SqlGetDbMaxFrom <- function(db_path) {
  
    con <- DBI::dbConnect(RSQLite::SQLite(), db_path)

    query <- as.character(stringr::str_glue("SELECT MAX(data_from) FROM metadata;"))
    res <- DBI::dbGetQuery(con, query)
    max_from <- res$`MAX(data_from)`

    DBI::dbDisconnect(con)

    return(max_from)
}


SqlGetDbMaxToPlusOneHour <- function(db_path, tz) {
  
    con <- DBI::dbConnect(RSQLite::SQLite(), db_path)

    query <- as.character(stringr::str_glue("SELECT MAX(data_to) FROM metadata;"))
    res <- DBI::dbGetQuery(con, query)
    max_to <- res$`MAX(data_to)`

    DBI::dbDisconnect(con)

    max_to <- suppressMessages(lubridate::ymd_hms(max_to, tz = tz) + lubridate::dhours(1))
    max_to <- FormatDateTimeIso8601(tp = max_to, tz = tz)

    max_to
}


PrintDbOutput <- function(curve_id, 
                          table_name, 
                          data_from, 
                          data_to, 
                          data_size) {
    empty_line <- c('','')

    cat(empty_line, sep='\r\n')

    if (nchar(table_name) < 24) {
        cat(as.character(stringr::str_glue("{curve_id} \t{table_name}\t\t\t\t[{data_from}] >>> [{data_to}] - [{data_size}]\r\n")))
    }
    else if (nchar(table_name) < 32) {
        cat(as.character(stringr::str_glue("{curve_id} \t{table_name}\t\t\t[{data_from}] >>> [{data_to}] - [{data_size}]\r\n")))
    } else if (nchar(table_name) > 39) {
        cat(as.character(stringr::str_glue("{curve_id} \t{table_name}\t[{data_from}] >>> [{data_to}] - [{data_size}]\r\n")))
    } else {
        cat(as.character(stringr::str_glue("{curve_id} \t{table_name}\t\t[{data_from}] >>> [{data_to}] - [{data_size}]\r\n")))
    }
}

GetIsoDuration <- function(h){ 
    
    diff <- as.integer(h)

    if (diff %% 24 == 0) {
        d <- h/24
        iso <- stringr::str_glue('P{d}D')
    } else if (diff < 24) {
        iso <- stringr::str_glue('PT{diff}H')
    } else if ((diff > 24) & (diff %% 24 != 0)) {
        h <- diff %% 24
        d <- diff %/% 24
        iso <- stringr::str_glue('P{d}DT{h}H')
        
    }
    iso <- as.character(iso)
    iso
}


PyDisplayPandas <- function(dataframe) {
  dt <- DT::datatable(dataframe, options = list(lengthChange = FALSE, 
                                                sDom  = '<"top">lrt<"bottom">ip',
                                                paging = FALSE))
  dt
}


JsonWriteCatalogue <- function(curve_list, json_file) {

    ws_token <- GetToken()
    cat_names <- names(curve_list)

    df_list <- list()
    for (i in 1:length(cat_names)) {
        df <- GetCurvesNamesDf(ws_token, curve_list[[i]])
        df_list <- append(df_list, list(df))
    }
    names(df_list) <- cat_names

    ExportDfListToJson(df_list, json_file)
}


JsonReadCatalogue <- function (json_file) {

    curves <- jsonlite::fromJSON(json_file)
    cat_vec <- names(curves)
    tb_list <- list()
    
    for (i in 1:length(curves)) {
        
        cat = stringr::str_sub(names(curves)[i], 4,6)
        curve_names <- curves[[i]]

        tb <- JsonGetCurvesTb(curve_names)
        if (!is.null(tb)) {
            names(tb) <- c('name', 'id')
            tb <- tb %>% dplyr::relocate(id, name)
            tb$id <- pillar::align(tb$id, align = 'left')
            tb$id <- trimws(tb$id)
            tb_list <- append(tb_list, list(tb))
        }

    }
    if (length(tb_list) == length(cat_vec)) {
        names(tb_list) <- cat_vec
    }

    return(tb_list)
}


JsonGetCurves <- function(curve_list) {

    tb <- tibble::as_tibble(curve_list)
    tb
}



JsonGetCurvesTb <- function(curve_list) {

    tb <- curve_list %>%
    tibble::as_tibble_col() %>%
    tidyr::unnest_wider(value, names_sep = '')

    if ('value1' %in% names(tb)) {

        tb <- tb %>% 
        dplyr::rename('name' = 'value1') %>%
        dplyr::mutate(id = as.integer(names(curve_list)))
        
        tb
        
    }

}



ExportDfListToJson <- function(df_list, json_file) {
    
    json <- jsonlite::toJSON(df_list, dataframe = 'values', encoding = 'UTF-8')

    jtext <- json %>% 
    stringr::str_replace_all('",', '" :') %>%
    stringr::str_replace_all('\\["', '"') %>%
    stringr::str_replace_all('\\],', ',') %>%
    stringr::str_replace_all('\\[', '{') %>%
    stringr::str_replace_all('\\],', '},') %>%
    stringr::str_replace_all('\\]\\]', '}') %>%
    stringr::str_replace_all('\\],', '},') %>%
    stringr::str_replace_all('\\]  \\]', '} }') %>%
    stringr::str_replace_all('\\]', '}') %>%
    stringr::str_replace_all(' \\}\n', '')

    WriteAsUTF8(x = jtext, path = json_file)

}


GetNameFromId <- function(ws_token, ids_vec) {
    
    ids_vec <- as.integer(ids_vec)
    names_vec <- character()

    for (id in ids_vec) {

        curve <- GetCurve(ws_token, curve_id = id)
        names_vec <- append(names_vec, curve_name)

    }
    names_vec <- stringi::stri_enc_toutf8(names_vec)
    names_vec
}


GetIdFromName <- function(ws_token, names_vec){

    ids_vec <- character()
    names_vec <- stringi::stri_enc_toutf8(names_vec)
    for (name in names_vec) {

        curve <- GetCurve(ws_token, curve_name = name)
        id <- curve$id
        ids_vec <- append(ids_vec, id)
    }
    ids_vec
}


GetCurvesNamesDf <- function(ws_token, names_vec){

    names_vec <- stringi::stri_enc_toutf8(names_vec)

    ids_vec <- GetIdFromName(ws_token, names_vec)

    if (length(names_vec) == length(ids_vec)) {
        df <- data.frame(ids = ids_vec, names = names_vec, stringsAsFactors = FALSE)
        df
    }
}


GetSeriesTibble <- function(instance, tz){
    
    tb <- tibble::as_tibble_col(instance$points) %>% 
    tidyr::unnest_wider(value, names_sep ='')

    # tb$value1 <- tb$value1[which(!is.nan(tb$value2))]

    if (('value1' %in% names(tb)) & ('value2' %in% names(tb))) {
        tb <- tb %>%
        dplyr::rename('times' = 'value1', 'values' = 'value2') %>% 
        dplyr::mutate(times = ConvertMilToChar(times, tz)) %>%
        dplyr::arrange(times) %>%
        dplyr::mutate(values = zoo::na.locf(values, na.rm = F))
        tb <- tb %>% tidyr::drop_na()
        tb    
    }
}


GetToken <- function(){ 

    USER <- Sys.info()[["user"]]
    WS_SECRETS <- as.character(stringr::str_glue('C:\\Users\\{USER}\\WATTSIGHT\\JSON\\SECRETS.json'))
    WS_SECRETS <- jsonlite::fromJSON(WS_SECRETS)

    WS_CLIENT_ID <- WS_SECRETS$Wattsight$CLIENT_ID
    WS_CLIENT_SECRET <- WS_SECRETS$Wattsight$CLIENT_SECRET

    ws_auth <- httr::authenticate(user = WS_CLIENT_ID, password = WS_CLIENT_SECRET)

    httr::set_config(ws_auth)

    auth_cnt <- httr::POST(url = "https://auth.wattsight.com/oauth2/token",
                           body = list(grant_type = "client_credentials"),
                           encode = "form")
    auth_cnt <- httr::content(auth_cnt)
    auth_cnt
}

GetAttribute <- function(ws_token, attribute) {
  
  att_list <- list('commodities', 'categories', 'areas', 'sources', 'scenarios', 'units', 'time_zones', 'curve_types', 'data_types', 'filters', 'functions', 'frequencies', 'stations', 'curve_states', 'versions')
  
  if (!attribute %in% att_list) {
    print(as.character(stringr::str_glue("[{attribute}] does not exist, must be in ['commodities', 'categories', 'areas', 'sources', 'scenarios', 'units', 'time_zones', 'curve_types', 'data_types', 'filters', 'functions', 'frequencies', 'stations', 'curve_states', 'versions']")))
    return(NA)
  }
 
  base_url <- as.character(stringr::str_glue('https://api.wattsight.com/api/{attribute}'))
  url_list <- httr::parse_url(base_url)
  query_url <- httr::build_url(url_list)
  cnt <- httr::GET(query_url, httr::add_headers('Authorization' = paste("Bearer", ws_token$access_token)))
  cnt <- httr::content(x = cnt, as = "parsed", encoding = "UTF-8")
  tb <- cnt %>% 
    tibble::as_tibble_col() %>%
    tidyr::unnest_wider(value, names_sep = '') %>% 
    dplyr::rename(key = valuekey, name = valuename)

  return(tb)  
}


SearchCurve <- function(ws_token, query_vec){
    
    url_list <- httr::parse_url("https://api.wattsight.com/api/curves")
    
    query <- paste(query_vec, collapse = ' ')
    
    query_list <- list(query = query)
    url_list$query <- query_list
    
    query_url <- httr::build_url(url_list)
    query_url <- paste0(query_url, '&only_accessible=true')

    cnt <- httr::GET(query_url,
                 httr::add_headers('Authorization' = paste("Bearer", ws_token$access_token)))

    cnt <- httr::content(x = cnt, as = "text", encoding = "UTF-8")
    cnt <- jsonlite::fromJSON(cnt)
    cnt
}


GetCurve <- function(ws_token, curve_id = NULL, curve_name = NULL) {
    
    url_list <- httr::parse_url("https://api.wattsight.com/api/curves/get")
    
    if (!is.null(curve_name)) {
        
        curve_name <- trimws(curve_name)
        curve_name <- stringi::stri_enc_toutf8(curve_name)

        url_list$query <- list(name = curve_name)
        
    } else if (!is.null(curve_id)) {
      
        url_list$query <- list(id = curve_id)
    }
    
    query_url <- httr::build_url(url_list)
    
    cnt <- httr::GET(query_url, httr::add_headers('Authorization' = paste("Bearer", ws_token$access_token)))
    
    cnt <- httr::content(x = cnt, as = 'parsed', encoding = "UTF-8")
    
    l <- lapply(cnt, function(i) list(unlist(i, recursive = TRUE)))
    
    new_list <- list()
    
    for (i in 1:length(l)) {
      
      if (length(l[[names(l)[i]]][[1]]) > 1) {
        
        new_list <- append(new_list, stringr::str_glue("{paste(l[[names(l)[i]]][[1]], collapse = ', ')}"))
        
      } else {
        
        new_list <- append(new_list, l[[names(l)[i]]][[1]])
      }
    }

    names(new_list) <- names(l)
    tb <- tibble::as_tibble(new_list)
    tb <- tb %>% dplyr::relocate(id, name, curve_type, data_type, unit, frequency, time_zone, created, modified)

    tb
}


GetTimeSeries <- function(ws_token,
                          curve_id = NULL,
                          curve_name = NULL,
                          freq = NULL, 
                          func = NULL,
                          tz = NULL,
                          data_from = NULL,
                          data_to = NULL) {

    if (!is.null(curve_name)) {
        curve_name <- trimws(curve_name)
        curve_name <- stringi::stri_enc_toutf8(curve_name)
        curve_id <- GetIdFromName(ws_token, curve_name)
    }

    if (!is.null(data_from)) {
        if (IsIso8601(dt_string = data_from) == FALSE) {
            data_from <- FormatDateTimeStringIso8601(dt_string = data_from, tz = tz)
        }
        
    }

    if (!is.null(data_to)) {
        if (IsIso8601(dt_string = data_to) == FALSE) {
            data_to <- FormatDateTimeStringIso8601(dt_string = data_to, tz = tz)
        }
    } 

    
    

    curve <- GetCurve(ws_token, curve_id = curve_id)
    
    data_from_a <- suppressMessages(lubridate::ymd_hms(curve$accessRange, tz = tz) + lubridate::ddays(1)) %>% lubridate::date()
    data_from_a <- as.character(data_from_a)
    
    data_from_a <- FormatDateTimeStringIso8601(dt_string = data_from_a, tz = tz)

    if (!is.null(data_from)) {

        if (data_from < data_from_a) {
            data_from <- data_from_a
        }
        
    } 

    if (is.null(data_from)) {
        data_from <- data_from_a
    }

    base_url <- as.character(stringr::str_glue('https://api.wattsight.com/api/series/{curve_id}'))
    

    if (length(base_url) == 1) {

        url_list <- httr::parse_url(base_url)
        
        params <- list(frequency = freq,
                       `function` = func, 
                       output_time_zone = tz,
                       from = data_from,
                       to = data_to)

        params[sapply(params, is.null)] <- NULL
        url_list$query <- params
        query_url <- httr::build_url(url_list)
        cnt <- httr::GET(query_url, httr::add_headers('Authorization' = paste("Bearer", ws_token$access_token)))
        cnt <- httr::content(x = cnt, as = "parsed", encoding = "UTF-8")
        points <- GetSeriesTibble(cnt, tz)

        meta_list <- cnt[names(cnt) != 'points']

        meta <- tibble::tibble(id = meta_list$id, 
                               name = meta_list$name, 
                               frequency = meta_list$frequency, 
                               time_zone = meta_list$time_zone, 
                               curve_type = curve$curve_type,
                               data_type = curve$data_type,
                               unit = curve$unit,
                               data_size = length(points[['times']]),
                               data_from = FormatTimePointIso8601(min(points[['times']]), tz = tz),
                               data_to = FormatTimePointIso8601(max(points[['times']]), tz = tz),
                               modified = FormatTimePointIso8601(meta_list$modified, tz = tz))
        
        meta <- meta %>% dplyr::relocate(id, name, curve_type, data_type, unit, frequency, time_zone, modified, data_from, data_to, data_size)

        list('metadata' = meta, 'points' = points)
    }
}


GetInstanceMetadata <- function(ws_token, 
                                issue_date, 
                                curve_id = NULL, 
                                curve_name = NULL,
                                tz = NULL){
  

    if (is.null(tz)) {
        tz = 'CET'
    }

    if (!is.null(curve_id)) {
      
      curve <- GetCurve(ws_token, curve_id = curve_id)
      curve_name <- curve$name
      
    } else if (!is.null(curve_name)) {
      
      curve_name <- trimws(curve_name)
      curve_name <- stringi::stri_enc_toutf8(curve_name)      
      curve <- GetCurve(ws_token, curve_name = curve_name)
      curve_id <- curve$id
    }
  
    curve_type <- curve$curve_type

    if (curve_type == 'TAGGED_INSTANCES') {
        
        base_url <- as.character(stringr::str_glue('https://api.wattsight.com/api/instances/tagged/{curve_id}/get'))
        params <- list(issue_date = issue_date, 
                       with_data = FALSE, 
                       tag = 'Avg')
        
    } else if (curve_type == 'INSTANCES') {
        
        base_url <- as.character(stringr::str_glue('https://api.wattsight.com/api/instances/{curve_id}/get'))
        params <- list(issue_date = issue_date, with_data = FALSE)
    }
    
    url_list <- httr::parse_url(base_url)

    url_list$query <- params
    query_url <- httr::build_url(url_list)

    cnt <- httr::GET(query_url, httr::add_headers('Authorization' = paste("Bearer", ws_token$access_token)))

    cnt <- httr::content(x = cnt, as = "text", encoding = "UTF-8")
    cnt <- jsonlite::fromJSON(cnt)
  
    tb <- tibble::as_tibble(cnt)

    tb$curve_type <- curve_type
    tb$data_type <- curve$data_type
    tb$unit <- curve$unit
    tb$created <- FormatTimePointIso8601(tb$created, tz = tz)
    tb$modified <- FormatTimePointIso8601(tb$modified, tz = tz)

    if ('tag' %in% names(tb)) {
        tb <- tb %>% dplyr::relocate(id, name, tag, curve_type, data_type, unit, frequency, issue_date, time_zone, created, modified)
    } else {
        tb <- tb %>% dplyr::relocate(id, name, curve_type, data_type, unit, frequency, issue_date, time_zone, created, modified)
    }
    
    if (curve_type == 'TAGGED_INSTANCES') {
        
        tb[1,1:length(names(tb))]

    } else {
        
        tb
    }
}


GetLatestInstanceMetadata <- function(ws_token, 
                                      curve_id = NULL, 
                                      curve_name = NULL,
                                      tz = NULL){
  
    if (is.null(tz)){
        tz = 'CET'
    }

    today <- lubridate::as_date(lubridate::today(), tz=tz)
    before_before_yesterday <- today + lubridate::ddays(-3)
    tomorrow <- today + lubridate::ddays(1)

    if (!is.null(curve_name)) {
      
      curve_name <- trimws(curve_name)
      curve_name <- stringi::stri_enc_toutf8(curve_name)
      curve <- GetCurve(ws_token, curve_name = curve_name)
      curve_id <- curve$id
      
    } else if (!is.null(curve_id)) {
      
      curve <- GetCurve(ws_token, curve_id = curve_id)
      
    }
    
    curve_type <- curve$curve_type

    if (curve_type == 'TAGGED_INSTANCES') {
        
        base_url <- as.character(stringr::str_glue('https://api.wattsight.com/api/instances/tagged/{curve_id}/latest'))
        
    } else if (curve_type == 'INSTANCES') {
        
        base_url <- as.character(stringr::str_glue('https://api.wattsight.com/api/instances/{curve_id}/latest'))
    }


    
    url_list <- httr::parse_url(base_url)

    params <- list(with_data = FALSE,
                   issue_date_to = tomorrow)
    
    url_list$query <- params
    query_url <- httr::build_url(url_list)
    
    cnt <- httr::GET(query_url, httr::add_headers('Authorization' = paste("Bearer", ws_token$access_token)))
    cnt <- httr::content(x = cnt, as = "parsed", encoding = "UTF-8")
  
    tb <- tibble::as_tibble(cnt)
    tb$curve_type <- curve_type
    tb$data_type <- curve$data_type
    tb$unit <- curve$unit
    tb$created <- FormatTimePointIso8601(tb$created, tz = tz)
    tb$modified <- FormatTimePointIso8601(tb$modified, tz = tz)
    
    if ('tag' %in% names(tb)) {
        tb <- tb %>% dplyr::relocate(id, name, tag, curve_type, data_type, unit, frequency, issue_date, time_zone, created, modified)
    } else {
        tb <- tb %>% dplyr::relocate(id, name, curve_type, data_type, unit, frequency, issue_date, time_zone, created, modified)
    }
    
    tb

}


GetInstance <- function(ws_token,
                        issue_date,  
                        curve_id = NULL,
                        curve_name = NULL,
                        freq = NULL, 
                        func = NULL,
                        tz = NULL,
                        filter = NULL,
                        data_from = NULL,
                        data_to = NULL,
                        tags = FALSE) {
  
    if (!is.null(curve_id)) {
      
      curve <- GetCurve(ws_token, curve_id = curve_id)

      
    } else if (!is.null(curve_name)) {
      
      curve_name <- trimws(curve_name)
      curve_name <- stringi::stri_enc_toutf8(curve_name)
      curve <- GetCurve(ws_token, curve_name = curve_name)
      curve_id <- curve$id
      
    }

    if (!is.null(data_from)) {
        if (IsIso8601(dt_string = data_from) == FALSE) {
            data_from <- FormatDateTimeStringIso8601(dt_string = data_from, tz = tz)
        }
        
    }

    if (!is.null(data_to)) {
        if (IsIso8601(dt_string = data_to) == FALSE) {
            data_to <- FormatDateTimeStringIso8601(dt_string = data_to, tz = tz)
        }
    } 
    
    curve_type <- curve$curve_type
    data_from_a <- suppressMessages(lubridate::ymd_hms(curve$accessRange, tz = tz) + lubridate::ddays(1)) %>% lubridate::date()
    data_from_a <- as.character(data_from_a)

    if (!is.null(data_from)) {
        if (data_from < data_from_a) {
            data_from <- data_from_a
        } 
    }

    if (is.null(data_from)) {
        data_from <- data_from_a
    }

    if (curve_type == 'TAGGED_INSTANCES') {

        if (!tags) {
            base_url <- as.character(stringr::str_glue('https://api.wattsight.com/api/instances/tagged/{curve_id}/get'))

            params <- list(issue_date = issue_date,
                           with_data = TRUE,
                           tag = 'Avg',
                           frequency = freq,
                           `function` = func, 
                           output_time_zone = tz,
                           filter = filter,
                           from = data_from,
                           to = data_to)
        } else {

            base_url <- as.character(stringr::str_glue('https://api.wattsight.com/api/instances/tagged/{curve_id}/tags'))
            url_list <- httr::parse_url(base_url)
            query_url <- httr::build_url(url_list)
            cnt <- httr::GET(query_url, httr::add_headers('Authorization' = paste("Bearer", ws_token$access_token)))
            cnt <- httr::content(x = cnt, as = "parsed", encoding = "UTF-8")
            tags <- as.character(cnt)

            meta_list <- list()
            points_list <- list()

            for (tag in tags) {
                base_url <- as.character(stringr::str_glue('https://api.wattsight.com/api/instances/tagged/{curve_id}/get'))

                params <- list(issue_date = issue_date,
                               with_data = TRUE,
                               tag = tag,
                               frequency = freq,
                               `function` = func, 
                               output_time_zone = tz,
                               filter = filter,
                               from = data_from,
                               to = data_to)

                url_list <- httr::parse_url(base_url)

                params[sapply(params, is.null)] <- NULL

                url_list$query <- params
                query_url <- httr::build_url(url_list)
                
                cnt <- httr::GET(query_url, httr::add_headers('Authorization' = paste("Bearer", ws_token$access_token)))
                
                cnt <- httr::content(x = cnt, as = "parsed", encoding = "UTF-8")
                cnt <- cnt[[1]]

                points <- GetSeriesTibble(cnt, tz)

                meta <- cnt[names(cnt) != 'points']
                meta <- tibble::as_tibble(meta)
                meta$curve_type <- curve_type
                meta$data_type <- curve$data_type
                meta$unit <- curve$unit
                meta$data_size <- length(points[['times']])

                meta$data_from <- FormatTimePointIso8601(min(points[['times']]), tz = tz)
                meta$data_to <- FormatTimePointIso8601(max(points[['times']]), tz = tz)
                meta$created <- FormatTimePointIso8601(meta$created, tz = tz)
                meta$modified <- FormatTimePointIso8601(meta$modified, tz = tz)
                meta <- meta %>% dplyr::relocate(id, name, tag, curve_type, data_type, unit, frequency, issue_date, time_zone, created, modified, data_from, data_to, data_size)
                
                if (tag != 'Avg') {
                    run <- as.character(stringr::str_glue('run{tag}'))
                } else {
                    run <- 'Avg'
                }

                meta_list[[run]] <- meta
                points_list[[run]] <- points
             
            }

            return(list('metadata' = meta_list, 'points' = points_list))
        }
        

        
    } else if (curve_type == 'INSTANCES') {
        
        base_url <- as.character(stringr::str_glue('https://api.wattsight.com/api/instances/{curve_id}/get'))

        params <- list(issue_date = issue_date,
                       with_data = TRUE,
                       frequency = freq,
                       `function` = func, 
                       output_time_zone = tz,
                       filter = filter,
                       from = data_from,
                       to = data_to)
    
    }

    if (curve_type == 'INSTANCES' | (curve_type == 'TAGGED_INSTANCES' & !tags)) {

        url_list <- httr::parse_url(base_url)

        params[sapply(params, is.null)] <- NULL

        url_list$query <- params
        query_url <- httr::build_url(url_list)
        
        cnt <- httr::GET(query_url, httr::add_headers('Authorization' = paste("Bearer", ws_token$access_token)))
        
        cnt <- httr::content(x = cnt, as = "parsed", encoding = "UTF-8")
        
        if (curve_type == 'TAGGED_INSTANCES') {
            
            cnt <- cnt[[1]]
        }
        
        points <- GetSeriesTibble(cnt, tz)
        
        meta <- cnt[names(cnt) != 'points']
        meta <- tibble::as_tibble(meta)
        meta$curve_type <- curve_type
        meta$data_type <- curve$data_type
        meta$unit <- curve$unit
        meta$data_size <- length(points[['times']])
        meta$data_from <- FormatTimePointIso8601(min(points[['times']]), tz = tz)
        meta$data_to <- FormatTimePointIso8601(max(points[['times']]), tz = tz)
        meta$created <- FormatTimePointIso8601(meta$created, tz = tz)
        meta$modified <- FormatTimePointIso8601(meta$modified, tz = tz)

        if ('tag' %in% names(meta)) {
            meta <- meta %>% dplyr::relocate(id, name, tag, curve_type, data_type, unit, frequency, issue_date, time_zone, created, modified, data_from, data_to, data_size)
        } else {
            meta <- meta %>% dplyr::relocate(id, name, curve_type, data_type, unit, frequency, issue_date, time_zone, created, modified, data_from, data_to, data_size)
        }

        return(list('metadata' = meta, 'points' = points))
    }

}


GetLatestInstance <- function(ws_token,
                              curve_id = NULL,
                              curve_name = NULL,
                              freq = NULL, 
                              func = NULL,
                              tz = NULL,
                              filter = NULL,
                              data_from = NULL,
                              data_to = NULL,
                              tags = FALSE) {
  

    if (is.null(tz)) {
        tz = 'CET'
    }

    if (is.null(freq)) {
        freq = 'H'
    }

    if (is.null(func)) {
        func = 'AVERAGE'
    }

    if (!is.null(curve_id)) {
      
      curve <- GetCurve(ws_token, curve_id = curve_id)
      meta <- GetLatestInstanceMetadata(ws_token = ws_token, curve_id = curve_id, tz = tz)

    } else if (!is.null(curve_name)) {
      
      curve_name <- trimws(curve_name)
      curve_name <- stringi::stri_enc_toutf8(curve_name)
      curve <- GetCurve(ws_token, curve_name = curve_name)
      curve_id <- curve$id
      meta <- GetLatestInstanceMetadata(ws_token = ws_token, curve_name = curve_name, tz = tz)
      
    }

    data_from_a <- suppressMessages(lubridate::ymd_hms(curve$accessRange, tz = tz) + lubridate::ddays(1)) %>% lubridate::date()
    data_from_a <- as.character(data_from_a)

    if (!is.null(data_from)) {
        if (IsIso8601(dt_string = data_from) == FALSE) {
            data_from <- FormatDateTimeStringIso8601(dt_string = data_from, tz = tz)
        }
        if (data_from < data_from_a) {
            data_from <- data_from_a
        } 
    }

    if (!is.null(data_to)) {
        if (IsIso8601(dt_string = data_to) == FALSE) {
            data_to <- FormatDateTimeStringIso8601(dt_string = data_to, tz = tz)
        }
    } 

    if (is.null(data_from)) {
        data_from <- data_from_a
    }

    curve_type <- curve$curve_type
    issue_date <- meta$issue_date

    if (curve_type == 'TAGGED_INSTANCES') {
        
        base_url <- as.character(stringr::str_glue('https://api.wattsight.com/api/instances/tagged/{curve_id}/get'))
        
    } else if (curve_type == 'INSTANCES') {
        
        base_url <- as.character(stringr::str_glue('https://api.wattsight.com/api/instances/{curve_id}/get'))
    }

    url_list <- httr::parse_url(base_url)

    while (TRUE) {
        params <- list(issue_date = issue_date,
                       with_data = TRUE,
                       frequency = freq,
                       `function` = func, 
                       output_time_zone = tz,
                       filter = filter,
                       from = data_from,
                       to = data_to)

        params[sapply(params, is.null)] <- NULL

        url_list$query <- params
        query_url <- httr::build_url(url_list)
        
        cnt <- httr::GET(query_url, httr::add_headers('Authorization' = paste("Bearer", ws_token$access_token)))
        
        cnt <- httr::content(x = cnt, as = "parsed", encoding = "UTF-8")
        
        if (curve_type == 'TAGGED_INSTANCES') {
            
            cnt <- cnt[[1]]
        }
        
        points <- GetSeriesTibble(cnt, tz)

        if (is.null(points)) {
            data_from_d <- suppressMessages(lubridate::ymd_hms(data_from, tz = tz))
            data_from_d <- strftime(data_from_d, "%Y-%m-%d %H:%M")

            data_to_d <- suppressMessages(lubridate::ymd_hms(data_to, tz = tz))
            data_to_d <- strftime(data_to_d, "%Y-%m-%d %H:%M")

            print(as.character(stringr::str_glue('No data for [{curve_name}] in the range [{data_from_d}] >>> [{data_to_d}]')))
            return(NA)
        }

        if (length(points[['times']]) > 5) {
            break
        }

        issue_date <- suppressMessages(lubridate::ymd_hms(meta$issue_date, tz = tz) + lubridate::ddays(-1))
        issue_date <- FormatDateTimeIso8601(issue_date, tz = tz) 
    }

    if (curve_type == 'INSTANCES' | (curve_type == 'TAGGED_INSTANCES' & !tags)) {

        meta <- cnt[names(cnt) != 'points']
        meta <- tibble::as_tibble(meta)
        meta$curve_type <- curve_type
        meta$data_type <- curve$data_type
        meta$unit <- stringi::stri_enc_toutf8(trimws(curve$unit))
        meta$data_size <- length(points[['times']])

        meta$data_from <- FormatTimePointIso8601(min(points[['times']]), tz = tz)
        meta$data_to <- FormatTimePointIso8601(max(points[['times']]), tz = tz)
        meta$created <- FormatTimePointIso8601(meta$created, tz = tz)
        meta$modified <- FormatTimePointIso8601(meta$modified, tz = tz)

        if ('tag' %in% names(meta)) {
            meta <- meta %>% dplyr::relocate(id, name, tag, curve_type, data_type, unit, frequency, issue_date, time_zone, created, modified, data_from, data_to, data_size)
        } else {
            meta <- meta %>% dplyr::relocate(id, name, curve_type, data_type, unit, frequency, issue_date, time_zone, created, modified, data_from, data_to, data_size)
        }
        
        return(list('metadata' = meta, 'points' = points))

    } else {

        base_url <- as.character(stringr::str_glue('https://api.wattsight.com/api/instances/tagged/{curve_id}/tags'))
        url_list <- httr::parse_url(base_url)
        query_url <- httr::build_url(url_list)
        cnt <- httr::GET(query_url, httr::add_headers('Authorization' = paste("Bearer", ws_token$access_token)))
        cnt <- httr::content(x = cnt, as = "parsed", encoding = "UTF-8")
        tags <- as.character(cnt)

        meta_list <- list()
        points_list <- list()

        for (tag in tags) {
            base_url <- as.character(stringr::str_glue('https://api.wattsight.com/api/instances/tagged/{curve_id}/get'))

            params <- list(issue_date = issue_date,
                           with_data = TRUE,
                           tag = tag,
                           frequency = freq,
                           `function` = func, 
                           output_time_zone = tz,
                           filter = filter,
                           from = data_from,
                           to = data_to)

            url_list <- httr::parse_url(base_url)

            params[sapply(params, is.null)] <- NULL

            url_list$query <- params
            query_url <- httr::build_url(url_list)
            
            cnt <- httr::GET(query_url, httr::add_headers('Authorization' = paste("Bearer", ws_token$access_token)))
            
            cnt <- httr::content(x = cnt, as = "parsed", encoding = "UTF-8")
            cnt <- cnt[[1]]

            points <- GetSeriesTibble(cnt, tz)

            meta <- cnt[names(cnt) != 'points']
            meta <- tibble::as_tibble(meta)
            meta$curve_type <- curve_type
            meta$data_type <- curve$data_type
            meta$unit <- curve$unit
            meta$data_size <- length(points[['times']])

            meta$data_from <- FormatTimePointIso8601(min(points[['times']]), tz = tz)
            meta$data_to <- FormatTimePointIso8601(max(points[['times']]), tz = tz)
            meta$created <- FormatTimePointIso8601(meta$created, tz = tz)
            meta$modified <- FormatTimePointIso8601(meta$modified, tz = tz)
            meta <- meta %>% dplyr::relocate(id, name, tag, curve_type, data_type, unit, frequency, issue_date, time_zone, created, modified, data_from, data_to, data_size)
            
            if (tag != 'Avg') {
                run <- as.character(stringr::str_glue('run{tag}'))
            } else {
                run <- 'Avg'
            }

            meta_list[[run]] <- meta
            points_list[[run]] <- points
            
        }

        return(list('metadata' = meta_list, 'points' = points_list))        
    }

}

SqlGetForecastMerge <- function(db_for, 
                                json_for, 
                                cluster_name, 
                                online = TRUE, 
                                full_normal = FALSE, 
                                freq = NULL, 
                                func = NULL, 
                                tz = NULL) {

    for_tb <- JsonReadCatalogue(json_for)
    for_list <- JsonGetCurves(for_tb[cluster_name])
    for_names <- for_list[[cluster_name]]$name

    no_curve <- for_names[which(substr(for_names, nchar(for_names), nchar(for_names)) == 'n')]
    mo_curve <- for_names[which(stringr::str_extract(for_names, 'ecmonthly') == 'ecmonthly')]
    ec_curve <- for_names[which(stringr::str_extract(for_names, '\\bec00\\b') == 'ec00')]

    no_table <- SqlGetTableName(no_curve)
    mo_table <- SqlGetTableName(mo_curve)
    ec_table <- SqlGetTableName(ec_curve)

    con <- DBI::dbConnect(RSQLite::SQLite(), db_for)

    query <- as.character(stringr::str_glue("SELECT * FROM {no_table};"))
    df <- DBI::dbGetQuery(con, query)
    no_tb <- tibble::as_tibble(df)

    query <- as.character(stringr::str_glue("SELECT * FROM {mo_table};"))
    df <- DBI::dbGetQuery(con, query)
    mo_tb <- tibble::as_tibble(df)

    DBI::dbDisconnect(con)

    if (online == TRUE) {

        ws_token <- GetToken()
        ec_tb <- GetLatestForecast(ws_token = ws_token, curve_name = ec_curve, freq = freq, func = func, tz = tz)

    } else if (online == FALSE) {

        con <- DBI::dbConnect(RSQLite::SQLite(), db_for)

        query <- as.character(stringr::str_glue("SELECT * FROM '{ec_table}';"))
        res <- DBI::dbGetQuery(con, query)
        points <- tibble::as_tibble(res)

        query <- as.character(stringr::str_glue("SELECT * FROM metadata WHERE table_name = '{ec_table}';"))
        res <- DBI::dbGetQuery(con, query)
        metadata <- tibble::as_tibble(res)

        ec_tb <- list(points = points, metadata = metadata)
    
        DBI::dbDisconnect(con)

    }

    min_x <- min(c(min(no_tb$times), min(mo_tb$times), min(ec_tb$points$times)))
    start_date <- as.POSIXct(min_x, origin = "1970-01-01", tz = tz)

    if (full_normal == TRUE) {

        max_x <- max(c(max(no_tb$times), max(mo_tb$times), max(ec_tb$points$times)))
        end_date <- as.POSIXct(max_x, origin = "1970-01-01", tz = tz)

    } else if (full_normal == FALSE) {

        end_date <- suppressMessages(lubridate::floor_date(lubridate::with_tz(lubridate::today(tzone = tz) + lubridate::dmonths(3), tzone = tz), 'day'))
    }

    time_index <- seq(from = start_date, to = end_date, by = "hour")
    time_index <- strftime(time_index, "%Y-%m-%dT%H:%M:%S%z")

    for (i in 1:length(time_index)) {

        last <- substr(time_index[i], nchar(time_index[i])-1, nchar(time_index[i]))
        time_index[i] <- stringr::str_replace(time_index[i], '\\d{2}$', as.character(stringr::str_glue(':{last}')))
    }

    merge_tb <-tibble(times = character(), values = numeric())

    times_vec <- c()
    values_vec <- c()

    for (i in 1:length(time_index)) {

        if (time_index[i] %in% ec_tb$points$times) {

            idx <- index(ec_tb$points$times)[which(ec_tb$points$times == time_index[i])]
                
                if (!is.na(ec_tb$points$times[idx])) { 
                    merge_tb <- dplyr::add_row(merge_tb, times = time_index[i], values = ec_tb$points$values[idx])
                }

        } else if (time_index[i] %in% mo_tb$times) {
            idx <- index(mo_tb$times)[which(mo_tb$times == time_index[i])]

                if (!is.na(mo_tb$times[idx])) { 
                    merge_tb <- dplyr::add_row(merge_tb, times = time_index[i], values = mo_tb$values[idx])

                }

        } else if (time_index[i] %in% no_tb$times) {
            idx <- index(no_tb$times)[which(no_tb$times == time_index[i])]

                if (!is.na(no_tb$times[idx])) { 
                    merge_tb <- dplyr::add_row(merge_tb, times = time_index[i], values = no_tb$values[idx])
                }
        }

    }

    merge_tb$values <- zoo::na.locf(merge_tb$values, na.rm = F)
    merge_tb <- dplyr::distinct(merge_tb, times, .keep_all= TRUE)
    merge_tb <- dplyr::arrange(merge_tb, times)

    tb <- list(metadata = ec_tb$metadata, points = merge_tb)
    tb$metadata$monthly_to <- max(mo_tb$times)

    return(tb) 

}

SqlPlotForecastMerge <- function(tb, tz = NULL) {
    
    if (is.null(tz)) {
        tz <- 'CET'
    }

    tb$points <- tb$points %>% dplyr::arrange(times)
    tb$points$times <- suppressMessages(lubridate::ymd_hms(tb$points$times, tz = tz))

    forecast_from <- suppressMessages(lubridate::ymd_hms(tb$metadata$data_from, tz = tz))
    forecast_to <- suppressMessages(lubridate::ymd_hms(tb$metadata$data_to, tz = tz))
    monthly_to <- suppressMessages(lubridate::ymd_hms(tb$metadata$monthly_to, tz = tz))
    
    forecast_times <- tb$points$times[which(tb$points$times <= forecast_to)]
    monthly_times <- tb$points$times[which(tb$points$times > forecast_to & tb$points$times <= monthly_to)]

    end_date_d <- suppressMessages(lubridate::floor_date(lubridate::with_tz(lubridate::today(tzone = tz) + lubridate::dmonths(3), tzone = tz), 'day'))

    if ('name' %in% names(tb$metadata)) {
        title <- as.character(stringr::str_glue("<b>{tb$metadata$name}</b>"))
    } else if ('curve_name' %in% names(tb$metadata)) {
        title <- as.character(stringr::str_glue("<b>{tb$metadata$curve_name}</b>"))
    }

    m <- list(
        l = 80,
        r = 80,
        b = 30,
        t = 60,
        pad = 0
    )

    fig <- plotly::plot_ly(width = 809, 
                           height = 500) %>% 
        layout(title = list(text = title,
               x = 0.5, 
               y = 1, 
               xanchor = "middle", 
               yanchor = "top",
               font = list(family = 'Helvetica', size = 18, color = 'navy'),
               pad = list(b = 5, t = 5, l = 5, r = 5))) %>%
        add_annotations(text = as.character(stringr::str_glue('[{tb$metadata$unit}]')),
                        x = 0.5,
                        y = 1.1,
                        yref = "paper",
                        xref = "paper",
                        xanchor = "middle",
                        yanchor = "top",
                        showarrow = FALSE,
                        font = list(size = 15, color = 'navy')) %>%

        layout(yaxis = list(title = '',
                            showgrid = FALSE,
                            gridcolor = 'darkgray',
                            automargin = TRUE,
                            range = list(min(tb$points$values), max(tb$points$values)),
                            showline = TRUE,
                            linewidth = 1.3,
                            linecolor = 'navy',
                            mirror = TRUE),

                xaxis = list(title = '',
                            showgrid = TRUE,
                            showline = TRUE,
                            linecolor = 'navy',
                            range = list(min(tb$points$times), end_date_d),
                            linewidth = 1.3,
                            mirror = TRUE,
                            type = 'date',
                            tickformat = "%d %b-%y<br>%H:%M"),
                margin = m) %>%
        layout(hovermode = 'x unified') %>%

        plotly::add_trace(data = tb$points, 
                           name = 'forecast',
                           x = ~forecast_times, 
                           y = ~tb$points$values[which(tb$points$times %in% forecast_times)], 
                           type = "scatter", 
                           mode = "lines", 
                           line = list(color = 'Crimson', width = 1.5, opacity=1)) %>%

        plotly::add_trace(data = tb$points, 
                          name = 'monthly',
                          x = ~monthly_times, 
                          y = ~tb$points$values[which(tb$points$times %in% monthly_times)], 
                          type = "scatter", 
                          mode = "lines", 
                          line = list(color = 'Indigo', width = 1, opacity=0.5, dash = "dash")) %>% 

        plotly::add_trace(data = tb$points, 
                          name = 'normal',
                          x = ~tb$points$times[which(tb$points$times > monthly_to)], 
                          y = ~tb$points$values[which(tb$points$times > monthly_to)], 
                          type = "scatter", 
                          mode = "lines", 
                          line = list(color = 'DeepPink', width = 0.5, opacity=0.5, dash = "dot")) %>% 

        plotly::config(displayModeBar = FALSE, showLink = FALSE)

    return(fig)
}

SqlGetForecastRelativeMerge <- function(db_for, 
                                        db_his, 
                                        json_for, 
                                        json_his,
                                        cluster_name, 
                                        online = TRUE, 
                                        full_normal = FALSE, 
                                        full_relative = FALSE, 
                                        freq = NULL, 
                                        func = NULL, 
                                        tz = NULL) {

    for_tb <- JsonReadCatalogue(json_for)
    for_list <- JsonGetCurves(for_tb[cluster_name])
    for_names <- for_list[[cluster_name]]$name

    no_curve <- for_names[which(substr(for_names, nchar(for_names), nchar(for_names)) == 'n')]
    mo_curve <- for_names[which(stringr::str_extract(for_names, 'ecmonthly') == 'ecmonthly')]
    ec_curve <- for_names[which(stringr::str_extract(for_names, '\\bec00\\b') == 'ec00')]

    no_table <- SqlGetTableName(no_curve)
    mo_table <- SqlGetTableName(mo_curve)
    ec_table <- SqlGetTableName(ec_curve)

    con <- DBI::dbConnect(RSQLite::SQLite(), db_for)

    query <- as.character(stringr::str_glue("SELECT * FROM {no_table};"))
    df <- DBI::dbGetQuery(con, query)
    no_tb <- tibble::as_tibble(df)

    query <- as.character(stringr::str_glue("SELECT * FROM {mo_table};"))
    df <- DBI::dbGetQuery(con, query)
    mo_tb <- tibble::as_tibble(df)

    DBI::dbDisconnect(con)

    his_tb <- JsonReadCatalogue(json_his)
    his_list <- JsonGetCurves(his_tb[cluster_name])
    his_names <- his_list[[cluster_name]]$name

    # ac_curve <- his_names[which(substr(his_names, nchar(his_names)-1, nchar(his_names)) != ' f')]
    # ac_table <- SqlGetTableName(ac_curve)

    ec00_curve <- his_names[which(stringr::str_extract(his_names, '\\bec00\\b') == 'ec00')]
    ec00_table <- SqlGetTableName(ec00_curve)

    con <- DBI::dbConnect(RSQLite::SQLite(), db_his)

    query <- as.character(stringr::str_glue("SELECT * FROM {ec00_table};"))
    df <- DBI::dbGetQuery(con, query)
    ec00_tb <- tibble::as_tibble(df)

    DBI::dbDisconnect(con)

    if (online == TRUE) {

        ws_token <- GetToken()
        ec_tb <- GetLatestForecast(ws_token = ws_token, curve_name = ec_curve, freq = freq, func = func, tz = tz)

    } else if (online == FALSE) {

        con <- DBI::dbConnect(RSQLite::SQLite(), db_for)

        query <- as.character(stringr::str_glue("SELECT * FROM '{ec_table}';"))
        res <- DBI::dbGetQuery(con, query)
        points <- tibble::as_tibble(res)

        query <- as.character(stringr::str_glue("SELECT * FROM metadata WHERE table_name = '{ec_table}';"))
        res <- DBI::dbGetQuery(con, query)
        metadata <- tibble::as_tibble(res)

        ec_tb <- list(points = points, metadata = metadata)
    
        DBI::dbDisconnect(con)

    }


    if (full_relative == TRUE) {

        min_x <- min(ec00_tb$times)
        start_date <- as.POSIXct(min_x, origin = "1970-01-01", tz = tz)

    } else if (full_relative == FALSE) {
        
        start_date <- suppressMessages(lubridate::floor_date(lubridate::with_tz(lubridate::today(tzone = tz) + lubridate::dmonths(-1), tzone = tz), 'day'))
    }


    if (full_normal == TRUE) {

        max_x <- max(c(max(no_tb$times), max(mo_tb$times), max(ec_tb$points$times)))
        end_date <- as.POSIXct(max_x, origin = "1970-01-01", tz = tz)

    } else if (full_normal == FALSE) {

        end_date <- suppressMessages(lubridate::floor_date(lubridate::with_tz(lubridate::today(tzone = tz) + lubridate::dmonths(3), tzone = tz), 'day'))
    }

    time_index <- seq(from = start_date, to = end_date, by = "hour")
    time_index <- strftime(time_index, "%Y-%m-%dT%H:%M:%S%z")

    for (i in 1:length(time_index)) {

        last <- substr(time_index[i], nchar(time_index[i])-1, nchar(time_index[i]))
        time_index[i] <- stringr::str_replace(time_index[i], '\\d{2}$', as.character(stringr::str_glue(':{last}')))
    }

    merge_tb <-tibble(times = character(), values = numeric())

    times_vec <- c()
    values_vec <- c()

    for (i in 1:length(time_index)) {

        if (time_index[i] %in% ec00_tb$times) {

            idx <- index(ec00_tb$times)[which(ec00_tb$times == time_index[i])]

            if (!is.na(ec00_tb$times[idx])) { 

                merge_tb <- dplyr::add_row(merge_tb, times = time_index[i], values = ec00_tb$values[idx])
            }

        } else if (time_index[i] %in% ec_tb$points$times) {

            idx <- index(ec_tb$points$times)[which(ec_tb$points$times == time_index[i])]
                
                if (!is.na(ec_tb$points$times[idx])) { 
                    merge_tb <- dplyr::add_row(merge_tb, times = time_index[i], values = ec_tb$points$values[idx])
                }

        } else if (time_index[i] %in% mo_tb$times) {
            idx <- index(mo_tb$times)[which(mo_tb$times == time_index[i])]

                if (!is.na(mo_tb$times[idx])) { 
                    merge_tb <- dplyr::add_row(merge_tb, times = time_index[i], values = mo_tb$values[idx])

                }

        } else if (time_index[i] %in% no_tb$times) {
            idx <- index(no_tb$times)[which(no_tb$times == time_index[i])]

                if (!is.na(no_tb$times[idx])) { 
                    merge_tb <- dplyr::add_row(merge_tb, times = time_index[i], values = no_tb$values[idx])
                }
        }

    }

    merge_tb$values <- zoo::na.locf(merge_tb$values, na.rm = F)
    merge_tb <- dplyr::distinct(merge_tb, times, .keep_all= TRUE)
    merge_tb <- dplyr::arrange(merge_tb, times)

    tb <- list(metadata = ec_tb$metadata, points = merge_tb)
    tb$metadata$monthly_to <- max(mo_tb$times)

    return(tb) 

}

SqlPlotForecastRelativeMerge <- function(tb, tz = NULL) {
    
    if (is.null(tz)) {
        tz <- 'CET'
    }

    tb$points <- tb$points %>% dplyr::arrange(times)
    tb$points$times <- suppressMessages(lubridate::ymd_hms(tb$points$times, tz = tz))

    forecast_from <- suppressMessages(lubridate::ymd_hms(tb$metadata$data_from, tz = tz))
    forecast_to <- suppressMessages(lubridate::ymd_hms(tb$metadata$data_to, tz = tz))
    monthly_to <- suppressMessages(lubridate::ymd_hms(tb$metadata$monthly_to, tz = tz))
    
    relative_times <- tb$points$times[which(tb$points$times < forecast_from)]
    forecast_times <- tb$points$times[which(tb$points$times >= forecast_from & tb$points$times <= forecast_to)]
    monthly_times <- tb$points$times[which(tb$points$times > forecast_to & tb$points$times <= monthly_to)]

    start_date_d <- suppressMessages(lubridate::floor_date(lubridate::with_tz(lubridate::today(tzone = tz) + lubridate::dmonths(-1), tzone = tz), 'day'))
    end_date_d <- suppressMessages(lubridate::floor_date(lubridate::with_tz(lubridate::today(tzone = tz) + lubridate::dmonths(3), tzone = tz), 'day'))

    window_values <- tb$points$values[which(tb$points$times >= start_date_d & tb$points$times <= end_date_d)]

    if ('name' %in% names(tb$metadata)) {

        title <- as.character(stringr::str_glue("<b>{tb$metadata$name}</b>"))

    } else if ('curve_name' %in% names(tb$metadata)) {
        
        title <- as.character(stringr::str_glue("<b>{tb$metadata$curve_name}</b>"))
    }

    m <- list(
        l = 80,
        r = 80,
        b = 30,
        t = 60,
        pad = 0
    )

    fig <- plotly::plot_ly(width = 809, 
                           height = 500) %>% 
        layout(title = list(text = title,
               x = 0.48, 
               y = 1, 
               xanchor = "middle", 
               yanchor = "top",
               font = list(family = 'Helvetica', size = 18, color = 'navy'),
               pad = list(b = 5, t = 5, l = 5, r = 5))) %>%
        add_annotations(text = as.character(stringr::str_glue('[{tb$metadata$unit}]')),
                        x = 0.5,
                        y = 1.1,
                        yref = "paper",
                        xref = "paper",
                        xanchor = "middle",
                        yanchor = "top",
                        showarrow = FALSE,
                        font = list(size = 15, color = 'navy')) %>%
        layout(yaxis = list(title = '',
                            showgrid = FALSE,
                            gridcolor = 'darkgray',
                            automargin = TRUE,
                            range = list(min(window_values), max(window_values)),
                            showline = TRUE,
                            linewidth = 1.3,
                            linecolor = 'navy',
                            mirror = TRUE),

                xaxis = list(title = '',
                            showgrid = TRUE,
                            showline = TRUE,
                            linecolor = 'navy',
                            range = list(start_date_d, end_date_d),
                            linewidth = 1.3,
                            mirror = TRUE,
                            type = 'date',
                            tickformat = "%d %b-%y<br>%H:%M"),
                margin = m) %>%
        layout(hovermode = 'x unified') %>%

        plotly::add_trace(data = tb$points, 
                           name = 'relative',
                           x = ~relative_times, 
                           y = ~tb$points$values[which(tb$points$times %in% relative_times)], 
                           type = "scatter", 
                           mode = "lines", 
                           line = list(color = 'darkblue', width = 0.5, opacity=0.5)) %>%

        plotly::add_trace(data = tb$points, 
                           name = 'forecast',
                           x = ~forecast_times, 
                           y = ~tb$points$values[which(tb$points$times %in% forecast_times)], 
                           type = "scatter", 
                           mode = "lines", 
                           line = list(color = 'crimson', width = 1.5, opacity=1)) %>%

        plotly::add_trace(data = tb$points, 
                          name = 'monthly',
                          x = ~monthly_times, 
                          y = ~tb$points$values[which(tb$points$times %in% monthly_times)], 
                          type = "scatter", 
                          mode = "lines", 
                          line = list(color = 'Indigo', width = 1, opacity=0.5, dash = "dash")) %>% 

        plotly::add_trace(data = tb$points, 
                          name = 'normal',
                          x = ~tb$points$times[which(tb$points$times > monthly_to)], 
                          y = ~tb$points$values[which(tb$points$times > monthly_to)], 
                          type = "scatter", 
                          mode = "lines", 
                          line = list(color = 'DeepPink', width = 0.5, opacity=0.5, dash = "dot")) %>% 

        plotly::config(displayModeBar = FALSE, showLink = FALSE)

    return(fig)
}


GetLatestForecast <- function(ws_token, 
                              curve_id = NULL, 
                              curve_name = NULL, 
                              json_file = NULL,
                              cluster_name = NULL,
                              ec = TRUE,
                              freq = NULL, 
                              func = NULL, 
                              tz = NULL, 
                              filter = NULL){

    if (!is.null(curve_id)) {
      
      curve_name <- GetNameFromId(ws_token, curve_id)
      
    } else if (!is.null(curve_name)) {
      
      curve_name <- trimws(curve_name)
      curve_name <-  stringi::stri_enc_toutf8(curve_name)
    }

    if (is.null(json_file) & !is.null(cluster_name)) {
        print("[json_file] argument missing!")
        return
    }   

    if (!is.null(json_file) & is.null(cluster_name)) {
        print("[cluster_name] argument missing!")
        return
    } 

    if (!is.null(json_file) & !is.null(cluster_name)) {

        json_file <- normalizePath(json_file)
        curve_list <- jsonlite::fromJSON(json_file)
        cluster_vec <- as.character(unname(curve_list[[cluster_name]]))

        if (ec == TRUE) {
            regexpat <- '\\bec00\\b' 
        } else if (ec == FALSE) {
            regexpat <- '\\bgfs00\\b' 
        }

        match <- stringr::str_extract(cluster_vec, regexpat)
        curve_name <- cluster_vec[which(!is.na(match))]

    }

    issue_vec <- c('00', '06', '12', '18')

    curves_created <- character()
    curve_names <- character()

    match <- stringr::str_extract(curve_name, 'ec\\d{2}')
    if (!is.na(match)) {
        match <- NA
        match <- stringr::str_extract(curve_name, 'ec\\d{2}ens')
        if (!is.na(match)) {
            issue_vec <- comprehenr::to_vec(for(issue in issue_vec) as.character(stringr::str_glue('ec{issue}ens')))
            regexpat <- 'ec\\d{2}ens'                          
        } else {
            issue_vec <- comprehenr::to_vec(for(issue in issue_vec) as.character(stringr::str_glue('ec{issue}')))
            regexpat <- 'ec\\d{2}'
        }
    } else {
        match <- NA
        match <- stringr::str_extract(curve_name, 'gfs\\d{2}ens')
        if (!is.na(match)) {
            issue_vec <- comprehenr::to_vec(for(issue in issue_vec) as.character(stringr::str_glue('gfs{issue}ens')))
            regexpat <- 'gfs\\d{2}ens'
        } else {
            issue_vec <- comprehenr::to_vec(for(issue in issue_vec) as.character(stringr::str_glue('gfs{issue}')))
            regexpat <- 'gfs\\d{2}'
        }
    }

    part_list <- strsplit(curve_name, ' ')[[1]]

    pos <- 1
    for (part in part_list) {
        match <- NA
        match <- stringr::str_extract(part, regexpat)
        if (!is.na(match)) {
            part_list <- part_list[-pos]
            break
        }
        pos <- pos + 1
    }

    curve_list <- list()

    regex <- '\\d{2}ens'

    for (issue in issue_vec){
        part_list <- append(part_list, issue, after = pos-1)
        curve_name <- paste(comprehenr::to_vec(for(part in part_list) as.character(stringr::str_glue('{part}'))), collapse=' ')

        part_list <- part_list[-pos]
        
        curve_list <- append(curve_list, list(curve))
        curve_names <- append(curve_names, curve_name)
        
        curve_meta <- GetLatestInstanceMetadata(ws_token = ws_token, curve_name = curve_name, tz = tz)
        curves_created <- append(curves_created, curve_meta$created)
    }

    latest_created <- max(curves_created)
    latest_idx <- which(latest_created == curves_created)
    latest_name <- curve_names[latest_idx]
    
    curves_created <- curves_created[-latest_idx]
    previous_created <- max(curves_created)
    curves_created <- append(curves_created, latest_created, after = latest_idx - 1)
    previous_idx <- which(previous_created == curves_created)
    previous_name <- curve_names[previous_idx]

    latest_ins <- GetLatestInstance(ws_token = ws_token, 
                                    curve_name = latest_name, 
                                    freq = freq, 
                                    func = func, 
                                    tz = tz,
                                    filter = filter,
                                    tags = FALSE)
    
    previous_ins <- GetLatestInstance(ws_token = ws_token, 
                                      curve_name = previous_name, 
                                      freq = freq, 
                                      func = func, 
                                      tz = tz,
                                      filter = filter,
                                      tags = FALSE)

    latest_points <- latest_ins$points
    previous_points <- previous_ins$points

    if ((length(previous_points[['times']]) > length(latest_points[['times']])) & (max(previous_points[['times']]) > max(latest_points[['times']]))){
        
        previous_extra <- dplyr::anti_join(previous_points, latest_points, by='times')

        if (length(previous_extra$times) > 0) {

            idx_vec <- which(previous_extra$times > max(latest_points[['times']]))

            if (length(idx_vec) > 0) {
                previous_extra <- previous_extra %>% slice(idx_vec)
                latest_points <- dplyr::bind_rows(previous_points, latest_points)
            }
        }  
    }

    latest_ins$metadata$data_size <- length(latest_points[['times']])
    latest_ins$metadata$data_from <- min(latest_points[['times']])
    latest_ins$metadata$data_to <- max(latest_points[['times']])
    
    if (!is.null(latest_points)){

        l <- list('metadata' = latest_ins$metadata,  'points' = latest_points)
        l
    }
}


GetRelativeForecast <- function(ws_token,  
                                offset, 
                                duration, 
                                issue_date_from,
                                issue_date_to,
                                curve_id = NULL,
                                curve_name = NULL,
                                freq = NULL,
                                func = NULL, 
                                tz = NULL, 
                                filter = NULL,
                                data_from = NULL,
                                data_to = NULL) {

    if (!is.null(curve_name)) {

        curve_name <- trimws(curve_name)
        curve_name <- stringi::stri_enc_toutf8(curve_name)
        curve_id <- GetIdFromName(ws_token = ws_token, names_vec = curve_name)

    } else if (!is.null(curve_id)) {
        curve_name <- GetNameFromId(ws_token = ws_token, curve_id = curve_id)
    }

    if (!is.null(data_from)) {
        if (IsIso8601(dt_string = data_from) == FALSE) {
            data_from <- FormatDateTimeStringIso8601(dt_string = data_from, tz = tz)
        }
        
    }

    if (!is.null(data_to)) {
        if (IsIso8601(dt_string = data_to) == FALSE) {
            data_to <- FormatDateTimeStringIso8601(dt_string = data_to, tz = tz)
        }
    } 

    offset <- GetIsoDuration(offset)
    duration <- GetIsoDuration(duration)
    
    curve <- GetCurve(ws_token, curve_name = curve_name)
    curve_id <- curve$id
    data_from_a <- suppressMessages(lubridate::ymd_hms(curve$accessRange, tz = tz) + lubridate::ddays(1)) %>% lubridate::date()
    data_from_a <- as.character(data_from_a)

    if (!is.null(data_from)) {
        if (data_from < data_from_a) {
            data_from <- data_from_a
        }
    }

    if (is.null(data_from)) {
        data_from <- data_from_a
    }

    if (!is.null(issue_date_from)) {

        if (issue_date_from < data_from_a) {
            issue_date_from <- suppressMessages(lubridate::ymd_hms(curve$accessRange, tz = tz)) %>% lubridate::date()
            issue_date_from <- as.character(issue_date_from)
        }
    }

    if (is.null(issue_date_from)) {
        issue_date_from <- suppressMessages(lubridate::ymd_hms(curve$accessRange, tz = tz)) %>% lubridate::date()
        issue_date_from <- as.character(issue_date_from)
    }
    
    if (curve$curve_type == 'INSTANCES') {

        base_url <- as.character(stringr::str_glue('https://api.wattsight.com/api/instances/{curve_id}/relative'))

        params <- list(data_offset = offset,
                       data_max_length = duration,
                       data_from = data_from,
                       data_to = data_to,
                       issue_date_from = issue_date_from,
                       issue_date_to = issue_date_to,
                       frequency = freq,
                       `function` = func, 
                       output_time_zone = tz,
                       filter = filter)

    } else if (curve$curve_type == 'TAGGED_INSTANCES') {

        base_url <- as.character(stringr::str_glue('https://api.wattsight.com/api/instances/tagged/{curve_id}/relative'))

        params <- list(data_offset = offset,
                       data_max_length = duration,
                       data_from = data_from,
                       data_to = data_to,
                       issue_date_from = issue_date_from,
                       issue_date_to = issue_date_to,
                       frequency = freq,
                       tag = 'Avg',
                       `function` = func, 
                       output_time_zone = tz,
                       filter = filter)
    }

    url_list <- httr::parse_url(base_url)    
    params[sapply(params, is.null)] <- NULL
    
    url_list$query <- params
    query_url <- httr::build_url(url_list)
    cnt <- httr::GET(query_url, httr::add_headers('Authorization' = paste("Bearer", ws_token$access_token)))
    
    cnt <- httr::content(x = cnt, as = "parsed", encoding = "UTF-8")
    
    points <- GetSeriesTibble(cnt, tz)
    
    meta <- cnt[names(cnt) != 'points']
    meta <- tibble::as_tibble(meta)

    meta$unit <- curve$unit
    meta$curve_type <- curve$curve_type
    meta$data_type <- curve$data_type

    meta$data_from <- FormatTimePointIso8601(min(points[['times']]), tz = tz)
    meta$data_to <- FormatTimePointIso8601(max(points[['times']]), tz = tz)
    meta$data_size <- length(points[['times']])


    meta <- meta[which(names(meta) != 'modified')]

    if ('tag' %in% names(meta)) {
        meta <- meta %>% dplyr::relocate(id, name, tag, curve_type, data_type, frequency, time_zone, data_from, data_to, data_size)
    } else {
        meta <- meta %>% dplyr::relocate(id, name, curve_type, data_type, frequency, time_zone, data_from, data_to, data_size)
    }
    
    list('metadata' = meta, 'points' = points)    

}




SqlCreateForecastEmptyDb <- function(db_path) {
  
    db_path <- normalizePath(db_path)

    con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
    
    RSQLite::dbSendStatement(con, 'PRAGMA encoding = "UTF-8"')
    RSQLite::dbSendStatement(con, 'PRAGMA journal_mode = WAL')
    RSQLite::dbSendStatement(con, 'PRAGMA busy_timeout = 8000')
    
    script <- 'CREATE TABLE IF NOT EXISTS metadata \ 
              (curve_id INTEGER NOT NULL, \
              table_name TEXT NOT NULL, \
              curve_name TEXT NOT NULL, \
              curve_type TEXT NOT NULL, \
              data_type TEXT NOT NULL, \
              unit TEXT NOT NULL, \
              frequency TEXT, \
              function TEXT, \
              issue_date TEXT, \
              created TEXT, \
              modified TEXT, \
              data_from TEXT, \
              data_to TEXT, \
              data_size INTEGER, \
              begin TEXT, \
              end TEXT, \
              PRIMARY KEY(curve_id, table_name));'
    
    
    DBI::dbWithTransaction(con, {
        DBI::dbExecute(con, script)
    })
    
    DBI::dbDisconnect(con)
}


SqlCreateForecastDb <- function(db_path, curves_json, func = NULL, tz = NULL) {
  
    ws_token <- GetToken()

    db_path <- normalizePath(db_path)
    curves_json <- normalizePath(curves_json)

    curve_list <- jsonlite::fromJSON(curves_json)
    cat_names <- names(curve_list)

    con <- DBI::dbConnect(RSQLite::SQLite(), db_path)

    RSQLite::dbSendStatement(con, 'PRAGMA encoding = "UTF-8"')
    RSQLite::dbSendStatement(con, 'PRAGMA journal_mode = WAL')
    RSQLite::dbSendStatement(con, 'PRAGMA busy_timeout = 8000')
    
    script <- 'CREATE TABLE IF NOT EXISTS metadata \ 
              (curve_id INTEGER NOT NULL, \
              table_name TEXT NOT NULL, \
              curve_name TEXT NOT NULL, \
              curve_type TEXT NOT NULL, \
              data_type TEXT NOT NULL, \
              unit TEXT NOT NULL, \
              frequency TEXT, \
              function TEXT, \
              issue_date TEXT, \
              created TEXT, \
              modified TEXT, \
              data_from TEXT, \
              data_to TEXT, \
              data_size INTEGER, \
              begin TEXT, \
              end TEXT, \
              PRIMARY KEY(curve_id, table_name));'
    
    DBI::dbWithTransaction(con, {
        DBI::dbExecute(con, script)
    })

    DBI::dbDisconnect(con)

    for (i in 1:length(cat_names)) {
        ws_token <- GetToken()
        freq <- substr(cat_names[i], nchar(cat_names[i]), nchar(cat_names[i]))
        names_vec <-  stringi::stri_enc_toutf8(curve_list[[i]])
        
        for (curve_name in names_vec) {
            
            SqlCreateForecastCatalogueTable(ws_token = ws_token, 
                                            db_path = db_path, 
                                            curve_name = curve_name, 
                                            freq = freq, 
                                            func = func, 
                                            tz = tz)
        }
    }
}


SqlCreateForecastTable <- function(ws_token, 
                                   db_path, 
                                   curve_id = NULL, 
                                   curve_name = NULL, 
                                   freq = NULL, 
                                   func = NULL, 
                                   tz = NULL) {
  
    if (!is.null(curve_id)) {
        curve_name <- GetNameFromId(ws_token, curve_id)
    }  else if (!is.null(curve_name)) {
        curve_name <- trimws(curve_name)
        curve_name <-  stringi::stri_enc_toutf8(curve_name)
    }

    data_from <- suppressMessages(lubridate::floor_date(lubridate::now(tzone = tz), 'hour'))
    data_from <- FormatDateTimeIso8601(tp = data_from, tz = tz)

    data_to <- suppressMessages(lubridate::floor_date(lubridate::now(tzone = tz) + lubridate::dyears(3), 'year'))
    data_to <- FormatDateTimeIso8601(tp = data_to, tz = tz) 

    db_path <- normalizePath(db_path)
    table_name <- SqlGetTableName(curve_name)

    curve <- GetCurve(ws_token, curve_name = curve_name)

    issue_date <- ''
    created <- ''
    modified <- ''
    begin <- ''
    end <- ''
    
    curve_type <- curve$curve_type

    if (curve_type == 'TIME_SERIES') {
        
        ins <- GetTimeSeries(ws_token = ws_token, 
                             curve_name = curve_name, 
                             freq = freq, 
                             func = func, 
                             tz = tz,
                             data_from = data_from,
                             data_to = data_to)
        tb <- ins$points

    created <- FormatTimePointIso8601(curve$created, tz = tz)

    } else {

        ins <- GetLatestInstance(ws_token = ws_token, 
                                 curve_name = curve_name, 
                                 freq = freq, 
                                 func = func, 
                                 tz = tz,
                                 data_from = data_from,
                                 data_to = data_to) 

        issue_date <- ins$metadata$issue_date
        created <- FormatTimePointIso8601(ins$metadata$created, tz = tz)
    }
    
    modified <- FormatTimePointIso8601(ins$metadata$modified, tz = tz)

    tb_times <- as.character(ins$points[['times']])
    tb_values <- zoo::na.locf(ins$points[['values']], na.rm = F)

    data_size <- length(ins$points[['times']])
    data_from <- min(ins$points[['times']])
    data_to <- max(ins$points[['times']])

    data_type <- curve$data_type
    curve_id <- curve$id
    curve_unit <- curve$unit
    
    if (!is.null(freq)) {
        frequency <- freq
    } else {
        frequency <- curve$frequency
    }

    if (!is.null(func)) {
        `function` <- func
    } else {
        `function` <- ''
    }

    con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
    
    com <- as.character(stringr::str_glue("INSERT OR REPLACE INTO 'metadata' \
                                          (curve_id, table_name, curve_name, \
                                          curve_type, data_type, unit, frequency, `function`, \
                                          issue_date, created, modified, data_from, data_to, data_size, begin, end) \
                                          VALUES({curve_id}, '{table_name}', '{curve_name}', \
                                          '{curve_type}', '{data_type}', '{curve_unit}', '{frequency}', \
                                          '{`function`}', '{issue_date}', '{created}', '{modified}', \
                                          '{data_from}', '{data_to}', {data_size}, '{begin}', '{end}');"))
    DBI::dbWithTransaction(con, {
        DBI::dbExecute(con, com)
    })

    com <- as.character(stringr::str_glue("CREATE TABLE IF NOT EXISTS '{table_name}' \
                                          (times TEXT NOT NULL, 'values' REAL, PRIMARY KEY(times));"))

    DBI::dbWithTransaction(con, {
        DBI::dbExecute(con, com)
    })

    for (i in seq_along(index(ins$points))) {

        if ((!is.na(tb_times[i])) & (!is.na(tb_values[i]))) {

            com <- as.character(stringr::str_glue("INSERT OR REPLACE INTO '{table_name}'(times, 'values') \
                                                  VALUES('{tb_times[i]}', {tb_values[i]});"))
            DBI::dbWithTransaction(con, {
                DBI::dbExecute(con, com)
            })
        }
    }

    DBI::dbDisconnect(con)

    data_from_d <- suppressMessages(lubridate::ymd_hms(data_from, tz = tz))
    data_from_d <- strftime(data_from_d, "%Y-%m-%d %H:%M")

    data_to_d <- suppressMessages(lubridate::ymd_hms(data_to, tz = tz))
    data_to_d <- strftime(data_to_d, "%Y-%m-%d %H:%M")

    PrintDbOutput(curve_id = curve_id, 
                  table_name = table_name, 
                  data_from = data_from_d, 
                  data_to = data_to_d, 
                  data_size = data_size)
}




SqlCreateForecastCatalogueTable <- function(ws_token, 
                                            db_path, 
                                            curve_name,
                                            freq = NULL, 
                                            func = NULL, 
                                            tz = NULL) {

    curve_name <-  stringi::stri_enc_toutf8(curve_name)                                 
    table_name <- SqlGetTableName(curve_name)
    
    curve <- GetCurve(ws_token = ws_token, curve_name = curve_name)
    
    issue_date <- ''
    created <- ''
    modified <- ''
    begin <- ''
    end <- ''

    data_from <- suppressMessages(lubridate::floor_date(lubridate::now(tzone = tz), 'hour'))
    data_from <- FormatDateTimeIso8601(tp = data_from, tz = tz)

    data_to <- suppressMessages(lubridate::floor_date(lubridate::now(tzone = tz) + lubridate::dyears(3), 'year'))
    data_to <- FormatDateTimeIso8601(tp = data_to, tz = tz)  

    curve_type <- curve$curve_type
    
    if (curve_type == 'TIME_SERIES') {
        
        ins <- GetTimeSeries(ws_token = ws_token, 
                             curve_name = curve_name, 
                             freq = freq, 
                             func = func, 
                             tz = tz,
                             data_from = data_from,
                             data_to = data_to)

        created <- FormatTimePointIso8601(curve$created, tz = tz)

        modified <- ins$metadata$modified
        
    } else {
        ins <- GetLatestInstance(ws_token = ws_token, 
                                 curve_name = curve_name, 
                                 freq = freq, 
                                 func = func, 
                                 tz = tz,
                                 data_from = data_from,
                                 data_to = data_to) 

        issue_date <- ins$metadata$issue_date
        created <- FormatTimePointIso8601(ins$metadata$created, tz = tz)
        
    }

    modified <- FormatTimePointIso8601(ins$metadata$modified, tz = tz)
    
    tb_times <- as.character(ins$points[['times']])
    tb_values <- zoo::na.locf(ins$points[['values']], na.rm = F)

    data_size <- length(ins$points[['times']])
    data_from <- min(ins$points[['times']])
    data_to <- max(ins$points[['times']])

    curve_id <- curve$id
    data_type <- curve$data_type

    unit <- curve$unit
    
    if (!is.null(freq)) {
        frequency <- freq
    } else {
        frequency <- curve$frequency
    }

    if (!is.null(func)) {
        `function` <- func
    } else {
        `function` <- ''
    }
    
    con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
    comm <- as.character(stringr::str_glue("INSERT OR REPLACE INTO 'metadata' \
                                           (curve_id, table_name, curve_name, \
                                           curve_type, data_type, unit, frequency, `function`, \
                                           issue_date, created, modified, data_from, data_to, data_size, begin, end) \
                                           VALUES({curve_id}, '{table_name}', '{curve_name}', \
                                           '{curve_type}', '{data_type}', '{unit}', '{frequency}', \
                                           '{`function`}', '{issue_date}', '{created}', '{modified}', \
                                           '{data_from}', '{data_to}', {data_size}, '{begin}', '{end}');"))
    DBI::dbWithTransaction(con, {
        DBI::dbExecute(con, comm)
    })
    
    com <- as.character(stringr::str_glue("CREATE TABLE IF NOT EXISTS '{table_name}' \
                                          ('times' TEXT NOT NULL, 'values' REAL, PRIMARY KEY(times));"))
    DBI::dbWithTransaction(con, {
        DBI::dbExecute(con, com)
    })
    

    for (i in seq_along(index(ins$points))) {

        if ((!is.na(tb_times[i])) & (!is.na(tb_values[i]))) {
            com <- as.character(stringr::str_glue("INSERT OR REPLACE INTO '{table_name}'(times, 'values') \
                                                  VALUES('{tb_times[i]}', {tb_values[i]});"))
            DBI::dbWithTransaction(con, {
                DBI::dbExecute(con, com)
            })
        }
    }


    DBI::dbDisconnect(con)

    data_from_d <- suppressMessages(lubridate::ymd_hms(data_from, tz = tz))
    data_from_d <- strftime(data_from_d, "%Y-%m-%d %H:%M")

    data_to_d <- suppressMessages(lubridate::ymd_hms(data_to, tz = tz))
    data_to_d <- strftime(data_to_d, "%Y-%m-%d %H:%M")

    PrintDbOutput(curve_id = curve_id, 
                  table_name = table_name, 
                  data_from = data_from_d, 
                  data_to = data_to_d, 
                  data_size = data_size)
}




SqlCreateForecastCluster <- function(db_path,
                                     json_file, 
                                     cluster_name,
                                     freq = NULL, 
                                     func = NULL, 
                                     tz = NULL) {

    db_path <- normalizePath(db_path)
    json_path <- normalizePath(json_file)

    if (is.null(tz)) {
        tz <- 'CET'
    }

    if (is.null(func)) {
        func <- 'AVERAGE'
    }

    tb_list <- JsonReadCatalogue(json_path)
    curve_names <- tb_list[[cluster_name]]$name

    ws_token <- GetToken()

    for (curve_name in curve_names) {

        table_name <- SqlGetTableName(curve_name)

        curve <- GetCurve(ws_token = ws_token, curve_name = curve_name)
        
        issue_date <- ''
        created <- ''
        modified <- ''
        begin <- ''
        end <- ''

        data_from <- suppressMessages(lubridate::floor_date(lubridate::now(tzone = tz), 'hour'))
        data_from <- FormatDateTimeIso8601(tp = data_from, tz = tz)

        data_to <- suppressMessages(lubridate::floor_date(lubridate::now(tzone = tz) + lubridate::dyears(3), 'year'))
        data_to <- FormatDateTimeIso8601(tp = data_to, tz = tz)  

        curve_type <- curve$curve_type
        
        if (curve_type == 'TIME_SERIES' | curve_type == 'TAGGED') {
            
            ins <- GetTimeSeries(ws_token = ws_token, 
                                 curve_name = curve_name, 
                                 freq = freq, 
                                 func = func, 
                                 tz = tz,
                                 data_from = data_from,
                                 data_to = data_to)

            created <- FormatTimePointIso8601(curve$created, tz = tz)

            modified <- ins$metadata$modified
            
        } else {
            ins <- GetLatestInstance(ws_token = ws_token, 
                                     curve_name = curve_name, 
                                     freq = freq, 
                                     func = func, 
                                     tz = tz,
                                     data_from = data_from,
                                     data_to = data_to) 
            
            if (is.na(ins)) {
                return('Operation Aborted')
            }

            issue_date <- ins$metadata$issue_date
            created <- FormatTimePointIso8601(ins$metadata$created, tz = tz)
        }

        modified <- FormatTimePointIso8601(ins$metadata$modified, tz = tz)
        
        tb_times <- as.character(ins$points[['times']])
        tb_values <- zoo::na.locf(ins$points[['values']], na.rm = F)

        data_size <- length(ins$points[['times']])
        data_from <- min(ins$points[['times']])
        data_to <- max(ins$points[['times']])

        curve_id <- curve$id
        data_type <- curve$data_type

        unit <- curve$unit
        
        if (!is.null(freq)) {
            frequency <- freq
        } else {
            frequency <- curve$frequency
        }

        if (!is.null(func)) {
            `function` <- func
        } else {
            `function` <- ''
        }
        
        con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
        comm <- as.character(stringr::str_glue("INSERT OR REPLACE INTO 'metadata' \
                                               (curve_id, table_name, curve_name, \
                                               curve_type, data_type, unit, frequency, `function`, \
                                               issue_date, created, modified, data_from, data_to, data_size, begin, end) \
                                               VALUES({curve_id}, '{table_name}', '{curve_name}', \
                                               '{curve_type}', '{data_type}', '{unit}', '{frequency}', \
                                               '{`function`}', '{issue_date}', '{created}', '{modified}', \
                                               '{data_from}', '{data_to}', {data_size}, '{begin}', '{end}');"))
        DBI::dbWithTransaction(con, {
            DBI::dbExecute(con, comm)
        })
        
        com <- as.character(stringr::str_glue("CREATE TABLE IF NOT EXISTS '{table_name}' \
                                              ('times' TEXT NOT NULL, 'values' REAL, PRIMARY KEY(times));"))
        DBI::dbWithTransaction(con, {
            DBI::dbExecute(con, com)
        })
        
        for (i in seq_along(index(ins$points))) {

            if ((!is.na(tb_times[i])) & (!is.na(tb_values[i]))) {
                com <- as.character(stringr::str_glue("INSERT OR REPLACE INTO '{table_name}'(times, 'values') \
                                                      VALUES('{tb_times[i]}', {tb_values[i]});"))
                DBI::dbWithTransaction(con, {
                    DBI::dbExecute(con, com)
                })
            }
        }

        data_from_d <- suppressMessages(lubridate::ymd_hms(data_from, tz = tz))
        data_from_d <- strftime(data_from_d, "%Y-%m-%d %H:%M")

        data_to_d <- suppressMessages(lubridate::ymd_hms(data_to, tz = tz))
        data_to_d <- strftime(data_to_d, "%Y-%m-%d %H:%M")

        PrintDbOutput(curve_id = curve_id, 
                      table_name = table_name, 
                      data_from = data_from_d, 
                      data_to = data_to_d, 
                      data_size = data_size)
    }
    
    DBI::dbDisconnect(con)
    con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
    RSQLite::dbSendStatement(con, "VACUUM;")
    DBI::dbDisconnect(con)   
    cat(as.character(stringr::str_glue('\r\n\r\nDB path: [{db_path}]\r\n')))
}




SqlUpdateForecastTable <- function(db_path, 
                                   curve_id = NULL, 
                                   curve_name = NULL, 
                                   freq = NULL, 
                                   func = NULL, 
                                   tz = NULL) {

    ws_token <- GetToken()

    db_path <- normalizePath(db_path)

    if (!is.null(curve_id)) {

        con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
        query <- as.character(stringr::str_glue("SELECT curve_name FROM 'metadata' WHERE curve_id = {curve_id}"))
        res <- DBI::dbGetQuery(con, query)
        DBI::dbDisconnect(con)

    } else if (!is.null(curve_name)) {
        curve_name <- trimws(curve_name)
        curve_name <- stringi::stri_enc_toutf8(curve_name)
    }

    table_name <- SqlGetTableName(curve_name = curve_name)

    con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
    comm <- as.character(stringr::str_glue("DELETE FROM '{table_name}'"))

    DBI::dbWithTransaction(con, {
        DBI::dbExecute(con, comm)
    })


    curve <- GetCurve(ws_token = ws_token, curve_name = curve_name)
    
    issue_date <- ''
    created <- ''
    modified <- ''
    begin <- ''
    end <- ''

    data_from <- suppressMessages(lubridate::floor_date(lubridate::now(tzone = tz), 'hour'))
    data_from <- FormatDateTimeIso8601(tp = data_from, tz = tz)

    data_to <- suppressMessages(lubridate::floor_date(lubridate::now(tzone = tz) + lubridate::dyears(3), 'year'))
    data_to <- FormatDateTimeIso8601(tp = data_to, tz = tz)  

    curve_type <- curve$curve_type
    
    if (curve_type == 'TIME_SERIES') {
        
        ins <- GetTimeSeries(ws_token = ws_token, 
                             curve_name = curve_name, 
                             freq = freq, 
                             func = func, 
                             tz = tz,
                             data_from = data_from,
                             data_to = data_to)

        created <- FormatTimePointIso8601(curve$created, tz = tz)

        modified <- ins$metadata$modified
        
    } else {
        ins <- GetLatestInstance(ws_token = ws_token, 
                                 curve_name = curve_name, 
                                 freq = freq, 
                                 func = func, 
                                 tz = tz,
                                 data_from = data_from,
                                 data_to = data_to) 

        issue_date <- ins$metadata$issue_date
        created <- FormatTimePointIso8601(ins$metadata$created, tz = tz)
        
    }

    modified <- FormatTimePointIso8601(ins$metadata$modified, tz = tz)
    
    tb_times <- as.character(ins$points[['times']])
    tb_values <- zoo::na.locf(ins$points[['values']], na.rm = F)

    data_size <- length(ins$points[['times']])
    data_from <- min(ins$points[['times']])
    data_to <- max(ins$points[['times']])

    curve_id <- curve$id
    data_type <- curve$data_type

    unit <- curve$unit
    
    if (!is.null(freq)) {
        frequency <- freq
    } else {
        frequency <- curve$frequency
    }

    if (!is.null(func)) {
        `function` <- func
    } else {
        `function` <- ''
    }
    
    con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
    comm <- as.character(stringr::str_glue("INSERT OR REPLACE INTO 'metadata' \
                                           (curve_id, table_name, curve_name, \
                                           curve_type, data_type, unit, frequency, `function`, \
                                           issue_date, created, modified, data_from, data_to, data_size, begin, end) \
                                           VALUES({curve_id}, '{table_name}', '{curve_name}', \
                                           '{curve_type}', '{data_type}', '{unit}', '{frequency}', \
                                           '{`function`}', '{issue_date}', '{created}', '{modified}', \
                                           '{data_from}', '{data_to}', {data_size}, '{begin}', '{end}');"))
    DBI::dbWithTransaction(con, {
        DBI::dbExecute(con, comm)
    })

    for (i in seq_along(index(ins$points))) {

        if ((!is.na(tb_times[i])) & (!is.na(tb_values[i]))) {
            com <- as.character(stringr::str_glue("INSERT OR REPLACE INTO '{table_name}'(times, 'values') \
                                                  VALUES('{tb_times[i]}', {tb_values[i]});"))
            DBI::dbWithTransaction(con, {
                DBI::dbExecute(con, com)
            })
        }
    }

    DBI::dbDisconnect(con)

    data_from_d <- suppressMessages(lubridate::ymd_hms(data_from, tz = tz))
    data_from_d <- strftime(data_from_d, "%Y-%m-%d %H:%M")

    data_to_d <- suppressMessages(lubridate::ymd_hms(data_to, tz = tz))
    data_to_d <- strftime(data_to_d, "%Y-%m-%d %H:%M")

    PrintDbOutput(curve_id = curve_id, 
                  table_name = table_name, 
                  data_from = data_from_d, 
                  data_to = data_to_d, 
                  data_size = data_size)

}




SqlUpdateForecastDb <- function(db_path, tz) {
  
    db_path <- normalizePath(db_path)
    con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
    query <- "SELECT curve_name FROM metadata;"
    res <- DBI::dbGetQuery(con, query)
    curve_names <- res$curve_name
    DBI::dbDisconnect(con) 
    
    match <- stringr::str_extract(curve_names, '.*n$')
    normal_vec <- match[which(!is.na(match))]
    
    curve_names <- curve_names[which(curve_names %notin% normal_vec)]

    for (curve_name in curve_names) {

        con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
        query <- as.character(stringr::str_glue("SELECT frequency, `function` FROM metadata WHERE curve_name = '{curve_name}';"))

        res <- tryCatch(DBI::dbGetQuery(con, query),
                        error = function(cond) {
                            print(as.character(stringr::str_glue("Error while processing [{curve_name}]")))
                            DBI::dbDisconnect(con)
                            return("Process interrupted")
                        })

        freq <- res$frequency
        func <- res$`function`
        DBI::dbDisconnect(con) 

        SqlUpdateForecastTable(db_path = db_path, 
                               curve_name = curve_name, 
                               freq = freq, 
                               func = func, 
                               tz = tz)
    }

    SqlTrimForecastDb(db_path)

    con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
    RSQLite::dbSendStatement(con, "VACUUM;")
    DBI::dbDisconnect(con) 
    cat(as.character(stringr::str_glue('\r\n\r\nDB path: [{db_path}]\r\n')))  
}



SqlTrimForecastDb <- function(db_path) {

    data_from <- suppressMessages(lubridate::floor_date(lubridate::now(tzone = tz), 'hour'))
    data_from <- FormatDateTimeIso8601(tp = data_from, tz = tz)

    con <- DBI::dbConnect(RSQLite::SQLite(), db_path)

    query <- "SELECT table_name FROM metadata;"
    res <- DBI::dbGetQuery(con, query)
    table_names <- res$table_name

    for (table_name in table_names) {
        com <- as.character(stringr::str_glue("DELETE FROM '{table_name}' WHERE times < '{data_from}';"))

        DBI::dbWithTransaction(con, {
            DBI::dbExecute(con, com)
        })
    }

    DBI::dbDisconnect(con)  
}



SqlCreateHistoryEmptyDb <- function(db_path) {
  
    db_path <- normalizePath(db_path)

    con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
    
    RSQLite::dbSendStatement(con, 'PRAGMA encoding = "UTF-8"')
    RSQLite::dbSendStatement(con, 'PRAGMA journal_mode = WAL')
    RSQLite::dbSendStatement(con, 'PRAGMA busy_timeout = 8000')
    
    script <- 'CREATE TABLE IF NOT EXISTS metadata \ 
              (curve_id INTEGER NOT NULL, \
              table_name TEXT NOT NULL, \
              curve_name TEXT NOT NULL, \
              curve_type TEXT NOT NULL, \
              data_type TEXT NOT NULL, \
              unit TEXT NOT NULL, \
              frequency TEXT, \
              function TEXT, \
              data_from TEXT, \
              data_to TEXT, \
              data_size INTEGER, \
              PRIMARY KEY(curve_id, table_name));'
    
    
    DBI::dbWithTransaction(con, {
        DBI::dbExecute(con, script)
    })
    
    DBI::dbDisconnect(con)
}


SqlCreateHistoryCluster <- function(db_path, 
                                    json_file, 
                                    cluster_name,
                                    offset, 
                                    duration, 
                                    data_from = NULL, 
                                    freq = NULL, 
                                    func = NULL, 
                                    tz = NULL) {


    db_path <- normalizePath(db_path)
    json_path <- normalizePath(json_file)

    if (is.null(tz)) {
        tz <- 'CET'
    }

    if (is.null(func)) {
        func <- 'AVERAGE'
    }

    data_to <- SqlGetDbMaxTo(db_path = db_path, tz = tz) 

    start_date <- as.POSIXct(data_from, origin = "1970-01-01", tz = tz)
    end_date <- suppressMessages(lubridate::ymd_hms(data_to, tz = tz))

    time_index <- seq(from = start_date, to = end_date, by = "hour")
    time_index <- strftime(time_index, "%Y-%m-%dT%H:%M:%S%z")

    for (i in 1:length(time_index)) {

        last <- substr(time_index[i], nchar(time_index[i])-1, nchar(time_index[i]))
        time_index[i] <- stringr::str_replace(time_index[i], '\\d{2}$', as.character(stringr::str_glue(':{last}')))
    }

    data_from <- lubridate::ymd(data_from, tz = tz)

    issue_date_from <- suppressMessages(lubridate::floor_date(lubridate::ymd(data_from, tz = tz) + lubridate::ddays(-3), 'day') %>% lubridate::date())
    issue_date_from <- as.character(issue_date_from)
    
    data_from <- FormatDateTimeIso8601(tp = data_from, tz = tz)
    
    data_to <- SqlGetDbMaxToPlusOneHour(db_path = db_path, tz = tz)
    issue_date_to <- FormatNextDay(tz = tz)

    tb_list <- JsonReadCatalogue(json_path)
    curve_names <- tb_list[[cluster_name]]$name

    for (curve_name in curve_names) {

        curve_name <- stringi::stri_enc_toutf8(curve_name)

        SqlCreateHistoryCatalogueTable(db_path = db_path, 
                                       offset = offset, 
                                       duration = duration, 
                                       data_from = data_from, 
                                       data_to = data_to,
                                       issue_date_from = issue_date_from,
                                       issue_date_to = issue_date_to,
                                       curve_name = curve_name, 
                                       time_index = time_index,
                                       freq = freq, 
                                       func = func, 
                                       tz = tz)        

    }

    con <- DBI::dbConnect(RSQLite::SQLite(), db_path)


    for (curve_name in curve_names) {

        ws_token <- GetToken()
        curve <- GetCurve(ws_token = ws_token, curve_name = curve_name)

        table_name <- SqlGetTableName(curve_name)
        
        curve_type <- curve$curve_type
        data_type <- curve$data_type
        curve_id <- curve$id
        curve_unit <- curve$unit
        
        if (!is.null(freq)) {
            frequency <- freq
        } else {
            frequency <- curve$frequency
        }

        if (!is.null(func)) {
            `function` <- func
        } else {
            `function` <- ''
        }

        query <- as.character(stringr::str_glue("SELECT * FROM '{table_name}';"))
        df <- DBI::dbGetQuery(con, query)
        tb <- tibble::as_tibble(df)
        data_times <- tb$times[which(!is.na(tb$values))]
        data_from_table <- min(data_times)
        data_to_table <- max(data_times)
        data_size <- length(data_times)

        com <- as.character(stringr::str_glue("INSERT OR REPLACE INTO metadata \
                                              (curve_id, table_name, curve_name, curve_type, data_type, \
                                              unit, frequency, `function`, data_from, data_to, data_size) \
                                              VALUES({curve_id}, '{table_name}', '{curve_name}', '{curve_type}', '{data_type}', \
                                              '{curve_unit}', '{frequency}', '{`function`}', \
                                              '{data_from_table}', '{data_to_table}', {data_size});"))
        DBI::dbWithTransaction(con, {
            DBI::dbExecute(con, com)
        })
    }

    DBI::dbDisconnect(con)

    min_to <- SqlUpdateHistoryMetadataTable(db_path)

    min_to <- suppressMessages(lubridate::ymd_hms(min_to, tz = tz))
    min_to <- strftime(min_to, "%Y-%m-%d %H:%M")

    cat(as.character(stringr::str_glue('\r\n\r\nDB path: [{db_path}]\r\n')))
    cat(as.character(stringr::str_glue('\r\nData to: [{min_to}]\r\n')))

}


SqlCreateHistoryTable <- function(db_path, 
                                  offset, 
                                  duration, 
                                  curve_id = NULL, 
                                  curve_name = NULL, 
                                  data_from = NULL,
                                  freq = NULL, 
                                  func = NULL, 
                                  tz = NULL) {
    ws_token <- GetToken()                                  
    
    db_path <- normalizePath(db_path)

    if (!is.null(curve_id)) {
        curve_name <- GetNameFromId(ws_token = ws_token, curve_id = curve_id)

    } else if (!is.null(curve_name)) {
        curve_name <- trimws(curve_name)
        curve_name <- stringi::stri_enc_toutf8(curve_name)
    }

    if (is.null(data_from)) {
        data_from <- SqlGetDbMinFrom(db_path = db_path)
    } 

    data_to <- SqlGetDbMaxToPlusOneHour(db_path = db_path, tz = tz) 

    issue_date_from <- suppressMessages(lubridate::floor_date(lubridate::ymd(data_from, tz = tz) + lubridate::ddays(-3), 'day') %>% lubridate::date())
    issue_date_from <- as.character(issue_date_from)

    issue_date_to <- FormatNextDay(tz)

    data_from <- FormatDateTimeStringIso8601(data_from, tz)

    table_name <- SqlGetTableName(curve_name)

    curve <- GetCurve(ws_token = ws_token, curve_name = curve_name)
    
    curve_type <- curve$curve_type
    
    if (curve_type == 'TIME_SERIES') {
        
        ins <- GetTimeSeries(ws_token = ws_token, 
                             curve_name = curve_name, 
                             freq = freq, 
                             func = func, 
                             tz = tz, 
                             data_from = data_from,
                             data_to = data_to)
        
    } else {

        ins <- GetRelativeForecast(ws_token = ws_token, 
                                   offset = offset,
                                   duration = duration,
                                   issue_date_from = issue_date_from,
                                   issue_date_to = issue_date_to,
                                   curve_name = curve_name, 
                                   freq = freq, 
                                   func = func, 
                                   tz = tz,
                                   filter = NULL,
                                   data_from = data_from,
                                   data_to = data_to)
    }
    
    if (is.null(tz)) {
        tz <- 'CET'
    }

    data_to <- SqlGetDbMaxTo(db_path, tz)
    start_date <- suppressMessages(lubridate::ymd_hms(data_from, tz = tz))
    end_date <- suppressMessages(lubridate::ymd_hms(data_to, tz = tz))
    
    time_index <- seq(from = start_date, to = end_date, by = "hour")
    time_index <- strftime(time_index, "%Y-%m-%dT%H:%M:%S%z")

    for (i in 1:length(time_index)) {

        last <- substr(time_index[i], nchar(time_index[i])-1, nchar(time_index[i]))
        time_index[i] <- stringr::str_replace(time_index[i], '\\d{2}$', as.character(stringr::str_glue(':{last}')))
    }

    tb_times <- as.character(ins$points[['times']])
    tb_values <- zoo::na.locf(ins$points[['values']], na.rm = F)

    curve_id <- curve$id

    data_type <- curve$data_type
    unit <- curve$unit
    
    if (!is.null(freq)) {
        frequency <- freq
    } else {
        frequency <- curve$frequency
    }

    if (!is.null(func)) {
        `function` <- func
    } else {
        `function` <- ''
    }

    con <- DBI::dbConnect(RSQLite::SQLite(), db_path)

    com <- as.character(stringr::str_glue("CREATE TABLE IF NOT EXISTS '{table_name}' \
                                           (times TEXT NOT NULL, 'values' REAL, PRIMARY KEY(times));"))

    DBI::dbWithTransaction(con, {
        DBI::dbExecute(con, com)
    })

    for (i in 1:length(time_index)) {

        if (time_index[i] %in% tb_times) {

            idx <- index(tb_times)[which(tb_times == time_index[i])]

            if (is.na(tb_values[idx])) {

                com <- as.character(stringr::str_glue("INSERT OR REPLACE INTO '{table_name}'(times, 'values') \
                                                      VALUES('{time_index[i]}', NULL);"))
            } else {

                com <- as.character(stringr::str_glue("INSERT OR REPLACE INTO '{table_name}'(times, 'values') \
                                                      VALUES('{time_index[i]}', {tb_values[idx]});"))
            }

        } else {

            com <- as.character(stringr::str_glue("INSERT OR REPLACE INTO '{table_name}'(times, 'values') \
                                                  VALUES('{time_index[i]}', NULL);"))           
        }

        DBI::dbWithTransaction(con, {
            DBI::dbExecute(con, com)
        })
    }

    DBI::dbDisconnect(con)

    con <- DBI::dbConnect(RSQLite::SQLite(), db_path)

    com <- as.character(stringr::str_glue("DELETE FROM '{table_name}' WHERE times > '{data_to}';"))

    DBI::dbWithTransaction(con, {
        DBI::dbExecute(con, com)
    })

    query <- as.character(stringr::str_glue("SELECT * FROM '{table_name}';"))
    res <- DBI::dbGetQuery(con, query)
    tb <- tibble::as_tibble(res)
    data_times <- tb$times[which(!is.na(tb$values))]

    data_size <- length(data_times)
    data_from <- min(data_times)
    data_to <- max(data_times)

    com <- as.character(stringr::str_glue("INSERT OR REPLACE INTO 'metadata' \
                                          (curve_id, table_name, curve_name, curve_type, data_type,\
                                          unit, frequency, `function`, data_from, data_to, data_size) \
                                          VALUES({curve_id}, '{table_name}', '{curve_name}', '{curve_type}', '{data_type}', \
                                          '{unit}', '{frequency}', '{`function`}', \
                                          '{data_from}', '{data_to}', {data_size});"))
    DBI::dbWithTransaction(con, {
        DBI::dbExecute(con, com)
    })

    DBI::dbDisconnect(con)

    data_from_d <- suppressMessages(lubridate::ymd_hms(data_from, tz = tz))
    data_from_d <- strftime(data_from_d, "%Y-%m-%d %H:%M")

    data_to_d <- suppressMessages(lubridate::ymd_hms(data_to, tz = tz))
    data_to_d <- strftime(data_to_d, "%Y-%m-%d %H:%M")

    PrintDbOutput(curve_id = curve_id, 
                  table_name = table_name, 
                  data_from = data_from_d, 
                  data_to = data_to_d, 
                  data_size = data_size)
}




SqlCreateHistoryDb <- function(db_path, 
                               curves_json, 
                               offset, 
                               duration, 
                               data_from, 
                               func = NULL, 
                               tz = NULL) {
                                                   
    ws_token <- GetToken()

    db_path <- normalizePath(db_path)
    curves_json <- normalizePath(curves_json)

    curve_list <- jsonlite::fromJSON(curves_json)
    cat_names <- names(curve_list)

    con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
    
    RSQLite::dbSendStatement(con, 'PRAGMA encoding = "UTF-8"')
    RSQLite::dbSendStatement(con, 'PRAGMA journal_mode = WAL')
    RSQLite::dbSendStatement(con, 'PRAGMA busy_timeout = 8000')
    
    script <- 'CREATE TABLE IF NOT EXISTS metadata \ 
              (curve_id INTEGER NOT NULL, \
              table_name TEXT NOT NULL, \
              curve_name TEXT NOT NULL, \
              curve_type TEXT NOT NULL, \
              data_type TEXT NOT NULL, \
              unit TEXT NOT NULL, \
              frequency TEXT, \
              function TEXT, \
              data_from TEXT, \
              data_to TEXT, \
              data_size INTEGER, \
              PRIMARY KEY(curve_id, table_name));'
    
    DBI::dbWithTransaction(con, {
        DBI::dbExecute(con, script)
    })

    DBI::dbDisconnect(con)

    if (is.null(tz)) {
        tz <- 'CET'
    }

    if (is.null(func)) {
        func <- 'AVERAGE'
    }

    data_to <- FormatEndOfdayIso8601(tz = tz)

    start_date <- as.POSIXct(data_from, origin = "1970-01-01", tz = tz)

    end_date <- suppressMessages(lubridate::with_tz(lubridate::now(tzone = tz)) + lubridate::ddays(1))
    end_date <- lubridate::floor_date(end_date, 'day')
    end_date <- end_date + lubridate::dhours(-1)

    time_index <- seq(from = start_date, to = end_date, by = "hour")
    time_index <- strftime(time_index, "%Y-%m-%dT%H:%M:%S%z")

    for (i in 1:length(time_index)) {

        last <- substr(time_index[i], nchar(time_index[i])-1, nchar(time_index[i]))
        time_index[i] <- stringr::str_replace(time_index[i], '\\d{2}$', as.character(stringr::str_glue(':{last}')))
    }

    data_from <- lubridate::ymd(data_from, tz = 'CET')

    issue_date_from <- suppressMessages(lubridate::floor_date(lubridate::ymd(data_from, tz = tz) + lubridate::ddays(-3), 'day') %>% lubridate::date())
    issue_date_from <- as.character(issue_date_from)
    
    data_from <- FormatDateTimeIso8601(tp = data_from, tz = tz)
    
    issue_date_to <- FormatNextDay(tz = tz)

    for (i in 1:length(cat_names)) {

        freq <- substr(cat_names[i], nchar(cat_names[i]), nchar(cat_names[i]))
        names_vec <- stringi::stri_enc_toutf8(curve_list[[i]])
        
        for (curve_name in names_vec) {
            
            SqlCreateHistoryCatalogueTable(db_path = db_path, 
                                           offset = offset, 
                                           duration = duration, 
                                           data_from = data_from, 
                                           data_to = data_to,
                                           issue_date_from = issue_date_from,
                                           issue_date_to = issue_date_to,
                                           curve_name = curve_name, 
                                           time_index = time_index,
                                           freq = freq, 
                                           func = func, 
                                           tz = tz)
        }
    }

    ws_token <- GetToken()

    con <- DBI::dbConnect(RSQLite::SQLite(), db_path)

    query <- "SELECT name FROM sqlite_master WHERE type='table' ORDER BY name;"
    res <- DBI::dbGetQuery(con, query)

    table_names <- res$name
    table_names <- table_names[table_names !='metadata']
    ws_token <- GetToken()
    for (i in 1:length(cat_names)) {

        names_vec <- stringi::stri_enc_toutf8(curve_list[[i]])
        
        for (curve_name in names_vec) {

            table_name <- SqlGetTableName(curve_name)

            curve <- GetCurve(ws_token = ws_token, curve_name = curve_name)
            
            curve_type <- curve$curve_type
            data_type <- curve$data_type
            curve_id <- curve$id
            curve_unit <- curve$unit
            
            if (!is.null(freq)) {
                frequency <- freq
            } else {
                frequency <- curve$frequency
            }

            if (!is.null(func)) {
                `function` <- func
            } else {
                `function` <- ''
            }

            query <- as.character(stringr::str_glue("SELECT * FROM '{table_name}';"))
            df <- DBI::dbGetQuery(con, query)
            tb <- tibble::as_tibble(df)
            data_times <- tb$times[which(!is.na(tb$values))]
            data_from_table <- min(data_times)
            data_to_table <- max(data_times)
            data_size <- length(data_times)

            com <- as.character(stringr::str_glue("INSERT OR REPLACE INTO 'metadata' \
                                                  (curve_id, table_name, curve_name, curve_type, data_type,\
                                                  unit, frequency, `function`, data_from, data_to, data_size) \
                                                  VALUES({curve_id}, '{table_name}', '{curve_name}', '{curve_type}', '{data_type}', \
                                                  '{curve_unit}', '{frequency}', '{`function`}', \
                                                  '{data_from_table}', '{data_to_table}', {data_size});"))
            DBI::dbWithTransaction(con, {
                DBI::dbExecute(con, com)
            })
        }
    }
    DBI::dbDisconnect(con)

    min_to <- SqlUpdateHistoryMetadataTable(db_path)

    min_to <- suppressMessages(lubridate::ymd_hms(min_to, tz = tz))
    min_to <- strftime(min_to, "%Y-%m-%d %H:%M")

    cat(as.character(stringr::str_glue('\r\n\r\nDB path: [{db_path}]\r\n')))
    cat(as.character(stringr::str_glue('\r\nData to: [{min_to}]\r\n')))

}



SqlCreateHistoryCatalogueTable <- function(db_path, 
                                           offset, 
                                           duration, 
                                           data_from, 
                                           data_to,
                                           issue_date_from,
                                           issue_date_to,
                                           curve_name, 
                                           time_index,
                                           freq = NULL, 
                                           func = NULL, 
                                           tz = NULL) {
    ws_token <- GetToken()                                  

    table_name <- SqlGetTableName(curve_name)

    curve <- GetCurve(ws_token = ws_token, curve_name = curve_name)
    
    curve_type <- curve$curve_type
    curve_id <- curve$id
    
    if (curve_type == 'TIME_SERIES') {
        
        ins <- GetTimeSeries(ws_token = ws_token, 
                             curve_name = curve_name, 
                             freq = freq, 
                             func = func, 
                             tz = tz,
                             data_from = data_from,
                             data_to = data_to)
        
    } else {

        ins <- GetRelativeForecast(ws_token = ws_token, 
                                   offset = offset,
                                   duration = duration,
                                   issue_date_from = issue_date_from,
                                   issue_date_to = issue_date_to,
                                   curve_name = curve_name, 
                                   freq = freq, 
                                   func = func, 
                                   tz = tz,
                                   filter = NULL,
                                   data_from = data_from,
                                   data_to = data_to) 
    }
    
    tb_times <- as.character(ins$points[['times']])
    tb_values <- zoo::na.locf(ins$points[['values']], na.rm = F)

    data_from <- min(ins$points[['times']])
    data_to <- max(ins$points[['times']])
    data_size <- length(ins$points[['times']])

    con <- DBI::dbConnect(RSQLite::SQLite(), db_path)

    com <- as.character(stringr::str_glue("CREATE TABLE IF NOT EXISTS '{table_name}' \
                                           (times TEXT NOT NULL, 'values' REAL, PRIMARY KEY(times));"))

    DBI::dbWithTransaction(con, {
        DBI::dbExecute(con, com)
    })

    for (i in 1:length(time_index)) {

        if (time_index[i] %in% tb_times) {

            idx <- index(tb_times)[which(tb_times == time_index[i])]

            if (is.na(tb_values[idx])) {

                com <- as.character(stringr::str_glue("INSERT OR REPLACE INTO '{table_name}'(times, 'values') \
                                                       VALUES('{time_index[i]}', NULL);"))
            } else {

                com <- as.character(stringr::str_glue("INSERT OR REPLACE INTO '{table_name}'(times, 'values') \
                                                       VALUES('{time_index[i]}', {tb_values[idx]});"))
            }

        } else {

            com <- as.character(stringr::str_glue("INSERT OR REPLACE INTO '{table_name}'(times, 'values') \
                                                   VALUES('{time_index[i]}', NULL);"))           
        }

        DBI::dbWithTransaction(con, {
            DBI::dbExecute(con, com)
        })
    }

    DBI::dbDisconnect(con)


    data_from_d <- suppressMessages(lubridate::ymd_hms(data_from, tz = tz))
    data_from_d <- strftime(data_from_d, "%Y-%m-%d %H:%M")

    data_to_d <- suppressMessages(lubridate::ymd_hms(data_to, tz = tz))
    data_to_d <- strftime(data_to_d, "%Y-%m-%d %H:%M")

    PrintDbOutput(curve_id = curve_id, 
                  table_name = table_name, 
                  data_from = data_from_d, 
                  data_to = data_to_d, 
                  data_size = data_size)
}



SqlUpdateHistoryDb <- function(db_path, offset, duration, tz) {
  
    db_path <- normalizePath(db_path)
    data_from <- SqlGetDbMinTo(db_path)
  
    data_to <- FormatEndOfdayIso8601(tz=tz)

    issue_date_from <- suppressMessages(lubridate::floor_date(lubridate::ymd_hms(data_from, tz = tz) + lubridate::ddays(-3), 'day') %>% lubridate::date())
    issue_date_from <- as.character(issue_date_from)

    issue_date_to <- FormatNextDay(tz = tz)

    start_date <- suppressMessages(lubridate::ymd_hms(data_from, tz = tz))

    end_date <- suppressMessages(lubridate::with_tz(lubridate::now(tzone = tz)) + lubridate::ddays(1))
    end_date <- lubridate::floor_date(end_date, 'day')
    end_date <- end_date + lubridate::dhours(-1)

    time_index <- seq(from = start_date, to = end_date, by = "hour")
    time_index <- strftime(time_index, "%Y-%m-%dT%H:%M:%S%z")
    
    for (i in 1:length(time_index)) {

        last <- substr(time_index[i], nchar(time_index[i])-1, nchar(time_index[i]))
        time_index[i] <- stringr::str_replace(time_index[i], '\\d{2}$', as.character(stringr::str_glue(':{last}')))
    }

    con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
    
    query <- as.character(stringr::str_glue("SELECT curve_name FROM metadata;"))
    res <- DBI::dbGetQuery(con, query)
    curve_names <- res$curve_name

    ws_token <- GetToken()
    
    for (curve_name in curve_names) {
      
        query <- as.character(stringr::str_glue("SELECT * FROM metadata WHERE curve_name = '{curve_name}';"))
        res <- DBI::dbGetQuery(con, query)
        
        curve_id <- res$curve_id
        curve_type <- res$curve_type
        freq <- res$frequency
        func <- res$`function`
        table_name <- res$table_name
        
        if (curve_type == 'TIME_SERIES') {
        
            ins <- GetTimeSeries(ws_token = ws_token, 
                                 curve_name = curve_name, 
                                 freq = freq, 
                                 func = func, 
                                 tz = tz, 
                                 data_from = data_from,
                                 data_to = data_to)
        } else {

            ins <- GetRelativeForecast(ws_token = ws_token, 
                                       offset = offset,
                                       duration = duration,
                                       issue_date_from = issue_date_from,
                                       issue_date_to = issue_date_to,
                                       curve_name = curve_name, 
                                       freq = freq, 
                                       func = func, 
                                       tz = tz,
                                       filter = NULL,
                                       data_from = data_from,
                                       data_to = data_to) 
        }
        
        data_min <- min(ins$points[['times']])
        data_max <- max(ins$points[['times']])
        data_size <- length(ins$points[['times']])
        
        tb_times <- as.character(ins$points[['times']])
        tb_values <- zoo::na.locf(ins$points[['values']], na.rm = F)
        
        for (i in 1:length(time_index)) {
    
            if (time_index[i] %in% tb_times) {

                idx <- index(tb_times)[which(tb_times == time_index[i])]

                if (is.na(tb_values[idx])) {

                    com <- as.character(stringr::str_glue("INSERT OR REPLACE INTO '{table_name}'(times, 'values') \
                                                          VALUES('{time_index[i]}', NULL);"))
                } else {

                    com <- as.character(stringr::str_glue("INSERT OR REPLACE INTO '{table_name}'(times, 'values') \
                                                          VALUES('{time_index[i]}', {tb_values[idx]});"))
                }

            } else {

                com <- as.character(stringr::str_glue("INSERT OR REPLACE INTO '{table_name}'(times, 'values') \
                                                      VALUES('{time_index[i]}', NULL);"))           
            }

            DBI::dbWithTransaction(con, {
                DBI::dbExecute(con, com)
            })
        }
        
        data_from_d <- suppressMessages(lubridate::ymd_hms(data_min, tz = tz))
        data_from_d <- strftime(data_from_d, "%Y-%m-%d %H:%M")
        
        data_to_d <- suppressMessages(lubridate::ymd_hms(data_max, tz = tz))
        data_to_d <- strftime(data_to_d, "%Y-%m-%d %H:%M")
        
        PrintDbOutput(curve_id = curve_id, 
                      table_name = table_name, 
                      data_from = data_from_d, 
                      data_to = data_to_d, 
                      data_size = data_size)  
    }
  
    DBI::dbDisconnect(con)
    
    
    con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
    query <- as.character(stringr::str_glue("SELECT table_name FROM metadata;"))
    res <- DBI::dbGetQuery(con, query)
    table_names <- res$table_name
    
    for (table_name in table_names) {

        com <- as.character(stringr::str_glue("DELETE FROM '{table_name}' WHERE times > '{data_to}';"))
        DBI::dbWithTransaction(con, {
            DBI::dbExecute(con, com)
        })  
        
        query <- as.character(stringr::str_glue("SELECT * FROM '{table_name}';"))
        res <- DBI::dbGetQuery(con, query)
        tb <- tibble::as_tibble(res)
        data_times <- tb$times[which(!is.na(tb$values))]

        data_size <- length(data_times)
        data_from <- min(data_times)
        data_to <- max(data_times)
        
        query <- as.character(stringr::str_glue("SELECT * FROM  metadata WHERE table_name = '{table_name}';"))
        res <- DBI::dbGetQuery(con, query)
        
        curve_id <- res$curve_id
        curve_name <- res$curve_name
        curve_type <- res$curve_type
        data_type <- res$data_type
        unit <- res$unit
        frequency <- res$frequency
        `function` <- res$`function`
    
        com <- as.character(stringr::str_glue("INSERT OR REPLACE INTO 'metadata' \
                                              (curve_id, table_name, curve_name, curve_type, data_type,\
                                              unit, frequency, `function`, data_from, data_to, data_size) \
                                              VALUES({curve_id}, '{table_name}', '{curve_name}', '{curve_type}', '{data_type}', \
                                              '{unit}', '{frequency}', '{`function`}', \
                                              '{data_from}', '{data_to}', {data_size});"))
        DBI::dbWithTransaction(con, {
            DBI::dbExecute(con, com)
        })
        
    }
  
    query <- as.character(stringr::str_glue("SELECT MAX(data_to) FROM  metadata;"))
    res <- DBI::dbGetQuery(con, query)
    max_to <- res$`MAX(data_to)`
    
    DBI::dbDisconnect(con)

    max_to <- suppressMessages(lubridate::ymd_hms(max_to, tz = tz))
    max_to <- strftime(max_to, "%Y-%m-%d %H:%M")
    cat(as.character(stringr::str_glue('\r\n\r\nDB path: [{db_path}]\r\n')))
    cat(as.character(stringr::str_glue('\r\nData to: [{max_to}]\r\n')))
}



SqlUpdateHistoryMetadataTable <- function(db_path) {
  
    con <- DBI::dbConnect(RSQLite::SQLite(), db_path)

    query <- as.character(stringr::str_glue("SELECT MAX(data_to) FROM metadata;"))
    res <- DBI::dbGetQuery(con, query)
    max_to <- res$`MAX(data_to)`

    query <- as.character(stringr::str_glue("SELECT table_name FROM metadata;"))
    res <- DBI::dbGetQuery(con, query)
    table_names <- res$table_name

    for (table_name in table_names) {
            
        query <- as.character(stringr::str_glue("SELECT * FROM '{table_name}';"))
        res <- DBI::dbGetQuery(con, query)
        tb <- tibble::as_tibble(res)
        data_times <- tb$times[which(!is.na(tb$values))]

        data_size <- length(data_times)
        data_from <- min(data_times)
        data_to <- max(data_times)
        
        query <- as.character(stringr::str_glue("SELECT * FROM metadata WHERE table_name = '{table_name}';"))
        res <- DBI::dbGetQuery(con, query)
        
        curve_id <- res$curve_id
        curve_name <- res$curve_name
        curve_type <- res$curve_type
        data_type <- res$data_type
        unit <- res$unit
        frequency <- res$frequency
        `function` <- res$`function`
    
        com <- as.character(stringr::str_glue("INSERT OR REPLACE INTO 'metadata' \
                                              (curve_id, table_name, curve_name, curve_type, data_type,\
                                              unit, frequency, `function`, data_from, data_to, data_size) \
                                              VALUES({curve_id}, '{table_name}', '{curve_name}', '{curve_type}', '{data_type}', \
                                              '{unit}', '{frequency}', '{`function`}', \
                                              '{data_from}', '{data_to}', {data_size});"))
        DBI::dbWithTransaction(con, {
            DBI::dbExecute(con, com)
        })

    }
    DBI::dbDisconnect(con)
    max_to
}


SqlDropCurveTable <- function (db_path, 
                               curve_id = NULL, 
                               curve_name = NULL, 
                               table_name = NULL) {
  
    db_path <- normalizePath(db_path)
    
    if (!is.null(curve_id)) {

        query <- as.character(stringr::str_glue("SELECT curve_name FROM 'metadata' WHERE curve_id = {curve_id}"))

        res <- tryCatch(
            DBI::dbGetQuery(con, query),
            error = function() {
                stop(as.character(stringr::str_glue("curve_id [{curve_id}] does not exist in [{db_path}]")))
            }
        )
        
        
        curve_name <- res$curve_name
        table_name <- SqlGetTableName(curve_name)

    } else if (!is.null(curve_name)) {

        curve_name <- trimws(curve_name)
        curve_name <- stringi::stri_enc_toutf8(curve_name)
        table_name <- SqlGetTableName(curve_name)
    }

    
    con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
    
    RSQLite::dbSendStatement(con, "PRAGMA foreign_keys = OFF;")
    
    comm <- as.character(stringr::str_glue("DROP TABLE '{table_name}';"))
    DBI::dbWithTransaction(con, {
        DBI::dbExecute(con, comm)
    })
    
    comm <- as.character(stringr::str_glue("DELETE FROM 'metadata' WHERE table_name = '{table_name}'"))
    DBI::dbWithTransaction(con, {
        DBI::dbExecute(con, comm)
    })
    
    cat(as.character(stringr::str_glue("DELETED [{table_name}]\r\n")))
    cat(c('',''), sep='\r\n')

    RSQLite::dbSendStatement(con, "PRAGMA foreign_keys = ON;")
    
    DBI::dbDisconnect(con)
    
    con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
    RSQLite::dbSendStatement(con, "VACUUM;")
    DBI::dbDisconnect(con)



}



SqlDropCluster <- function (db_path, json_file, cluster_name) {

    json_path <- normalizePath(json_file)
    tb_list <- JsonReadCatalogue(json_file)
    curve_names <- tb_list[[cluster_name]]$name
  
    db_path <- normalizePath(db_path)
    con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
    RSQLite::dbSendStatement(con, "PRAGMA foreign_keys = OFF;")

    for (curve_name in curve_names) {

        query <- as.character(stringr::str_glue("SELECT table_name FROM metadata WHERE curve_name = '{curve_name}';"))
        res <- tryCatch(DBI::dbGetQuery(con, query),
                        error = function(cond) {
                            print(as.character(stringr::str_glue("Error while processing [{curve_name}]")))
                            DBI::dbDisconnect(con)
                            return("Process interrupted")
                        })

        table_name <- res$table_name

        comm <- as.character(stringr::str_glue("DROP TABLE '{table_name}';"))
        DBI::dbWithTransaction(con, {
            DBI::dbExecute(con, comm)
        })
        
        comm <- as.character(stringr::str_glue("DELETE FROM 'metadata' WHERE table_name = '{table_name}'"))
        DBI::dbWithTransaction(con, {
            DBI::dbExecute(con, comm)
        })
        
        cat(as.character(stringr::str_glue("DELETED [{table_name}]\r\n")))
        cat(c('',''), sep='\r\n')
    }
    
    RSQLite::dbSendStatement(con, "PRAGMA foreign_keys = ON;")
    
    DBI::dbDisconnect(con)
    
    con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
    RSQLite::dbSendStatement(con, "VACUUM;")
    DBI::dbDisconnect(con)


}



SqlGetTables <- function(db_path) {

    db_path <- normalizePath(db_path)
    con <- DBI::dbConnect(RSQLite::SQLite(), db_path)

    query <- as.character(stringr::str_glue("SELECT name FROM sqlite_master WHERE type='table' ORDER BY name;"))
    res <- DBI::dbGetQuery(con, query)
    tb <- tibble::as_tibble(res)

    count_vec <- integer()
    
    for (i in 1:length(res$name)) {
        query <- as.character(stringr::str_glue("SELECT COUNT(*) FROM {res$name[i]};"))
        res2 <- DBI::dbGetQuery(con, query)
        count_vec[i] <- as.integer(res2$`COUNT(*)`)
    }

    DBI::dbDisconnect(con)

    tb['count'] <- count_vec
    
    tb
}

SqlGetTableColumnNames <- function(db_path, table_name){

    db_path <- normalizePath(db_path)

    con <- DBI::dbConnect(RSQLite::SQLite(), db_path)

    column_names <- DBI::dbListFields(con, table_name)

    DBI::dbDisconnect(con)

    column_names
}


SqlGetTableName <- function(curve_name) {

    curve_name <- trimws(curve_name)
    curve_name <- stringi::stri_enc_toutf8(curve_name)

    table_name <- curve_name %>% 
    stringr::str_replace_all(' ', '_') %>%
    stringr::str_replace_all('/', '') %>%
    stringr::str_replace_all('>', '') %>%
    stringr::str_replace_all('-', '')

    table_name <- stringi::stri_enc_toutf8(table_name)
}




SqlGetMetadataTable <- function (db_path, tz = NULL) {

    db_path <- normalizePath(db_path)

    if (!is.null(tz)) {
        tz = 'CET'
    }
    
    con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
    query <- as.character(stringr::str_glue("SELECT * FROM 'metadata';"))
    res <- DBI::dbGetQuery(con, query)
    DBI::dbDisconnect(con)
    tb <- tibble::as_tibble(res)
    
    tb
}


SqlGetCurveMetadata <- function (db_path, curve_id = NULL, curve_name = NULL, table_name = NULL) {

    db_path <- normalizePath(db_path)
    con <- DBI::dbConnect(RSQLite::SQLite(), db_path)

    if (!is.null(curve_id)) {
        query <- as.character(stringr::str_glue("SELECT curve_name FROM 'metadata' WHERE curve_id = {curve_id}"))

    } else if (!is.null(curve_name)) {
        curve_name <- trimws(curve_name)
        curve_name <- stringi::stri_enc_toutf8(curve_name)
        query <- as.character(stringr::str_glue("SELECT * FROM 'metadata' WHERE curve_name = '{curve_name}';"))

    } else if (!is.null(table_name)) {
        table_name <- stringi::stri_enc_toutf8(table_name)
        query <- as.character(stringr::str_glue("SELECT * FROM 'metadata' WHERE table_name = '{table_name}';"))
    }
    
    res <- DBI::dbGetQuery(con, query)
    DBI::dbDisconnect(con)
    tb <- tibble::as_tibble(res)

    return(tb)
}


SqlGetCurveTableZoo <- function(db_path, curve_id = NULL, curve_name = NULL, table_name = NULL, tz = NULL) {
  
    db_path <- normalizePath(db_path)

    con <- DBI::dbConnect(RSQLite::SQLite(), db_path)

    if (!is.null(curve_id)) {

        query <- as.character(stringr::str_glue("SELECT table_name FROM 'metadata' WHERE curve_id = {curve_id}"))
        res <- DBI::dbGetQuery(con, query)
        table_name <- res$table_name
        
    } else if (!is.null(curve_name)) {
        curve_name <- trimws(curve_name)
        curve_name <- stringi::stri_enc_toutf8(curve_name)
        query <- as.character(stringr::str_glue("SELECT table_name FROM 'metadata' WHERE curve_name = '{curve_name}'"))
        res <- DBI::dbGetQuery(con, query)
        table_name <- res$table_name

    } else if (!is.null(table_name)) {
        table_name <- trimws(table_name)
        table_name <- stringi::stri_enc_toutf8(table_name) 
    }

    if (is.null(tz)) {
        tz <- 'CET'
    }

    query <- as.character(stringr::str_glue("SELECT * FROM '{table_name}';"))
    df <- DBI::dbGetQuery(con, query)
    DBI::dbDisconnect(con)

    tb <- tibble::as_tibble(df)
    tb <- tb %>% dplyr::arrange(times)
    tb
}


SqlGetCurveTable <- function(db_path, 
                             curve_id = NULL, 
                             curve_name = NULL, 
                             table_name = NULL, 
                             tz = NULL) {
  
    db_path <- normalizePath(db_path)

    con <- DBI::dbConnect(RSQLite::SQLite(), db_path)

    if (!is.null(curve_id)) {

        query <- as.character(stringr::str_glue("SELECT table_name FROM 'metadata' WHERE curve_id = {curve_id}"))
        res <- DBI::dbGetQuery(con, query)
        table_name <- res$table_name
        
    } else if (!is.null(curve_name)) {

        curve_name <- trimws(curve_name)
        curve_name <- stringi::stri_enc_toutf8(curve_name)
        query <- as.character(stringr::str_glue("SELECT table_name FROM 'metadata' WHERE curve_name = '{curve_name}'"))
        res <- DBI::dbGetQuery(con, query)
        table_name <- res$table_name

    } else if (!is.null(table_name)) {

        table_name <- trimws(table_name)
        table_name <- stringi::stri_enc_toutf8(table_name) 
    }

    if (is.null(tz)) {
        tz <- 'CET'
    }

    query <- as.character(stringr::str_glue("SELECT * FROM '{table_name}';"))
    df <- DBI::dbGetQuery(con, query)
    DBI::dbDisconnect(con)

    tb <- tibble::as_tibble(df)
    tb <- tb %>% dplyr::arrange(times)
    tb$times <- suppressMessages(lubridate::ymd_hms(tb$times, tz = tz))
    tb
}


PlotCurve <- function(tb, tz = NULL) {
    
    if (is.null(tz)) {
        tz <- 'CET'
    }
    
    tb$points <- tb$points %>% dplyr::arrange(times)
    tb$points$times <- suppressMessages(lubridate::ymd_hms(tb$points$times, tz = tz))
    
    title <- as.character(stringr::str_glue("<b>{tb$metadata$name}</b>"))

    m <- list(
        l = 80,
        r = 80,
        b = 30,
        t = 60,
        pad = 0
    )
    fig <- plotly::plot_ly(data = tb$points, x = ~tb$points$times, y = ~tb$points$values, type = "scatter", mode = "lines", line = list(color = 'crimson', width = 1.3), width = 809, height = 500) %>% 
        layout(title = list(text = title,
               x = 0.5, 
               y = 1, 
               xanchor = "middle", 
               yanchor = "top",
               font = list(family = 'Helvetica', size = 18, color = 'navy'),
               pad = list(b = 5, t = 5, l = 5, r = 5))) %>%
        add_annotations(text = as.character(stringr::str_glue('[{tb$metadata$unit}]')),
                        x = 0.5,
                        y = 1.1,
                        yref = "paper",
                        xref = "paper",
                        xanchor = "middle",
                        yanchor = "top",
                        showarrow = FALSE,
                        font = list(size = 15, color = 'navy')) %>%
        layout(yaxis = list(title = '',
                            showgrid = FALSE,
                            gridcolor = 'darkgray',
                            automargin = TRUE,
                            range = list(min(tb$points$values), max(tb$points$values)),
                            showline = TRUE,
                            linewidth = 1.3,
                            linecolor = 'navy',
                            mirror = TRUE),

                xaxis = list(title = '',
                            showgrid = TRUE,
                            showline = TRUE,
                            linecolor = 'navy',
                            range = list(min(tb$points$times), max(tb$points$times)),
                            linewidth = 1.3,
                            mirror = TRUE,
                            type = 'date',
                            tickformat = "%d %b-%y<br>%H:%M"),
                margin = m) %>%
        layout(hovermode = 'x unified') %>%

        plotly::config(displayModeBar = FALSE, showLink = FALSE)

    return(fig)
}




SqlPlotCurve <- function(db_path, curve_id = NULL, curve_name = NULL, table_name = NULL, tz = NULL) {
    
    db_path <- normalizePath(db_path)
    con <- DBI::dbConnect(RSQLite::SQLite(), db_path) 
    
    if (!is.null(curve_id)) {

        query <- as.character(stringr::str_glue("SELECT * FROM 'metadata' WHERE curve_id = {curve_id}"))
        
    } else if (!is.null(curve_name)) {
        curve_name <- trimws(curve_name)
        curve_name <- stringi::stri_enc_toutf8(curve_name)
        query <- as.character(stringr::str_glue("SELECT * FROM 'metadata' WHERE curve_name = '{curve_name}'"))

    } else if (!is.null(table_name)) {
        table_name <- trimws(table_name)
        table_name <- stringi::stri_enc_toutf8(table_name)
        query <- as.character(stringr::str_glue("SELECT * FROM 'metadata' WHERE table_name = '{table_name}'"))
    }
    
    res <- DBI::dbGetQuery(con, query)
    meta <- tibble::as_tibble(res)
    
    table_name <- meta$table_name
    
    query <- as.character(stringr::str_glue("SELECT * FROM '{table_name}';"))
    df <- DBI::dbGetQuery(con, query)
    DBI::dbDisconnect(con)
    
    if (is.null(tz)) {
        tz <- 'CET'
    }
    
    tb <- tibble::as_tibble(df)
    tb <- tb %>% dplyr::arrange(times)
    tb$times <- suppressMessages(lubridate::ymd_hms(tb$times, tz = tz))
    
    title <- as.character(stringr::str_glue("<b>{meta$curve_name}</b>"))

    m <- list(
        l = 80,
        r = 80,
        b = 30,
        t = 60,
        pad = 0
    )
    fig <- plotly::plot_ly(data = tb, x = ~tb$times, y = ~tb$values, type = "scatter", mode = "lines", line = list(color = 'crimson', width = 1.3), width = 809, height = 500) %>% 
        layout(title = list(text = title,
               x = 0.5, 
               y = 1, 
               xanchor = "middle", 
               yanchor = "top",
               font = list(size = 18, color = 'navy'),
               pad = list(b = 5, t = 5, l = 5, r = 5))) %>%
        add_annotations(text = as.character(stringr::str_glue('[{meta$unit}]')),
                        x = 0.5,
                        y = 1.1,
                        yref = "paper",
                        xref = "paper",
                        xanchor = "middle",
                        yanchor = "top",
                        showarrow = FALSE,
                        font = list(size = 15, color = 'navy')) %>%
        layout(yaxis = list(title = '',
                            showgrid = FALSE,
                            gridcolor = 'darkgray',
                            automargin = TRUE,
                            range = list(min(tb$values), max(tb$values)),
                            showline = TRUE,
                            linewidth = 1.3,
                            linecolor = 'navy',
                            mirror = TRUE),

                xaxis = list(title = '',
                            showgrid = TRUE,
                            showline = TRUE,
                            linecolor = 'navy',
                            range = list(min(tb$times), max(tb$times)),
                            linewidth = 1.3,
                            mirror = TRUE,
                            type = 'date',
                            tickformat = "%d %b-%y<br>%H:%M"),
                margin = m) %>%
        layout(hovermode = 'x unified') %>%

        plotly::config(displayModeBar = FALSE, showLink = FALSE)

        
    return(fig)
}


SqlPlotCluster <- function(db_path, json_path, cluster_name, tz = NULL) {
    
    if (is.null(tz)) {
        tz <- 'CET'
    }

    db_type <- ''
    if (substr(json_path, nchar(json_path)-7, nchar(json_path)-5) == 'FOR') {
        db_type <- 'FOR'
    } else if (substr(json_path, nchar(json_path)-7, nchar(json_path)-5) == 'HIS') {
        db_type <- 'HIS'
    } else {
        print(as.character(stringr::str_glue("{json_path} is not valid. Must be named [CC_FOR.json] or [CC_HIS.json]")))
        return(NA)
    }

    m <- list(
        l = 80,
        r = 80,
        b = 30,
        t = 60,
        pad = 0
    )

    tb_list <- JsonReadCatalogue(json_file = json_path)


    for (name in names(tb_list)) {
        if (name == cluster_name) {
            curve_names <- tb_list[[cluster_name]]$name
        }
    }

    if (length(curve_names) == 1) {
        x_title <- 0.5
    } else {
        x_title <- 0.47
    }

    unit_vec <- c()
    short_names <- c()

    for (curve_name in curve_names) {
        s <- strsplit(x = curve_name, split = " ")
        s <- s[[1]]

        unit_vec <- append(unit_vec, s[length(s)-3])

        if (s[1] == "con") {
            if (substr(curve_name, nchar(curve_name), nchar(curve_name)) != 'n') {
                lim <- length(s)-4
                short_names <- append(short_names, paste(s[3:lim], collapse = ' '))
            } else {
                short_names <- append(short_names, 'normal')
            }

        } else if (s[1] == "exc") {
            short_names <- append(short_names, s[2])

        } else if (s[1] == "cap") {

            if (stringr::str_detect(curve_name, '\\bnuc\\b') == TRUE) {

                short_names <- append(short_names, s[4])
            } else {
                short_names <- append(short_names, s[5])
            }
        } else if (stringr::str_detect(curve_name, '^inf\\b') == TRUE) {

            short_names <- append(short_names, s[1])

        } else if (stringr::str_detect(curve_name, '\\bhydro\\b') == TRUE | stringr::str_detect(curve_name, '\\bbiomass\\b') == TRUE) {
            short_names <- append(short_names, s[4])

        } else {

            if (substr(curve_name, nchar(curve_name), nchar(curve_name)) != 'n') {
                lim <- length(s)-4
                short_names <- append(short_names, paste(s[4:lim], collapse = ' '))
            } else {
                short_names <- append(short_names, 'normal')
            }
        }
    }

    unique_units <- unique(unit_vec)

    db_path <- normalizePath(db_path)
    con <- DBI::dbConnect(RSQLite::SQLite(), db_path) 

    fig <- plotly::plot_ly(width = 809, height = 500)

    title <- as.character(stringr::str_glue("<b>{cluster_name}</b>"))

    if (length(unique_units) == 1) {

        unit <- as.character(stringr::str_glue("[{unit_vec[1]}]"))

        max_x <- c()
        min_x <- c()

        max_y <- c()
        min_y <- c()

        i <- 1
        for (curve_name in curve_names) {

            width <- 1.3
            opacity <- 1

            if (substr(curve_name, nchar(curve_name), nchar(curve_name)) == 'n') {
                color <- 'black'
            } else if (grepl('monthly', curve_name, fixed = TRUE) == TRUE) {
                color <- 'forestgreen'
                width <- 2
            } else if (stringr::str_detect(curve_name, '\\bgfs00\\b') == TRUE) {
                color <- 'red'
                width <- 2
            } else if (stringr::str_detect(curve_name, '\\bec00\\b') == TRUE) {
                color <- 'blue'
                width <- 2
            } else if (stringr::str_detect(curve_name, 'gfs\\d{2}') == TRUE) {
                color <- 'crimson'
                opacity <- .5
            } else if (stringr::str_detect(curve_name, 'ec\\d{2}') == TRUE) {
                color <- 'royalblue'
                opacity <- .5
            } 

            table_name <- SqlGetTableName(curve_name = curve_name)
            
            query <- as.character(stringr::str_glue("SELECT * FROM '{table_name}';"))
            df <- DBI::dbGetQuery(con, query)
            
            tb <- tibble::as_tibble(df)
            tb <- tb %>% dplyr::arrange(times)

            times <- suppressMessages(lubridate::ymd_hms(tb$times, tz = tz))
            values <- zoo::na.locf(tb$values, na.rm = F)
            values <- round(x = values, digits = 2)

            if (db_type == 'FOR') {
                if (stringr::str_detect(curve_name, '\\bec\\d{2}ens\\b') == FALSE & grepl('monthly', curve_name, fixed = TRUE) == FALSE & substr(curve_name, nchar(curve_name), nchar(curve_name)) != 'n') {

                    min_x <- append(min_x, min(times))

                    if (stringr::str_detect(curve_name, 'gfs\\d{2}') == TRUE | stringr::str_detect(curve_name, 'ec\\d{2}') == TRUE) {
                        max_x <- append(max_x, max(times))
                    } else {
                        max_time <- min(min_x) + lubridate::ddays(5)
                        max_x <- append(max_x, max_time)                    
                    }

                    max_y <- append(max_y, max(values))
                    min_y <- append(min_y, min(values))
                }
            } else {
                min_date <- suppressMessages(lubridate::floor_date(lubridate::with_tz(lubridate::now(tzone = tz)+ lubridate::ddays(-30), tzone = tz), 'day'))
                min_x <- append(min_x, min_date)

                max_x <- append(max_x, max(times))

                min_y <- append(min_y, min(values))
                max_y <- append(max_y, max(values))
            }


            if (stringr::str_detect(curve_name, 'gfs\\d{2}') == TRUE | stringr::str_detect(curve_name, 'ec\\d{2}') == TRUE) {
                fig <- add_trace(fig, x = times, y = values, type = "scatter", name = short_names[i], mode = "lines", line = list(width = width, color = color), opacity = opacity)
                
            } else {
                fig <- add_trace(fig, x = times, y = values, type = "scatter", name = short_names[i], mode = "lines", line = list(width = width, color = color), opacity = opacity)
            }
            
            i <- i + 1
        }

    } else {
        unit <- names(summary(as.factor(unit_vec)) %>% sort(decreasing = T))[1]

        max_x <- c()
        min_x <- c()

        max_y <- c()
        min_y <- c()

        i <- 1
        for (curve_name in curve_names) {

            width <- 1.3

            if (substr(curve_name, nchar(curve_name), nchar(curve_name)) == 'n') {
                color <- 'black'
            } else if (grepl('monthly', curve_name, fixed = TRUE) == TRUE) {
                color <- 'forestgreen'
                width <- 2
            } else if (stringr::str_detect(curve_name, '\\bgfs00\\b') == TRUE) {
                color <- 'red'
                width <- 2
            } else if (stringr::str_detect(curve_name, '\\bec00\\b') == TRUE) {
                color <- 'navy'
                width <- 2
            } else if (stringr::str_detect(curve_name, 'gfs\\d{2}') == TRUE) {
                color <- 'crimson'
            } else if (stringr::str_detect(curve_name, 'ec\\d{2}') == TRUE) {
                color <- 'royalblue'
            } 

            table_name <- SqlGetTableName(curve_name = curve_name)
            
            query <- as.character(stringr::str_glue("SELECT * FROM '{table_name}';"))
            df <- DBI::dbGetQuery(con, query)
            
            tb <- tibble::as_tibble(df)
            tb <- tb %>% dplyr::arrange(times)

            times <- suppressMessages(lubridate::ymd_hms(tb$times, tz = tz))
            values <- zoo::na.locf(tb$values, na.rm = F)
            values <- round(x = values, digits = 2)

            if (unit_vec[i] != unit) {
                if (unit_vec[i] == 'gwh') {
                    values <- values * 1000
                }
            }

            if (stringr::str_detect(curve_name, '\\bec\\d{2}ens\\b') == FALSE & grepl('monthly', curve_name, fixed = TRUE) == FALSE & substr(curve_name, nchar(curve_name), nchar(curve_name)) != 'n') {

                min_x <- append(min_x, min(times))

                if (stringr::str_detect(curve_name, 'gfs\\d{2}') == TRUE | stringr::str_detect(curve_name, 'ec\\d{2}') == TRUE) {
                    max_x <- append(max_x, max(times))
                } else {
                    max_time <- min(min_x) + lubridate::ddays(15)
                    max_x <- append(max_x, max_time)                    
                }

                max_y <- append(max_y, max(values))
                min_y <- append(min_y, min(values))
            }

            if (stringr::str_detect(curve_name, 'gfs\\d{2}') == TRUE | stringr::str_detect(curve_name, 'ec\\d{2}') == TRUE) {
                fig <- add_trace(fig, x = times, y = values, type = "scatter", name = short_names[i], mode = "lines", line = list(width = width, color = color))
                
            } else {
                fig <- add_trace(fig, x = times, y = values, type = "scatter", name = short_names[i], mode = "lines", line = list(width = width), colors = "Dark2")
            }
            
            i <- i + 1
        }
        unit <- as.character(stringr::str_glue("[{unit}]"))
    }

    DBI::dbDisconnect(con)

    fig <- fig %>%  
    
        layout(title = list(text = title, 
                           x = x_title, 
                           y = 1, 
                           xanchor = "middle", 
                           yanchor = "top",
                           font = list(size = 18, color = 'navy'),
                           pad = list(b = 5, t = 5, l = 5, r = 5))) %>%

        add_annotations(text = unit,
                        x = 0.5,
                        y = 1.1,
                        yref = "paper",
                        xref = "paper",
                        xanchor = "middle",
                        yanchor = "top",
                        showarrow = FALSE,
                        font = list(size = 15, color = 'navy')) %>%

        layout(yaxis = list(title = '',
                            showgrid = FALSE,
                            gridcolor = 'darkgray',
                            automargin = TRUE,
                            range = list(min(min_y), max(max_y)),
                            showline = TRUE,
                            linewidth = 1.3,
                            linecolor = 'navy',
                            mirror = TRUE,
                            hoverformat = '.2f'),

                xaxis = list(title = '',
                            showgrid = TRUE,
                            showline = TRUE,
                            linecolor = 'navy',
                            range = list(min(min_x), max(max_x)),
                            linewidth = 1.3,
                            mirror = TRUE,
                            type = 'date',
                            tickformat = "%d %b-%y<br>%H:%M"),

                margin = m) %>%

        layout(hovermode = 'x unified') %>%

        plotly::config(displayModeBar = FALSE, showLink = FALSE)

        
    return(fig)
}


PlotEnsembleCurve <- function(tagins, tz = NULL) {
    
    if (is.null(tz)) {
        tz <- 'CET'
    }

    m <- list(
        l = 80,
        r = 80,
        b = 30,
        t = 60,
        pad = 0
    )

    title <- as.character(stringr::str_glue("<b>{tagins$metadata$Avg$name}</b>"))
    unit <- as.character(stringr::str_glue('[{tagins$metadata$Avg$unit}]'))
    
    fig <- plotly::plot_ly(width = 809, height = 500)

    max_x <- c()
    min_x <- c()

    max_y <- c()
    min_y <- c()

    for (i in 2:length(tagins$points)) {

        if (i < 11) {
            name <- as.character(stringr::str_glue('run0{i-1}'))
        } else {
            name <- as.character(stringr::str_glue('run{i-1}'))
        }
        
        times <- suppressMessages(lubridate::ymd_hms(tagins$points[[i]]$times, tz = tz))
        values <- zoo::na.locf(tagins$points[[i]]$values, na.rm = F)

        max_x <- append(max_x, max(times))
        min_x <- append(min_x, min(times))

        max_y <- append(max_y, max(values))
        min_y <- append(min_y, min(values))

        fig <- add_trace(fig, x = times, y = values, type = "scatter", name = name, mode = "lines", line = list(color = 'royalblue', width = .7), opacity = .3)
    }

    avg_times <- suppressMessages(lubridate::ymd_hms(tagins$points$Avg$times, tz = tz))
    avg_values <- zoo::na.locf(tagins$points$Avg$values, na.rm = F)

    fig <- add_trace(fig, x = ~avg_times, y = ~avg_values, type = "scatter", name = 'Avg', mode = "lines", line = list(color = 'crimson', width = 1.6))

    fig <- fig %>%  layout(title = list(text = title, 
                           x = 0.5, 
                           y = 1, 
                           xanchor = "middle", 
                           yanchor = "top",
                           font = list(size = 18, color = 'navy'),
                           pad = list(b = 5, t = 5, l = 5, r = 5))) %>%
        add_annotations(text = unit,
                        x = 0.5,
                        y = 1.1,
                        yref = "paper",
                        xref = "paper",
                        xanchor = "middle",
                        yanchor = "top",
                        showarrow = FALSE,
                        font = list(size = 15, color = 'navy')) %>%
        layout(yaxis = list(title = '',
                            showgrid = FALSE,
                            gridcolor = 'darkgray',
                            automargin = TRUE,
                            range = list(min(min_y), max(max_y)),
                            showline = TRUE,
                            linewidth = 1.3,
                            linecolor = 'navy',
                            mirror = TRUE),

                xaxis = list(title = '',
                            showgrid = TRUE,
                            showline = TRUE,
                            linecolor = 'navy',
                            range = list(min(min_x), max(max_x)),
                            linewidth = 1.3,
                            mirror = TRUE,
                            type = 'date',
                            tickformat = "%d %b-%y<br>%H:%M"),
                margin = m) %>%
        layout(hovermode = 'closest') %>%

        plotly::config(displayModeBar = FALSE, showLink = FALSE)

    return(fig)
}


SqlGetForHisCurveMerge <- function(db_for, 
                                   db_his, 
                                   curve_id = NULL, 
                                   curve_name = NULL, 
                                   table_name = NULL, 
                                   tz = NULL) {
    
    db_for <- normalizePath(db_for)
    
    con <- DBI::dbConnect(RSQLite::SQLite(), db_for)
    
    if (!is.null(curve_id)) {
        query <- as.character(stringr::str_glue("SELECT table_name, curve_name, unit FROM 'metadata' WHERE curve_id = {curve_id}"))
        res <- DBI::dbGetQuery(con, query)
        if (length(rownames(res)) == 0) {
            print(as.character(stringr::str_glue('curve_id [{curve_id}] does not exist in [{db_for}]')))
            DBI::dbDisconnect(con)
            return(NA)
        }

    } else if (!is.null(curve_name)) {
        curve_name <- trimws(curve_name)
        curve_name <- stringi::stri_enc_toutf8(curve_name)
        query <- as.character(stringr::str_glue("SELECT table_name, curve_name, unit FROM 'metadata' WHERE curve_name = '{curve_name}'"))
        res <- DBI::dbGetQuery(con, query)
        if (length(rownames(res)) == 0) {
            print(as.character(stringr::str_glue('Curve name [{curve_name}] does not exist in [{db_for}]')))
            DBI::dbDisconnect(con)
            return(NA)
        }

    } else if (!is.null(table_name)) {
        table_name <- trimws(table_name)
        table_name <- stringi::stri_enc_toutf8(table_name)
        query <- as.character(stringr::str_glue("SELECT table_name, curve_name, unit FROM 'metadata' WHERE table_name = '{table_name}'"))
        res <- DBI::dbGetQuery(con, query)
        if (length(rownames(res)) == 0) {
            print(as.character(stringr::str_glue('table_name [{table_name}] does not exist in [{db_for}]')))
            DBI::dbDisconnect(con)
            return(NA)
        }
    }

    table_name <- res$table_name
    for_name <- res$curve_name
    unit <- res$unit

    query <- as.character(stringr::str_glue("SELECT * FROM '{table_name}' ORDER BY times ASC;"))
    df <- DBI::dbGetQuery(con, query)
    DBI::dbDisconnect(con)
    
    tb_for <- tibble::as_tibble(df)
    data_to <- max(tb_for$times)

    limit <- min(tb_for$times)

    tb_for$times <- suppressMessages(lubridate::ymd_hms(tb_for$times, tz = tz))
    tb_for$values <- zoo::na.locf(tb_for$values, na.rm = F)
    
    db_his <- normalizePath(db_his)
    con <- DBI::dbConnect(RSQLite::SQLite(), db_his)

    if (substr(for_name, 1, 3) == 'exc') {

        his_name <- substr(for_name, 1, nchar(for_name)-15)
        his_name <- paste(his_name, 'mw cet h a', collapse =' ')

    } else {
        his_name <- for_name
    }

    query <- as.character(stringr::str_glue("SELECT table_name FROM metadata WHERE curve_name = '{his_name}'"))
    res <- DBI::dbGetQuery(con, query)

    if (length(rownames(res)) == 0) {
        print(as.character(stringr::str_glue('curve_name [{his_name}] does not exist in [{db_his}]')))
        DBI::dbDisconnect(con)
        return(NA)
    }

    table_name <- res$table_name

    query <- as.character(stringr::str_glue("SELECT * FROM '{table_name}' ORDER BY times ASC;"))
    df <- DBI::dbGetQuery(con, query)
    DBI::dbDisconnect(con)
    
    tb_his <- tibble::as_tibble(df)

    data_from <- min(tb_his$times)

    tb_his$times <- suppressMessages(lubridate::ymd_hms(tb_his$times, tz = tz))

    tb_his$values <- zoo::na.locf(tb_his$values, na.rm = F)
    
    merge_tb <- dplyr::bind_rows(tb_his, tb_for)
    merge_tb <- merge_tb[!duplicated(merge_tb$times),] %>% dplyr::arrange()

    metadata <- list()
    metadata$for_name <- for_name
    metadata$his_name <- his_name
    metadata$unit <- unit
    metadata$limit <- limit
    metadata$data_from <- data_from
    metadata$data_to <- data_to
    metadata$data_size <- length(merge_tb$times)

    metadata <- tibble::as_tibble(metadata)
    
    return(list(metadata = metadata, points = merge_tb))
}



SqlPlotForHisCurveMerge <- function(merge_tb, tz = NULL) {
    
    title <- as.character(stringr::str_glue('<span style = "color:navy;font-size:14pt;font-weigth:bold;">{merge_tb$metadata$his_name} </span> <span style ="color:black;font-size:14pt;font-weigth:bold;">-</span> <span style = "color:crimson;font-size:14pt;font-weigth:bold;"> {merge_tb$metadata$for_name}</span>'))

    if (substr(merge_tb$metadata$for_name, 1, 3) == 'exc') {
        x_title <- 0.51
    } else {
        x_title <- 0.5
    }

    m <- list(
        l = 80,
        r = 80,
        b = 30,
        t = 80,
        pad = 0
    )

    if (is.null(tz)) {
        tz <- 'CET'
    }

    limit_date <- suppressMessages(lubridate::ymd_hms(merge_tb$metadata$limit, tz = tz))

    his_times <- merge_tb$points$times[which(merge_tb$points$times <= limit_date)]
    his_vals <- merge_tb$points$values[which(merge_tb$points$times <= limit_date)]

    for_times <- merge_tb$points$times[which(merge_tb$points$times > limit_date)]
    for_vals <- merge_tb$points$values[which(merge_tb$points$times > limit_date)]

    if (as.integer(max(for_times) - limit_date) < 30) {
        x_display_min <- limit_date + lubridate::ddays(-as.integer(max(for_times) - limit_date))
        x_display_max <- max(for_times)

    } else {
        x_display_min <- limit_date + lubridate::ddays(-15)
        x_display_max <- limit_date + lubridate::ddays(15)
    }


    y_display_min <- min(merge_tb$points$values[which(merge_tb$points$times >= x_display_min & merge_tb$points$times < x_display_max)])
    y_display_max <- max(merge_tb$points$values[which(merge_tb$points$times >= x_display_min & merge_tb$points$times < x_display_max)])
    
    fig <- plotly::plot_ly(data = merge_tb$points, width = 809, height = 500) %>% 

        add_trace(x = ~his_times, y = ~his_vals, type = "scatter", name = 'historic', mode = "lines", line = list(color = 'navy', width = 1.3)) %>%

        add_trace(x = ~for_times, y = ~for_vals, type = "scatter", name = 'forecast', mode = "lines", line = list(color = 'crimson', width = 1.3)) %>%

        layout(title = list(text = title, 
                            x = x_title, 
                            y = 0.96, 
                            xanchor = "middle", 
                            yanchor = "top",
                            pad = list(b = 5, t = 5, l = 5, r = 5))) %>%
        add_annotations(text = as.character(stringr::str_glue('[{merge_tb$metadata$unit}]')),
                        x = 0.5,
                        y = 1.1,
                        yref = "paper",
                        xref = "paper",
                        xanchor = "middle",
                        yanchor = "top",
                        showarrow = FALSE,
                        font = list(size = 15, color = 'navy')) %>%
        layout(yaxis = list(title = '',
                            showgrid = FALSE,
                            gridcolor = 'darkgray',
                            automargin = TRUE,
                            range = list(y_display_min, y_display_max),
                            showline = TRUE,
                            linewidth = 1.3,
                            linecolor = 'navy',
                            mirror = TRUE),

                xaxis = list(title = '',
                            showgrid = TRUE,
                            showline = TRUE,
                            linecolor = 'navy',
                            range = list(x_display_min, x_display_max),
                            linewidth = 1.3,
                            mirror = TRUE,
                            type = 'date',
                            tickformat = "%d %b-%y<br>%H:%M"),
                margin = m) %>%
        layout(hovermode = 'x unified') %>%

        plotly::config(displayModeBar = FALSE, showLink = FALSE)
        
    return(fig)
}


SqlGetDbMerge <- function(db_path, tz) {

    db_path <- normalizePath(db_path)
    con <- DBI::dbConnect(RSQLite::SQLite(), db_path)

    query <- "SELECT MAX(data_from) FROM metadata;"
    res <- DBI::dbGetQuery(con, query)
    data_from_max <- res$`MAX(data_from)`
    data_from_max <- suppressMessages(lubridate::ymd_hms(data_from_max, tz = tz))

    query <- "SELECT MAX(data_to) FROM metadata;"
    res <- DBI::dbGetQuery(con, query)

    data_to_max <- res$`MAX(data_to)`
    data_to_max <- suppressMessages(lubridate::ymd_hms(data_to_max, tz = tz))

    time_index <- seq(from = data_from_max, to = data_to_max, by = "hour")

    full_zoo <- suppressWarnings(zoo::zoo(,time_index))

    query <- "SELECT name FROM sqlite_master WHERE type='table' ORDER BY name;"
    res <- DBI::dbGetQuery(con, query)

    table_names <- res$name
    table_names <- table_names[table_names !='metadata']

    DBI::dbDisconnect(con)

    no_data_names <- character()
    for (table_name in table_names) {

        tb <- SqlGetCurveTableZoo(db_path = db_path, table_name = table_name)
        ts <- suppressWarnings(zoo::read.zoo(tb, format = "%Y-%m-%dT%H:%M:%S", tz = tz))
        ts <- suppressWarnings(ts[!duplicated(index(ts)),])

        if(length(index(ts)) > 0) {
            full_zoo <- suppressWarnings(zoo::merge.zoo(full_zoo, ts, all = TRUE))
        } else {
            no_data_names <- append(no_data_names, table_name)
        }
    }

    table_names <- table_names[which(table_names %notin% no_data_names)]
    names(full_zoo) <- table_names

    full_zoo <- zoo::fortify.zoo(full_zoo, name = 'times')
    full_zoo <- full_zoo[1:length(full_zoo$times) - 1,]

    return(full_zoo)
}


SqlGetClusterMerge <- function(db_path, 
                               json_file, 
                               cluster_name, 
                               tz,
                               ec = NULL) {

    db_path <- normalizePath(db_path)
    json_file <- normalizePath(json_file)

    curve_list <- jsonlite::fromJSON(json_file)
    cluster_vec <- as.character(unname(curve_list[[cluster_name]]))
    

    if (!is.null(ec)) {

        if (ec == TRUE) {
            regexpat <- '\\bec\\d{2}' 
        } else if (ec == FALSE) {
            regexpat <- '\\bgfs\\d{2}' 
        }

        match <- stringr::str_extract(cluster_vec, regexpat)
        curve_vec <- cluster_vec[which(!is.na(match))]
    } else {
        curve_vec <- cluster_vec
    }

    con <- DBI::dbConnect(RSQLite::SQLite(), db_path)

    curve_string <- toString(sprintf("'%s'", curve_vec))
    query <- as.character(stringr::str_glue('SELECT MAX(data_from) FROM (SELECT data_from FROM metadata WHERE curve_name IN ({curve_string}));'))

    con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
    res <- DBI::dbGetQuery(con, query)
    data_from_max <- res$`MAX(data_from)`
    data_from_max <- suppressMessages(lubridate::ymd_hms(data_from_max, tz = tz))

    curve_string <- toString(sprintf("'%s'", curve_vec))
    query <- as.character(stringr::str_glue('SELECT MAX(data_to) FROM (SELECT data_to FROM metadata WHERE curve_name IN ({curve_string}));'))

    con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
    res <- DBI::dbGetQuery(con, query)
    data_to_max <- res$`MAX(data_to)`
    data_to_max <- suppressMessages(lubridate::ymd_hms(data_to_max, tz = tz))

    DBI::dbDisconnect(con)

    time_index <- seq(from = data_from_max, to = data_to_max, by = "hour")

    full_zoo <- suppressWarnings(zoo::zoo(,time_index))

    table_names <- curve_vec %>% 
                   stringr::str_replace_all(' ', '_') %>%
                   stringr::str_replace_all('/', '') %>%
                   stringr::str_replace_all('>', '') %>%
                   stringr::str_replace_all('-', '')

    no_data_names <- character()
    for (table_name in table_names) {

        tb <- SqlGetCurveTableZoo(db_path = db_path, table_name = table_name)
        ts <- suppressWarnings(zoo::read.zoo(tb, format = "%Y-%m-%dT%H:%M:%S", tz = tz))
        ts <- suppressWarnings(ts[!duplicated(index(ts)),])

        if(length(index(ts)) > 0) {
            full_zoo <- suppressWarnings(zoo::merge.zoo(full_zoo, ts, all = TRUE))
        } else {
            no_data_names <- append(no_data_names, table_name)
        }
    }

    table_names <- table_names[which(table_names %notin% no_data_names)]
    names(full_zoo) <- table_names

    full_zoo <- zoo::fortify.zoo(full_zoo, name = 'times')
    full_zoo <- full_zoo[1:length(full_zoo$times) - 1,]

    return(full_zoo)
}

SqlCreateImportDb <- function(db_path) {

  db_path = normalizePath(db_path)
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  RSQLite::dbSendStatement(con, 'PRAGMA encoding = "UTF-8"')
  RSQLite::dbSendStatement(con, 'PRAGMA journal_mode = WAL')
  RSQLite::dbSendStatement(con, 'PRAGMA busy_timeout = 8000')
  DBI::dbDisconnect(con)

  return(TRUE)
}

SqlCreateImportTableExcel <- function(xl_path, db_path, table_name) {

    db_path = normalizePath(db_path)
    xl_path = normalizePath(xl_path)
  
    con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
    query <- "SELECT name FROM sqlite_master WHERE type='table' ORDER BY name;"
    res <- DBI::dbGetQuery(con, query)
    table_names <- res$name

    if (table_name %in% table_names) {
        print(as.character(stringr::str_glue("[{table_name}] already exists in [{db_path}]")))
        DBI::dbDisconnect(con)
        return(FALSE)
    }
  
    df <- readxl::read_xlsx(xl_path)
    times <- df$times
    times <- lubridate::ceiling_date(x = times, unit = "hour")
    times <- strftime(times, "%Y-%m-%dT%H:%M:%S%z")
    last <- substr(times, nchar(times)-1, nchar(times))
    times <- stringr::str_replace(times, '\\d{2}$', as.character(stringr::str_glue(':{last}'))) 
    df$times <- times
    
    RSQLite::dbWriteTable(conn = con, name = table_name, value = df)
    DBI::dbDisconnect(con)

    return(TRUE)
}

ExcelReadSingleSeries <- function(xl_path) {

  xl_path = normalizePath(xl_path)
  df <- readxl::read_xlsx(xl_path)

  times <- df$times
  times <- lubridate::ceiling_date(x = times, unit = "hour")
  times <- strftime(times, "%Y-%m-%d %H:%M")
  df$times <- times

  return(df)
}

SqlGetImportTable <- function(db_path, table_name, tz) {
  
    con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  
    query <- "SELECT name FROM sqlite_master WHERE type='table' ORDER BY name;"
    res <- DBI::dbGetQuery(con, query)
    table_names <- res$name

    if (table_name %notin% table_names) {
        print(as.character(stringr::str_glue("[{table_name}] does not exist in [{db_path}]")))
        DBI::dbDisconnect(con)
        return(NA)
    }

    query <- as.character(stringr::str_glue("SELECT * FROM '{table_name}'"))
    res <- DBI::dbGetQuery(con, query)
    DBI::dbDisconnect(con)
    
    times <- res$times
    times <- suppressMessages(lubridate::ymd_hms(times, tz = tz))
    times <- lubridate::ceiling_date(x = times, unit = "hour")
    times <- strftime(times, "%Y-%m-%d %H:%M")
    res$times <- times
    
    tb <- tibble::as_tibble(res)
    tb
}

SqlDropImportTable <- function(db_path, table_name) {
  
    con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
    
    query <- "SELECT name FROM sqlite_master WHERE type='table' ORDER BY name;"
    res <- DBI::dbGetQuery(con, query)
    table_names <- res$name
    
    if (table_name %notin% table_names) {
      print(as.character(stringr::str_glue("[{table_name}] does not exist in [{db_path}]")))
      DBI::dbDisconnect(con)
      return(FALSE)
    }
  
    comm <- as.character(stringr::str_glue("DROP TABLE '{table_name}';"))

    DBI::dbWithTransaction(con, {
        DBI::dbExecute(con, comm)
    })

    DBI::dbDisconnect(con)

    return(TRUE)
}
