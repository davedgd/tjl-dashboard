#------
# Notes
# -----

# Transactions does not cover refunds! (e.g., see 1 refund on March 21, 2019)
# amount_multival in TransactionsV2 is about multiple sources of payment (e.g., CC + gift card)

# NOTE: V2 API does not support pulling up various components; must use V1 API for this
# https://docs.connect.squareup.com/api/connect/v1#v1versusv2

# V2 transaction_id generally links to V1 id, except in certain cases (see JL_Misc)

# NOTE: dplyr map can be much slower than sapply, but the code is more readable/consistent (see https://stackoverflow.com/questions/45101045/why-use-purrrmap-instead-of-lapply); however using map with ~ . notation is nearly as fast; for instance:

#sapply(sapply(rawTransactionsV2, function(x) sapply(x$tenders, function(y) y$id)), paste, collapse = ";")
#rawTransactionsV2 %>% map("tenders") %>% map_depth(2, "amount_money") %>% map_depth(2, "amount") %>% map_chr(paste, collapse = ";")
#rawTransactionsV2 %>% map(~ .$tenders) %>% map_depth(2, ~ .$amount_money$amount) %>% map_chr(paste, collapse = ";")

# ---------------------
# Establish Square APIs
# ---------------------

api_instance_locations <- squareconnect$LocationsApi()
api_instance_locations$api_client$configuration$access_token <- square_access_token

api_instance_transactions_V2 <- squareconnect$TransactionsApi()
api_instance_transactions_V2$api_client$configuration$access_token <- square_access_token

api_instance_transactions_V1 <- squareconnect$V1TransactionsApi()
api_instance_transactions_V1$api_client$configuration$access_token <- square_access_token

# ----------------
# Helper Functions
# ----------------

BlankTo <- function(x, to = "NA")
  if (identical(x, "")) to else x

# ---------
# Locations
# ---------

FetchLocations <- function(save = FALSE, trySaved = FALSE) {
  
  rawSave <- paste0("cache/locations.pickle")
  
  if (trySaved) {
    
    rawLocations <- py_load_object(rawSave)
    
    message(paste("Loaded", length(rawLocations), "saved locations..."))
    
  } else {
    
    api_response_locations <- api_instance_locations$list_locations()
    rawLocations <- api_response_locations$locations
    
    if (save)
      py_save_object(rawLocations, file = rawSave)
    
    message(paste("Fetched", length(rawLocations), "locations..."))
    
  }
  
  return(rawLocations)

}

LocationsDF <- function(rawLocations) {

  LocationsDF <- tibble(
    id = rawLocations %>% map_chr(~ .$id),
    name = rawLocations %>% map_chr(~ .$name),
    business_name = rawLocations %>% map_chr(~ .$business_name),
    street_address =  trimws(paste(
      rawLocations %>% map_chr(~ ifelse(is.null(.$address$address_line_1), "", .$address$address_line_1)), 
      rawLocations %>% map_chr(~ ifelse(is.null(.$address$address_line_2), "", .$address$address_line_2)), 
      rawLocations %>% map_chr(~ ifelse(is.null(.$address$address_line_3), "", .$address$address_line_3))
      )),
    city = rawLocations %>% map_chr(~ .$address$locality),
    zip = rawLocations %>% map_chr(~ .$address$postal_code)
    )
  
  return(LocationsDF)
  
}

# ---------------
# Transactions V2
# ---------------

FetchTransactionsV2 <- function (theLocation, beginTime, endTime, save = FALSE, trySaved = TRUE) {
  
  message(paste("Now working on transactions (V2) for location:", theLocation))
  
  beginToEnd <- gsub(":", ".", paste(beginTime, endTime, sep = "_"))
  
  buildSave <- paste0("cache/buildTransactionsV2_", theLocation,"_", beginToEnd, ".pickle")
  rawSave <- paste0("cache/rawTransactionsV2_", theLocation,"_", beginToEnd, ".pickle")

  hasCursor <- TRUE
  buildTransactionsV2 <- list()
  beginTime <- str_c(str_sub(rfc3339(beginTime), start = 0, end = -3), ":", str_sub(rfc3339(beginTime), start = -2)) #paste0(format_ISO8601(as_datetime(beginTime)), "Z")
  endTime <- str_c(str_sub(rfc3339(endTime), start = 0, end = -3), ":", str_sub(rfc3339(endTime), start = -2)) #paste0(format_ISO8601(as_datetime(endTime)), "Z")
  
  if (trySaved & file.exists(buildSave) & file.exists(rawSave)) {
    
    #message("Found saved files; attempting to load...")
    
    #buildTransactionsV2 <- py_load_object(buildSave)
    rawTransactionsV2 <- py_load_object(rawSave)
    
    message(paste("Loaded", length(rawTransactionsV2), "saved transactions (V2)..."))
    
  } else {
  
    while(hasCursor) {
      buildTransactionsV2[[length(buildTransactionsV2) + 1]] <- if (length(buildTransactionsV2) == 0) 
        api_instance_transactions_V2$list_transactions(location_id = theLocation, end_time = endTime, begin_time = beginTime) else
        api_instance_transactions_V2$list_transactions(location_id = theLocation, end_time = endTime, begin_time = beginTime, cursor = buildTransactionsV2[[length(buildTransactionsV2)]]$cursor)
      
      cursor <- buildTransactionsV2[[length(buildTransactionsV2)]]$cursor
      
      if (is.null(cursor))
        hasCursor <- FALSE
    }
    
    if (length(buildTransactionsV2) > 1)
      rawTransactionsV2 <- lapply(buildTransactionsV2, function(x) x$transactions) %>% flatten() else
      rawTransactionsV2 <- buildTransactionsV2$transactions
    
    if (save) {
      py_save_object(buildTransactionsV2, file = buildSave)
      py_save_object(rawTransactionsV2, file = rawSave)
    }
    
    message(paste("Fetched", length(rawTransactionsV2), "transactions (V2)..."))
    
  }
  
  return(rawTransactionsV2)
}

TransactionsV2_DF <- function (rawTransactionsV2, save = FALSE, trySaved = TRUE, saveFile = NA) {
  
  if (trySaved & file.exists(saveFile)) {
    
    res <- load(saveFile) %>% get()
    message(paste("Loaded", nrow(res), "transactions (V2)..."))
    return(res)
    
    } else {
    
    TransactionsV2 <- tibble(
      timestamp = rawTransactionsV2 %>% map_chr(~ .$created_at),
      amount_multival = rawTransactionsV2 %>% map(~ .$tenders) %>% map_depth(2, ~ .$amount_money$amount) %>% map_chr(paste, collapse = ";"),
      payment_id_multival = rawTransactionsV2 %>% map(~ .$tenders) %>% map_depth(2, ~ .$id) %>% map_chr(paste, collapse = ";"), 
      customer_id_multival = rawTransactionsV2 %>% map(~ .$tenders) %>% map_depth(2, ~ ifelse(is.null(.$customer_id), "NA", .$customer_id)) %>% map_chr(paste, collapse = ";"),
      transaction_id = rawTransactionsV2 %>% map_chr(~ .$id),
      location_id = rawTransactionsV2 %>% map_chr(~ .$location_id)
    )
    
    TransactionsV2$amount <- TransactionsV2 %>% pull(amount_multival) %>% str_split(";") %>% map(as.numeric) %>% map(sum) %>% as_vector()
    TransactionsV2 <- TransactionsV2 %>% 
      mutate(datetime = ymd_hms(timestamp, tz = "EST5EDT", quiet = TRUE))
        
    if (save)
      save(TransactionsV2, file = saveFile)
    
    message(paste("Processed", nrow(TransactionsV2), "transactions (V2)..."))
    
    }
  
  return(TransactionsV2)
  
}

# ---------------
# Transactions V1
# ---------------

FetchTransactionsV1 <- function (theLocation, beginTime, endTime, save = FALSE, trySaved = TRUE) {
  
  message(paste("Now working on transactions (V1) for location:", theLocation))
  
  beginToEnd <- gsub(":", ".", paste(beginTime, endTime, sep = "_"))
  
  buildSave <- paste0("cache/buildTransactionsV1_", theLocation,"_", beginToEnd, ".pickle")
  rawSave <- paste0("cache/rawTransactionsV1_", theLocation,"_", beginToEnd, ".pickle")

  hasBatchToken <- TRUE
  buildTransactionsV1 <- list()
  beginTime <- str_c(str_sub(rfc3339(beginTime), start = 0, end = -3), ":", str_sub(rfc3339(beginTime), start = -2))
  endTime <- str_c(str_sub(rfc3339(endTime), start = 0, end = -3), ":", str_sub(rfc3339(endTime), start = -2))
  
  if (trySaved & file.exists(buildSave) & file.exists(rawSave)) {
    
    #message("Found saved files; attempting to load...")
    
    #buildTransactionsV1 <- py_load_object(buildSave)
    rawTransactionsV1 <- py_load_object(rawSave)
    
    message(paste("Loaded", length(rawTransactionsV1), "saved transactions (V1)..."))
    
  } else {
    
    while (hasBatchToken) {
      buildTransactionsV1[[length(buildTransactionsV1) + 1]] <- if (length(buildTransactionsV1) == 0)
        api_instance_transactions_V1$list_payments(location_id = theLocation, end_time = endTime, begin_time = beginTime, limit = 200L, order = "DESC", batch_token = NULL) else
          api_instance_transactions_V1$list_payments(location_id = theLocation, end_time = endTime, begin_time = beginTime, limit = 200L, order = "DESC", batch_token = api_instance_transactions_V1$api_client$last_response$getbatch_token())
      
      batchToken <- api_instance_transactions_V1$api_client$last_response$getbatch_token()
      
      if (is.null(batchToken))
        hasBatchToken <- FALSE
    }
    
    rawTransactionsV1 <- buildTransactionsV1 %>% flatten()
    
    if (save) {
      py_save_object(buildTransactionsV1, file = buildSave)
      py_save_object(rawTransactionsV1, file = rawSave)
    }
    
    message(paste("Fetched", length(rawTransactionsV1), "transactions (V1)..."))
    
  }
  
  return(rawTransactionsV1)
  
}

TransactionsV1_DF <- function (rawTransactionsV1, save = FALSE, trySaved = TRUE, saveFile = NA) {

  # note: no equivalent of transaction_id in V1; customer_id also not in V1

  if (trySaved & file.exists(saveFile)) {
    
    res <- load(saveFile) %>% get()
    message(paste("Loaded", nrow(res), "transactions (V1)..."))
    return(res)
    
  } else {
  
    TransactionsV1 <- tibble(
    created_at = rawTransactionsV1 %>% map_chr(~ .$created_at),
    total_collected_money = rawTransactionsV1 %>% map_int(~ .$total_collected_money$amount),
    net_sales_money = rawTransactionsV1 %>% map_int(~ .$net_sales_money$amount),
    net_total_money = rawTransactionsV1 %>% map_int(~ .$net_total_money$amount),
    gross_sales_money = rawTransactionsV1 %>% map_int(~ .$gross_sales_money$amount),
    discount_money = rawTransactionsV1 %>% map_int(~ .$discount_money$amount),
    inclusive_tax_applied_money_multival = rawTransactionsV1 %>% map(~ .$inclusive_tax) %>% map_depth(2, ~ .$applied_money$amount) %>% map_chr(paste, collapse = ";"),
    
    inclusive_tax_fee_id_multival = rawTransactionsV1 %>% map(~ .$inclusive_tax) %>% map_depth(2, ~ .$fee_id) %>% map_chr(paste, collapse = ";"),
    inclusive_tax_name_multival = rawTransactionsV1 %>% map(~ .$inclusive_tax) %>% map_depth(2, ~ .$name) %>% map_chr(paste, collapse = ";"),
    inclusive_tax_rate_multival = rawTransactionsV1 %>% map(~ .$inclusive_tax) %>% map_depth(2, ~ .$rate) %>% map_chr(paste, collapse = ";"),
    
    inclusive_tax_money = rawTransactionsV1 %>% map_int(~ .$inclusive_tax_money$amount),
    additive_tax_money = rawTransactionsV1 %>% map_int(~ .$additive_tax_money$amount),
    tax_money = rawTransactionsV1 %>% map_int(~ .$tax_money$amount),
    tip_money = rawTransactionsV1 %>% map_int(~ .$tip_money$amount),
    surcharge_money = rawTransactionsV1 %>% map_int(~ .$surcharge_money$amount),
    processing_fee_money = rawTransactionsV1 %>% map_int(~ .$processing_fee_money$amount),
    refunded_money = rawTransactionsV1 %>% map_int(~ .$refunded_money$amount),
    id = rawTransactionsV1 %>% map_chr(~ .$id),
    merchant_id = rawTransactionsV1 %>% map_chr(~ .$merchant_id),
    item_name_multival = rawTransactionsV1 %>% map(~ .$itemizations) %>% map_depth(2, ~ .$name) %>% map_chr(paste, collapse = ";"),
    item_variation_name_multival = rawTransactionsV1 %>% map(~ .$itemizations) %>% map_depth(2, ~ .$item_variation_name) %>% map_chr(paste, collapse = ";"),
    
    
    item_modifiers_name_multival_nested = rawTransactionsV1 %>% map(~ .$itemizations) %>% map_depth(2, ~ .$modifiers) %>% map_depth(3, ~ .$name) %>% map_depth(2, unlist) %>% map_depth(2, paste, collapse = ",") %>% map_chr(paste, collapse = ";"),
    item_modifiers_modifier_option_id_multival_nested = rawTransactionsV1 %>% map(~ .$itemizations) %>% map_depth(2, ~ .$modifiers) %>% map_depth(3, ~ .$modifier_option_id) %>% map_depth(2, unlist) %>% map_depth(2, paste, collapse = ",") %>% map_chr(paste, collapse = ";"),
    item_modifiers_applied_money_multival_nested = rawTransactionsV1 %>% map(~ .$itemizations) %>% map_depth(2, ~ .$modifiers) %>% map_depth(3, ~ .$applied_money$amount) %>% map_depth(2, unlist) %>% map_depth(2, paste, collapse = ",") %>% map_chr(paste, collapse = ";"),
    
    item_qty_multival = rawTransactionsV1 %>% map(~ .$itemizations) %>% map_depth(2, ~ .$quantity) %>% map_chr(paste, collapse = ";"),
    
    
    item_discount_id_multival_nested = rawTransactionsV1 %>% map(~ .$itemizations) %>% map_depth(2, ~ .$discounts) %>% map_depth(3, ~ .$discount_id) %>% map_depth(2, unlist) %>% map_depth(2, paste, collapse = ",") %>% map_chr(paste, collapse = ";"),
    item_discount_name_multival_nested = rawTransactionsV1 %>% map(~ .$itemizations) %>% map_depth(2, ~ .$discounts) %>% map_depth(3, ~ .$name) %>% map_depth(2, unlist) %>% map_depth(2, paste, collapse = ",") %>% map_chr(paste, collapse = ";"),
    
    item_unique_name_count = rawTransactionsV1 %>% map(~ .$itemizations) %>% map_depth(2, ~ .$quantity) %>% map_int(length),
    item_total_qty = rawTransactionsV1 %>% map(~ .$itemizations) %>% map_depth(2, ~ .$quantity) %>% map(as_vector) %>% map_dbl(sum),
    
    item_single_qty_money_multival = rawTransactionsV1 %>% map(~ .$itemizations) %>% map_depth(2, ~ .$single_quantity_money$amount) %>% map_chr(paste, collapse = ";"),
    item_gross_sales_money_multival = rawTransactionsV1 %>% map(~ .$itemizations) %>% map_depth(2, ~ .$gross_sales_money$amount) %>% map_chr(paste, collapse = ";"),
    item_net_sales_money_multival = rawTransactionsV1 %>% map(~ .$itemizations) %>% map_depth(2, ~ .$net_sales_money$amount) %>% map_chr(paste, collapse = ";"),
    item_discount_money_multival = rawTransactionsV1 %>% map(~ .$itemizations) %>% map_depth(2, ~ .$discount_money$amount) %>% map_chr(paste, collapse = ";"),
    item_total_money_multival = rawTransactionsV1 %>% map(~ .$itemizations) %>% map_depth(2, ~ .$total_money$amount) %>% map_chr(paste, collapse = ";"),
    
    
    item_detail_category_name_multival = rawTransactionsV1 %>% map(~ .$itemizations) %>% map_depth(2, ~ .$item_detail$category_name) %>% map_chr(paste, collapse = ";"),
    item_id_multival = rawTransactionsV1 %>% map(~ .$itemizations) %>% map_depth(2, ~ .$item_detail$item_id) %>% map_chr(paste, collapse = ";"),
    item_variation_id_multival = rawTransactionsV1 %>% map(~ .$itemizations) %>% map_depth(2, ~ .$item_detail$item_variation_id) %>% map_chr(paste, collapse = ";")
    )
    
    dim(TransactionsV1)
    
    TransactionsV1$raw_total <- 
    map(
      Map(
        "+",
        Map(
          "*",
          TransactionsV1 %>% pull(item_qty_multival) %>% str_split(";") %>% map(as.numeric),
          TransactionsV1 %>% pull(item_single_qty_money_multival) %>% str_split(";") %>% map(as.numeric)
          ),
        TransactionsV1 %>% pull(item_modifiers_applied_money_multival_nested) %>% str_split(";") %>% map(str_split, ",") %>% map_depth(2, BlankTo, to = "0") %>% map_depth(2, as.numeric) %>% map_depth(2, sum) %>% map(unlist)
      ),
      sum) %>%
    as_vector()
    
    TransactionsV1 <- TransactionsV1 %>% 
      mutate(datetime = ymd_hms(created_at, tz = "EST5EDT", quiet = TRUE))
    
    if (save)
      save(TransactionsV1, file = saveFile)
    
    message(paste("Processed", nrow(TransactionsV1), "transactions (V1)..."))
    
    }
  
  return(TransactionsV1)
  
}

# -----------------------
# Transactions Multi-Year
# -----------------------

FetchYearlyTransactions <- function(fetchFunction, theLocation, endTime, numYears = 1, byMonth = FALSE, ...) {
  
  if (!byMonth) {
    
    timeDF <- data.frame(
      endTime = c(endTime, endTime - years(1:(numYears - 1))),
      beginTime = endTime - years(1:numYears)
    )
    
    message(paste("Fetching", nrow(timeDF), "years of transactions for location:", theLocation))
    
  } else {
    
    timeDF <- data.frame(
      endTime = c(endTime, endTime - months(1:(numYears * 12 - 1))),
      beginTime = endTime - months(1:(numYears * 12))
    )
    
    message(paste("Fetching", nrow(timeDF), "years of transactions by month..."))
    
  }
  
  res <- list()
  
  for (eachTime in 1:nrow(timeDF)) {
    res[[eachTime]] <- fetchFunction(theLocation, beginTime = timeDF$beginTime[eachTime], endTime = timeDF$endTime[eachTime], ...)
  }
  
  res <- res %>% flatten()
  
  message(paste("Returned", length(res), "transactions across", nrow(timeDF), "years..."))
  
  return(res)

}

# -------------------
# Update Transactions
# -------------------

Update_Transactions <- function (Transactions_DF, fetchFunction = FetchTransactionsV2, processFunction = TransactionsV2_DF, endTime = Sys.time(), save = TRUE, trySaved = TRUE, saveFile = NULL) {
  
  message("Performing requested update...")
  
  if ("location_id" %in% names(Transactions_DF)) locationCol <- "location_id" else locationCol <- "merchant_id"
  
  theLocations <- unique(Transactions_DF %>% pull(locationCol))
  
  beginTime <- max(Transactions_DF$datetime) %>% floor_date("day") # floor down to avoid missing transactions
  
  res <- theLocations %>%
    map(~ fetchFunction(
      theLocation = .,
      beginTime = beginTime,
      endTime = endTime,
      save = save,
      trySaved = trySaved)) %>%
    flatten() %>%
    processFunction(
      save = FALSE,
      trySaved = FALSE,
      saveFile = "") %>% 
    bind_rows(Transactions_DF) %>% 
    distinct()
  
  if (save) {
    saveName <- basename(saveFile) %>% file_path_sans_ext()
    assign(saveName, res)
    eval(parse(text = paste0("save(", saveName,", file = saveFile)")))
  }
  
  message("Update complete!")
  
  return(res)
  
}
