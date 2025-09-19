

# Pull unique sender, receiver, and modality values from a dataframe
get_unique_values <- function(
  df, 
  sender_cols, receiver_cols, 
  mode_column
) {
  # Extract unique values for each sender column
  unique_senders <- lapply(sender_cols, function(col) sort(unique(df[[col]])))
  names(unique_senders) <- sender_cols
  
  # Extract unique values for each receiver column
  unique_receivers <- lapply(receiver_cols, function(col) sort(unique(df[[col]])))
  names(unique_receivers) <- receiver_cols
  
  # Extract unique values for the mode column
  if (length(mode_column) == 0) {
    unique_modes <- list()
  } 
  # else if (mode_column == ATTR_NAMES$CONTEXT_ID) {
  #   unique_modes <- ATTR_NAMES$CONTEXT_ID;
  # }
  else {
    unique_modes <- sort(unique(df[[mode_column]]))
  }
  
  ret_list <- list(
    unique_senders = unique_senders,
    unique_receivers = unique_receivers,
    unique_modes = unique_modes
  )
  
  return(ret_list);
}


#' Generate a multidimensional array for window and weight parameters
#'
#' This function constructs a multidimensional array representing all combinations of sender(s), receiver(s), and mode(s),
#' with an additional axis for weight and window parameters. The resulting array can be used to efficiently look up or modify
#' window and weight values for each unique combination in your data, which is useful for network accumulation and modeling.
#'
#' @param df A data.frame containing the data to extract unique values for senders, receivers, and modes.
#' @param sender_cols Character vector of column names in `df` to use as sender(s). Can be empty or NULL if not applicable.
#' @param receiver_cols Character vector of column names in `df` to use as receiver(s). Can be empty or NULL if not applicable.
#' @param mode_column Character string giving the column name in `df` to use as the mode (e.g., modality, channel). Can be empty or NULL if not applicable.
#' @param default_window Numeric value to use as the default window for all combinations (default: 1).
#' @param default_weight Numeric value to use as the default weight for all combinations (default: 1).
#'
#' @return A multidimensional array with dimensions [sender(s), receiver(s), mode(s), weight/window], where the last axis has two levels: "weight" and "window".
#'         The array is initialized with the default weight and window values, and has named dimensions for easy indexing.
#'
#' @examples
#' df <- data.frame(sender = c("A", "B"), receiver = c("X", "Y"), mode = c("chat", "resource"))
#' arr <- context_tensor(df, sender_cols = "sender", receiver_cols = "receiver", mode_column = "mode")
#' arr["A", "X", "chat", "weight"] # Access the weight for sender A, receiver X, mode chat
#'
#' @export
context_tensor <- function(
  df, 
  sender_cols = NULL, 
  receiver_cols = NULL, 
  mode_column = ATTR_NAMES$CONTEXT_ID, 
  default_window = 1,
  default_weight = 1
) {
  unique_list = get_unique_values(df, sender_cols, receiver_cols, mode_column)
  weight_window_axis <- c("weight", "window");
  
  # Pull out unique senders, receivers, and modes from unique_list
  unique_senders <- unique_list$unique_senders
  unique_receivers <- unique_list$unique_receivers;
  unique_modes <- if(is.factor(unique_list$unique_modes))
      as.numeric(unique_list$unique_modes)
    else unique_list$unique_modes
  ;
  
  # Set dimension indices to unique values
  dim_names <- list()
  for (i in seq_along(sender_cols)) {
    dim_names[[paste0("sender_", i)]] <- unique_senders[[i]]
  }
  for (i in seq_along(receiver_cols)) {
    dim_names[[paste0("receiver_", i)]] <- unique_receivers[[i]]
  }
  if (length(unique_modes) > 0) {
    dim_names[["modes"]] <- unique_modes
  }
  dim_names[["weight_window"]] <- weight_window_axis
   
  # Set dims accordingly s.t. our array is [S, R, M, weight/window]
  dims <- c()
  dims <- c(sapply(unique_senders, length), sapply(unique_receivers, length))
  
  if (length(unique_modes > 0)) { 
    dims <- c(dims, length(unique_modes))
  }
  
  dims <- c(dims, length(weight_window_axis))

  # Init multidimensional array; include sender, receiver, mode columns; default weight; default window as attributes 
  multidim_arr <- array(0, dim = dims, dimnames = dim_names)
  multidim_arr <- array(rep(c(default_weight, default_window), each = prod(unlist(dims))/2), dim = dims, dimnames = dim_names)
  
  si <- seq_along(sender_cols);
  ri <- seq_along(receiver_cols) + ifelse(length(si) > 0, si[length(si)], 0);
  
  if(length(ri) > 0) {
    mi <- seq_along(ri) + ri[length(ri)];
  }
  else {
    mi <- 1;
  }
  
  attr(multidim_arr, "sender_cols") <- sender_cols
  attr(multidim_arr, "receiver_cols") <- receiver_cols
  attr(multidim_arr, "mode_column") <- mode_column
  attr(multidim_arr, "sender_inds") <- si
  attr(multidim_arr, "receiver_inds") <- ri
  attr(multidim_arr, "mode_inds") <- mi
  attr(multidim_arr, "default_window") <- default_window
  attr(multidim_arr, "default_weight") <- default_weight
  
  return(multidim_arr)
}

#' Deprecated Alias for context_tensor
#'
#' Use \code{context_tensor()} instead. This alias will be removed in a future release.
#'
#' @param ... Arguments passed to \code{context_tensor()}.
#' 
#' @return A multidimensional array with dimensions [sender(s), receiver(s), mode(s), weight/window], where the last axis has two levels: "weight" and "window".
#'         The array is initialized with the default weight and window values, and has named dimensions for easy indexing.
#'
#' @seealso \code{\link{context_tensor}}
#' @export
windows_weights <- function(...) {
  .Deprecated("context_tensor", "tma v0.2.0");
  do.call(context_tensor, list(...))
}

# If sender_cols, receiver_cols, or mode_column are empty, have our standin. Be sure to fill in 
# standin as expected in multidim_arr. 
# We do this so we don't fail when they don't specify senders/receivers/mode
preprocess <- function(
  df, 
  multidim_arr,
  use_standin = TRUE
) {
  # Get sender, receiver, and mode columns + unique values from multidim_arr
  sender_cols <- attr(multidim_arr, "sender_cols")
  receiver_cols <- attr(multidim_arr, "receiver_cols")
  mode_column <- attr(multidim_arr, "mode_column")
  
  unique_list = get_unique_values(df, sender_cols, receiver_cols, mode_column)
  # Pull out unique senders, receivers, and modes from unique_list
  unique_senders <- unique_list$unique_senders
  unique_receivers <- unique_list$unique_receivers
  unique_modes <- unique_list$unique_modes
  
  old_unique_senders <- unique_senders
  old_unique_receivers <- unique_receivers
  old_unique_modes <- unique_modes
  if (length(old_unique_senders) == 0) {
    old_unique_senders <- NA  # Placeholder for missing senders
  }
  if (length(old_unique_receivers) == 0) {
    old_unique_receivers <- NA  # Placeholder for missing receivers
  }
  if (length(old_unique_modes) == 0) {
    old_unique_modes <- NA # Placeholder for missing modes
  }
  
  # Ensure standin is present for any empty columns
  if(isTRUE(use_standin)) {
    if (length(sender_cols) == 0) {
      sender_cols <- "standin"
      unique_senders <- list(standin = "standin")
    }
    if (length(receiver_cols) == 0) {
      receiver_cols <- "standin"
      unique_receivers <- list(standin = "standin")
    }
    if (length(mode_column) == 0) {
      mode_column <- "standin"
      unique_modes <- "standin"
    }
  }
  
  # Create a new multidimensional array to hold expanded values
  weight_window_axis = c("weight", "window")
  
  # Construct dimension names dynamically
  dim_names <- list()
  for (i in seq_along(sender_cols)) {
    dim_names[[paste0("sender_", i)]] <- unique_senders[[i]]
  }
  for (i in seq_along(receiver_cols)) {
    dim_names[[paste0("receiver_", i)]] <- unique_receivers[[i]]
  }
  dim_names[["modes"]] <- unique_modes
  dim_names[["weight_window"]] <- weight_window_axis
  
  dims <- c()
  for (i in seq_along(sender_cols)) {
    dims <- c(dims, length(unique_senders[[i]]))
  }
  for (i in seq_along(receiver_cols)) {
    dims <- c(dims, length(unique_receivers[[i]]))
  }
  dims <- c(dims, length(unique_modes), length(weight_window_axis))
  
  new_multidim_arr <- array(0, dim = dims, dimnames = dim_names)
  
  combinations <- expand.grid(
    new_senders = do.call(paste, c(expand.grid(unique_senders), sep = "_")),
    new_receivers = do.call(paste, c(expand.grid(unique_receivers), sep = "_")),
    new_modes = unique_modes,
    old_senders = do.call(paste, c(expand.grid(old_unique_senders), sep = "_")),
    old_receivers = do.call(paste, c(expand.grid(old_unique_receivers), sep = "_")),
    old_modes = old_unique_modes,
    stringsAsFactors = FALSE
  )
  
  combinations <- combinations[
    (# just modality
      (combinations$new_senders == "standin" & combinations$new_receivers == "standin" & combinations$new_modes == combinations$old_modes) | 
        # just sender
        (combinations$new_senders == combinations$old_senders & combinations$new_receivers == "standin" & combinations$new_modes == "standin") | 
        # just receiver
        (combinations$new_senders == "standin" & combinations$new_receivers == combinations$old_receivers & combinations$new_modes == "standin") | 
        # modality + sender
        (combinations$new_senders ==  combinations$old_senders & combinations$new_receivers == "standin" & combinations$new_modes == combinations$old_modes) | 
        # modality + receiver
        (combinations$new_senders == "standin" & combinations$new_receivers == combinations$old_receivers & combinations$new_modes == combinations$old_modes) | 
        # sender + receiver
        (combinations$new_senders == combinations$old_senders & combinations$new_receivers == combinations$old_receivers & combinations$new_modes == "standin") | 
        # modality + sender + receiver
        (combinations$new_senders == combinations$old_senders & combinations$new_receivers == combinations$old_receivers & combinations$new_modes == combinations$old_modes)) 
    ,
  ]
  
  # remove NA rows
  combinations <- combinations[rowSums(is.na(combinations)) < ncol(combinations), ]
  
  for (i in seq_len(nrow(combinations))) {
    new_s <- combinations$new_senders[i]
    new_r <- combinations$new_receivers[i]
    new_m <- combinations$new_modes[i]
    orig_s <- combinations$old_senders[i]
    orig_r <- combinations$old_receivers[i]
    orig_m <- combinations$old_modes[i]
    
    new_index_list <- c(strsplit(new_s, "_")[[1]], strsplit(new_r, "_")[[1]], new_m)
    old_index_list <- c(strsplit(orig_s, "_")[[1]], strsplit(orig_r, "_")[[1]], orig_m)
    old_index_list <- old_index_list[!is.na(old_index_list) & old_index_list != "NA"]
    
    weight_new_index_list <- c(new_index_list, "weight")
    window_new_index_list <- c(new_index_list, "window")
    
    new_multidim_arr[matrix(as.vector(weight_new_index_list), 1)] <- do.call(`[`, c(list(multidim_arr), as.list(old_index_list), list("weight")))
    new_multidim_arr[matrix(as.vector(window_new_index_list), 1)] <- do.call(`[`, c(list(multidim_arr), as.list(old_index_list), list("window")))
  }
  
  # Return the updated multidimensional array and the new column identifiers
  result <- list(
    tensor = new_multidim_arr,
    sender_cols = sender_cols,
    receiver_cols = receiver_cols,
    mode_column = mode_column
  )
  
  
  return(result)
}


# Returns ground vectors to consider for a response line
# This should return the actual ground vectors from the context i.e. not just the codes
apply_windows <- function(
  multidim_arr, 
  sender_cols,
  receiver_cols,
  time_column,
  duration_column,
  mode_column,
  r_vec_qeid, 
  context, 
  codes, 
  time_unit = 'auto'
) {  
  # first line is response line--since we aren't including response lines, just provide a dummy vec w/ 0s @ codes
  if (nrow(context) == 1) { 
    temp_vec <- context[.N,]
    temp_vec[, codes] <- 0

    return(temp_vec)
  }
  
  # get response row; r_i is receiver values
  context_response_row <- context[.N];
  receiver_cols <- as.character(as.vector(receiver_cols))
  r_i <- unlist(context_response_row[, ..receiver_cols]);
  response_row_time_val <- context_response_row[[time_column]]; # get timestamp in response row
  context_ground_rows <- context[QEID != r_vec_qeid]; # get potential ground rows
  g_vecs <- context_ground_rows[,{
    # matrix with sender and modality values for all relevant ground rows
    sm_mat <- as.matrix(.SD[, c(unlist(sender_cols), mode_column), with = FALSE])

    # initialize receivers cols of the querying matrix
    # receivers <- list()
    # for (k in 1:length(receiver_cols)) {
    #   receivers[[k]] <- as.matrix(rep(r_i[[k]], nrow(sm_mat)))
    # }
    # receivers <- do.call(cbind, receivers)
    receivers <- matrix(r_i, nrow = .N, ncol = length(r_i), byrow = TRUE, dimnames = list( NULL, names(r_i)));

    # get matrix used to query multidim arr
    # if (nrow(sm_mat) == 1) {
    #   mult_arr_mat <- cbind(t(as.matrix(sm_mat[,1:length(sender_cols)])), receivers, sm_mat[,ncol(sm_mat)], "window")
    # } else {
    #   mult_arr_mat <- cbind(sm_mat[,1:length(sender_cols)], receivers, sm_mat[,ncol(sm_mat)], "window")
    # }
    # mult_arr_mat <- cbind(sm_mat[,1:length(sender_cols), drop = F], receivers, sm_mat[, ncol(sm_mat), drop = FALSE], "window");
    mult_arr_mat <- cbind(.SD[, c(sender_cols, mode_column), with = FALSE], receivers, "window")
    si <- seq_along(sender_cols);
    data.table::setcolorder(mult_arr_mat, c(si, seq_along(receiver_cols) + length(sender_cols) + 1, length(sender_cols) + 1, ncol(mult_arr_mat)))
    mult_arr_mat <- as.matrix(mult_arr_mat);

    # ensure vectors project forward in time far enough that they are kept in the ground
    .SD[as.vector((.SD[[time_column]] + as.numeric(multidim_arr[mult_arr_mat])) >= response_row_time_val), ];
  }]
  # browser()
  
  # context[[time_column]] <- as.POSIXct(context[[time_column]], origin = "1970-01-01")
  # 
  # 
  # g_vecs_i <- context[, {
  #   times = .SD[[time_column]]
  #   difftime(
  #     times +
  #     multidim_arr[as.matrix(
  #       .SD[, c(sender_cols, receiver_cols, mode_column), with = FALSE][, `:=`("window"="window")][, (receiver_cols) := .SD[.N, c(receiver_cols), with = FALSE]]
  #     )],
  #     times[.N],
  #     time_unit
  #   ) >= 0 & QEID != r_vec_qeid
  # }]
  # g_vecs <- context[g_vecs_i,]
  # # browser(expr = !identical(g_vecs_old, g_vecs))
  
  return(g_vecs)
}

# Applies weights to ground vectors for a response line 
# This should return the weighted ground vectors with just the codes
apply_weights <- function(
    multidim_arr, 
    sender_cols,
    receiver_cols,
    mode_column,
    codes,
    context,
    g_vecs 
) {
  # we have ground vectors w/ metadata

  # get response row and associated receivers
  context_response_row <- context[.N]; 
  receiver_cols <- as.character(as.vector(receiver_cols))
  r_i <- unlist(context_response_row[, ..receiver_cols]);
  
  # get weighting vector
  sm_mat <- as.matrix(g_vecs[, c(unlist(sender_cols), mode_column), with = FALSE]) 
  receivers <- list()
  for (k in 1:length(receiver_cols)) {
    receivers[[k]] <- as.matrix(rep(r_i[[k]], nrow(sm_mat)))
  }
  receivers <- do.call(cbind, receivers)
  
  # if (nrow(sm_mat) == 1) { 
  #   mult_arr_mat <- cbind(t(as.matrix(sm_mat[,1:length(sender_cols)])), receivers, sm_mat[, ncol(sm_mat)], "weight")
  # } else {
  # }
  mult_arr_mat <-cbind(sm_mat[,1:length(sender_cols), drop = FALSE], receivers, sm_mat[,ncol(sm_mat), drop = FALSE], "weight")
  weight_vec <-  as.numeric(multidim_arr[mult_arr_mat])
  
  # apply to all g_vecs
  g_w_vecs <- as.matrix(g_vecs[,..codes]) * weight_vec
  
  # g_w_vecs <- as.matrix(g_vecs[, {
  #   .SD[, c(codes), with = FALSE] * multidim_arr[as.matrix(
  #     .SD[, c(sender_cols, receiver_cols, mode_column), with = FALSE][, `:=`("weight"="weight")][, (receiver_cols) := .SD[.N, c(receiver_cols), with = FALSE]]
  #   )]
  # }])
  
  return(g_w_vecs)
}

# 0 out diagonal of a square matrix
f_diag <- function(
    mat # a square matrix
) {
  if (!is.matrix(mat) || nrow(mat) != ncol(mat)) {
    stop("Input must be a square matrix.")
  }
  
  diag(mat) <- 0
  return(mat)
}

accum_multidim <- function(
  time_column,
  codes, 
  context_model,
  tensor = context_tensor(context_model$model$raw.input), 
  duration_column = "", 
  norm_by = "No", # TODO: implement l1 normalization
  return_ena_set = FALSE,
  units = context_model[["model"]][["unit.labels"]],
  time_unit = 'auto',
  ... # TODO: implement binarization for T/ENA
) {
  .Deprecated("accumulate", "tma v0.2.0");
  multidim_arr <- tensor;

  df <- context_model$model$raw.input;
  result <- preprocess(df, multidim_arr);
  
  multidim_arr <- result$tensor
  sender_cols <- result$sender_cols
  receiver_cols <- result$receiver_cols
  mode_column <- result$mode_column
  
  adj_vector_names <- NULL;
  adj_vectors <- list()
  # units <- context_model[["model"]][["unit.labels"]]
  for (unit in units) {
    # List to store adjacency vectors for this unit
    adj_vector <- list()
    
    # Ensure context has column "standin" with values "standin" for all lines
    context <- context_model[["model"]][["contexts"]][[unit]]
    context[,"standin"] = "standin"
    
    # List to store connection matrices before accumulation for this unit
    counting_matrices <- list()
    counting_matrices_counter = 1
    
    # For each context, get unit's lines in that context (r_vec)
    context_unit_rows <- context[QEUNIT == unit];
    # print(paste("unit: ", unit));
    for (i in seq(1, nrow(context_unit_rows))) { # NOTE: now, i is the index within the rows corresponding to the units i.e. if the unit speaks 4 times in context, 1, ..., 4; NOT the absolute index in the context
      
      r_vec = context_unit_rows[i,] 
      
      # Get all lines *strictly* before this line, which are individual g_vec, and only get those that project
      # far enough
      r_vec_qeid <- context_unit_rows$QEID[i]; # this is the QE id; NOT the absolute index in the context 
      context_before_r_vec <- context[QEID <= r_vec_qeid];
      
      g_vecs <- apply_windows(multidim_arr, 
                              sender_cols, receiver_cols, 
                              time_column, duration_column, mode_column,
                              r_vec_qeid, context_before_r_vec, 
                              codes, time_unit)

      # ensure there are zero connections if g_vecs is empty
      if (nrow(g_vecs) == 0) {
        temp <- copy(r_vec) # make sure to deep copy this; shallow copy will also zero out r_vec
        temp[, (codes) := 0]
        g_vecs <- data.table(temp)
      }
      
      # Query multidimensional array to reweight g^w_vec = w^{srm} * g_vec
      g_w_vecs <- apply_weights(multidim_arr, sender_cols, receiver_cols, mode_column, codes, context_before_r_vec, g_vecs);
      
      # Sum all g^w_vec together as g^{ws}_vec
      g_ws_vec <- colSums(g_w_vecs)
      
      # Compute counting matrix = g^{ws}_vec r_vec^T + 0.5w_{resp}^{srm} * f_diag(r_vec r_vec^T)
      # get response vector; need to extract senders, receivers, modality
      
      sender_cols <-  as.character(as.vector(sender_cols))
      receiver_cols <- as.character(as.vector(receiver_cols))
      mode_column <-  as.character(as.vector(mode_column))
      
      s_response <- unlist(r_vec[, ..sender_cols]);
      r_response <- unlist(r_vec[, ..receiver_cols]);
      m_response <- unlist(r_vec[, ..mode_column]);
    
      w_rr = as.numeric(multidim_arr[matrix(c(s_response, r_response, m_response, "weight"), 1)])

      r_vec <- as.numeric(unname(c(r_vec[, ..codes])))
      g_ws_vec <- as.numeric(g_ws_vec)
      
      # ONA 
      if (return_ena_set == FALSE) { 
        counting_matrix = (g_ws_vec %*% t(r_vec)) + 0.5 * w_rr * f_diag(r_vec %*% t(r_vec))
      } 
      
      # ENA (w_rr * rr^T + (qr^T + rq^T))
      else { 
        counting_matrix = w_rr * (r_vec %*% t(r_vec)) + (g_ws_vec %*% t(r_vec) + r_vec %*% t(g_ws_vec)) 
      }
      
      counting_matrices[[counting_matrices_counter]] = counting_matrix
      counting_matrices_counter = counting_matrices_counter + 1
    }
    
    # Sum all counting matrices to get Omega matrix 
    omega_matrix = Reduce("+", counting_matrices)
    rownames(omega_matrix) = codes
    colnames(omega_matrix) = codes
    
    if (return_ena_set == FALSE) { # ONA
      # Make into adjacency vector
      row_col_combinations <- expand.grid(rownames(omega_matrix), colnames(omega_matrix))
      adj_vector_names <<- paste(row_col_combinations[,1], "&", row_col_combinations[,2])
      
      adj_vector <- omega_matrix[cbind(match(row_col_combinations[, 1], rownames(omega_matrix)), 
                                       match(row_col_combinations[, 2], colnames(omega_matrix)))]
      
      # adj_vector <- setNames(adj_vector, adj_vector_names)
    }
    else { 
      upper_indices <- which(upper.tri(omega_matrix, diag = FALSE)) 
      adj_vector <- omega_matrix[upper_indices]
      upper_rows <- rownames(omega_matrix)[row(omega_matrix)[upper_indices]]
      upper_cols <- colnames(omega_matrix)[col(omega_matrix)[upper_indices]]
      adj_vector_names <<- paste(upper_rows, "&", upper_cols)
      
      # adj_vector <- setNames(adj_vector, adj_vector_names)
    }
  
    if (norm_by == "l2") {
      l2_adj_vector <- sqrt(sum(adj_vector^2))
      normed_adj_vector <- adj_vector / l2_adj_vector
    } 
    else {
      normed_adj_vector <- adj_vector
    }
    
    # Save unit's adjacency vec
    adj_vectors[[unit]] <- normed_adj_vector
  }

  adj_key <- NULL
  if(return_ena_set == FALSE) {
    adj_key <- t(expand.grid(codes, codes))
  }
  else {
    adj_key <- namesToAdjacencyKey(codes)
  }
  adj_vector_names <- apply(adj_key, 2, paste, collapse = " & ")
  
  context_model$rotation <- structure(list(
    codes = codes,
    adjacency.key = structure(
      sapply(adj_vector_names, function(x) strsplit(x, " & ")[[1]], simplify = TRUE),
      dimnames = list( NULL, adj_vector_names)
    )
  ), class = c("ena.rotation.set", "list"));
  
  
  meta_cols <- context_model$`_function.params`$units.by;
  connection_counts <- data.table::rbindlist(lapply(units, function(ctx) {
    ctx_model <- context_model$model$contexts[[ctx]]
    ctx_unit <- attr(ctx_model, "tma.unit")
    ctx_adj <- adj_vectors[[ctx]];
    ctx_adj_dt <- as.data.table(matrix(ctx_adj, byrow = TRUE, nrow = 1, dimnames = list(NULL, adj_vector_names)))
  }));
  connection_counts <- connection_counts[, lapply(.SD, reclass, "ena.co.occurrence"), .SDcols = adj_vector_names];
  connection_counts$ENA_UNIT <- units;
  # connection_counts <- cbind(connection_counts, data.table::rbindlist(lapply(context_model$model$contexts, attr, "tma.unit")))
  connection_counts <- cbind(connection_counts, data.table::rbindlist(lapply(units, function(u) attr(context_model$model$contexts[[u]], "tma.unit"))))
  
  for(jj in c("ENA_UNIT", meta_cols)) {
    data.table::set(connection_counts, j = jj, value = reclass(connection_counts[[jj]], "ena.metadata"));
  }
  # data.table::set(connection_counts, j = "QEUNIT", value = reclass(connection_counts$QEUNIT, "ena.metadata"));
  
  #data.table::rbindlist(lapply(context_model$model$contexts, attr, "tma.unit"));
  
  data.table::setcolorder(connection_counts, c("ENA_UNIT", meta_cols, adj_vector_names));
  
  context_model$connection.counts <- connection_counts;
  
  class(context_model$connection.counts) <- c("ena.connections", "ena.matrix", "data.table", "data.frame");
  
  context_model$meta.data <- context_model$connection.counts[, sapply(context_model$connection.counts, is, "ena.metadata"), with = FALSE];
  class(context_model$meta.data) = c("ena.matrix", class(context_model$meta.data));
  
  context_model$model$row.connection.counts <- structure(
    data.table::copy(context_model$connection.counts),
    class = c("row.connections", "ena.matrix", "data.table", "data.frame")
  );
  
    
  # Return accumulation (adjacency vecs for all units)
  return(context_model)
}
