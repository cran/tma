#' Accumulate Connections from a Multidimensional Array and Context Model
#'
#' This function processes a context model and a multidimensional array of window/weight parameters to compute connection counts for each unit of analysis.
#' It applies the context model to the array, using sender, receiver, and mode columns (as defined in the array attributes), and accumulates co-occurrence or adjacency matrices for each unit. The result is a set of connection counts and row-level connection matrices, suitable for network analysis (e.g., ENA/ONA).
#'
#' @param context_model A context model object (as produced by `tma::contexts`) containing contexts for each unit of analysis.
#' @param codes Character vector of code names to use for constructing adjacency matrices.
#' @param tensor A multidimensional array (see `context_tensor`) containing window and weight values for each sender/receiver/mode combination. Defaults to an array generated from the context model's raw input.
#' @param time_column Character string giving the name of the time column in the context model. If NULL, uses the default context column ID.
#' @param ordered Logical; if TRUE (default), computes ordered adjacency matrices (ONA); if FALSE, computes unordered (ENA-style) matrices.
#' @param binary Logical; if TRUE (default), binarizes the connection counts (not currently implemented in this function).
#'
#' @return The input `context_model` with additional fields:
#'   \item{connection.counts}{A data.table of accumulated connection counts for each unit.}
#'   \item{model$row.connection.counts}{A data.table of row-level connection matrices for each unit.}
#'   \item{meta.data}{A data.table of metadata columns for each unit.}
#'   The class of the returned object is updated to reflect the type of accumulation (ordered or unordered).
#'
#' @details
#' This function is used to perform accumulation of network connections for each unit, based on the context model and tensor parameters. It supports both ordered and unordered accumulation, and returns results suitable for further network analysis or visualization.
#'
#' @export
accumulate <- function(
  context_model,
  codes,
  tensor = context_tensor(context_model$model$raw.input),
  time_column = NULL,
  ordered = FALSE,
  binary = TRUE
) {
  send_cols = attr(tensor, "sender_cols");
  recv_cols = attr(tensor, "receiver_cols");
  mode_cols = attr(tensor, "mode_column");

  if(is.null(time_column)) {
    time_column <- ATTR_NAMES$CONTEXT_COL_ID;
  }
  
  context_model$rotation <- structure(list(
    codes = codes,
    adjacency.key = adjacency_key(codes, !ordered)
  ), class = c("ena.rotation.set", "list"));
  adj_vector_names <- colnames(context_model$rotation$adjacency.key);
  adj_vector_names_full <- colnames(adjacency_key(codes, FALSE));
  
  ind_mat <- matrix(nrow = length(codes), ncol = length(codes));
  units_by <- context_model$`_function.params`$units.by;
  meta_cols <- c("QEID", "QEUNIT", units_by);
  
  mode_dim_1 <- dim(tensor)[1] == 1 && attr(tensor, "mode_column") == ATTR_NAMES$CONTEXT_ID;

  result_cpp <- lapply(context_model$model$contexts, function(ctx) {
    ctx_fun <- function(ctx__) {
      unit_context_lookup <- as.matrix(ctx__[, lapply(.SD, function(x) as.numeric(as.factor(x))), .SDcols = c(send_cols, recv_cols, mode_cols)]);
      unit_rows <- attr(ctx__, "tma.unit_rows");
      this_unit <- attr(ctx__, "tma.unit");
      
      unit_to_last <- seq.int(last(unit_rows));
      res2 <- apply_tensor(
        tensor,
        attr(tensor, "dim"),
        attr(tensor, "sender_inds") - 1,
        attr(tensor, "receiver_inds") - 1,
        attr(tensor, "mode_inds") - 1,
        unit_context_lookup[unit_to_last,, drop = FALSE] - 1,
        unit_rows - 1,
        as.matrix(ctx__[unit_to_last, ..codes]),
        ctx__[unit_to_last,][[time_column]]
      );
      
      rcc <- data.table::as.data.table(res2$row_connection_counts);
      colnames(rcc) <- adj_vector_names_full;
      rcc <- rcc[, lapply(.SD, reclass, "ena.co.occurrence"), .SDcols = adj_vector_names_full];
      
      unit_row_meta <- ctx__[unit_rows, meta_cols, with = FALSE];
      unit_row_meta <- unit_row_meta[, lapply(.SD, reclass, "ena.metadata"), .SDcols = meta_cols];
      rcc <- cbind(unit_row_meta, rcc);
      rcc <- reclass(x = rcc, c("ordered.row.connections", "row.connections", "ena.matrix"));
      
      cc <- data.table::as.data.table(res2$connection_counts);
      colnames(cc) <- adj_vector_names_full;
      cc <- cc[, lapply(.SD, reclass, "ena.co.occurrence"), .SDcols = adj_vector_names_full];
      cc <- cbind(this_unit, cc);
      cc <- reclass(x = cc, c("ordered.ena.connections", "ena.connections", "ena.matrix"));
      
      rcc
    }
    
    if(is.data.frame(ctx)) {
      res <- ctx_fun(ctx);
    }
    else {
      results <- lapply(ctx, ctx_fun);
      
      res <- data.table::rbindlist(results);
      res <- reclass(x = res, c("ordered.row.connections", "row.connections", "ena.matrix"));
    }
    
    return(res);
  })
  
  row_connection_counts <- rbindlist(result_cpp);
  data.table::setorderv(row_connection_counts, "QEID");
  row_connection_counts <- reclass(x = row_connection_counts, c("ordered.row.connections", "row.connections", "ena.matrix"));
  context_model$model$row.connection.counts <- row_connection_counts;
  
  cc_type <- NULL
  if(isTRUE(ordered)) {
    class(context_model) <- c("ena.ordered.set", class(context_model));
    
    # connection_counts <- data.table::as.data.table(do.call(rbind, lapply(result_cpp, colSums.ena.matrix, binary = FALSE)));
    connection_counts <- data.table::as.data.table(do.call(rbind, lapply(result_cpp, function(rcpp) {
      colSums(as.matrix.ena.matrix(rcpp))  
    })));
    connection_counts <- reclass(x = connection_counts, c("ordered.ena.connections", "ena.connections", "ena.matrix"));
    connection_counts <- connection_counts[, lapply(.SD, reclass, "ena.co.occurrence"), .SDcols = colnames(connection_counts)];
    
    cc_type <- "ordered.ena.connections";
  }
  else {
    class(context_model) <- c("ena.unordered.set", class(context_model));
    
    context_model$model$row.connection.counts <- as.unordered(context_model$model$row.connection.counts);
    
    connection_counts <- context_model$model$row.connection.counts[, lapply(.SD, colSums.ena.matrix, binary = binary), by = "QEUNIT", .SDcols = adj_vector_names];
    connection_counts <- connection_counts[, lapply(.SD, reclass, "ena.co.occurrence"), .SDcols = adj_vector_names];
    cc_type <- "unordered.ena.connections"
  }
  
  cc_meta <- data.table::rbindlist(lapply(result_cpp, function(rc) rc[1, c("QEUNIT", units_by), with = FALSE]));
  cc_meta$ENA_UNIT <- context_model$model$unit.labels;
  cc_meta <- cc_meta[, lapply(.SD, reclass, "ena.metadata"), .SDcols = colnames(cc_meta)];
  
  connection_counts <- cbind(cc_meta, connection_counts)
  connection_counts <- reclass(connection_counts, c(cc_type, "ena.connections", "ena.matrix"));
  context_model$connection.counts <- connection_counts;
  
  context_model$meta.data <- context_model$connection.counts[, sapply(context_model$connection.counts, is, "ena.metadata"), with = FALSE];
  class(context_model$meta.data) = c("ena.matrix", class(context_model$meta.data));
  
  return(context_model);
}
