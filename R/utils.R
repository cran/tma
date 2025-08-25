# Adjacency Key
#
# @param codes character vector
# @param upper logical, default, TRUE, returns the key for the upper triangle, FALSE - not implemented yet
#
# @return matrix
adjacency_key <- function(codes, upper = TRUE) {
  expanded <- expand.grid(codes, codes);
  rownames(expanded) <- apply(expanded, 1, paste, collapse = " & ");
  if(upper == TRUE) {
    upper_inds <- which(upper.tri(matrix(nrow = length(codes), ncol = length(codes))));
    t(expanded[upper_inds,])
  }
  else {
    t(expanded);
  }
}

list_vec_apply <- Vectorize(FUN = function(x, y = NULL, fn) {
  fn(x, y)
}, vectorize.args = c("x", "y"), SIMPLIFY = FALSE);


as.undirected.matrix <- function(x, m = matrix(1, nrow = nrow(x), ncol = ncol(x))) {
  upper_tri_indices <- upper.tri(x);
  ut_matrix <- x * m;
  ut_matrix[upper_tri_indices] <- ut_matrix[upper_tri_indices] + t(ut_matrix)[upper_tri_indices]
  
  #make matrix symmetrical
  ut_matrix[lower.tri(ut_matrix)] = NA
  
  ut_matrix
}

#' Extract Upper Triangular Elements
#' 
#' This function extracts the elements from the upper triangular part of a
#' matrix.
#' 
#' @param x A numeric matrix from which to extract upper triangular elements.
#' @param diag A logical value indicating whether to include the diagonal
#'   elements. Defaults to FALSE.
#' 
#' @return A vector containing the upper triangular elements of the matrix.
as.undirected.vector <- function(x, diag = FALSE) {
  upper_tri_indices <- upper.tri(x, diag = diag);
  x[upper_tri_indices]
}


as.ena.ordered.set <- function(x) {
  to <- c("ena.ordered.set")
  if(!inherits(x = x, what = "ena.set")) {
    to <- c(to, "ena.set")
  }
  class(x) <- c(to, class(x));
  return(x);
}
ena.set.directed <- function(data, units, conversations, codes, ...) {
  as.ena.ordered.set(
    list(
      meta.data = NULL,
      model = list(
        model.type = "Directed",
        raw.input = data.table::as.data.table(data)
      ),
      rotation = list (
        codes = codes,
        nodes = NULL
      ),
      plots = list(),
      "_function.params" = list(
        units = units,
        conversations = conversations,
        codes = codes
      )
    )
  )
}

# Reclass vector
#
# @param x vector
# @param cl character - new class
# @param stringsAsFactors logical, default: default.stringsAsFactors()
#
# @return vector with class: c(cl, class(x))
reclass <- function(x, cl, stringsAsFactors = FALSE) {
  if(!stringsAsFactors && is.factor(x)) {
    x <- as.character(x)
  }
  class(x) = c(cl, class(x))
  x
}

# paste columns to together
#
# @param x coerced to data.frame prior to subseting cols
# @param cols character vector - default: colnames(x)
# @param sep character default "."
#
# @return character vector
merge_columns <- function(x, cols = colnames(x), sep = ".") {
  do.call(paste, c(as.data.frame(x)[, cols, drop = FALSE], sep = sep))
}

##
#' Convert Adjacency Key to Character (S3 method)
#'
#' This S3 method converts an adjacency key object (typically a 2-row matrix or list of pairs) into a character vector, concatenating each pair with ' & '.
#'
#' @param x An adjacency key object (matrix or list) to convert to character.
#' @param ... Additional arguments (unused).
#'
#' @return A character vector where each element is a concatenation of the adjacency key pair.
#' @export
'as.character.adjacency.key' <- function(x, ...) {
  mapply(paste, x[[1]], x[[2]], MoreArgs = list(sep = " & "))
}

#' Convert Adjacency Key to Double (S3 method)
#'
#' This S3 method converts an adjacency key object to a numeric (double) vector, applying as.numeric to each element.
#'
#' @param x An adjacency key object (matrix or list) to convert to numeric.
#' @param ... Additional arguments (unused).
#'
#' @return A numeric vector representation of the adjacency key.
#' @export
'as.double.adjacency.key' <- function(x, ...) {
  sapply(x, as.numeric)
}

##
#' Print Method for Network Matrix (S3 method)
#'
#' This S3 method prints a network matrix object, optionally including metadata. It adjusts the class and attaches adjacency key names for improved readability.
#'
#' @param x An object of class "network.matrix" to print.
#' @param include.meta Logical; whether to include metadata in the printout (currently not used).
#' @param ... Additional arguments passed to lower-level print methods.
#'
#' @return Invisibly returns the printed object.
#' @export
'print.network.matrix' <- function(x, include.meta = TRUE, ...) {
  x_ <- data.table::copy(x);
  x_model <- attr(x, "model");
  x_cls <- class(x);
  class(x_) <- x_cls[ (which(x_cls == "network.matrix") + 1):length(x_cls) ];
  if(!is.null(x_model)) {
    adj_key <- x_model$rotation$adjacency.key;
    attr(x_, "names") <- as.character(adj_key);
  }
  
  print(x_);
}

##
#' Extract Metadata or Columns from Network Matrix (S3 method)
#'
#' This S3 method allows convenient extraction of metadata columns from a network matrix object using the $ operator. If the requested column is metadata, it is returned from the model's meta.data; otherwise, the standard extraction is performed.
#'
#' @param x An object of class "network.matrix".
#' @param i Name of the column or metadata field to extract.
#'
#' @return The requested column or metadata field from the network matrix.
#' @export
"$.network.matrix" <- function (x, i) {
  meta.data <- attr(x, "model")$meta.data;
  meta.cols <- colnames(meta.data);
  
  # browser()
  if(data.table::`%chin%`(i, meta.cols)) {
    meta.data[[i]];
  }
  else {
    x[[i]];
  }
}

#' Title
#'
#' @param x TBD
#'
#' @return TBD
#' @export
"names.network.connections" <- function(x) {
  use.adjacency.key = getOption("tma.print.adjkey", TRUE);
  
  x_cls <- class(x);
  x_ <- data.table::copy(x);
  class(x_) <- x_cls[ (which(x_cls == "network.connections") + 1):length(x_cls) ]; 
  
  if(use.adjacency.key == TRUE) {
    x_model <- attr(x, "model");
    adj_key <- x_model$rotation$adjacency.key;
    as.character(adj_key);
  }
  else {
    names(x_);
  }
}

#' Re-class vector as network.connection
#'
#' @param x Vector to re-class
#'
#' @return re-classed vector
#' @export
as.network.connection <- function(x) {
  if(is.factor(x)) {
    x = as.character(x)
  }
  class(x) = c("network.connection", class(x))
  x
}

##
#' Convert Network Connections to Matrix (S3 method)
#'
#' This S3 method extracts the connection columns from a network connections object and returns them as a numeric matrix. It is used to facilitate matrix operations on network connection data.
#'
#' @param x An object of class "network.connections" (or compatible data.table/data.frame) containing connection columns (of class "network.connection").
#' @param ... Additional arguments passed to `as.matrix`.
#'
#' @return A numeric matrix of network connections (rows = units/contexts, columns = connections).
#'
#' @export
as.matrix.network.connections <- function(x, ...) {
  x_ <- data.table::copy(x);
  x_cls <- class(x);
  class(x_) <- x_cls[ (which(x_cls == "network.matrix") + 1):length(x_cls) ];
  code_columns <- which(sapply(x_, inherits, what = "network.connection"));
  as.matrix(x_[, c(code_columns), with = F], ...);
}

##
#' Column Sums for ENA Matrices (S3 method)
#'
#' This S3 method computes column sums for ENA matrix objects, with optional binarization. It is used to summarize connection counts across rows (e.g., for each unit or context).
#'
#' @param x An object of class "ena.matrix" (or compatible matrix/data.frame) containing connection data.
#' @param na.rm Logical; whether to remove missing values (passed to `colSums`).
#' @param dims Integer; which dimensions to sum over (passed to `colSums`).
#' @param binary Logical; if TRUE, binarizes the matrix before summing (i.e., all nonzero values become 1).
#'
#' @return A numeric vector of column sums for the matrix.
#'
#' @export
'colSums.ena.matrix' <- function(x, na.rm = FALSE, dims = 1L, binary = FALSE) {
  x_mat <- as.matrix(x);
  if(isTRUE(binary)) {
    x_mat[x_mat > 0] <- 1;
  }
  colSums(x_mat);  
}

#' Unorder Connections in a Matrix
#'
#' This function takes a matrix and creates an unordered version of its connections, combining upper and lower triangular elements.
#'
#' @param x A matrix or data frame containing the connections. The input should be a square matrix.
#'
#' @return A data.table with ordered connections, reclassified as "unordered.ena.connections", "ena.connections", and "ena.matrix".
#'
#' @export
'as.unordered.ordered.ena.connections' <- function(x) {
  m <- as.matrix(x);
  sq_size <- sqrt(ncol(m));
  ind_mat <- matrix(seq.int(ncol(m)), nrow = sq_size, ncol = sq_size);
  ind_ut <- ind_mat[upper.tri(ind_mat)];
  ind_lt <- ind_mat[lower.tri(ind_mat)];
  
  # m_unordered <- data.table::as.data.table(m[, ind_ut, drop = FALSE] + m[, ind_lt, drop = FALSE]);
  m_unordered <- data.table::as.data.table(t(apply(m, 1, function(mm) (mm[as.vector(ind_mat)] + mm[as.vector(t(ind_mat))])[ind_ut], simplify = T)));
  m_unordered <- m_unordered[, lapply(.SD, reclass, "ena.co.occurrence"), .SDcols = colnames(m_unordered)];
  
  m_unordered <- cbind(x[,find_meta_cols(x), with = FALSE], m_unordered);
  m_unordered <- reclass(x = m_unordered, c("unordered.ena.connections", "ena.connections", "ena.matrix"));
  
  m_unordered
}

##
#' Convert Ordered Row Connections to Unordered (S3 method)
#'
#' This S3 method takes a matrix or data frame of ordered row connections (e.g., from ONA) and produces an unordered version by summing upper and lower triangular elements for each connection.
#'
#' @param x An object of class "ordered.row.connections" (or compatible matrix/data.frame) containing ordered connection data. The input should be a square matrix or have square number of columns.
#'
#' @return A data.table with unordered row connections, reclassified as "unordered.row.connections", "row.connections", and "ena.matrix".
#'
#' @export
'as.unordered.ordered.row.connections' <- function(x) {
  # m <- as.matrix(x);
  m <- as.matrix.ena.matrix(x);
  sq_size <- sqrt(ncol(m));
  ind_mat <- matrix(seq.int(ncol(m)), nrow = sq_size, ncol = sq_size);
  ind_ut <- ind_mat[upper.tri(ind_mat, diag = FALSE)];
  ind_lt <- ind_mat[lower.tri(ind_mat, diag = FALSE)];
  
  # m_unordered <- data.table::as.data.table(m[, ind_ut, drop = FALSE] + m[, ind_lt, drop = FALSE]);
  m_unordered <- data.table::as.data.table(t(apply(m, 1, function(mm) (mm[as.vector(ind_mat)] + mm[as.vector(t(ind_mat))])[ind_ut], simplify = T)));
  m_unordered <- m_unordered[, lapply(.SD, reclass, "ena.co.occurrence"), .SDcols = colnames(m_unordered)];
  
  m_unordered <- cbind(x[,find_meta_cols(x), with = FALSE], m_unordered);
  m_unordered <- reclass(x = m_unordered, c("unordered.row.connections", "row.connections", "ena.matrix"));
  
  m_unordered
}

#' Default Method for as.unordered
#'
#' This function provides the default method for handling the input \code{x} when no specific method is available.
#'
#' @param x Any object that you want to apply the default method to.
#'
#' @return The input object \code{x}, unchanged.
#' @export
'as.unordered.default' <- function(x){
  x
}

#' Convert to Unordered Factor
#'
#' This function is a generic method to convert an object to an unordered factor.
#' It dispatches methods based on the class of the input object.
#'
#' @param x An object to be converted to an unordered factor.
#'
#' @return An unordered factor representation of the input object.
#'
#' @export
'as.unordered' <- function(x) {
  UseMethod("as.unordered")
}

#region Moved from rENA ----

#' Find metadata columns
#'
#' @param x data.table (or frame) to search for columns of class ena.metadata
#'
#' @return logical vector
#' @export
find_meta_cols <- function(x) {
   sapply(x, is, class2 = "ena.metadata")
}

#' Remove meta columns from a data.table or data.frame
#'
#' This function removes columns of class `ena.meta.data` from the input object.
#'
#' @param x A `data.table` or `data.frame` object from which meta columns should be removed.
#'
#' @return A `data.frame` with columns of class `ena.meta.data` removed.
#' @export
remove_meta_data <- function(x) {
   as.data.frame(x)[, !find_meta_cols(x), drop = F]
}

#' Matrix without metadata
#'
#' @param x Object to convert to  a matrix
#' @param ... 	additional arguments to be passed to or from methods
#'
#' @return matrix
#' @export
as.matrix.ena.matrix <- function(x, ...) {
  class(x) = class(x)[-1]
  x = remove_meta_data(x)
  as.matrix(x, ...)
}

namesToAdjacencyKey <- function(vector, upper_triangle = TRUE) {
  upperTriIndices = triIndices(length(vector)) + 1;
  matrix(vector[upperTriIndices], nrow=2)
}

#' @title Names to Adjacency Key
#'
#' @description Convert a vector of strings, representing the names of a square matrix, to an adjacency key matrix.
#'
#' @details Returns a matrix with 2 rows and choose(length(vector), 2) columns, where each column represents a unique pair of names from the input vector, corresponding to the upper triangle of a square matrix.
#'
#' @param vector Vector representing the names of a square matrix.
#' @param upper_triangle Not Implemented.
#'
#' @return A character matrix with 2 rows and choose(length(vector), 2) columns. Each column contains a pair of names representing a unique adjacency (edge) between nodes in the original square matrix.
#' @export
namesToAdjacencyKey <- function(vector, upper_triangle = TRUE) {
  upperTriIndices = triIndices(length(vector)) + 1;
  matrix(vector[upperTriIndices], nrow=2)
}
