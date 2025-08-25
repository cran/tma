#' accumulate_threads
#'
#' @param data TBD
#' @param units_by TBD
#' @param conversation_rules TBD
#' @param code_cols TBD
#' @param ... TBD
#' @param conversation_splits TBD
#' @param as_directed TBD
#' @param window_size TBD
#' @param meta_data TBD
#'
#'
#' @return TBD
#' @export
accumulate_threads <- function(
  data,
  units_by,
  conversation_rules,
  code_cols,
  ...,
  conversation_splits = NULL,
  as_directed = FALSE,
  window_size = 4,
  meta_data = units_by
) {
  if(!data.table::is.data.table(data)) {
    data <- data.table::as.data.table(data);
  }
  
  model <- contexts(
    x = data,
    units_by = units_by,
    hoo_rules = conversation_rules,
    split_rules = conversation_splits
  );
  model <- accumulate_context_threads(
    x = model, 
    codes = code_cols,
    as_directed = as_directed,
    window_size = window_size,
    meta.data = meta_data,
    ...
  );
  
  return(model);
}