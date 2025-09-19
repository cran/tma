.onLoad <- function(libname, pkgname) {
  globalVariables(c(
    "..units_by", "..by", ".I", "QEID", "CID", "..conversations",
    "KEYCOL", "QEUNIT", "..sender_cols", "..receiver_cols", "..mode_column",
    "..codes"
  ))
}


## Attributes for special columns in data.tables, currently used when 
## creating contexts

#' Special attribute names for context columns
#'
#' A named list of string constants used as attribute keys for special columns in data.tables
#' within the TMA package, primarily for context creation and identification.
#'
#' @format A named list with elements:
#' \describe{
#'   \item{CONTEXT_ID}{A string used to identify the context table column.}
#'   \item{CONTEXT_COL_ID}{A string used to identify the context row ID column.}
#' }
#' @keywords internal
#' @export
ATTR_NAMES <- list(
  CONTEXT_ID = "__CONTEXT_TBL__",
  CONTEXT_COL_ID = "__CONTEXT_ROWID__"
)