.onLoad <- function(libname, pkgname) {
  globalVariables(c(
    "..units_by", "..by", ".I", "QEID", "CID", "..conversations",
    "KEYCOL", "QEUNIT", "..sender_cols", "..receiver_cols", "..mode_column",
    "..codes"
  ))
}


ATTR_NAMES <- list(
  CONTEXT_ID = "__CONTEXT_TBL__",
  CONTEXT_COL_ID = "__CONTEXT_ROWID__"
)