##
#' @title Find conversations by unit
#'
#' @description
#' Identify and extract rows corresponding to conversations for specified units in a dataset or context model. Useful for subsetting and analyzing conversational windows in network analysis.
#'
#' @details
#' This function groups rows by conversation (using `conversation.by` columns), identifies which rows are associated with the specified units and codes, and returns indices for each conversation, as well as metadata about which rows to include or exclude.
#'
#' @param x A data.frame or context model containing conversation data.
#' @param units Character vector of unit identifiers to extract conversations for.
#' @param units.by Character vector of column names specifying unit grouping (default: from context model attributes).
#' @param codes Character vector of code columns to use for identifying coded rows.
#' @param conversation.by Character vector of column names to group by conversation.
#' @param window Integer; window size for co-occurrence (default: 4).
#' @param conversation.exclude Character vector of conversation keys to exclude.
#' @param id_col Character; column name for unit IDs (default: "QEUNIT").
#'
#' @return A list with elements:
#'   \item{conversations}{List of row indices for each conversation.}
#'   \item{unitConvs}{Unique conversation keys for the specified units.}
#'   \item{allRows}{All row indices included for the units.}
#'   \item{unitRows}{Row indices for the units with codes.}
#'   \item{convRows}{All row indices for the unit's conversations.}
#'   \item{toRemove}{Rows not meeting co-occurrence criteria.}
#'
#' @export
tma.conversations = function(
  x, units, 
  units.by = NULL, codes = NULL, conversation.by = NULL, 
  window = 4, conversation.exclude = c(),
  id_col = "QEUNIT"
) {
  if(is.null(units.by)) {
    units.by = x$`_function.params`$units.by;
    if(is.null(units.by)) {
      stop("Unable to find values for `units.by`")
    }
  }
  
  if(is(x, "data.frame")) {
    rawAcc2 = data.table::data.table(x);
  }
  else {
    rawAcc2 = x$model$raw.input;
  }

  rawAcc2$KEYCOL = merge_columns(rawAcc2, conversation.by);
  conversationsTable2 = rawAcc2[, paste(.I, collapse = ","), by = c(conversation.by)];
  rows2 = lapply(conversationsTable2$V1, function(x) as.numeric(unlist(strsplit(x, split=","))));
  names(rows2) = merge_columns(conversationsTable2, conversation.by);
  unitRows2 = rawAcc2[[id_col]];

  codedRows = rawAcc2[, rowSums(.SD), .SDcols = codes] > 0
  codedUnitRows2 = which(unitRows2 %in% units & codedRows)
  codedUnitRows2 = codedUnitRows2[!(codedUnitRows2 %in% as.vector(unlist(rows2[conversation.exclude])))]
  codedUnitRowConvs2 = rawAcc2[codedUnitRows2, KEYCOL];

  codedUnitRowConvsAll = NULL;
  codedUnitRowConvsAll2 = NULL;
  unitRowsNotCooccurred = c()
  if(length(codedUnitRows2) > 0) {
    codedUnitRowConvsAll = unique(unlist(sapply(X = 1:length(codedUnitRows2), simplify = F, FUN = function(x) {
      thisConvRows = rows2[[codedUnitRowConvs2[x]]]
      thisRowInConv = which(thisConvRows == codedUnitRows2[x])
      winUse = ifelse(is.infinite(window), thisRowInConv, window)
      thisRowAndWindow = rep(thisRowInConv,winUse) - (winUse-1):0
      # coOccursFound = all(rawAcc2[thisConvRows[thisRowAndWindow[thisRowAndWindow > 0]], lapply(.SD, sum), .SDcols=codes] > 0)
      coOccursFound = sum(rawAcc2[thisConvRows[thisRowAndWindow[thisRowAndWindow > 0]], lapply(.SD, sum), .SDcols=codes]) > 1
      # browser(expr = { thisConvRows[thisRowInConv] == 12 })
      if(coOccursFound) {
        thisConvRows[thisRowAndWindow[thisRowAndWindow > 0]]
      }
      else {
        unitRowsNotCooccurred <<- c(unitRowsNotCooccurred, thisConvRows[thisRowInConv]);
        
        NULL
      }
    })))
  }

  # browser()
  unitConvs <- unique(rawAcc2[codedUnitRows2, KEYCOL]);
  return(list(
    conversations = as.list(rows2),
    unitConvs = unitConvs,
    allRows = codedUnitRowConvsAll,
    unitRows = codedUnitRows2,
    convRows = unique(unlist(sapply(unitConvs, function(x) { rows2[[x]] }))),
    toRemove = unitRowsNotCooccurred
  ));
}


#' @title Interactive Conversation Viewer
#'
#' @description
#' Launch an interactive HTML viewer for conversations and codes for a specified unit or set of units. Useful for exploring and validating conversation windows and code assignments in the TMA workflow.
#'
#' @param x A context model or data.frame containing conversation data.
#' @param wh Character or integer; unit(s) to view.
#' @param text_col Character; column name for text (default: "text").
#' @param units.by Character vector of unit grouping columns (default: from context model attributes).
#' @param conversation.by Character vector of conversation grouping columns (default: from context model attributes).
#' @param codes Character vector of code columns (default: from context model attributes).
#' @param window_size Integer; window size for co-occurrence (default: from context model attributes).
#' @param more_cols Character vector of additional columns to include in the viewer.
#' @param in_browser Logical; if TRUE, open in system browser, otherwise use RStudio viewer (default: FALSE).
#' @param id_col Character; column name for unit IDs (default: "QEUNIT").
#'
#' @return A list containing the viewer data and metadata (invisibly). The function is called for its side effect of launching the viewer.
#' @export
view <- function(
  x, wh, 
  text_col = "text", 
  units.by = x$`_function.params`$units.by,
  conversation.by = x$`_function.params`$conversation.by,
  codes = x$rotation$codes,
  window_size = x$`_function.params`$window_size,
  more_cols = NULL,
  in_browser = FALSE,
  id_col = "QEUNIT"
) {
  unit_conv <- tma.conversations(x = x, units = wh, units.by = units.by, conversation.by = conversation.by, codes = codes, window = window_size, id_col = id_col);
  # rows <- x$model$contexts[[wh]];
  # if(is.null(rows)) {
  #   stop(paste0("Now rows found for context: ", wh));
  # }

  cols <- unique(c("QEID", id_col, units.by, conversation.by, text_col, more_cols, codes));
  cols <- cols[cols %in% colnames(x$model$raw.input)];
  tbl <- x$model$raw.input[unit_conv$convRows, cols, with = FALSE];
  unit_conv$unitRows <- unit_conv$unitRows;
  unit_conv$toRemove <- unit_conv$toRemove;
  unit_conv$data <- tbl;
  unit_conv$units <- wh;
  unit_conv$window <- window_size;
  tbl_json <- jsonlite::toJSON(unit_conv, auto_unbox = TRUE);

  tmp_html <- tempfile(fileext = ".html")
  html_lines <- readLines(system.file(package="tma", paste0("apps/viewer.html")));
  html_lines[grepl("//_ENA_MODEL_//", x = html_lines)] <- paste0("data = ", tbl_json, ";");
  writeLines(text = html_lines, con = tmp_html);
  
  if(in_browser == TRUE) {
    browseURL(tmp_html);
  }
  else {
    rstudioapi::viewer(tmp_html)
  }
}
