
#' @title Internal: Simple window decay (legacy)
#' @description
#' Internal helper for window decay, used in TMA v0.1.0. Not exported. Kept for backward compatibility with legacy decay function creation.
#' 
#' @param x Numeric vector of time differences.
#' @param args List of arguments (expects `window_size`).
#' @return Numeric vector (0/1) indicating whether each value is within the window.
#' @export
simple_window <- function(x, args = NULL) { 
  .Deprecated("tma::context_tensor", "tma v0.2.0");
  (x <= args$window_size) * 1 
}

#' @title Internal: Decay function factory (legacy)
#' @description
#' Internal factory for creating decay functions, used in TMA v0.1.0. Not exported. Kept for backward compatibility with legacy code.
#' 
#' @param what Function to use as the decay kernel.
#' @param ... Named parameters to pass to `what`.
#' @return A function that applies the specified decay kernel to its input.
#' @export 
decay <- function(what, ...) {
  .Deprecated("tma::context_tensor", "tma v0.2.0");
  args <- list(...);
  function(x) {
    do.call(what, list(x = x, args = args))
  }
}
