testthat::context("Test C++ 1d indexing into multidimensional array")

test_that("calculate_1d_index works correctly", {
  # Test case 1: Simple 2D case
  indices <- c(1, 2)
  dims <- c(3, 4)
  testthat::expect_equal(calculate_1d_index(indices, dims), 7)
  
  # Test case 2: Simple 3D case
  indices <- c(1, 2, 3) 
  dims <- c(3, 4, 5)
  testthat::expect_equal(calculate_1d_index(indices, dims), 43)
  
  # Test case 3: Edge case with single dimension
  indices <- c(2)
  dims <- c(5)
  testthat::expect_equal(calculate_1d_index(indices, dims), 2)
  
  # Test case 4: Edge case with zero indices
  indices <- c(0, 0, 0)
  dims <- c(3, 4, 5)
  testthat::expect_equal(calculate_1d_index(indices, dims), 0)
  
  # Test case 5: Invalid input (mismatched lengths)
  indices <- c(1, 2)
  dims <- c(3, 4, 5)
  testthat::expect_error(calculate_1d_index(indices, dims), "Number of indices must match number of dimensions.")
})

test_that("calculate_1d_index using multidim array", {
  data(test_mockdata, package = "tma")
  mock_data <- test_mockdata;
  mock_data <- mock_data[mock_data$chatGroup == "PAM",]
  
  # Initialize TMA units and contexts
  unit_cols <- c("userID", "condition")
  codes <- c("A", "B", "C")
  HOO_rules_model <- tma:::rules(
    modality %in% "chat" & chatGroup %in% UNIT$chatGroup & condition %in% UNIT$condition, modality %in% "resource" & userID %in% UNIT$userID & condition %in% UNIT$condition
  )
  
  context_model <- tma:::contexts(
    x = mock_data,
    units = unit_cols,
    hoo_rules = HOO_rules_model
  )
  
  time_column = "timeStamp"
  
  # Get new TMA results
  sender_cols <- c("role")
  receiver_cols <- c("role")
  mode_column <- "modality"
  
  multidim_arr <- context_tensor(mock_data, sender_cols = sender_cols, receiver_cols = receiver_cols, mode_column = mode_column, default_window = 10, default_weight = 20)
  # multidim_arr[seq.int(prod(dim(multidim_arr)))] <- seq.int(prod(dim(multidim_arr)))
  
  testthat::expect_equal(multidim_arr[1,1,1,1], 20)
  testthat::expect_equal(multidim_arr[2,2,2,2], 10)
  
  testthat::expect_equal(multidim_arr[calculate_1d_index(c(2,2,2,2)-1, dim(multidim_arr)) + 1], 10)
  testthat::expect_equal(multidim_arr[calculate_1d_index(c(1,1,1,2)-1, dim(multidim_arr)) + 1], 10)
  testthat::expect_equal(multidim_arr[calculate_1d_index(c(1,1,1,1)-1, dim(multidim_arr)) + 1], 20)
})