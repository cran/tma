testthat::context("Test making multidimnesional array")
data(test_mockdata, package = "tma")
mock_data <- test_mockdata;

testthat::test_that("no senders, no receivers, modality", {
  
  # load mock data
  mock_data <- mock_data[mock_data$chatGroup == "PAM",]
  
  # generate multidim arr
  mode_column <- "modality"
  tester <- context_tensor(mock_data, sender_cols = list(), receiver_cols = list(), mode_column, default_window = 1, default_weight = 2) 
  
  # verify all windows 1, all weights 2
  
  testthat::expect_true(all(tester[,"window"] == 1))
  testthat::expect_true(all(tester[,"weight"] == 2))
})

testthat::test_that("sender, no receivers, no modality", {
  
  # load mock data
  mock_data <- mock_data[mock_data$chatGroup == "PAM",]
  
  # generate multidim arr
  sender_cols <- list("role")
  tester <- context_tensor(mock_data, sender_cols = sender_cols, receiver_cols = list(), mode_column = list(), default_window = 1, default_weight = 2) 
  
  # verify all windows 1, all weights 2
  
  testthat::expect_true(all(tester[,"window"] == 1))
  testthat::expect_true(all(tester[,"weight"] == 2))
})

testthat::test_that("no senders, receiver, no modality", {
  
  # load mock data
  mock_data <- mock_data[mock_data$chatGroup == "PAM",]
  
  # generate multidim arr
  receiver_cols <- list("language")
  tester <- context_tensor(mock_data, sender_cols = list(), receiver_cols = receiver_cols, mode_column = list(), default_window = 1, default_weight = 2) 
  
  # verify all windows 1, all weights 2
  
  testthat::expect_true(all(tester[,"window"] == 1))
  testthat::expect_true(all(tester[,"weight"] == 2))
})

testthat::test_that("sender, no receivers, modality", {
  
  # load mock data
  mock_data <- mock_data[mock_data$chatGroup == "PAM",]
  
  # generate multidim arr
  mode_column <- "modality"
  sender_cols <- list("role")
  tester <- context_tensor(mock_data, sender_cols = sender_cols, receiver_cols = list(), mode_column, default_window = 1, default_weight = 2) 
  
  # verify all windows 1, all weights 2
  
  testthat::expect_true(all(tester[,,"window"] == 1))
  testthat::expect_true(all(tester[,,"weight"] == 2))
})

testthat::test_that("no senders, receiver, modality", {
  
  # load mock data
  mock_data <- mock_data[mock_data$chatGroup == "PAM",]
  
  # generate multidim arr
  mode_column <- "modality"
  receiver_cols <- list("language")
  tester <- context_tensor(mock_data, sender_cols = list(), receiver_cols = receiver_cols, mode_column, default_window = 1, default_weight = 2) 
  
  # verify all windows 1, all weights 2
  
  testthat::expect_true(all(tester[,,"window"] == 1))
  testthat::expect_true(all(tester[,,"weight"] == 2))
})


testthat::test_that("sender, receiver, no modality", {
  
  # load mock data
  mock_data <- mock_data[mock_data$chatGroup == "PAM",]
  
  # generate multidim arr
  sender_cols <- list("role")
  receiver_cols <- list("language")
  tester <- context_tensor(mock_data, sender_cols = sender_cols, receiver_cols = receiver_cols, mode_column = list(), default_window = 1, default_weight = 2) 
  
  # verify all windows 1, all weights 2
  
  testthat::expect_true(all(tester[,,"window"] == 1))
  testthat::expect_true(all(tester[,,"weight"] == 2))
})

testthat::test_that("sender, receiver, modality", {
  
  # load mock data
  mock_data <- mock_data[mock_data$chatGroup == "PAM",]
  
  # generate multidim arr
  sender_cols <- list("role")
  receiver_cols <- list("language")
  mode_column <- "modality"
  tester <- context_tensor(mock_data, sender_cols, receiver_cols, mode_column, default_window = 1, default_weight = 2)
  
  # verify all windows 1, all weights 2
  
  testthat::expect_true(all(tester[,,,"window"] == 1))
  testthat::expect_true(all(tester[,,,"weight"] == 2))
})

testthat::test_that("senders, receiver, modality", {
  
  # load mock data
  mock_data <- mock_data[mock_data$chatGroup == "PAM",]
  
  # generate multidim arr
  sender_cols <- c("role", "language")
  receiver_cols <- c("role")
  mode_column <- "modality"
  tester <- context_tensor(mock_data, sender_cols = sender_cols, receiver_cols = receiver_cols, mode_column = mode_column, 1, 2)
  
  # verify all windows 1, all weights 2
  
  testthat::expect_true(all(tester[,,,,"window"] == 1))
  testthat::expect_true(all(tester[,,,,"weight"] == 2))
})

testthat::test_that("sender, receivers, modality", {
  
  # load mock data
  mock_data <- mock_data[mock_data$chatGroup == "PAM",]
  
  # generate multidim arr
  sender_cols <- c("role")
  receiver_cols <- c("role", "language")
  mode_column <- "modality"
  tester <- context_tensor(mock_data, sender_cols = sender_cols, receiver_cols = receiver_cols, mode_column = mode_column, 1, 2)
  
  # verify all windows 1, all weights 2
  
  testthat::expect_true(all(tester[,,,,"window"] == 1))
  testthat::expect_true(all(tester[,,,,"weight"] == 2))
})

testthat::test_that("senders, receivers, modality", {
  
  # load mock data
  mock_data <- mock_data[mock_data$chatGroup == "PAM",]
  
  # generate multidim arr
  sender_cols <- list("role", "language")
  receiver_cols <- list("role", "handwriting")
  mode_column <- "modality"
  tester <- context_tensor(mock_data, sender_cols, receiver_cols, mode_column, default_window = 1, default_weight = 2)
  
  # verify all windows 1, all weights 2
  
  testthat::expect_true(all(tester[,,,,,"window"] == 1))
  testthat::expect_true(all(tester[,,,,,"weight"] == 2))
})







