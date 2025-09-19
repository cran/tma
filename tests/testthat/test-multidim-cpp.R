# TODO: make sure to change the below to deal with accumulate
testthat::context("Test C++ accumulation with multidimensional array")
data(test_mockdata, package = "tma")
mock_data <- test_mockdata;

test_that("no senders, no receivers, modality", {
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
  mode_column = "modality"
  
  multidim_arr <- context_tensor(mock_data, sender_cols = c(), receiver_cols = c(), mode_column, 0, 0)
  
  multidim_arr["chat", "weight"] <- 20
  multidim_arr["resource", "weight"] <- 20
  multidim_arr["chat", "window"] <- 360
  multidim_arr["resource", "window"] <- 1280

  suppressWarnings({
    result_old <- accum_multidim(
      tensor = multidim_arr, 
      time_column = time_column,
      codes = codes,
      context_model = context_model
    )
  })
  result_new <- accumulate(
    context_model = context_model, tensor = multidim_arr, time_column = time_column, codes = codes, binary = TRUE, ordered = TRUE
  )
  
  # Compare vectors across all 3 units 
  user1_old <- as.matrix(result_old$connection.counts[1,])
  user1_new <- as.matrix(result_new$connection.counts[1,])
  user2_old <- as.matrix(result_old$connection.counts[2,])
  user2_new <- as.matrix(result_new$connection.counts[2,])
  user3_old <- as.matrix(result_old$connection.counts[3,])
  user3_new <- as.matrix(result_new$connection.counts[3,])
  
  testthat::expect_equal(user1_old, user1_new)
  testthat::expect_equal(user2_old, user2_new)
  testthat::expect_equal(user3_old, user3_new)
})

test_that("sender, no receivers, no modality", {
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
  sender_cols = c("role")
  
  multidim_arr <- context_tensor(mock_data, sender_cols = sender_cols, receiver_cols = c(), mode_column = c(), 0, 0)
  
  multidim_arr["Mentor", "weight"] <- 1
  multidim_arr["Player", "weight"] <- 2
  multidim_arr["Mentor", "window"] <- 360
  multidim_arr["Player", "window"] <- 180
  
  suppressWarnings({
    result_old <- accum_multidim(
      tensor = multidim_arr, 
      time_column = time_column,
      codes = codes,
      context_model = context_model
    )
  })
  result_new <- accumulate(
    context_model = context_model, tensor = multidim_arr, time_column = time_column, codes = codes, ordered = TRUE
  )
  
  # Compare vectors across all 3 units 
  
  user1_old <- as.matrix(result_old$connection.counts[1,])
  user1_new <- as.matrix(result_new$connection.counts[1,])
  user2_old <- as.matrix(result_old$connection.counts[2,])
  user2_new <- as.matrix(result_new$connection.counts[2,])
  user3_old <- as.matrix(result_old$connection.counts[3,])
  user3_new <- as.matrix(result_new$connection.counts[3,])
  
  testthat::expect_true(all(user1_old == user1_new))
  testthat::expect_true(all(user2_old == user2_new))
  testthat::expect_true(all(user3_old == user3_new))
})

test_that("no senders, receiver, no modality", {
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
  receiver_cols = c("language")
  
  multidim_arr <- context_tensor(mock_data, sender_cols = c(), receiver_cols = receiver_cols, mode_column = c(), 0, 0)
  
  multidim_arr["L1", "weight"] <- 1
  multidim_arr["L2", "weight"] <- 5
  multidim_arr["L1", "window"] <- 360
  multidim_arr["L2", "window"] <- 180
  
  suppressWarnings({
    result_old <- accum_multidim(
      tensor = multidim_arr, 
      time_column = time_column,
      codes = codes,
      context_model = context_model
    )
  })
  result_new <- accumulate(
    context_model = context_model, tensor = multidim_arr, time_column = time_column, codes = codes, ordered = TRUE
  )
  
  # Compare vectors across all 3 units 
  user1_old <- as.matrix(result_old$connection.counts[1,])
  user1_new <- as.matrix(result_new$connection.counts[1,])
  user2_old <- as.matrix(result_old$connection.counts[2,])
  user2_new <- as.matrix(result_new$connection.counts[2,])
  user3_old <- as.matrix(result_old$connection.counts[3,])
  user3_new <- as.matrix(result_new$connection.counts[3,])
  
  testthat::expect_equal(user1_old, user1_new)
  testthat::expect_equal(user2_old, user2_new)
  testthat::expect_equal(user3_old, user3_new)
})

test_that("sender, no receiver, modality", {
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
  sender_cols = c("role")
  mode_column = "modality"
  
  multidim_arr <- context_tensor(mock_data, sender_cols = sender_cols, receiver_cols = c(), mode_column = mode_column, 0, 0)
  
  multidim_arr["Mentor", "chat", "weight"] <- 3
  multidim_arr["Mentor", "resource", "weight"] <- 1
  multidim_arr["Player", "chat", "weight"] <- 2
  multidim_arr["Player", "resource", "weight"] <- 5
  
  multidim_arr["Mentor", "chat", "window"] <- 180
  multidim_arr["Mentor", "resource", "window"] <- 60
  multidim_arr["Player", "chat", "window"] <- 180
  multidim_arr["Player", "resource", "window"] <- 360
  
  suppressWarnings({
    result_old <- accum_multidim(
      tensor = multidim_arr, 
      time_column = time_column,
      codes = codes,
      context_model = context_model
    )
  })
  result_new <- accumulate(
    context_model = context_model, tensor = multidim_arr, time_column = time_column, codes = codes, ordered = TRUE
  )
  
  
  
  # Compare vectors across all 3 units 
  
  user1_old <- as.matrix(result_old$connection.counts[1,])
  user1_new <- as.matrix(result_new$connection.counts[1,])
  user2_old <- as.matrix(result_old$connection.counts[2,])
  user2_new <- as.matrix(result_new$connection.counts[2,])
  user3_old <- as.matrix(result_old$connection.counts[3,])
  user3_new <- as.matrix(result_new$connection.counts[3,])
  
  testthat::expect_true(all(user1_old == user1_new))
  testthat::expect_true(all(user2_old == user2_new))
  testthat::expect_true(all(user3_old == user3_new))
})

test_that("no sender, receiver, modality", {
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
  receiver_cols = c("language")
  mode_column = "modality"
  
  multidim_arr <- context_tensor(mock_data, sender_cols = c(), receiver_cols = receiver_cols, mode_column = mode_column, 0, 0)
  
  multidim_arr["L1", "chat", "weight"] <- 5
  multidim_arr["L1", "resource", "weight"] <- 1
  multidim_arr["L2", "chat", "weight"] <- 3
  multidim_arr["L2", "resource", "weight"] <- 2
  
  multidim_arr["L1", "chat", "window"] <- 180
  multidim_arr["L1", "resource", "window"] <- 360
  multidim_arr["L2", "chat", "window"] <- 60
  multidim_arr["L2", "resource", "window"] <- 120
  
  suppressWarnings({
    result_old <- accum_multidim(
      tensor = multidim_arr, 
      time_column = time_column,
      codes = codes,
      context_model = context_model
    )
  })
  result_new <- accumulate(
    context_model = context_model, tensor = multidim_arr, time_column = time_column, codes = codes, ordered = TRUE
  )
  
  # Compare vectors across all 3 units 
  
  user1_old <- as.matrix(result_old$connection.counts[1,])
  user1_new <- as.matrix(result_new$connection.counts[1,])
  user2_old <- as.matrix(result_old$connection.counts[2,])
  user2_new <- as.matrix(result_new$connection.counts[2,])
  user3_old <- as.matrix(result_old$connection.counts[3,])
  user3_new <- as.matrix(result_new$connection.counts[3,])
  
  testthat::expect_true(all(user1_old == user1_new))
  testthat::expect_true(all(user2_old == user2_new))
  testthat::expect_true(all(user3_old == user3_new))
})

test_that("sender, receiver, no modality", {
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
  sender_cols = c("role")
  receiver_cols = c("language")
  
  win_weights <- context_tensor(mock_data, sender_cols = sender_cols, receiver_cols = receiver_cols, mode_column = c(), 0, 0)
  
  win_weights["Mentor", "L1", "weight"] <- 1
  win_weights["Mentor", "L2", "weight"] <- 5
  win_weights["Player", "L1", "weight"] <- 3
  win_weights["Player", "L2", "weight"] <- 1
  
  win_weights["Mentor", "L1", "window"] <- 360
  win_weights["Mentor", "L2", "window"] <- 180
  win_weights["Player", "L1", "window"] <- 250
  win_weights["Player", "L2", "window"] <- 200

  suppressWarnings({
    result_old <- accum_multidim(
      tensor = win_weights, 
      time_column = time_column,
      codes = codes,
      context_model = context_model
    )
  })
  result_new <- accumulate(
    context_model = context_model, tensor = win_weights, time_column = time_column, codes = codes, ordered = TRUE
  )
  
  
  # Compare vectors across all 3 units 
  
  user1_old <- as.matrix(result_old$connection.counts[1,])
  user1_new <- as.matrix(result_new$connection.counts[1,])
  user2_old <- as.matrix(result_old$connection.counts[2,])
  user2_new <- as.matrix(result_new$connection.counts[2,])
  user3_old <- as.matrix(result_old$connection.counts[3,])
  user3_new <- as.matrix(result_new$connection.counts[3,])
  
  testthat::expect_true(all(user1_old == user1_new))
  testthat::expect_true(all(user2_old == user2_new))
  testthat::expect_true(all(user3_old == user3_new))
})

test_that("sender, receiver, modality", {
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
  sender_cols = c("role")
  receiver_cols = c("language")
  mode_column = "modality"
  
  multidim_arr <- context_tensor(mock_data, sender_cols = sender_cols, receiver_cols = receiver_cols, mode_column = mode_column, 0, 0)
  
  multidim_arr["Mentor", "L1", "chat", "weight"] <- 5
  multidim_arr["Mentor", "L1", "resource", "weight"] <- 5
  multidim_arr["Mentor", "L2", "chat", "weight"] <- 7
  multidim_arr["Mentor", "L2", "resource", "weight"] <- 2
  
  multidim_arr["Player", "L1", "chat", "weight"] <- 3
  multidim_arr["Player", "L1", "resource", "weight"] <- 5
  multidim_arr["Player", "L2", "chat", "weight"] <- 5
  multidim_arr["Player", "L2", "resource", "weight"] <- 1
  
  multidim_arr["Mentor", "L1", "chat", "window"] <- 800
  multidim_arr["Mentor", "L1", "resource", "window"] <- 500
  multidim_arr["Mentor", "L2", "chat", "window"] <- 20
  multidim_arr["Mentor", "L2", "resource", "window"] <- 310
  
  multidim_arr["Player", "L1", "chat", "window"] <- 320
  multidim_arr["Player", "L1", "resource", "window"] <- 200
  multidim_arr["Player", "L2", "chat", "window"] <- 150
  multidim_arr["Player", "L2", "resource", "window"] <- 360

  suppressWarnings({
    result_old <- accum_multidim(
      tensor = multidim_arr, 
      time_column = time_column,
      codes = codes,
      context_model = context_model
    )
  })
  result_new <- accumulate(
    context_model = context_model, tensor = multidim_arr, time_column = time_column, codes = codes, ordered = TRUE
  )
  
  # Compare vectors across all 3 units 
  
  user1_old <- as.matrix(result_old$connection.counts[1,])
  user1_new <- as.matrix(result_new$connection.counts[1,])
  user2_old <- as.matrix(result_old$connection.counts[2,])
  user2_new <- as.matrix(result_new$connection.counts[2,])
  user3_old <- as.matrix(result_old$connection.counts[3,])
  user3_new <- as.matrix(result_new$connection.counts[3,])
  
  testthat::expect_true(all(user1_old == user1_new))
  testthat::expect_true(all(user2_old == user2_new))
  testthat::expect_true(all(user3_old == user3_new))
  
})

test_that("senders, receivers, modality", {
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
  sender_cols <- c("role", "language")
  receiver_cols <- c("role", "handwriting")
  mode_column <- "modality"
  
  multidim_arr <- context_tensor(mock_data, sender_cols = sender_cols, receiver_cols = receiver_cols, mode_column = mode_column, 0, 0)
  
  
  multidim_arr["Player", "L1", "Player", "poor", "chat", "weight"] <- 1
  multidim_arr["Player", "L1", "Player", "poor", "resource", "weight"] <- 1
  multidim_arr["Player", "L1", "Player", "good", "chat", "weight"] <- 1
  multidim_arr["Player", "L1", "Player", "good", "resource", "weight"] <- 8
  multidim_arr["Player", "L1", "Mentor", "poor", "chat", "weight"] <- 1
  multidim_arr["Player", "L1", "Mentor", "poor", "resource", "weight"] <- 1
  multidim_arr["Player", "L1", "Mentor", "good", "chat", "weight"] <- 1
  multidim_arr["Player", "L1", "Mentor", "good", "resource", "weight"] <- 7
  multidim_arr["Player", "L2", "Player", "poor", "chat", "weight"] <- 7
  multidim_arr["Player", "L2", "Player", "poor", "resource", "weight"] <- 4
  multidim_arr["Player", "L2", "Player", "good", "chat", "weight"] <- 1
  multidim_arr["Player", "L2", "Player", "good", "resource", "weight"] <- 1
  multidim_arr["Player", "L2", "Mentor", "poor", "chat", "weight"] <- 1
  multidim_arr["Player", "L2", "Mentor", "poor", "resource", "weight"] <- 3
  multidim_arr["Player", "L2", "Mentor", "good", "chat", "weight"] <- 3
  multidim_arr["Player", "L2", "Mentor", "good", "resource", "weight"] <- 12
  multidim_arr["Mentor", "L1", "Player", "poor", "chat", "weight"] <- 1
  multidim_arr["Mentor", "L1", "Player", "poor", "resource", "weight"] <- 10
  multidim_arr["Mentor", "L1", "Player", "good", "chat", "weight"] <- 1
  multidim_arr["Mentor", "L1", "Player", "good", "resource", "weight"] <- 9
  multidim_arr["Mentor", "L1", "Mentor", "poor", "chat", "weight"] <- 1
  multidim_arr["Mentor", "L1", "Mentor", "poor", "resource", "weight"] <- 1
  multidim_arr["Mentor", "L1", "Mentor", "good", "chat", "weight"] <- 7
  multidim_arr["Mentor", "L1", "Mentor", "good", "resource", "weight"] <- 7
  multidim_arr["Mentor", "L2", "Player", "poor", "chat", "weight"] <- 7
  multidim_arr["Mentor", "L2", "Player", "poor", "resource", "weight"] <- 1
  multidim_arr["Mentor", "L2", "Player", "good", "chat", "weight"] <- 6
  multidim_arr["Mentor", "L2", "Player", "good", "resource", "weight"] <- 2
  multidim_arr["Mentor", "L2", "Mentor", "poor", "chat", "weight"] <- 1
  multidim_arr["Mentor", "L2", "Mentor", "poor", "resource", "weight"] <- 3
  multidim_arr["Mentor", "L2", "Mentor", "good", "chat", "weight"] <- 5
  multidim_arr["Mentor", "L2", "Mentor", "good", "resource", "weight"] <- 5
  
  multidim_arr["Player", "L1", "Player", "poor", "chat", "window"] <- 360
  multidim_arr["Player", "L1", "Player", "poor", "resource", "window"] <- 10 
  multidim_arr["Player", "L1", "Player", "good", "chat", "window"] <- 360
  multidim_arr["Player", "L1", "Player", "good", "resource", "window"] <- 310
  multidim_arr["Player", "L1", "Mentor", "poor", "chat", "window"] <- 360
  multidim_arr["Player", "L1", "Mentor", "poor", "resource", "window"] <- 300
  multidim_arr["Player", "L1", "Mentor", "good", "chat", "window"] <- 360
  multidim_arr["Player", "L1", "Mentor", "good", "resource", "window"] <- 320
  multidim_arr["Player", "L2", "Player", "poor", "chat", "window"] <- 360
  multidim_arr["Player", "L2", "Player", "poor", "resource", "window"] <- 350
  multidim_arr["Player", "L2", "Player", "good", "chat", "window"] <- 360
  multidim_arr["Player", "L2", "Player", "good", "resource", "window"] <- 189
  multidim_arr["Player", "L2", "Mentor", "poor", "chat", "window"] <- 121
  multidim_arr["Player", "L2", "Mentor", "poor", "resource", "window"] <- 782
  multidim_arr["Player", "L2", "Mentor", "good", "chat", "window"] <- 333
  multidim_arr["Player", "L2", "Mentor", "good", "resource", "window"] <- 343
  multidim_arr["Mentor", "L1", "Player", "poor", "chat", "window"] <- 845
  multidim_arr["Mentor", "L1", "Player", "poor", "resource", "window"] <- 800
  multidim_arr["Mentor", "L1", "Player", "good", "chat", "window"] <- 500
  multidim_arr["Mentor", "L1", "Player", "good", "resource", "window"] <- 20
  multidim_arr["Mentor", "L1", "Mentor", "poor", "chat", "window"] <- 70
  multidim_arr["Mentor", "L1", "Mentor", "poor", "resource", "window"] <- 10
  multidim_arr["Mentor", "L1", "Mentor", "good", "chat", "window"] <- 20
  multidim_arr["Mentor", "L1", "Mentor", "good", "resource", "window"] <- 100
  multidim_arr["Mentor", "L2", "Player", "poor", "chat", "window"] <- 360
  multidim_arr["Mentor", "L2", "Player", "poor", "resource", "window"] <- 200
  multidim_arr["Mentor", "L2", "Player", "good", "chat", "window"] <- 360
  multidim_arr["Mentor", "L2", "Player", "good", "resource", "window"] <- 350
  multidim_arr["Mentor", "L2", "Mentor", "poor", "chat", "window"] <- 360
  multidim_arr["Mentor", "L2", "Mentor", "poor", "resource", "window"] <- 270
  multidim_arr["Mentor", "L2", "Mentor", "good", "chat", "window"] <- 180
  multidim_arr["Mentor", "L2", "Mentor", "good", "resource", "window"] <- 360
  
  
  result_new <- accumulate(
    context_model = context_model, tensor = multidim_arr, time_column = time_column, codes = codes, ordered = TRUE
  )
  
  # Get old TMA results
  #' User 1: A & C = 2, all else 0. good
  #' User 2: A & B = 3, A & C = 1, C & B = 2.5, B & C = 2.5. good
  #' User 3: A & A = 1, B & A = 3, C & A = 4, A & C = 4, B & C = 3. good
  result_old <- data.frame(
    `A & A` = c(0, 0, 1),
    `B & A` = c(0, 0, 3),
    `C & A` = c(0, 0, 4),
    `A & B` = c(0, 3, 0),
    `B & B` = c(0, 0, 0),
    `C & B` = c(0, 2.5, 0),
    `A & C` = c(2, 1, 4),
    `B & C` = c(0, 2.5, 3),
    `C & C` = c(0, 0, 0)
  )
  
  result_old$ENA_UNIT <- c("User1::FirstHalf", "User2::FirstHalf", "User3::FirstHalf")
  
  # Reorder columns so ENA_UNIT is the first column
  result_old <- result_old[, c("ENA_UNIT", colnames(result_old)[-ncol(result_old)])]
  
  colnames(result_old) <- c("ENA_UNIT", "A & A", "B & A", "C & A", "A & B", "B & B", "C & B", "A & C", "B & C", "C & C")
  
  # Compare vectors across all 3 units 
  
  # Compare vectors across all 3 units 
  
  user1_old <- as.matrix(result_old[1,-1])
  user1_new <- as.matrix(result_new$connection.counts[1,])
  user2_old <- as.matrix(result_old[2,-1])
  user2_new <- as.matrix(result_new$connection.counts[2,])
  user3_old <- as.matrix(result_old[3,-1])
  user3_new <- as.matrix(result_new$connection.counts[3,])
  
  testthat::expect_true(all(user1_old == user1_new))
  testthat::expect_true(all(user2_old == user2_new))
  testthat::expect_true(all(user3_old == user3_new))
})
test_that("no senders, no receivers, no modality", {
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
  
  multidim_arr <- context_tensor(mock_data, sender_cols = c(), receiver_cols = c(), 1, 1)
  result_new <- accumulate(
    context_model = context_model, 
    tensor = multidim_arr, 
    codes = codes, ordered = TRUE
  )
  result_new2 <- accumulate(
    context_model = context_model, codes = codes, ordered = TRUE
  )
  
  testthat::expect_equal(as.matrix(result_new$connection.counts), as.matrix(result_new2$connection.counts))
})