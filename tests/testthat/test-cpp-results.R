# test_that("compare cpp to std", {
#   test_smalldata = read.csv(system.file(package = "tma", 'extdata/test_data_3.csv'), stringsAsFactors = FALSE);
#   
#   hoo_rules <- conversation_rules(
#     (Condition == UNIT$Condition & GroupName == UNIT$GroupName & Context == "Public"),
#     (Condition == UNIT$Condition & GroupName == UNIT$GroupName & UserName == UNIT$UserName)
#   );  
#   units_by <- c("GroupName", "UserName");
#   code_cols <- which(colnames(test_smalldata) %in%  c("A","B","C"));
#   network_contexts <-  contexts(test_smalldata, units_by = units_by, hoo_rules = hoo_rules);
#   networks2 <- accumulate_contexts(network_contexts, codes = code_cols, return.dena.set = TRUE, return.ena.set = FALSE);
#   all_networks <- accumulate_networks(network_contexts, (code_cols - 1), decay_function = tma:::decay(simple_window, window_size = 4));
#   
#   testthat::expect_true( all(all_networks == as.matrix(networks2$connection.counts[, -(1:3)])) );
# })