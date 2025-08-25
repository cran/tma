testthat::context("Test conversations");
 
data("RS.data");
RS.data$Context <- "Public";
RS.data$Row <- seq(nrow(RS.data));
  
units_by <- c("Condition", "GroupName", "UserName");
hoo_rules <- conversation_rules(
  # (Condition == UNIT$Condition & UserName == UNIT$UserName)
  (Condition == UNIT$Condition & GroupName == UNIT$GroupName & UserName == UNIT$UserName)
);

test_that("verify context length", {
  rs_contexts_tables <- contexts(RS.data, units_by = units_by, hoo_rules = hoo_rules)
  
  testthat::expect_equal(length(rs_contexts_tables$model$contexts), data.table:::uniqueN(RS.data, by = units_by))
})

# test_that("verify accumulation", {
#   code_cols <- names(RS.data)[15:20];
#   
#   networks <- 
#     contexts(RS.data, units_by = units_by, hoo_rules = hoo_rules) |> 
#     accumulate_contexts(codes = code_cols)
#   ;
#   testthat::expect_equal(length(networks$model$contexts), data.table:::uniqueN(RS.data, by = units_by))
# })
