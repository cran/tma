# test_that("small network", {
#   data(test_smalldata);
#   sm <- test_smalldata;
# 
#   units_by <- c("GroupName", "UserName");
#   
#   hoo_rules <- conversation_rules(
#     (Condition == UNIT$Condition & GroupName == UNIT$GroupName & Context == "Public"),
#     (Condition == UNIT$Condition & GroupName == UNIT$GroupName & UserName == UNIT$UserName)
#   );  
#   
#   networks <- 
#     contexts(sm, units_by = units_by, hoo_rules = hoo_rules) |> 
#     accumulate_contexts(codes = c("A","B","C"))
#   ;
#   
#   testthat::expect_true(any(networks$model$contexts$`Group_1::User1`$Context == "Private"))
#   testthat::expect_false(any(networks$model$contexts$`Group_1::User2`$Context == "Private"))
# })
