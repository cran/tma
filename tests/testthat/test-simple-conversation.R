testthat::context("Test conversations");
 
data("RS.data");
# RS.data$Context <- "Public";
# RS.data$Row <- seq(nrow(RS.data));
  
units_by <- c("Condition", "UserName");
hoo_rules <- conversation_rules(
  (Condition == UNIT$Condition & GroupName == UNIT$GroupName)
);
codes = c('Data','Technical.Constraints','Performance.Parameters','Client.and.Consultant.Requests','Design.Reasoning','Collaboration');

test_that("verify context length", {
  # rs_contexts_tables <- contexts(RS.data, units_by = units_by, hoo_rules = hoo_rules)
  context_model <- contexts(
    RS.data,
    units_by = make.names(units_by),
    hoo_rules = hoo_rules
  )
  
  multidim_arr <- context_tensor(
    RS.data, 
    sender_cols = NULL,
    receiver_cols = NULL,
    default_window = 4,
    default_weight = 1
  )
  
  result_new <- accumulate(
    tensor = multidim_arr, 
    codes = codes,
    context_model = context_model
  )
  
  testthat::expect_equal(length(context_model$model$contexts), data.table:::uniqueN(RS.data, by = units_by))
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
