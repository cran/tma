// [[Rcpp::depends(RcppArmadillo)]]

#include <RcppArmadillo.h>

//' @title n choose 2
//' @param n int
// [[Rcpp::export]]
int choose_two(int n) {
  return (n * (n - 1)) / 2;
}

arma::rowvec vector_to_summed_uppertri(arma::vec blank) {
  int code_count = pow(blank.size(), 0.5);
  int tri_size = choose_two(code_count);
  arma::rowvec network_vector = arma::rowvec(tri_size);
  
  arma::mat blank_mat(blank);
  blank_mat.reshape(code_count, code_count);
  
  blank_mat = blank_mat + blank_mat.t();
  arma::colvec blank_vec = blank_mat.as_col();
  arma::uvec upper_indices = arma::trimatu_ind( arma::size(blank_mat), 1 );
  network_vector = arma::conv_to< arma::rowvec >::from(blank_vec.elem(upper_indices));
  
  return network_vector;
}

// std::vector<double> accumulate_network(
// [[Rcpp::export]]
Rcpp::List accumulate_network(
  std::string unit,
  SEXP unit_rows_sexp, 
  Rcpp::NumericVector code_cols,
  Rcpp::Function decay_function,
  int time_col = -1,
  bool ordered = true
) {
  arma::vec blank(code_cols.size() * code_cols.size());
  Rcpp::DataFrame unit_rows = Rcpp::as<Rcpp::DataFrame>(Rcpp::wrap(unit_rows_sexp));
  Rcpp::NumericMatrix codes(unit_rows.nrow(), code_cols.size());

  Rcpp::CharacterVector qeunit_col = unit_rows["QEUNIT"];
  Rcpp::NumericVector qeunit_row_indices;
  
  for(int i = 0; i < qeunit_col.size(); i++) {
    if(qeunit_col[i] == unit) {
      qeunit_row_indices.push_back(i);
    }
  }
  
  arma::mat individual_networks(qeunit_row_indices.size(), (code_cols.size() * code_cols.size()) );
  for(int i = 0; i < code_cols.size(); i++) {
    Rcpp::NumericVector     unit_code_col = unit_rows[code_cols[i]];
    Rcpp::NumericMatrix::Column codes_col = codes(Rcpp::_, i);
    codes_col = unit_code_col;
  }

  // Environment decay_fun_env(decay_function);
  Rcpp::Environment decay_fun_env = decay_function.environment();
  
  // Rcpp::Rcout << "3" << std::endl;
  for(int i = 0; i < qeunit_row_indices.size(); i++) {
    int unit_response_row = qeunit_row_indices[i];
    
    arma::uvec ground_rows = arma::regspace<arma::uvec>(0, 1, unit_response_row);
    Rcpp::NumericVector ground_rows2 = Rcpp::as<Rcpp::NumericVector>(Rcpp::wrap(ground_rows));
    Rcpp::NumericVector ground_dist_from_response(ground_rows.size());
    if (time_col == -1) {
      ground_dist_from_response = Rcpp::seq(0, ground_rows.size()-1);
      // std::reverse(ground_dist_from_response.begin(), ground_dist_from_response.end());
    }
    else {
      arma::uvec times = Rcpp::as<arma::uvec>(wrap(unit_rows[time_col]));
      arma::uvec ground_times = times.elem(ground_rows);
      arma::uword response_time = ground_times[ground_times.size() - 1];
      arma::uvec response_times(ground_times.n_elem, arma::fill::value(response_time));
      
      ground_dist_from_response = response_times - ground_times;
    }
    
    decay_fun_env["UNIT"] = unit;
    decay_fun_env["RESPONSE_INDEX"] = unit_response_row + 1;
    // Rcpp::Rcout << "Rows: " << (ground_rows + 1) << std::endl;
    decay_fun_env["ROWS"] = (ground_rows + 1);
    
    Rcpp::List myList(unit_rows.size());
    Rcpp::CharacterVector namevec;
    std::string namestem = "Column Heading ";
    for (int i=0; i < unit_rows.ncol(); i++) {
      // arma::uvec fnd = arma::find(ground_rows == i);
      // Rcpp::Rcout << "Fnd: " << fnd.size() << std::endl;
      // if(fnd.size() > 0) {
        // Rcpp::Rcout << "i: " << i << std::endl;
        Rcpp::CharacterVector v = unit_rows(i);
        myList[i] = v[ground_rows2]; // adding vectors
        // Rcpp::Rcout << "V : " << v << std::endl;
        // myList[i] = myList
        // namevec.push_back(namestem+std::string(1,(char)(((int)'a') + i))); // making up column names
      // }
    }
    myList.attr("names") = unit_rows.attr("names");
    Rcpp::DataFrame dfout(myList);
    decay_fun_env["FULL_CONTEXT"] = dfout;
    // arma::uvec all_rows(ground_rows.size() + 1);
    // for(int z = 0; z < ground_rows.size(); z++) {
    //   all_rows[z] = ground_rows[z] + 1;
    // }
    // // all_rows[all_rows.size() - 1] = unit_response_row + 2;
    
    arma::vec decay_effect = Rcpp::as<arma::vec>(Rcpp::wrap(decay_function(ground_dist_from_response)));

    Rcpp::NumericMatrix ground_codes_raw = codes( Rcpp::Range( ground_rows[0], ground_rows[ground_rows.size() - 1]  ), Rcpp::_);
    
    arma::mat ground_codes_mat = Rcpp::as<arma::mat>(ground_codes_raw);
    arma::mat codes_decayed(ground_codes_mat.n_rows, ground_codes_mat.n_cols);
    for(int j = 0; j < codes_decayed.n_rows; j ++) {
      codes_decayed.row(j) = ground_codes_mat.row(j) * decay_effect[j];
    }

    arma::rowvec response_row = codes_decayed.tail_rows(1);
    arma::rowvec ground_summarized = arma::sum(codes_decayed);
    
    arma::rowvec ground_summarized_no_resp = ground_summarized - response_row;
    arma::mat connection_matrix = ground_summarized_no_resp.t() * response_row;
    
    arma::mat self_connection_matrix = response_row.t() * response_row;
    self_connection_matrix.diag() *= 0;
    self_connection_matrix = self_connection_matrix * 0.5;
    
    arma::mat full_connection_matrix = connection_matrix + self_connection_matrix;
    arma::vec full_connection_vec = arma::vectorise(full_connection_matrix);
    
    individual_networks.row(i) = full_connection_vec.t();
    blank = blank + full_connection_vec;
  }

  Rcpp::NumericMatrix individual_networks_nm = Rcpp::as<Rcpp::NumericMatrix>(Rcpp::wrap(individual_networks));
  
  Rcpp::Function subset("[.data.frame");
  individual_networks_nm.attr("unit_rows") = subset(unit_rows, qeunit_row_indices + 1, R_MissingArg);
  Rcpp::CharacterVector classes = {"unit.rows","list"};
  individual_networks_nm.attr("class") = classes;
 
  arma::rowvec network_vector;
  if(ordered == false) {
    network_vector = vector_to_summed_uppertri(blank);
  }
  else {
    network_vector = blank.t();
  }
  // Rcpp::Rcout << "5" << std::endl;
  
  return Rcpp::List::create(
    Rcpp::Named("networks") = Rcpp::as<std::vector<double>>(Rcpp::wrap(network_vector)),
    Rcpp::Named("row_networks") = individual_networks_nm
  );
}

//' fast accumulate networks
//' @param x TBD
//' @param code_cols TBD
//' @param decay_function TBD
//' @param time_col TBD
//' @param ordered TBD
//' 
// [[Rcpp::export]]
Rcpp::List accumulate_networks(
    Rcpp::List x,
    Rcpp::NumericVector code_cols,
    Rcpp::Function decay_function,
  int time_col = -1,
  bool ordered = true
) {
  Rcpp::List model = x["model"];
  Rcpp::List contexts = model["contexts"];
  Rcpp::CharacterVector units = model["unit.labels"];
  
  // Rcpp::Rcout << "N codes: " << code_cols.size() << std::endl;
  int ncol = code_cols.size() * code_cols.size();
  if(ordered == false) {
    ncol = choose_two(code_cols.size());
    // Rcpp::Rcout << "Ncol: " << ncol << std::endl;
  }
  Rcpp::NumericMatrix blank(units.size(), ncol);
  Rcpp::List unit_networks_by_row = Rcpp::List::create();
  
  for(int i = 0; i < units.size(); i++) {
    Rcpp::NumericMatrix::Row unit_row = blank.row(i);
    std::string unit_name = Rcpp::as<std::string>(Rcpp::wrap(units[i]));
    
    Rcpp::List accumulated = accumulate_network(unit_name, contexts[unit_name], code_cols, decay_function, time_col, ordered);
    Rcpp::NumericVector unit_network_conv = Rcpp::as<Rcpp::NumericVector>(Rcpp::wrap(accumulated["networks"]));
    Rcpp::NumericMatrix row_networks = accumulated["row_networks"];
    
    unit_networks_by_row[unit_name] = row_networks;
    unit_row = unit_network_conv;
  }
  
  return Rcpp::List::create(
    Rcpp::Named("networks") = blank,
    Rcpp::Named("networks_by_row") = unit_networks_by_row
  );
}

// [[Rcpp::export]]
arma::mat calculate_adjacency_matrix(
  arma::rowvec ground, arma::rowvec response, double response_weight,
  bool ordered = true
) {
  arma::mat response_matrix = response.t() * response;
  arma::mat ground_by_response = ground.t() * response;
  
  if(ordered == true) {
    response_matrix.diag().zeros();
    return ground_by_response + (0.5 * response_weight * response_matrix);
  }
  else {
    arma::mat response_by_response = response.t() * response;
    arma::mat response_by_ground = response.t() * ground;
    
    return (response_weight * response_matrix) + (ground_by_response + response_by_ground);
  }
}

// [[Rcpp::export]]
arma::rowvec adjacency_matrix_to_vector(arma::mat x, bool full = true) {
 if(full == true) {
   return arma::vectorise(x).t();
 }
 else {
   arma::mat combined = arma::trimatu(x, 1) + arma::trimatl(x, -1);
   arma::uvec combined_tri_inds = arma::trimatu_ind(arma::size(combined), 1); 
   return arma::vectorise(combined(combined_tri_inds)).t();
 }
}

std::vector<std::vector<int>> matrix_to_2d_vector(Rcpp::NumericMatrix mat) {
  int n = mat.nrow();
  std::vector<std::vector<int>> vec(n);
  
  for (int i = 0; i < n; ++i) {
    Rcpp::NumericVector row = mat(i, Rcpp::_); // Extract the i-th row
    vec[i] = Rcpp::as<std::vector<int>>(row); // Convert the row to std::vector<int>
  }
  return vec;
}

// [[Rcpp::export]]
int calculate_1d_index_rev(const std::vector<int>& indices, const std::vector<int>& dims) {
  if (indices.size() != dims.size()) {
     throw std::invalid_argument("Number of indices must match number of dimensions.");
  }
 
  Rcpp::Rcout << "Indices: ";
  int index = 0;
  int stride = 1;
  for (int i = dims.size() - 1; i >= 0; --i) {
    Rcpp::Rcout << indices[i] << " ";
    if (indices[i] < 0 || indices[i] >= dims[i]) {
      throw std::out_of_range("Index out of range.");
    }
    index += indices[i] * stride;
    stride *= dims[i];
  }
  return index;
}


// [[Rcpp::export]]
int calculate_1d_index(const std::vector<int>& indices, const std::vector<int>& dims) {
  if (indices.size() != dims.size()) {
    throw std::invalid_argument("Number of indices must match number of dimensions.");
  }

 // formula for mode-k tensor in R^{n_1 \times ... \times n_k}
 // idx = i_0 + \sum_{v = 1}^k [i_v * \Pi_{j = 0}^{v-1}n_j] (this is reindexed to index from 0, b/c we in C++)
 // the below operationalizes this

  size_t linear_index = 0;
  size_t stride = 1;

  for (size_t v = 0; v < indices.size(); ++v) {
    linear_index += indices[v] * stride;
    stride *= dims[v];
  }

  return linear_index;
}

void insert_element(arma::uvec& vec, double value) {
  // Add a new row at the end
  vec.insert_rows(vec.n_rows, 1);
  
  // Assign the value to the new element
  vec(vec.n_rows - 1) = value;
}


//' Apply windowing and weighting to context data for network accumulation (C++ backend)
//'
//' This function implements the core logic for accumulating network connections using a multidimensional parameter array (context_tensor),
//' efficiently applying window and weight parameters to each response line in a unit's context. It is designed for use in the TMA package
//' to speed up accumulation calculations by leveraging C++ and Armadillo for matrix operations.
//'
//' @param tensor NumericVector. The multi-dimensional context_tensor array created in R can be supplied as-is; it is automatically converted to a 1D vector when passed to this function via Rcpp.
//' @param dims IntegerVector. The dimensions of the original context_tensor array.
//' @param dims_sender std::vector<int>. Indices of sender dimensions in the array.
//' @param dims_receiver std::vector<int>. Indices of receiver dimensions in the array.
//' @param dims_mode std::vector<int>. Indices of mode dimensions in the array.
//' @param context_matrix NumericMatrix. Matrix representation of the context for a single unit (rows = context lines, columns = factors).
//' @param unit_rows std::vector<int>. Indices of the response rows for the unit in the context.
//' @param codes arma::mat. Matrix of codes (nrow = context lines, ncol = number of codes).
//' @param times NumericVector. Vector of time values for each context line.
//' @param ordered bool. If TRUE, returns a full adjacency matrix; if FALSE, returns only upper triangle (ENA style).
//'
//' @return A list with two elements:
//'   - row_connection_counts: A matrix of connection counts for each response line (rows = response lines, columns = connections).
//'   - connection_counts: A vector of accumulated connection counts for the unit (length = number of connections).
//'
//' @export
// [[Rcpp::export]]
Rcpp::List apply_tensor(
   Rcpp::NumericVector tensor,
   Rcpp::IntegerVector dims,
   std::vector<int> dims_sender,
   std::vector<int> dims_receiver,
   std::vector<int> dims_mode,
   Rcpp::NumericMatrix context_matrix,
   std::vector<int> unit_rows,
   arma::mat codes,
   Rcpp::NumericVector times,
   bool ordered = true
) {
  const int WINDOW_DIM = 1;
  const int WEIGHT_DIM = 0;
  const bool IS_DEFAULT_CASE = (dims.size() == 1 && dims[0] == 2); // Check if tensor is just [weight, window]

  std::vector<int> dims_v(dims.begin(), dims.end()); // vectorize dims (colnames of senders, receivers, and modes along with weight/window)  
  
  int code_cnt = codes.n_cols; // get # of codes 

  // convert context datatable to 2D C vector
  std::vector<std::vector<int>> context_lookup = matrix_to_2d_vector(context_matrix);
 
  int index = 0; 
  // int dim_start = dims.size() - 1; // start processing dims in reverse order (not using rn)
 
  arma::mat g_w_vec(code_cnt, code_cnt, arma::fill::zeros); // initialize g_w_vec matrix as |codes| x |codes| of all 0s 
  arma::mat unit_row_connection_counts(unit_rows.size(), code_cnt * code_cnt); // initialize matrix to return of |unit's rows in context| x |codes^2| (ordered, for now)
  arma::uvec upper_indices = trimatu_ind( size(g_w_vec), 1 ); // indices of upper triangle; for ENA 
 
  int response_win;
  int response_weight;
  if(IS_DEFAULT_CASE) {
    // Default case: tensor is just [weight, window]
    response_win = tensor[1];
    response_weight = tensor[0];
  }

  // iterate over all of this unit's rows 
  for(int i = 0; i < unit_rows.size(); ++i) {
    int unit_row_i = unit_rows[i]; // get current unit row
   
    // get response line for this current unit row  
    std::vector<int> response_context_lookup = context_lookup[unit_row_i];
    arma::rowvec row_vec = codes.row(unit_row_i); // get response codes 
    double response_time = times[unit_row_i]; // get response time

    // add a 1 to the end of response line; indicates that we are getting windows
    response_context_lookup.push_back(WINDOW_DIM);

    if(!IS_DEFAULT_CASE) {
      // if not default case, we need to get response window from tensor
      // set the end of response line (set to 1 earlier) to 1; indicates that we are getting windows
      response_context_lookup[response_context_lookup.size() - 1] = WINDOW_DIM;
      // calculate_1d_index calculates index offsetted into 1d multidim_arr (vectorized); this gets idx of receiver vals, and then we get them
      int response_win_idx = calculate_1d_index(response_context_lookup, dims_v);
      response_win = tensor[response_win_idx];
    }
   
    // vector of rows to keep 
    arma::uvec ground_rows_to_include;
   
    // vector of weights (to apply to kept rows)
    arma::colvec ground_row_weights;
   
    // get response codes, unweighted
    arma::rowvec r_vec = codes.row(unit_row_i);
   
    // proceed w/ TIF and weighting of g_vecs if it isn't the first row (otherwise, no ground)
    arma::rowvec g_ws_vec(code_cnt, arma::fill::zeros);
   
    if(unit_row_i > 0) {
      // only look at stuff from beginning of ground to the current response line 
      std::vector<std::vector<int>> context_ground = std::vector<std::vector<int>>(context_lookup.begin(), context_lookup.begin() + unit_row_i);
     
      // unfiltered g_vecs are ground rows from just before response line all the way to top of context
      for (int ground_row = unit_row_i - 1; ground_row >= 0; ground_row --) {
        double ground_row_time = times[ground_row]; // get time for this ground row (not doing select-all-times at once approach...?)
       
        std::vector<int> row_v = context_ground[ground_row]; // get actual ground row from context (using ground_row idx)

        // Set the values related to the response row to those of the response row in the ground vector; this way, we can pull all the values from 
        // multidim_arr using just the ground row 
        // Adjust array lookup to account for response row values
        row_v.push_back(WINDOW_DIM);
       
        for (const auto& dim : dims_receiver) {
          row_v[dim] = response_context_lookup[dim];
        }
        // std::reverse(row_v.begin(), row_v.end());
       
        double row_win = response_win; // default to response window (for default case)
        if(!IS_DEFAULT_CASE) {
          int row_win_idx = calculate_1d_index(row_v, dims_v);
          row_win = tensor[row_win_idx]; // get ground window
        }

        double ground_row_time_adj = ground_row_time + row_win; // this computation for the TIF makes sense, I think (?)
        double diff = ground_row_time_adj - response_time;

        if(diff > 0) {
          insert_element(ground_rows_to_include, ground_row); 
         
          row_v[row_v.size() - 1] = WEIGHT_DIM;
         
          // apply weights
          int row_wgt = response_weight;
          if(!IS_DEFAULT_CASE) {
            int row_wgt_idx = calculate_1d_index(row_v, dims_v);
            row_wgt = tensor[row_wgt_idx];
          }
         
          ground_row_weights.insert_rows(ground_row_weights.n_rows, 1);
          ground_row_weights(ground_row_weights.n_rows - 1) = row_wgt;
        }
      }

      if(ground_rows_to_include.size() > 0) {
        arma::mat ground_codes = codes.rows(ground_rows_to_include);
        
        // apply weights w/ hadamard (element-wise) product
        // Rcpp::Rcout << "Ground weight vector: " << ground_row_weights << std::endl;
        arma::mat ground_codes_by_weight = ground_codes.each_col() % ground_row_weights;
        
        // sum codes 
        g_ws_vec = arma::sum(ground_codes_by_weight); 
      }
      else {
        // Handle just the response row here
      }
    }
   
    // No ground available
    else {
      // std::cout << "-- No Ground on first row --" << std::endl;
    }
    
    if(!IS_DEFAULT_CASE) {
      // if not default case, we need to get response weight from tensor
      // set the end of response line (set to 1 earlier) to 0; indicates that we are getting weights
      response_context_lookup[response_context_lookup.size() - 1] = WEIGHT_DIM;
      int response_wgt_idx = calculate_1d_index(response_context_lookup, dims_v); // same thing as above, except for receiver weight 
      response_weight = tensor[response_wgt_idx];
    }
   
    // make g_w_vec, adjacency matrix
    arma::mat resp = calculate_adjacency_matrix(g_ws_vec, row_vec, response_weight, ordered); 
    g_w_vec = g_w_vec + resp;
    // std::cout<< "using wght: " << std::endl << g_w_vec << std::endl;
   
    // add this response line's connection counts to a matrix of |unit rows| x |connections| 
    unit_row_connection_counts.row(i) = adjacency_matrix_to_vector(resp);
  }
 
  // deallocateArray(multidim_as_array2, dims);
 
  // vectorize this unit's connection counts and return what we'd like for a single unit 
  arma::rowvec unit_connection_counts = adjacency_matrix_to_vector(g_w_vec);
 
  Rcpp::List return_list = Rcpp::List::create(
    Rcpp::Named("row_connection_counts") = unit_row_connection_counts,
    Rcpp::Named("connection_counts") = unit_connection_counts
  );
 
  return(return_list);
}


// @title Indices representing an adjacnecey key
// @description Create a matrix of indices representing a co-occurrence
//              adjacency vector.  `len` represents the length of a side in a
//              square matrix.
// @param len Integer
// @param row Which row(s) to return, default to -1, returning both rows. 0
//            returns the top row, 1 will return the bottom row
//
// @return matrix with two rows
// [[Rcpp::export]]
arma::umat triIndices(int len, int row = -1) {
  int vL = len;
  int vS = ( (vL * (vL + 1)) / 2) - vL ;
  int s = 0;

  arma::umat vR = arma::umat(2, vS, arma::fill::zeros);
  arma::umat vRone = arma::umat(1, vS, arma::fill::zeros);
  for( int i = 2; i <= vL; i++ ) {
    for (int j = 0; j < i-1; j++ ) {
      vR(0, s) = j;
      vR(1, s) = i-1;
      if(row == 0) {
        vRone[s] = j;
      } else if (row == 1) {
        vRone[s] = i -1;
      }
      s++;
    }
  }

  if(row == -1) {
    return vR;
  } else {
    return vRone;
  }
}


/*** R
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
send_cols <- c()
recv_cols <- c()
mode_column = "modality"

multidim_arr <- make_multidim_arr(mock_data, sender_cols = c(), receiver_cols = c(), mode_column, 0, 0)

multidim_arr["chat", "weight"] <- 1
multidim_arr["resource", "weight"] <- 2
multidim_arr["chat", "window"] <- 360
multidim_arr["resource", "window"] <- 180

ctx <- context_model$model$contexts[[3]];
unit_context_lookup <- as.matrix(ctx[, lapply(.SD, function(x) as.numeric(as.factor(x))), .SDcols = c(send_cols, recv_cols, mode_column)]);
unit_rows <- attr(ctx, "tma.unit_rows")
unit_to_last <- seq.int(last(unit_rows));
res <- apply_tensor(
  multidim_arr,
  attr(multidim_arr, "dim"),
  attr(multidim_arr, "sender_inds") - 1,
  attr(multidim_arr, "receiver_inds") - 1,
  attr(multidim_arr, "mode_inds") - 1,
  unit_context_lookup[unit_to_last,, drop = FALSE] - 1,
  unit_rows - 1,
  as.matrix(ctx[unit_to_last, ..codes]),
  ctx[unit_to_last,][[time_column]]
);
print(res$connection_counts)
# user2_old <- as.numeric(unname(as.vector(result_old$connection.counts[1,4:length(colnames(result_old$connection.counts))])))
# # user2_new <- as.numeric(unname(as.vector(result_new$connection.counts[2,4:length(colnames(result_new$connection.counts))])))
# user2_new <- res$connection_counts[1,]
# testthat::expect_equal(user2_old, user2_new)
# 
# res_2 <- apply_windows_d(
#   multidim_arr,
#   attr(multidim_arr, "dim"),
#   attr(multidim_arr, "sender_inds") - 1,
#   attr(multidim_arr, "receiver_inds") - 1,
#   attr(multidim_arr, "mode_inds") - 1,
#   unit_context_lookup[unit_to_last,, drop = FALSE] - 1,
#   unit_rows - 1,
#   as.matrix(ctx[unit_to_last, ..codes]),
#   ctx[unit_to_last,][[time_column]],
#   ordered = FALSE
# );
# print(res$connection_counts)
# print(res_2$connection_counts)
#
*/
