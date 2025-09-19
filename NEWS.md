# tma 0.3.1

* added exports: ATTR_NAMES, adjacency_key 
* added class as and is S3 methods for: qe.metadata, qe.data, qe.code, qe.unit, qe.horizon
* moved jsonlite to Suggests, only required in less often used tma::view()

# tma 0.3.0

* context_tensor function for controlling weights and windows across modalities
* deprecated prior implementations: windows_weights, accum_multidim, decay, simple_window

# tma 0.2.0

* Wide variety of updates, including the abiltiy to supply a windows and weighting matrix used for granular
  control of the accumulation across modalities (see `?windows_weights)

# tma 0.1.0

* Beta version - never submitted to CRAN
