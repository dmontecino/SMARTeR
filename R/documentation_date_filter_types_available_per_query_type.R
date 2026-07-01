#' Date filter types available per query type
#'
#' This dataset contains information about the date filter types available for each query type.
#' It is used to specify the allowed date filter types for different types of queries.
#'
#' @format ## date_filter_types_available_per_query_type
#' 
#' A data frame with 26 rows and 10 columns:
#' 
#'   \describe{
#'     \item{typeKey}{Type of query.}
#'     \item{date_filter_type:assetdeploymentdate}{The filter assetdeploymentdate is available for each query type.}
#'     \item{ate_filter_type:missionenddate}{The filter missionenddate is available for each query type.}
#'   }
#'
#' @source This dataset was generated using the `create_date_filter_types_available_per_query_type.R` script.
#' @keywords dataset
"date_filter_types_available_per_query_type"