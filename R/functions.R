#' @title Run matlab extractor
#' @description If MATLAB is available on system, run script to create CSV
#'   from MAT files
#' @export
#' @param mat_file Character, file name with extension ".m"
#' @examples
#' library(matlabr)
#' library(here)
#' extract_csvs_from_mats("edf_mat_to_csv.m")
extract_csvs_from_mats <- function(mat_file) {
  # TODO: Add args for data path to CSN instead of hard coded in .m files
  if (matlabr::have_matlab()) {
    # run only if matlab is available
    matlabr::run_matlab_script(here('matlab', 'converters', mat_file))
  }
}
