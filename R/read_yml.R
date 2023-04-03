#' Read YAML file into a list
#' @description read script metadata file in the repository or in a local folder.
#' @param metadata_file a URL or file name containing script metadata
#' @param max_input_size the maximum input size in characters when reading from a URL (default: 1e6)
#' @return a list containing the parsed metadata tags
#' @export
#' @importFrom yaml yaml.load
#' @importFrom yaml yaml.load_file
#' @importFrom utils packageDescription
#' @importFrom RCurl url.exists
#' @examples
#'   a <- "https://github.com/phuse-org/phuse-scripts/raw/master"
#'   b <- "development/R/scripts"
#'   c <- "Draw_Dist2_R.yml"
#'   f1 <- paste(a,b,c, sep = '/')
#'   r1 <- read_yml(f1)
#' @author Hanming Tu
#' @name read_yml
# ---------------------------------------------------------------------------
# HISTORY   MM/DD/YYYY (developer) - explanation
#  08/31/2017 (htu) - initial creation
#  04/03/2023 (htu) - improved error handling, parameter validation, 
#    input size limit, and package dependencies
# https://www.yamllint.com/

library('yaml')
library('RCurl')

read_yml <- function(metadata_file, max_input_size = 1e6) {
  # Validate input parameters
  if (!is.character(metadata_file)) {
    stop("metadata_file must be a character string")
  }
  if (is.na(max_input_size) || max_input_size <= 0) {
    stop("max_input_size must be a positive integer")
  }
  
  r <- list()
  if (nchar(metadata_file) == 0) {
    return(r)
  }
  
  if (startsWith(metadata_file, "http://") || startsWith(metadata_file, "https://")) {
    # Check if URL exists and read contents
    # if (!utils::url.exists(metadata_file)) {
    if (!url.exists(metadata_file)) {
      stop("URL does not exist: ", metadata_file)
    }
    input <- readChar(metadata_file, nchars = max_input_size)
    # Parse YAML contents
    tryCatch(
      {
        r <- yaml.load(input)
      },
      error = function(e) {
        stop("Invalid YAML format in file: ", metadata_file)
      }
    )
  } else if (file.exists(metadata_file)) {
    # Read contents from file
    r <- yaml.load_file(metadata_file)
  } else {
    stop("File does not exist: ", metadata_file)
  }
  
  
  return(r)
}


