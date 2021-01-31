#' @importFrom dplyr pull
#' @importFrom magrittr extract2

pull_extract <- function(df, col_name){

	df %>%
		pull({{ col_name }}) %>%
		extract2(1)
}
