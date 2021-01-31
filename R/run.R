#' @importFrom tidyr unnest
#' @importFrom dplyr row_number n lag

get_statement_data <- function(){

	list_statements() %>%
		statement_frame() %>%
		statement_unpack() %>%
		mutate(page_data = clean_page(page_data)) %>%
		unnest(page_data) %>%
		statement_summarise()
}

