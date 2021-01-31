load_statement_data <- function(loc){

	bank_statement_file(loc) %>%
		map(parse_statement_text)
}

#' @importFrom pdftools pdf_data

parse_statement_text <- function(loc){

	pdf_data(loc) %>%
		map(mutate, x_end = x + width) %>%
		map(mutate, y_end = y + height)
}
