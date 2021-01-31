statement_frame <- function(data){

	data %>%
		tibble(statement_name = .)
}

statement_unpack <- function(data){

	data %>%
		mutate(statement_date = parse_statement_date(statement_name),
					 page_data = load_statement_data(statement_name),
				   .keep = "unused") %>%
		unnest(page_data) %>%
		statement_filter_page()
}

statement_filter_page <- function(data){

	data %>%
		group_by(statement_date) %>%
		mutate(page_number = row_number(), .before = page_data) %>%
		filter(page_number <= n() - 2) %>%
		ungroup()
}
