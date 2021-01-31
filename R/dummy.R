make_dummy_page <- function(slice = 1){

	list_statements() %>%
		tibble(statement_name = .) %>%
		slice(1) %>%
		mutate(statement_date = parse_statement_date(statement_name),
					 page_data = get_statement_data(statement_name),
					 .keep = "unused") %>%
		unnest(page_data) %>%
		group_by(statement_date) %>%
		mutate(page_number = row_number(), .before = page_data) %>%
		filter(page_number <= n() - 2) %>%
		ungroup() %>%
		mutate(page_data = clean_page(page_data)) %>%
		slice(slice) %>%
		pull_extract(page_data)
}

