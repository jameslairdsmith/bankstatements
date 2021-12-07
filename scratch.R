list_statements() %>%
	tibble(statement_name = .) %>%
	# mutate(statement_id = dplyr::row_number(), .before = dplyr::everything()) %>%
	mutate(statement_date = parse_statement_date(statement_name)) %>%
	mutate(pages_data = get_statement_data(statement_name)) %>%
	select(-statement_name) %>%
	dplyr::rowwise() %>%
	mutate(n_pages_statement = length(pages_data)) %>%
	dplyr::ungroup() %>%
	tidyr::unnest(pages_data) %>%
	dplyr::rename(page_data = pages_data) %>%
	dplyr::group_by(statement_date) %>%
	mutate(page_number = dplyr::row_number()) %>%
	dplyr::filter(page_number <= n_pages_statement - 2) %>%
	select(-n_pages_statement) %>%
	ungroup() %>%
	# dplyr::slice(2) %>%
	# pull_extract(page_data) %>%
	# View() %>%
	dplyr::rowwise() %>%
	mutate(date_df = extract_date(page_data, page_number)) %>%
	mutate(transactions_df = extract_transactions(page_data, page_number)) %>%
	mutate(transactions_df = date_transactions(transactions_df, date_df)) %>%
	# pull_extract(inc) %>%
	# View() %>%
	{.}
