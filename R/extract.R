#' @importFrom dplyr case_when

summarise_elements <- function(){

	make_dummy_page(2) %>%
		bind_rows(make_dummy_page(1)) %>%
		mutate(line_type = case_when(
			x < 100 ~ "trans_date",
			x > 100 & x < 120 ~ "payment_method",
			x > 350 & x < 400 ~ "expense",
			x > 400 ~ "income",
			TRUE ~ "trans_desc"
		)) %>%
		group_by(y, line_type) %>%
		summarise(x_vec = paste(x, collapse = " "),
							min_x_start = min(x),
							max_x_end = max(x_end),
							max_y_end = max(y_end),
							text = str_c(text, collapse = " "),
							.groups = "drop") %>%
		# filter(!line_type %in% c("trans_date", "payment_method")) %>%
		arrange(y, min_x_start) %>%
		mutate(is_amt = if_else(lag(line_type %in% c("income", "expense")), TRUE, FALSE)) %>%
		mutate(is_amt = if_else(is.na(is_amt), TRUE, is_amt)) %>%
		mutate(cumsum = cumsum(is_amt)) %>%
		group_by(line_type, cumsum) %>%
		summarise(#x_vec = paste(x_vec, collapse = " "),
							min_y_start = min(y),
							max_y_end = max(max_y_end),
							min_x_start = min(min_x_start),
							max_x_end = max(max_x_end),
							text = str_c(text, collapse = " "),
							.groups = "drop") %>%
		arrange(cumsum) %>%
		select(cumsum, text, line_type) %>%
		tidyr::pivot_wider(names_from = line_type, values_from = text) %>%
		# head(10) %>%
		# View() %>%
		{.}

}
