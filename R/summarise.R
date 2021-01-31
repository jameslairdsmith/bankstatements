#' @importFrom tidyr fill pivot_wider
#' @importFrom readr parse_number
#' @importFrom lubridate dmy

statement_summarise <- function(data){

	data %>%
		mutate(line_type = case_when(
			x < 100 ~ "trans_date",
			x > 100 & x < 120 ~ "payment_method",
			x > 350 & x < 400 ~ "expense",
			x > 400 ~ "income",
			TRUE ~ "trans_desc"
		)) %>%
		filter(!(line_type == "trans_date" & text %in% known_date_mismatches())) %>%
		group_by(statement_date, page_number, y, line_type) %>%
		summarise(x_vec = paste(x, collapse = " "),
							min_x_start = min(x),
							max_x_end = max(x_end),
							max_y_end = max(y_end),
							text = str_c(text, collapse = " "),
							.groups = "drop") %>%
		arrange(statement_date, page_number, y, min_x_start) %>%
		group_by(statement_date, page_number) %>%
		mutate(is_amt = if_else(lag(line_type %in% c("income", "expense")), TRUE, FALSE)) %>%
		mutate(is_amt = if_else(is.na(is_amt), TRUE, is_amt)) %>%
		mutate(transaction_id = cumsum(is_amt)) %>%
		group_by(statement_date, page_number, line_type, transaction_id) %>%
		summarise(
			min_y_start = min(y),
			max_y_end = max(max_y_end),
			min_x_start = min(min_x_start),
			max_x_end = max(max_x_end),
			text = str_c(text, collapse = " "),
			.groups = "drop") %>%
		select(statement_date, page_number, transaction_id, text, line_type) %>%
		pivot_wider(names_from = line_type, values_from = text) %>%
		select(statement_date,
					 page_number,
					 transaction_id,
					 trans_date,
					 trans_desc,
					 income,
					 expense) %>%
		arrange(desc(statement_date), page_number, transaction_id) %>%
		mutate(trans_date = dmy(trans_date),
					 expense = parse_number(expense),
					 income = parse_number(income)) %>%
		fill(trans_date, .direction = "down")
}
