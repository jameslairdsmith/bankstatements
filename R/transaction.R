#' @importFrom dplyr left_join

date_transactions <- function(transactions_df, dates_df){

	transactions_df %>%
		arrange(y_paymt) %>%
		left_join(dates_df,
							by = c("y_paymt" = "y")) %>%
		list()
}

#' @importFrom readr parse_number

extract_expense <- function(df){

	df %>%
		mutate(x_end = x + width) %>%
		filter(x_end %in% 387:390) %>%
		filter(!text %in% known_amount_exclusions()) %>%
		mutate(amount = -parse_number(text)) %>%
		filter(!is.na(amount)) %>%
		select(x, y, x_end, amount)
}

extract_income <- function(df){

	df %>%
		mutate(x_end = x + width) %>%
		filter(x_end %in% 470:472) %>%
		filter(!text %in% known_amount_exclusions()) %>%
		mutate(amount = parse_number(text)) %>%
		filter(!is.na(amount)) %>%
		select(x, y, x_end, amount)
}

known_amount_exclusions <- function(){

	c("Contact", "see", "ut", "in")
}

#' @importFrom dplyr bind_cols bind_rows

extract_transactions <- function(df, page_num){

	extract_expense(df) %>%
		bind_rows(extract_income(df)) %>%
		arrange(y) %>%
		rename(x_trans = x,
					 y_trans = y,
					 x_trans_end = x_end) %>%
		bind_cols(extract_payment_method(df, page_num)) %>%
		list()

}

extract_payment_method <- function(df, page_num){

	if(page_num == 1){
		x_pos <- 113
	} else {
		x_pos <- 112
	}

	df %>%
		mutate(x_end = x + width) %>%
		filter(x %in% x_pos) %>%
		select(x, y, x_end, payment_method = text) %>%
		arrange(y) %>%
		rename(x_paymt = x,
					 y_paymt = y,
					 x_paymt_end = x_end) %>%
		list()
}

