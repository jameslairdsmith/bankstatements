#' @importFrom purrr map

clean_page <- function(page_data){

	map(page_data, clean_one_page)
}

clean_one_page <- function(page_data){

	page_data %>%
		behead_page() %>%
		drop_balance_rows() %>%
		drop_balance_col()
}

behead_page <- function(page_data){

	cut_off_point <-
		page_data %>%
		mutate(is_line = if_else(str_detect(text, "m") & lag(str_detect(text, "Pay")),
														 TRUE, FALSE)) %>%
		filter(is_line) %>%
		count(y) %>%
		pull(y)

	if(length(cut_off_point) != 1){
		stop("Too many page cutoffs", call. = FALSE)
	}

	page_data %>%
		filter(y > cut_off_point)
}

find_balance_rows <- function(page_data){

	page_data %>%
		mutate(balance_row_brought = if_else(str_detect(text, "BROUGHT") &
																				 	lag(str_detect(text, "BALANCE")),
																 TRUE, FALSE)) %>%
		mutate(balance_row_carried = if_else(str_detect(text, "CARRIED") &
																				 	lag(str_detect(text, "BALANCE")),
																				 TRUE, FALSE)) %>%
		mutate(is_balance_row = balance_row_carried | balance_row_brought) %>%
		filter(is_balance_row) %>%
		pull(y)
}

drop_balance_rows <- function(page_data){

	balance_rows <- find_balance_rows(page_data)

	page_data %>%
		filter(!y %in% balance_rows)
}

drop_balance_col <- function(page_data){

	page_data %>%
		filter(x < 500)
}
