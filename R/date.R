#' @importFrom stringr str_remove_all
#' @importFrom lubridate ymd

parse_statement_date <- function(x){

	ymd(str_remove_all(x, "-Statement.pdf"))
}

#' @importFrom stringr str_c
#' @importFrom lubridate dmy

extract_date <- function(df, page_num){

	x_pos <- c(52, 53, 62, 63, 75, 76, 77, 78, 79)

	df %>%
		filter(x %in% x_pos) %>%
		filter(!text %in% known_date_mismatches()) %>%
		group_by(y) %>%
		summarise(date_text = str_c(text, collapse = " ")) %>%
		mutate(dates = dmy(date_text)) %>%
		select(y, dates, date_text) %>%
		list()
}


known_date_mismatches <- function(){

	c("Date", "Acco", "A")
}
