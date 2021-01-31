list_statements <- function(){

	file_loc <- bank_statement_file()

	sort(list.files(file_loc), decreasing = TRUE)
}


bank_statement_file <- function(file_name = ""){

	system.file("bank_statements",
							file_name,
							package = "bankstatements",
							mustWork = TRUE)
}
