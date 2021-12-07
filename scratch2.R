get_statement_data() %>%
	# filter(!str_detect(trans_desc, "Brian")) %>%
	mutate(month_date = lubridate::floor_date(trans_date, "month")) %>%
	mutate(expense_class = case_when(
		str_detect(trans_desc, "MINISTRY") ~ "Clothes",
		str_detect(trans_desc, "PARCELFORCE") ~ "Clothes",
		str_detect(trans_desc, "BLUNDSTONE") ~ "Clothes",
		str_detect(trans_desc, "ECONOMIST SUBSCRIP") ~ "News sub",
		str_detect(trans_desc, "THE BREAKFAST CLUB") ~ "Resturants",
		str_detect(trans_desc, "45 Jermyn Street London") ~ "Resturants",
		str_detect(trans_desc, "PAPAJOHNS") ~ "Fast Food",
		str_detect(trans_desc, "Brian") ~ "Rent",
	)) %>%
	group_by(expense_class, month_date) %>%
	summarise(expenses = sum(expense, na.rm = T)) %>%
	ggplot(aes(x = month_date, y = expenses, fill = expense_class)) +
	ggplot2::geom_col()
	# filter(!is.na(income))
	# ggplot(aes(x = trans_date, y = income)) +
	# ggplot2::geom_col()

get_statement_data() %>%
	arrange(desc(expense)) %>%
	View()
	# head(20)
