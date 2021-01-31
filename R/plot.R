#' @importFrom ggplot2 ggplot aes geom_point scale_y_reverse
#' @importFrom ggrepel geom_text_repel

plot_page <- function(page_data){

	page_data %>%
		ggplot(aes(x = x, y = y, label = text)) +
		geom_point() +
		geom_text_repel(size = 2) +
		scale_y_reverse()
}

plot_dummy_page <- function(slice = 1){

	make_dummy_page(slice) %>%
		plot_page()
}
