record_user_info <- function(con, table, userInfo) {
	dbWriteTable(con, table, as.data.frame(userInfo), row.names = FALSE, append = TRUE)
}
