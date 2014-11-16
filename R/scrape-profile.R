scrape_profile <- function(profileId) {
	userAgent <- 'Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/37.0.2062.120 Safari/537.36'

	profileUrl <- paste("http://www.amazon.com/gp/pdp/profile", profileId, sep="/")
	profilePage <- rvest::html(httr::GET(profileUrl, httr::user_agent(userAgent)))

	extract_number <- function(numericString) {
		gsub("\\D", "", numericString, perl = TRUE) %>% as.integer
	}

	nodes <- profilePage %>% rvest::html_nodes('.profile-info span') %>% rvest::html_text()

	ranking <- nodes %>% grep("ranking:", ., value = TRUE) %>% extract_number()

	votes <- nodes %>% grep("\\(\\d[^ ]* of \\d[^)]*\\)", ., value = TRUE) %>% stringr::str_split(" of ") %>% unlist %>% extract_number()
	reviewsHelpful <- votes[1]
	reviewsVoted <- votes[2]

	reviews <- profilePage %>% rvest::html_nodes('.reviews-link') %>% rvest::html_text() %>% extract_number()

	list(userid = profileId,
		  rank = ranking,
		  reviews = reviews,
		  reviews_voted = reviewsVoted,
		  reviews_helpful = reviewsHelpful,
		  timestamp = format(Sys.time(), "%F %T %z")
	)
}
