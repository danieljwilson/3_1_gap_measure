################################################################################
# download tweets

require(httr)
require(jsonlite)
require(dplyr)

bearer_token <- "AAAAAAAAAAAAAAAAAAAAAG73dgEAAAAAOfuygaOp8QFqH40KwTuslB9LYGg%3Ddg3kFUhMs9TNN8pG9aBHYXzdN29P7n2tlTaFCajsrdBmyQBngb"
headers <- c(`Authorization` = sprintf('Bearer %s', bearer_token))

params = list(
  `query` = 'from:lorenz_spreen',
  `max_results` = '500',
  `tweet.fields` = 'created_at,lang,conversation_id,public_metrics',
  `since_id` = '1610914805560545280'
)


response <- httr::GET(url = 'https://api.twitter.com/2/tweets/search/all', httr::add_headers(.headers=headers), query = params)

recent_search_body <-
  content(
    response,
    as = 'parsed',
    type = 'application/json',
    simplifyDataFrame = TRUE
  )

View(recent_search_body$data)

