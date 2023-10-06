

################################################################################
# annotate with GPT
library(stringr)
# Then, put your API key in the quotes below: 
my_API <- "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"

#The "hey_chatGPT function will help you access the API and prompt GPT 
hey_chatGPT <- function(answer_my_question) {
  chat_GPT_answer <- POST(
    url = "https://api.openai.com/v1/chat/completions",
    add_headers(Authorization = paste("Bearer", my_API)),
    content_type_json(),
    encode = "json",
    body = list(
      model = "gpt-3.5-turbo-0301",
      temperature = 0,
      messages = list(
        list(
          role = "user",
          content = answer_my_question
        )
      )
    )
  )
  str_trim(content(chat_GPT_answer)$choices[[1]]$message$content)
}

# Read in your dataset
data <- recent_search_body$data

# Create a "gpt" column
data$gpt <- NA

# Run a loop over your dataset and prompt ChatGPT - an example prompt for sentiment is given
for (i in 1:nrow(data)) {
  question <- "Is the sentiment of this text positive, neutral, or negative? Answer only with a number: 1 if positive, 2 if neutral, and 3 if negative. Here is the text:"
  text <- data$text[i]
  print(text)
  concat <- paste(question, text)
  result <- hey_chatGPT(concat)
  while(length(result) == 0){
    result <- hey_chatGPT(concat)
    print(result)
  }
  print(result)
  data$gpt[i] <- result
}

# Summarize data
hist(as.numeric(data$gpt))

test = data %>%
  filter(public_metrics$retweet_count > 100)

table(data$gpt)

# Get value counts
value_counts <- as.data.frame(table(data$gpt))

# Rename the columns
names(value_counts) <- c("Value", "Count")

# Create bar plot
ggplot(value_counts, aes(x = Value, y = Count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme_minimal() +
  xlab("Value") + 
  ylab("Count") +
  ggtitle("Bar plot of value counts")

##################

# Convert to weekday
data$weekday <- weekdays(ymd_hms(data$created_at), abbreviate = TRUE)

# Group by weekday and category
data_grouped <- data %>%
  group_by(weekday, gpt) %>%
  summarise(value = length(gpt), .groups = "drop")

# Order weekdays
data_grouped$weekday <- factor(data_grouped$weekday, levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))

# Create bar plot
ggplot(data_grouped, aes(x = weekday, y = value, fill = gpt)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  xlab("Weekday") +
  ylab("Summed Value") +
  ggtitle("Bar plot grouped by weekday and category")

# Create bar plot
ggplot(data_grouped, aes(x = weekday, y = value, fill = gpt)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  xlab("Weekday") +
  ylab("Summed Value") +
  ggtitle("Bar plot grouped by weekday and category")
