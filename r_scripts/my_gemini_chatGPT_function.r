library(httr)

library(jsonlite)

chat_GPT_yek <- function(prompt, 
                    modelName = "gpt-4o-mini",
                    temperature = 1,
                    max_tokens = 2048,
                    top_p = 1,
                    apiKey = Sys.getenv("OPENAI_API_KEY")) {
  
  # Parameters
  params <- list(
    model = modelName,
    temperature = temperature,
    max_tokens = max_tokens,
    top_p = top_p
  )
  
  if(nchar(apiKey)<1) {
    apiKey <- readline("Paste your API key here: ")
    Sys.setenv(chatGPT_API_KEY = apiKey)
  }
  
  # Add the new message to the chat session messages
  chatHistory_GPT <<- append(chatHistory_GPT, list(list(role = "user", content = prompt)))
  
  response <- POST(
      url = "https://api.openai.com/v1/chat/completions",
      add_headers("Authorization" = paste("Bearer", apiKey)),
      content_type_json(),
      body = toJSON(c(params, list(messages = chatHistory_GPT)), auto_unbox = TRUE)
    )
    
    if (response$status_code > 200) {
      stop(content(response))
    }
    
    response <- content(response)
    answer <- trimws(response$choices[[1]]$message$content)
    chatHistory_GPT <<- append(chatHistory_GPT, list(list(role = "assistant", content = answer)))

  # return
  return(answer)
  
}

chat_gemini_yek <- function(prompt, 

                      temperature=0.5,

                      api_key=Sys.getenv("GOOGLE_API_KEY"),

                      model="gemini-1.5-flash-latest") {

  

  if(nchar(api_key)<1) {

    api_key <- readline("Paste your API key here: ")

    Sys.setenv(GEMINI_API_KEY = api_key)

  }

  

  model_query <- paste0(model, ":generateContent")

  

  # Add new message

  chatHistory_GEMINI <<- append(chatHistory_GEMINI, list(list(role = 'user', 

                                                parts = list(

                                                  list(text = prompt)

                                                ))))

  

  response <- POST(

    url = paste0("https://generativelanguage.googleapis.com/v1beta/models/", model_query),

    query = list(key = api_key),

    content_type_json(),

    body = toJSON(list(

      contents = chatHistory_GEMINI,

      generationConfig = list(

        temperature = temperature

      )

    ),  auto_unbox = T))

  

  if(response$status_code>200) {

    chatHistory_GEMINI <<- chatHistory_GEMINI[-length(chatHistory_GEMINI)]

    stop(paste("Status Code - ", response$status_code))

  } else {

    answer <- content(response)$candidates[[1]]$content$parts[[1]]$text

    chatHistory_GEMINI <<- append(chatHistory_GEMINI, list(list(role = 'model', 

                                          parts = list(list(text = answer)))))

  }

  

  return(answer)

  

}

chatHistory_GEMINI <- list() # I added this, however it can be set outside this script. It is necessary to keep track of previous chat.

chatHistory_GPT <- list()