---
title: "ngramfun"
author: "suszanna"
date: "2/6/2021"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

#' Data Cleaning and Input Function; Calls the matching functions
```{r}
ngrams <- function(input){
  # Create a dataframe
  input <- data_frame(text = input)
  # Clean the Inpput
  replace_reg <- "[^[:alpha:][:space:]]*"
  input <- input %>%
    mutate(text = str_replace_all(text, replace_reg, ""))
  # Find word count, separate words, lower case
  input_count <- str_count(input, boundary("word"))
  input_words <- unlist(str_split(input, boundary("word")))
  input_words <- tolower(input_words)
  # Call the matching functions
  y <- ifelse(input_count == 1, bigram(input_words), 
              ifelse (input_count == 2, trigram(input_words), quadgram(input_words)))
  # Output
  paste(input, y, sep = " ")
}

```

Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.
