library(rvest)
library(purrr)
library(xml2)
library(readr)
library(dplyr)
library(stringr)

#Função que pega apenas os posts da Rainha Frágil
extract_articles <- function(x){
  tryCatch({
    webpage <- read_html(x) 
    text <- html_nodes(webpage, ".post-content > p") 
    text_data <- html_text(text) 
    Article <- paste(text_data,collapse="\n") 
    return(Article) 
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

extract_links <- function(x){
  tryCatch({
    webpage <- read_html(x) 
    text <- html_nodes(webpage, "article") %>% html_nodes("h2") %>% html_nodes("a") %>% html_attr("href") 
    return(text)
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

#Looping que pega os posts de cada data
df <- data.frame()
df <- rbind(df, c("texto", "link"))
for(ano in 2008:2020){
  for(mes in 01:12){
    rainha_link <- str_c("https://fragilreino.com/", ano, "/", mes, "/")
    cat("acessando link ", rainha_link, "\n")
    links <- extract_links(rainha_link)
    for(i in 1:length(links)){
      cat("acessando link ", links[i], "\n")
      text_links <- extract_articles(links[i])
      cat(text_links)
      if (length(text_links) > 0){ 
        df <- rbind(df, c( str_c(mes, "/", ano, ",", i), text_links))
      }
    }
  }
}

#Corrigindo a dataframe
df <- df %>% slice(2:n())
colnames(df) <- c("Data", "Texto")

#Salvando dataframe em arquivo csv
write_csv(df, "rainha.todos.os.textos.csv")



