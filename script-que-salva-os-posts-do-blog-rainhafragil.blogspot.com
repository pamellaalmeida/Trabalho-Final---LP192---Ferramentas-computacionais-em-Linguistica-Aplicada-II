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
    texts <- html_nodes(webpage, "a[class=post]") 
    article <- "" 
    i <- 1
    autors <- html_nodes(webpage,"blockquote") %>% html_nodes("span")
    autors_vector <- c()
    for (autor in autors){
      if (i %%2==1){
        autors_vector <- c(autors_vector, html_text(autor))
      }
      i <- i+1
    }
    i <- 1
    for (text in texts){
      if(str_detect(autors_vector[i], "Samia")){ 
        article <- str_c(article, "\n", html_text(text))
      }
      i <- i + 1
    }
    return(article) 
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

#Looping que pega os posts de cada data
df <- data.frame()
df <- rbind(df, c("texto", "link"))
for(mes in c("04", "05", "06", "07", "08", "09", "10", "11", "12")){
  for (dia in c("01", "02", "03", "05", "07", "08", "10", "12", "14", "15", "16", "19", "20", "22", "24", "25", "26", "28", "30")){
    rainha_link <- str_c("http://web.archive.org/web/20030223203425/http://rainhafragil.blogspot.com/2002_", mes, "_", dia, "_rainhafragil_archive.html")
    cat("acessando link ", rainha_link, "\n")
    text_links <- extract_articles(rainha_link)
    cat(text_links)
    if (length(text_links) > 0){ 
      df <- rbind(df, c( str_c(2002, "/", mes), text_links))
    }
  }
}

#Corrigindo a dataframe
df <- df %>% slice(2:n())
colnames(df) <- c("Data", "Texto")

#Salvando dataframe em arquivo csv
write_csv(df, "rainha.textos.do.blog.frainhafragil.blogspot.csv")
