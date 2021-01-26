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
    texts <- html_nodes(webpage, "p") %>% html_nodes("span[class=posts]") 
    article <- "" 
    i <- 1
    autors <- html_nodes(webpage,"td[class=tabela2]") %>% html_nodes("span[class=link1]")
    autors_vector <- c()
    for (autor in autors){
      if (i %%2==1){
        autors_vector <- c(autors_vector, html_text(autor))
      }
      i <- i+1
    }
    i <- 1
    for (text in texts){
      if(str_detect(autors_vector[i], "Rainha")){ 
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
for(ano in 2004:2008){
  for (mes in c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")){
    rainha_link <- str_c("http://fragilreino.blogspot.com/", ano, "/", mes, "/?interstitial=ABqL8_iWd40r-jrsJ2LX_aOxOgqaoGyavAb4OKJqH7-KSPBZTl__yt8NoCJ-M8j8")
    cat("acessando link ", rainha_link, "\n")
    text_links <- extract_articles(rainha_link)
    cat(text_links)
    if (length(text_links) > 0){ 
      df <- rbind(df, c( str_c(mes, "/", ano), text_links))
    }
  }
}

#Corrigindo a dataframe
df <- df %>% slice(2:n())
colnames(df) <- c("Data", "Texto")

#Salvando dataframe em arquivo csv
write_csv(df, "rainha.textos.do.blogspot.csv")


