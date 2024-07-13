library(yfR)
library(plyr)
library(tidyr)
sp500 = GetSP500Stocks()

setwd = ("C:/Users/abner/Documents/R")
ibov = read.csv("C:/Users/abner/Documents/R/acoes-listadas_excel.csv", 
                  sep=';',header=TRUE, stringsAsFactors=FALSE,
                 quote="",encoding= "latin1")

ibov$TickersSa = paste(ibov$Código, '.SA', sep='')

dados_ibov = yf_get(
  tickers = head(ibov$TickersSa),
  first_date = Sys.Date() - 5,
  last_date = Sys.Date(),
  bench_ticker = benchmark
)

dados_ibov2 = dlply(dados_ibov, .(ticker), 
              function(x){rownames(x) = x$row; x$row= NULL; x})

#PETR4SA = dados_ibov2["PETR4.SA"]
#colunas = dados_ibov$ticker

'''
#Renomear colunas 
colnames(dados_ibov) = c("ref_date",
                         paste("price_adjusted", dados_ibov[[]]))
'''
acao = dados_ibov2[[1]][,c("ref_date", "price_adjusted")]

colnames(acao) = c("ref_date", paste("price_adjusted", dados_ibov2[[2]][1,c("ticker")]))


for (i in 1:nrow(dados_ibov)) {
  
  matriz_acao = dados_ibov2[[i]][,c("ref_date", "price_adjusted")]
  
  colnames(matriz_acao) = c("ref_date", paste("price_adjusted", dados_ibov2[[i]][1,c("ticker")]))
  
  acao = merge(acao,matriz_acao, by="ref_date")
} 

f = 
ggplot() +
  geom_line(data=acao, aes(x=ref_date, y=`price_adjusted VIIA3.SA`, color="Via Varejo"))+
  geom_line(data=acao, aes(x=ref_date, y=`price_adjusted MGLU3.SA.y`, color="Magalu"))+
  geom_line(data=acao, aes(x=ref_date, y=`price_adjusted HAPV3.SA`, color="Hapvida"))+
  geom_line(data=acao, aes(x=ref_date, y=`price_adjusted NTCO3.SA`, color="Natura"))+
  geom_line(data=acao, aes(x=ref_date, y=`price_adjusted PETR4.SA`, color="Petrobras"))+
  
xlab("Data")+
ylab("preco")

print(f)

f$labels$colour = "Acoes"




