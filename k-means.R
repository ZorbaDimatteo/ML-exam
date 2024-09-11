# Installare il pacchetto readxl (se non è già installato)
install.packages("readxl")
install.packages("here")


# Caricare il pacchetto
library(readxl)

library(here)
library(dplyr) 

file_path <- here("online_retail_II.xlsx")

dataset <- read_excel(file_path)

# Visualizza i nomi delle colonne prima della modifica
colnames(dataset)

# Rinomina una colonna (es. rinominare 'old_name' in 'new_name')
dataset <- dataset %>%
  rename(CustomerID = `Customer ID`)

# Verifica i nuovi nomi delle colonne
colnames(dataset)


# Visualizzare le prime righe del dataset
head(dataset)


# Visualizzare la struttura del dataset
str(dataset)

# Riassumere le variabili numeriche
summary(dataset)

# Filtra le righe con CustomerID mancante e salvale in un nuovo dataframe
data_missing_customer <- dataset %>%
  filter(is.na(CustomerID))

dataset <- dataset %>% 
  
  select(-Invoice, -StockCode, -Description) 

# Verifica la nuova struttura

head(data_missing_customer)

# Calcolare il totale netto per ogni cliente 

features <- dataset %>% 
  
  filter(!is.na(CustomerID)) %>% # filtro le righe in cui CustomerID è nullo o undefined
  
  group_by(CustomerID) %>% 
  
  summarise( 
    
    TotalSpent = sum(Quantity * Price), 
    
    NetQuantity = sum(Quantity), 
    
    Frequency = n(), 
    
    Recency = as.numeric(difftime(Sys.Date(), max(InvoiceDate), units = "days")), 
    
    Monetary = sum(Quantity * Price) 
    
  ) %>% 
  
 filter(NetQuantity > 0)  # Opzionale: filtrare clienti con acquisti netti positivi 

head(dataset)
summary(dataset)
summary(features)
head(features)

# normalizzazione
data_normalized <- scale(features[, c("Recency", "Frequency", "Monetary")]) 

head(data_normalized)

## applicazione del k-means

## elbow method
# 
install.packages("factoextra")
install.packages("ggplot2")

library(factoextra)
library(ggplot2)

set.seed(123)  # Per rendere i risultati riproducibili

# Esegui k-means per un range di cluster (ad esempio da 1 a 10 cluster)
wcss <- function(k) {
  kmeans(data_normalized, k, nstart = 25)$tot.withinss
}

# Calcola WCSS per ciascun valore di k
k.values <- 1:10
wcss_values <- sapply(k.values, wcss)

# Crea un grafico per visualizzare il gomito
plot(k.values, wcss_values, type="b", pch = 19, frame = FALSE,
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares (WCSS)",
     main="Elbow Method for Optimal K")


# Metodo del Silhouette Score 

install.packages("cluster")

library(cluster) 

set.seed(123)  # Per rendere i risultati riproducibili

# Funzione per calcolare il Silhouette Score per ogni numero di cluster
calculate_silhouette <- function(k) {
  km.res <- kmeans(data_normalized, centers = k, nstart = 25)
  ss <- silhouette(km.res$cluster, dist(data_normalized))
  mean(ss[, 3])  # Ritorna il punteggio medio silhouette
}

# Valori di k da testare (ad esempio, da 2 a 10 cluster)
k.values <- 2:10
silhouette_scores <- sapply(k.values, calculate_silhouette)


# Crea un grafico del Silhouette Score per ciascun valore di k
plot(k.values, silhouette_scores, type = "b", pch = 19, frame = FALSE,
     xlab = "Number of clusters K",
     ylab = "Average Silhouette Score",
     main = "Silhouette Method for Optimal K")


# In entrambe le analisi il numero di k ottimale è 8

