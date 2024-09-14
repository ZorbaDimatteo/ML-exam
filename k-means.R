# Installare il pacchetto readxl (se non è già installato)
install.packages("readxl")
install.packages("here")

install.packages("ggplot2")
install.packages("cluster")

library(cluster) 
library(ggplot2)

# Caricare il pacchetto
library(readxl)

library(here)
library(dplyr) 

file_path <- here("online_retail_II.xlsx")

# Leggi i due fogli
df1 <- read_excel(file_path, sheet = "Year 2009-2010")
df2 <- read_excel(file_path, sheet = "Year 2010-2011")

# Unisci i dataset
dataset <- bind_rows(df1, df2)

# Visualizza il dataset combinato
head(dataset)

dataset <- dataset %>%
  rename(CustomerID = `Customer ID`)


# Riassumere le variabili numeriche
summary(dataset)
# capture.output(summary(dataset), file = "summary.txt")


# Contare la distribuzione dei CustomerID per ogni Country
country_distribution <- dataset %>%
  group_by(Country) %>%
  summarise(CustomerCount = n_distinct(CustomerID)) %>%
  arrange(desc(CustomerCount)) # ordinare in ordine decrescente di CustomerCount

# Visualizzare la distribuzione per country
# Filtrare solo i paesi con CustomerCount > 1000
filtered_distribution <- country_distribution %>%
  filter(CustomerCount > 10)

# Creare il grafico a barre
ggplot(filtered_distribution, aes(x = reorder(Country, -CustomerCount), y = CustomerCount)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Distribuzione dei clienti per Paese (CustomerCount > 10)",
       x = "Paese",
       y = "Numero di Clienti") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

dataset <- dataset %>% 
  
  select(-StockCode, -Description, -Country) 

dataset$InvoiceDate <- as.Date(dataset$InvoiceDate, format = "%Y-%m-%d")

# dataset ripulito
head(dataset)

# calcolo delle feature Recensy, TotalSpent, NetQuantity, InvoiceCount

# calcolo della recensy
dataset <- dataset %>% mutate( Recency = as.numeric(difftime(max(InvoiceDate), InvoiceDate, units = "days")), )
head(dataset)

dataset_clean <- dataset %>% 
  
  filter(!is.na(CustomerID)) %>% # filtro le righe in cui CustomerID è nullo o undefined
  
  group_by(CustomerID) %>% 
  
  summarise( 
    TotalSpent = sum(Quantity * Price), 
    NetQuantity = sum(Quantity), 
    InvoiceCount = n_distinct(Invoice), 
    Recency = min(Recency, na.rm = TRUE)
  ) %>% 
  
  filter(TotalSpent > 0) # filtrare clienti con spesa totale positivi

summary(dataset_clean)
head(dataset_clean)

#rimozione degli outlier prima della normalizzazione
# spiegare lo step sul documento

# Calcolare il primo e il terzo quartile di TotalSpent
Q1 <- quantile(dataset_clean$TotalSpent, 0.25, na.rm = TRUE)
Q3 <- quantile(dataset_clean$TotalSpent, 0.75, na.rm = TRUE)
IQR_value <- Q3 - Q1

# Definire i limiti oltre i quali considerare i valori come outliers
lower_bound <- Q1 - 1.5 * IQR_value
upper_bound <- Q3 + 1.5 * IQR_value

# Visualizzare i limiti
cat("Lower bound:", lower_bound, "\n")
cat("Upper bound:", upper_bound, "\n")

# Filtrare il dataset per rimuovere gli outliers di TotalSpent
dataset_clean <- dataset_clean %>%
  filter(TotalSpent >= lower_bound & TotalSpent <= upper_bound)

# Visualizzare il dataset senza outliers
head(dataset_clean)
summary(dataset_clean)
# normalizzazione
data_normalized <- scale(dataset_clean[, c("Recency","TotalSpent", "NetQuantity", "InvoiceCount")]) 

head(data_normalized)

## applicazione del k-means

## elbow method

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

# In entrambe le analisi il numero di k ottimale è 3 o 4


# Calcola il silhouette score per k = 3 
kmeans_result3 <- kmeans(data_normalized, centers = 3, nstart = 25) 

silhouette_result3 <- silhouette(kmeans_result3$cluster, dist(data_normalized))
mean(silhouette_result3[, 3])  # Punteggio medio silhouette

# Calcola il silhouette score per k = 4
kmeans_result4 <- kmeans(data_normalized, centers = 4, nstart = 25) 

silhouette_result4 <- silhouette(kmeans_result4$cluster, dist(data_normalized))
mean(silhouette_result4[, 3])  # Punteggio medio silhouette

# il silhouette score è migliore per k = 3

kmeans_result3$centers
kmeans_result3$size
kmeans_result3$cluster

dataset_clean$Cluster <- as.factor(kmeans_result3$cluster) 

summary(kmeans_result3)
summary(dataset_clean)
head(dataset_clean)

data <- dataset_clean

# Boxplot per la variabile TotalSpent divisa per Cluster
ggplot(data, aes(x = factor(Cluster), y = TotalSpent)) + 
  geom_boxplot() +
  labs(title = "Boxplot of TotalSpent for Cluster",
       x = "Cluster",
       y = "Total Spent in pounds") +
  theme_minimal()

# Boxplot per la variabile NetQuantity divisa per Cluster

ggplot(data, aes(x = factor(Cluster), y = NetQuantity)) + 
  geom_boxplot() +
  labs(title = "Boxplot di NetQuantity per Cluster",
       x = "Cluster",
       y = "Net Quantity") +
  theme_minimal()


# Boxplot per la variabile InvoiceCount divisa per Cluster
ggplot(data, aes(x = factor(Cluster), y = InvoiceCount)) + 
  geom_boxplot() +
  labs(title = "Boxplot di InvoiceCount per Cluster",
       x = "Cluster",
       y = "InvoiceCount") +
  theme_minimal()

# Boxplot per la variabile Recency divisa per Cluster
ggplot(data, aes(x = factor(Cluster), y = Recency)) + 
  geom_boxplot() +
  labs(title = "Boxplot di Recency per Cluster",
       x = "Cluster",
       y = "Recency") +
  theme_minimal()

