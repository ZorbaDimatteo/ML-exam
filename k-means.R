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
data_normalized <- scale(features[, c("Recency", "Frequency", "Monetary")]) 

head(data_normalized)
