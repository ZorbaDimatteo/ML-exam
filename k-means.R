# Installare il pacchetto readxl (se non è già installato)
install.packages("readxl")

# Caricare il pacchetto
library(readxl)

setwd("***")

# Caricare un file Excel specificando il percorso
dataset <- read_excel("./online_retail_II.xlsx")

# Visualizzare le prime righe del dataset
head(dataset)


# Visualizzare la struttura del dataset
str(dataset)

# Riassumere le variabili numeriche
summary(dataset)



