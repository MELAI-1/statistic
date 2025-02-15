#################LOAD THE NECESSERAY LIBRARY#############
install.packages("table1")

library(table1)
library(dplyr)
library(ggplot2)

################LOAD THE DATA ##########################
load("Meniscus.RData")

##########SUMMARY OF THE DATA################

#1-datavisualisation

#the type of the data
str(meni)

#the names of the variables
print("this dataset has:")
print(dim(meni))

#the name of the variables
print(names(meni))
#the first 6 rows of meni
print(head(meni))
#the last 6 rows of meni
print(tail(meni))
print("the variables of my dataset")

#2-checking of the missing data
print("the total number of missing values")
print(sum(is.na(meni)))

print("the total number of missing values per colomn")
print(colSums(is.na(meni)))


########### create table 1 ############################################
table1 <- table1(~ SDQ + math_conf + science_conf + religion + ageg, 
                 data = meni,
                 overall = TRUE)

########### number interview per days ############################

hist(meni$dateI, 
     breaks = "days",    # Regroupement par mois
     col = "lightblue", 
     main = "Number of Interviews per Day", 
     xlab = "Date(year 2022)", 
     ylab = "count"
     ,border='black')


# Séparer les données par district
meni_wakiso <- meni[meni$district == "Wakiso", ]
meni_kalungu <- meni[meni$district == "Kalungu", ]

# Créer un histogramme superposé
# Assurez-vous que la colonne dateI est bien au format Date
meni$dateI <- as.Date(meni$dateI)

# Séparer les données par district
meni_wakiso <- meni[meni$district == "Wakiso", ]
meni_kalungu <- meni[meni$district == "Kalungu", ]

# Définir les limites communes pour que les deux histogrammes aient la même échelle
xlim_range <- range(meni$dateI)  # Étendue des dates
ylim_range <- range(table(meni$dateI))  # Étendue des fréquences

# Créer un histogramme pour Wakiso (bleu clair)
hist(meni_wakiso$dateI, 
     breaks = "days", 
     col = rgb(0.1, 0.4, 0.8, 0.5), # Bleu semi-transparent
     border = "black",
     main = "Number of Interviews per Day",
     xlab = "Date (year 2022)",
     ylab = "Count"
)

# Ajouter Kalungu en superposition (rouge clair) sans axes
hist(meni_kalungu$dateI, 
     breaks = "days", 
     col = rgb(0.8, 0.2, 0.2, 0.5), # Rouge semi-transparent
     border = "black",
     add = TRUE,   # Superpose sur Wakiso
     axes = FALSE  # Empêche la redondance des axes
     
     
)

# Redessiner uniquement l'axe X
axis(1)  # Trace seulement l'axe X correctement

# Ajouter une légende corrigée
legend("topright", legend = c("Wakiso", "Kalungu"),
       fill = c(rgb(0.1, 0.4, 0.8, 0.5), rgb(0.8, 0.2, 0.2, 0.5)),
       border = "black")

library(ggplot2)

# Assurez-vous que la colonne dateI est bien au format Date
meni$dateI <- as.Date(meni$dateI)

# Histogramme avec ggplot2
library(ggplot2)
library(dplyr)  # Pour grouper les données

# Assurez-vous que la colonne dateI est bien au format Date
meni$dateI <- as.Date(meni$dateI)

# Histogramme des interviews par jour et par district
ggplot(meni, aes(x = dateI, fill = district)) +
  geom_bar(position = "stack", color = "black") +  # Nombre total par jour
  scale_fill_manual(values = c("Wakiso" = "lightblue", "Kalungu" = "salmon")) +
  labs(title = "Number of Interviews per Day",
       x = "Date (year 2022)",
       y = "Count",
       fill = "District") +
  theme_minimal()

