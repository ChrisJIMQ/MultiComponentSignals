library("Durga")
library(readxl)

##Formatting - Combined Row 1 and Row 2 to create unambiguous column names
mothHeadings <- read_excel("moth_data.xlsx", n_max = 1, .name_repair = "minimal")
Moths <- read_excel("moth_data.xlsx", skip = 1)

butterflyHeadings <- read_excel("butterfly_data.xlsx", n_max = 1, .name_repair = "minimal")
Butterflies <- read_excel("butterfly_data.xlsx", skip = 1)


#Formatting - A loop that fills each blank space with the content before it in the top rows
mothHeaders <- names(mothHeadings)
for (i in seq_along(mothHeaders)) {
  if (mothHeaders[i] == "") {
    mothHeaders[i] <- mothHeaders[i-1]
  }
}
names(Moths) <- paste(mothHeaders, mothHeadings)

butterflyHeaders <- names(butterflyHeadings)
for (i in seq_along(butterflyHeaders)) {
  if (butterflyHeaders[i] == "") {
    butterflyHeaders[i] <- butterflyHeaders[i-1]
  }
}
names(Butterflies) <- paste(butterflyHeaders, butterflyHeadings)

##Graphing Figure 1 - four bar graphs of each signalling score for the dorsal and ventral side of butterflies, and the forewing and hindwing of moths
pageWidthLarge <- 12
pageHeightLarge <- 8
pagePaper <- 'special'

#This code generates the figure within the project file, it has already been generated, and can be accessed.
png(filename = "Figure 1.png", width = pageWidthLarge, height = pageHeightLarge
    , units = "in", pointsize = 20,  res = 1000, type = "cairo")

layout(matrix(c(1,2,3, 4), nrow = 2, ncol = 2, byrow = TRUE))

par(mar = c(5,4,2,2)+0.1)

forewingCounts <- table(Moths$`Forewing Warning Colour Score`)
barplot(forewingCounts, col = c("grey", "#fff000", "#ffcc00", "#ff9900", "#ff6600", "#ff3300"))
mtext("A", side = 3, line = 0.5, cex = 1, adj = 0.05)

hindwingCounts <- table(Moths$`Hindwing Warning Colour Score`)
barplot(hindwingCounts, col = c("grey", "#fff000", "#ffcc00", "#ff9900", "#ff6600", "#ff3300"))
mtext("B", side = 3, line = 0.5, cex = 1, adj = 0.05)

ventralCounts <- table(Butterflies$`Ventral side Female Warning Colour Score`)
barplot(ventralCounts, col = c("grey", "#fff000", "#ffcc00", "#ff9900", "#ff6600", "#ff3300"))
mtext("C", side = 3, line = 0.5, cex = 1, adj = 0.05)

dorsalCounts <- table(Butterflies$`Dorsal Side Female Warning Colour Score`)
barplot(dorsalCounts, col = c("grey", "#fff000", "#ffcc00", "#ff9900", "#ff6600", "#ff3300"))
mtext("D", side = 3, line = 0.5, cex = 1, adj = 0.05)

dev.off()


##Defining "Wing signals" and "Body signals"
#Moth allocation
forewingSpots <- Moths$`Forewing Pattern` %in% c("patches", "dots", "spots")
hindwingSpots <- Moths$`Hindwing Pattern` %in% c("patches", "dots", "spots")
mothBodyStripes <- Moths$`Body Abdomen pattern` %in% c("vertical stripes",  "horizontal stripes")

#We will only include signals that score a 5 on the database, as that is most likely to be warning coloured
forewingHighWarning <- Moths$`Forewing Warning Colour Score` == 5 
hindwingHighWarning <- Moths$`Hindwing Warning Colour Score` == 5 

#Butterfly allocation
ventralWingSpots <- Butterflies$`Ventral side Female Pattern` %in% c("patches", "dots", "spots")
dorsalWingSpots <- Butterflies$`Dorsal Side Female Pattern` %in% c("patches", "dots", "spots")
butterflyBodyStripes <- Butterflies$`Body Female Abdomen pattern` %in% c("vertical stripes", "horizontal stripes")

ventralHighWarning <- Butterflies$`Ventral side Female Warning Colour Score` == 5
dorsalHighWarning <- Butterflies$`Dorsal Side Female Warning Colour Score` == 5

###This next section creates logical arguments to pull the data needed for the analysis. Necessary definitions are as follows:
## Warning Coloured Moth - If a 5 is scored on the forewing of the species
## Warning Coloured Butterfly - If a 5 is scored on the ventral side of the species
## All statistical analyses (chi- square tests) were completed in vassar stats (http://vassarstats.net/tab2x2.html) 


#Question 1, how many moths and butterflies are highly like to be warning coloured
forewingHighTotal <- forewingSpots & forewingHighWarning
sum(forewingHighTotal)
ventralHighTotal <- ventralWingSpots & ventralHighWarning
sum(ventralHighTotal)
#Moths = 52
#Butterflies = 95


#Question 2. Where are these signals found
forewingOnlyLocation <- forewingHighTotal & !mothBodyStripes
sum(forewingOnlyLocation)
forewingBoth <- forewingHighTotal & mothBodyStripes
sum(forewingBoth)
#Moths forewings only - 30
#Moths both forewings and abdomen - 22

ventralOnlyLocation <- ventralHighTotal & !butterflyBodyStripes
sum(ventralOnlyLocation)
ventralBoth <- ventralHighTotal & butterflyBodyStripes
sum(ventralBoth)
#Butterflies wings only - 65
#Butterflies wings and abdomen  - 30