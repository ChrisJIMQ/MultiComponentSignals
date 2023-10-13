library("Durga")
library(readxl)
##Durga is used for estimating and comparing effect sizes##
##All figures are already pre-generated and can be found within the output folder##
#Universal figure dimensions
pageWidthLarge <- 12
pageHeightLarge <- 6
pagePaper <- 'special'

##Definitions##
#flight OR fly = Insect Flight Cage experiment
#Rest = Resting photo experiment
#Tether = Tethering experiment
#Measurements = Wing and body measurements


flight <- read_excel("~/MultiComponentSignals/3 - Amata Nigriceps/Amata analysis/data/Insect Flight Cage.xlsx", na = c("N/A", "NA"))
View(flight)

rest <- read_excel("~/MultiComponentSignals/3 - Amata Nigriceps/Amata analysis/data/Resting Spreadsheet.xlsx", na = c("N/A", "NA"))
View(rest)

tether <- read_excel("~/MultiComponentSignals/3 - Amata Nigriceps/Amata analysis/data/Tethering.xlsx", na = c("N/A", "NA"))
View(tether)

Measurements <- read_excel("~/MultiComponentSignals/3 - Amata Nigriceps/Amata analysis/data/Amata Measurements.xlsx", na = c("N/A", "NA"))
View(Measurements)

##InterReliability Analysis - Comparing the two scorers to ensure there is no innate differences/bias between the two
#Tester 1 = Chris
#Tester 2 = Paige
interReliabilityChris <- read_excel("~/MultiComponentSignals/3 - Amata Nigriceps/Amata analysis/data/Chris - Intereliability .xlsx", na = c("N/A", "NA"))
View(interReliabilityChris)

interReliabilityPaige <- read_excel("~/MultiComponentSignals/3 - Amata Nigriceps/Amata analysis/data/Paige - Intereliability.xlsx", na = c("N/A", "NA"))
View(interReliabilityPaige)

interReliabilityChris$LWProp <- interReliabilityChris$`Left Orange Wing Pixel #` / 
  interReliabilityChris$`Left Whole Wing Pixel #`
interReliabilityChris$RWProp <- interReliabilityChris$`Right Orange Wing Pixel #` / 
  interReliabilityChris$`Right Whole Wing Pixel #`
interReliabilityChris$BProp <- interReliabilityChris$`Body Orange Pixel #` / 
  interReliabilityChris$`Whole body pixel count`


interReliabilityPaige$LWProp <- interReliabilityPaige$`Left Orange Wing Pixel #` / 
  interReliabilityPaige$`Left Whole Wing Pixel #`
interReliabilityPaige$RWProp <- interReliabilityPaige$`Right Orange Wing Pixel #` / 
  interReliabilityPaige$`Right Whole Wing Pixel #`
interReliabilityPaige$BProp <- interReliabilityPaige$`Body Orange Pixel #` / 
  interReliabilityPaige$`Whole body pixel count`

bodyComparison <- interReliabilityChris[, c("Video ID", "Moth No.", "Camera Angle", "Camera Side", "BProp")]
names(bodyComparison) <- c("Video ID", "Moth No.", "Camera Angle", "Camera Side", "Chris")
bodyComparison$Paige <- interReliabilityPaige$BProp
bodyComparison$segment <- "Body"

leftWingComparison <- interReliabilityChris[, c("Video ID", "Moth No.", "Camera Angle", "Camera Side", "LWProp")]
names(leftWingComparison) <- c("Video ID", "Moth No.", "Camera Angle", "Camera Side", "Chris")
leftWingComparison$Paige <- interReliabilityPaige$LWProp
leftWingComparison$segment <- "Left Wing"

rightWingComparison <- interReliabilityChris[, c("Video ID", "Moth No.", "Camera Angle", "Camera Side", "RWProp")]
names(rightWingComparison) <- c("Video ID", "Moth No.", "Camera Angle", "Camera Side", "Chris")
rightWingComparison$Paige <- interReliabilityPaige$RWProp
rightWingComparison$segment <- "Right Wing"

Comparison <- rbind(leftWingComparison, rightWingComparison, bodyComparison)
noNA <- na.omit(Comparison)
t.test(noNA$Chris, noNA$Paige, paired = TRUE)
#Paired t.test to compare if the two analysers were comparable, 33 frames were compared from both scorers
di <- DurgaDiff(noNA, groups = c("Tester 1" = "Chris", "Tester 2" = "Paige"))

png(filename = "~/MultiComponentSignals/3 - Amata Nigriceps/Amata analysis/output/Figure B1.png", width = pageWidthLarge, height = 8
    , units = "in", pointsize = 20,  res = 1000, type = "cairo")
DurgaPlot(di, left.ylab = "Proportion of orange visible")
dev.off()

#### Spreadsheet organisation ####
##Calculating the proportion of orange shown by each moth in each experiment
#Calculating the proportion of each individual segment
rest$LWProp <- rest$`Left Orange Wing Pixel #` / 
  rest$`Left Whole Wing Pixel #`
rest$RWProp <- rest$`Right Orange Wing Pixel #` / 
  rest$`Right Whole Wing Pixel #`
rest$BProp <- rest$`Body Orange Pixel #` / 
  rest$`Whole body pixel count`
rest$WProp <- rowMeans(rest[, c("LWProp", "RWProp")], na.rm = TRUE)

flight$LWProp <- flight$`Left Orange Wing Pixel #` / 
  flight$`Left Whole Wing Pixel #`
flight$RWProp <- flight$`Right Orange Wing Pixel #` / 
  flight$`Right Whole Wing Pixel #`
flight$BProp <- flight$`Body Orange Pixel #` / 
  flight$`Whole body pixel count`
flight$WProp <- rowMeans(flight[, c("LWProp", "RWProp")], na.rm = TRUE)

tether$LWProp <- tether$`Left Orange Wing Pixel #` / 
  tether$`Left Whole Wing Pixel #`
tether$RWProp <- tether$`Right Orange Wing Pixel #` / 
  tether$`Right Whole Wing Pixel #`
tether$BProp <- tether$`Body Orange Pixel #` / 
  tether$`Whole body pixel count`
tether$WProp <- rowMeans(tether[, c("LWProp", "RWProp")], na.rm = TRUE)

#Generating the mean proportion of orange shown by each moth in each experiment
#IFC = Insect Flight Cage
meanrest <- aggregate(rest[, c("WProp", "BProp")],
                      list(mothID = rest$`Moth No.`), 
                      FUN = function(x) mean(x, na.rm = TRUE))
meanrest$condition <- "Resting"

meanflight <- aggregate(flight[, c("WProp", "BProp")],
                        list(mothID = flight$`Moth No.`), 
                        FUN = function(x) mean(x, na.rm = TRUE))
meanflight$condition <- "IFC"

meantether <- aggregate(tether[, c("WProp", "BProp")],
                         list(mothID = tether$`Moth No.`),
                         FUN = function(x) mean(x, na.rm = TRUE))
meantether$condition <- "Tethering"

##Preliminary analysis - testing for a relationship between body and wing size
#if condition = flying then it is insect flight cage, but if combinedcondition = Flying then it is either Insect Flight Cage or Tethering
#"combined condition" used for the preliminary analysis, and for presentation, "condition" will be used for final thesis/paper product
prelim <- rbind(meanrest, meanflight, meantether)
prelim$combinedcondition <- ifelse(prelim$condition == "Resting", "Resting", "IFC")

#This moth was not used in the analysis, as only males were used in the analysis. So it needs to be removed
prelimMeasurements <- Measurements[-14, ]

prelim$forewinglength <- prelimMeasurements$`Forewing length (mm)`
prelim$bodylength <- prelimMeasurements$`Body length (mm)`
prelim$bodywidth <- prelimMeasurements$`body width (mm)`
prelim$hindwinglength <- prelimMeasurements$`Hindwing length (mm)`

png(filename = "~/MultiComponentSignals/3 - Amata Nigriceps/Amata analysis/output/Figure B2.png", width = pageWidthLarge, height = 8
    , units = "in", pointsize = 20,  res = 1000, type = "cairo")

layout(matrix(c(1,2,3, 4), nrow = 2, ncol = 2, byrow = TRUE))

par(mar = c(5,4,2,2)+0.1)

plot(WProp ~ forewinglength, prelim[prelim$combinedcondition == "Resting", ], pch = 21, col = "black", bg = 4)
RfwlL <- lm(WProp ~ forewinglength, prelim[prelim$combinedcondition == "Resting", ])
abline(RfwlL, col = "blue", lwd = 2)
mtext("A", side = 3, line = -1.5, cex = 1, adj = 0.05)
summary(RfwlL)
plot(WProp ~ forewinglength, prelim[prelim$combinedcondition == "IFC", ], pch = 21, col = "black", bg = 4)
FlightfwlL <- lm(WProp ~ forewinglength, prelim[prelim$combinedcondition == "IFC", ])
abline(FlightfwlL, col = "blue", lwd = 2)
mtext("B", side = 3, line = -1.5, cex = 1, adj = 0.05)
summary(FlightfwlL)

plot(BProp ~ bodylength, prelim[prelim$combinedcondition == "Resting", ], pch = 21, col = "black", bg = 4)
RblL <-  lm(BProp ~ bodylength, prelim[prelim$combinedcondition == "Resting", ])
abline(RblL, col = "blue", lwd = 2)
mtext("C", side = 3, line = -1.5, cex = 1, adj = 0.05)
summary(RblL)
plot(BProp ~ bodylength, prelim[prelim$combinedcondition == "IFC", ], pch = 21, col = "black", bg = 4)
FlightblL <- lm(BProp ~ bodylength, prelim[prelim$combinedcondition == "IFC", ])
abline(FlightblL, col = "blue", lwd = 2)
mtext("D", side = 3, line = -1.5, cex = 1, adj = 0.05)
summary(FlightblL)

dev.off()

##Analysis 1 - Is there a difference between the wing orange % visibility in the body and wing in each experiment?
#Analysis 1 = Body VS Wing
#Convert prelim from wide to long format
prelimLong <- data.frame(mothID = rep(prelim$mothID, 2), 
                         totprop = c(prelim$WProp, prelim$BProp),
                         bodyPart = c(rep("Wing", nrow(prelim)), rep("Body", nrow(prelim))),
                         condition = rep(prelim$condition, 2))

png(filename = "~/MultiComponentSignals/3 - Amata Nigriceps/Amata analysis/output/Figure 5.png", width = pageWidthLarge, height = pageHeightLarge
    , units = "in", pointsize = 20,  res = 1000, type = "cairo")

layout(matrix(c(1,2,3), nrow = 1, ncol = 3, byrow = TRUE))

par(mar = c(5,4,2,2)+0.1)

Restd <- DurgaDiff(totprop*100~bodyPart, prelimLong[prelimLong$condition == "Resting", ], id.col = "mothID")
DurgaPlot(Restd, left.ylab = "% of orange visible",
          error.bars.type = "SD")
Restd
mtext("A", side = 3, line = -2.25, cex = 1, adj = 0.05)

Flyd <- DurgaDiff(totprop*100~bodyPart, prelimLong[prelimLong$condition == "IFC", ], id.col = "mothID")
DurgaPlot(Flyd, left.ylab = "% of orange visible",
          error.bars.type = "SD")
mtext("B", side = 3, line = -2.25, cex = 1, adj = 0.05)

Tetherd <- DurgaDiff(totprop*100~bodyPart, prelimLong[prelimLong$condition == "Tethering",], id.col = "mothID")
DurgaPlot(Tetherd, left.ylab = "% of orange visible",
          error.bars.type = "SD")
Tetherd
mtext("C", side = 3, line = -2.25, cex = 1, adj = 0.05)

dev.off()

##Analysis 2 - Is there a difference between the wing orange % visibility between the three experiments when looking at the body and wing seperately
#Analysis 2 = Flight VS Rest
png(filename = "~/MultiComponentSignals/3 - Amata Nigriceps/Amata analysis/output/Figure 6.png", width = pageWidthLarge, height = pageHeightLarge
    , units = "in", pointsize = 20,  res = 1000, type = "cairo")

layout(matrix(c(1,2), nrow = 1, ncol = 2, byrow = TRUE))

par(mar = c(4,4,0.5, 1)+0.1)

Bd <- DurgaDiff(BProp*100~condition, prelim)
DurgaPlot(Bd, left.ylab = "Body Orange % Visibility")
mtext("A", side = 3, line = -2.25, cex = 1, adj = 0.05)
Bd

Wd <- DurgaDiff(WProp*100~condition, prelim)
DurgaPlot(Wd, left.ylab = "Wing Orange % Visibility")
mtext("B", side = 3, line = -2.25, cex = 1, adj = 0.05)
Wd

dev.off()


