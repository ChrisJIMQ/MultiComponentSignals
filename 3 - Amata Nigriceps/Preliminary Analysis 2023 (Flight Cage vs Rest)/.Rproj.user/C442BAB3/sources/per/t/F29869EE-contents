library("Durga")
library(readxl)

#Durga for estimating and visualising Effect sizings

flight <- read_excel("Insect Flight Cage.xlsx", na = c("N/A", "NA"))
View(flight)

rest <- read_excel("Resting Spreadsheet.xlsx", na = c("N/A", "NA"))
View(rest)

tether <- read_excel("Tethering.xlsx", na = c("N/A", "NA"))
View(tether)

Measurements <- read_excel("Amata Measurements.xlsx", na = c("N/A", "NA"))
View(Measurements)
#Separate spreadsheets of experiments, taken from masterdoc

#InterReliability Analysis
interReliabilityChris <- read_excel("Chris - Intereliability .xlsx", na = c("N/A", "NA"))
View(interReliabilityChris)

interReliabilityPaige <- read_excel("Paige - Intereliability.xlsx", na = c("N/A", "NA"))
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
#No significant difference between the two
di <- DurgaDiff(noNA, groups = c("Tester 1" = "Chris", "Tester 2" = "Paige"))
DurgaPlot(di, left.ylab = "Proportion of orange visible")

#Main Analysis 
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


#Proportions of each segment, now average them

#Averaging the 3 rest replicates
#Rest Columns
meanrest <- aggregate(rest[, c("WProp", "BProp")],
                      list(mothID = rest$`Moth No.`), 
                      FUN = function(x) mean(x, na.rm = TRUE))
meanrest$condition <- "Resting"

#Flight Columns
#IFC = Insect Flight Cage
meanflight <- aggregate(flight[, c("WProp", "BProp")],
                        list(mothID = flight$`Moth No.`), 
                        FUN = function(x) mean(x, na.rm = TRUE))
meanflight$condition <- "IFC"

#Tether Columns
meantether <- aggregate(tether[, c("WProp", "BProp")],
                         list(mothID = tether$`Moth No.`),
                         FUN = function(x) mean(x, na.rm = TRUE))
meantether$condition <- "Tethering"



#### Spreadsheet organisation ####
#First step: Turn every wing/body pixel value into a percentage. Create a new spreadhseet with these pixel values?

#if condition = flying then it is insect flight cage, but if combinedcondition = Flying then it is either Insect Flight Cage or Tethering
prelim <- rbind(meanrest, meanflight, meantether)
prelim$combinedcondition <- ifelse(prelim$condition == "Resting", "Resting", "IFC")

prelimMeasurements <- Measurements[-14, ]

#Preliminary analysis of body sizes 
prelim$forewinglength <- prelimMeasurements$`Forewing length (mm)`
prelim$bodylength <- prelimMeasurements$`Body length (mm)`
prelim$bodywidth <- prelimMeasurements$`body width (mm)`
prelim$hindwinglength <- prelimMeasurements$`Hindwing length (mm)`

plot(WProp ~ forewinglength, prelim[prelim$combinedcondition == "Resting", ])
RfwlL <- lm(WProp ~ forewinglength, prelim[prelim$combinedcondition == "Resting", ])
abline(RfwlL)
summary(RfwlL)
plot(WProp ~ forewinglength, prelim[prelim$combinedcondition == "IFC", ])
FlightfwlL <- lm(WProp ~ forewinglength, prelim[prelim$combinedcondition == "IFC", ])
summary(FlightfwlL)

plot(BProp ~ bodylength, prelim[prelim$combinedcondition == "Resting", ])
RblL <-  lm(BProp ~ bodylength, prelim[prelim$combinedcondition == "Resting", ])
summary(RblL)
plot(BProp ~ bodylength, prelim[prelim$combinedcondition == "IFC", ])
FlightblL <- lm(BProp ~ bodylength, prelim[prelim$combinedcondition == "IFC", ])
summary(FlightblL)



#Report R-Squared, F-Statistic, slope (forewinglength Estimate), and p-value
#We fitted a linear model, and no evidence of a relationship was found in.. 6 times


#LWd <- DurgaDiff(LWProp*100~condition, prelim)
#par(mar = c(5,4,0.5, 1)+0.1)
#DurgaPlot(LWd, box = TRUE, left.ylab = "Left Wing Orange %")

#RWd <- DurgaDiff(RWProp*100~condition, prelim)
#par(mar = c(5,4,0.5, 1)+0.1)
#DurgaPlot(RWd, box = TRUE, left.ylab = "Right Wing Orange %")

Wd <- DurgaDiff(WProp*100~condition, prelim)
par(mar = c(5,4,0.5, 1)+0.1)
DurgaPlot(Wd, left.ylab = "Wing Orange % Visibility")
Wd

Bd <- DurgaDiff(BProp*100~condition, prelim)
par(mar = c(5,4,0.5, 1)+0.1)
DurgaPlot(Bd, left.ylab = "Body Orange % Visibility")
Bd

#l <- lm((LWProp / RWProp)~Angle, flight)
#summary(l)

#combinedcondition is for presentations
#For paper, use condition


#Convert prelim from wide to long format Flight vs Rest
prelimLong <- data.frame(mothID = rep(prelim$mothID, 2), 
                         totprop = c(prelim$WProp, prelim$BProp),
                         bodyPart = c(rep("Wing", nrow(prelim)), rep("Body", nrow(prelim))),
                                      condition = rep(prelim$condition, 2))
Flyd <- DurgaDiff(totprop*100~bodyPart, prelimLong[prelimLong$condition == "IFC", ], id.col = "mothID")
par(mar = c(5,4,0.5, 1)+0.1)
DurgaPlot(Flyd, left.ylab = "% of orange visible")

Restd <- DurgaDiff(totprop*100~bodyPart, prelimLong[prelimLong$condition == "Resting", ], id.col = "mothID")
par(mar = c(5,4,0.5, 1)+0.1)
DurgaPlot(Restd, left.ylab = "% of orange visible")
Restd

Tetherd <- DurgaDiff(totprop*100~bodyPart, prelimLong[prelimLong$condition == "Tethering",], id.col = "mothID")
par(mar = c(5,4,0.5, 1)+0.1)
DurgaPlot(Tetherd, left.ylab = "% of orange visible")
Tetherd

#Reject the Null hypothesis at a 95% CI
#Paige and Chris difference
#Body Length and wing length

#Transferring the plots into grouped figures

#Flight vs Rest - Seperate experiments
pageWidthLarge <- 12
pageHeightLarge <- 6
pagePaper <- 'special'

png(filename = "Figure 1.png", width = pageWidthLarge, height = pageHeightLarge
     , units = "in", pointsize = 20,  res = 1000, type = "cairo")

layout(matrix(c(1,2,3), nrow = 1, ncol = 3, byrow = TRUE))

par(mar = c(5,4,2,2)+0.1)
#par(mar = c(5,4,0.5, 1)+0.1)


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

pageWidthLarge <- 12
pageHeightLarge <- 6
pagePaper <- 'special'

#Body vs wing - between experiments
pageWidthLarge <- 12
pageHeightLarge <- 6
pagePaper <- 'special'

png(filename = "Figure 2.png", width = pageWidthLarge, height = pageHeightLarge
    , units = "in", pointsize = 20,  res = 1000, type = "cairo")

layout(matrix(c(1,2), nrow = 1, ncol = 2, byrow = TRUE))

par(mar = c(4,4,0.5, 1)+0.1)

Bd <- DurgaDiff(BProp*100~condition, prelim)
DurgaPlot(Bd, left.ylab = "Body Orange % Visibility",
          error.bars.type = "SD")
mtext("A", side = 3, line = -2.25, cex = 1, adj = 0.05)
Bd

Wd <- DurgaDiff(WProp*100~condition, prelim)
DurgaPlot(Wd, left.ylab = "Wing Orange % Visibility",
          error.bars.type = "SD")
mtext("B", side = 3, line = -2.25, cex = 1, adj = 0.05)
Wd

dev.off()

##Full violins?
