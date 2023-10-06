library("Durga")
library(readxl)

#Formatting - Combined Row 1 and Row 2 to create unambiguous column names
mothHeadings <- read_excel("Signalling Scoring Moths VitoCJI.xlsx", n_max = 1, .name_repair = "minimal")
Moths <- read_excel("Signalling Scoring Moths VitoCJI.xlsx", skip = 1)

butterflyHeadings <- read_excel("butterfly_data_Vito_CJI.xlsx", n_max = 1, .name_repair = "minimal")
Butterflies <- read_excel("butterfly_data_Vito_CJI.xlsx", skip = 1)


##A loop that fills each blank space with the content before it in the top rows
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

#Bar graphs
pageWidthLarge <- 12
pageHeightLarge <- 8
pagePaper <- 'special'

png(filename = "Figure 2.png", width = pageWidthLarge, height = pageHeightLarge
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


#Allocating Forewing Spots and Body Stripes
#Moth allocation
forewingSpots <- Moths$`Forewing Pattern` %in% c("patches", "dots", "spots")
hindwingSpots <- Moths$`Hindwing Pattern` %in% c("patches", "dots", "spots")
mothBodyStripes <- Moths$`Body Abdomen pattern` %in% c("vertical stripes",  "horizontal stripes")

#A score on a 4-5 is probably warning colored, while a 5 is highly probably it is warning coloured
forewingProbablyWarning <- Moths$`Forewing Warning Colour Score` >= 4
forewingHighWarning <- Moths$`Forewing Warning Colour Score` == 5 
hindwingProbablyWarning <- Moths$`Hindwing Warning Colour Score` >= 4
hindwingHighWarning <- Moths$`Hindwing Warning Colour Score` == 5 

#Butterfly allocation - Females only as most likely to be warning coloured over males
ventralWingSpots <- Butterflies$`Ventral side Female Pattern` %in% c("patches", "dots", "spots")
dorsalWingSpots <- Butterflies$`Dorsal Side Female Pattern` %in% c("patches", "dots", "spots")
butterflyBodyStripes <- Butterflies$`Body Female Abdomen pattern` %in% c("vertical stripes", "horizontal stripes")

ventralProbablyWarning <- Butterflies$`Ventral side Female Warning Colour Score` >=4
ventralHighWarning <- Butterflies$`Ventral side Female Warning Colour Score` == 5
dorsalProbablyWarning <- Butterflies$`Dorsal Side Female Warning Colour Score` >= 4
dorsalHighWarning <- Butterflies$`Dorsal Side Female Warning Colour Score` == 5


#Biological question: How likely is it that a moth/butterfly with both wing spots and abdomen spots are aposematic?
#For Moths
forewinga <- table(forewingSpots & mothBodyStripes, forewingProbablyWarning)
print(forewinga)
forewingProbably <- chisq.test(forewinga)
forewingProbably$expected
forewingProbably$observed

forewingb <- table(forewingSpots & mothBodyStripes, forewingHighWarning)
print(forewingb)
forewingHigh <- chisq.test(forewingb)
forewingHigh$expected
forewingHigh$observed

hindwinga <- table(hindwingSpots & mothBodyStripes, hindwingProbablyWarning)
print(hindwinga)
hindwingProbably <- chisq.test(hindwinga)
hindwingProbably$expected
hindwingProbably$observed

hindwingb <- table(hindwingSpots & mothBodyStripes, hindwingHighWarning)
print(hindwingb)
hindwingHigh <- chisq.test(hindwingb)
hindwingHigh$expected
hindwingHigh$observed

#For Butterflies
ventrala <- table(ventralWingSpots & butterflyBodyStripes, ventralProbablyWarning)
print(ventrala)
ventralProbably <- chisq.test(ventrala)
ventralProbably$expected
ventralProbably$observed

ventralb <- table(ventralWingSpots & butterflyBodyStripes, ventralHighWarning)
print(ventralb)
ventralHigh <- chisq.test(ventralb)
ventralHigh$expected
ventralHigh$observed

dorsala <- table(dorsalWingSpots & butterflyBodyStripes, dorsalProbablyWarning)
print(dorsala)
dorsalProbably <- chisq.test(dorsala)
dorsalProbably$expected
dorsalProbably$observed

dorsalb <- table(dorsalWingSpots & butterflyBodyStripes, dorsalHighWarning)
print(dorsalb)
dorsalHigh <- chisq.test(dorsalb)
dorsalHigh$expected
dorsalHigh$observed
#biological question: How likely is it that a moth/butterfly with just wingspots OR abdomen stripes are aposematic?

#one-signal probability
#For Moths
forewingc <- table(xor(forewingSpots, mothBodyStripes), forewingProbablyWarning)
print(forewingc)
forewingOneProbably <- chisq.test(forewingc)
forewingOneProbably$expected
forewingOneProbably$observed

forewingd <- table(xor(forewingSpots, mothBodyStripes), forewingHighWarning)
print(forewingd)
forewingOneHigh <- chisq.test(forewingd)
forewingOneHigh$expected
forewingOneHigh$observed

hindwingc <- table(xor(hindwingSpots, mothBodyStripes), hindwingProbablyWarning)
print(hindwingc)
hindwingOneProbably <- chisq.test(hindwingc)
hindwingOneProbably$expected
hindwingOneProbably$observed

hindwingd <- table(xor(hindwingSpots, mothBodyStripes), hindwingHighWarning)
print(hindwingd)
hindwingOneHigh <- chisq.test(hindwingd)
hindwingOneHigh$expected
hindwingOneHigh$observed

#For Butterflys
ventralc <- table(xor(ventralWingSpots, butterflyBodyStripes), ventralProbablyWarning)
print(ventralc)
ventralOneProbably <- chisq.test(ventralc)
ventralOneProbably$expected
ventralOneProbably$observed

ventrald <- table(xor(ventralWingSpots, butterflyBodyStripes), ventralHighWarning)
print(ventrald)
ventralOneHigh <- chisq.test(ventrald)
ventralOneHigh$expected
ventralOneHigh$observed

dorsalc <- table(xor(dorsalWingSpots, butterflyBodyStripes), dorsalProbablyWarning)
print(dorsalc)
dorsalOneProbably <- chisq.test(dorsalc)
dorsalOneProbably$expected
dorsalProbablyHigh$observed

dorsald <- table(xor(dorsalWingSpots, butterflyBodyStripes), dorsalHighWarning)
print(dorsald)
dorsalOneHigh <- chisq.test(dorsald)
dorsalOneHigh$expected
dorsalOneHigh$observed



#Question 1. How many moths are warning coloured? (4-5)
totalMothProbability <- forewingProbablyWarning | hindwingProbablyWarning
#130 moths are probably warning coloured, out of 472

#Are butterflies more likely to be aposematic? 2by2 (seperate questions)

#Question 2. Where on the wing are they? Hindwing and forewing and body. This utilises probably warning (4-5)
forewingTotal <- forewingSpots & forewingProbablyWarning
sum(forewingTotal)

hindwingTotal <- hindwingSpots & hindwingProbablyWarning
sum(hindwingTotal)

bodyTotal <- mothBodyStripes & totalMothProbability
sum(bodyTotal)

forewingBody <- forewingTotal & bodyTotal
sum(forewingBody)

hindwingBody <- hindwingTotal & bodyTotal
sum(hindwingBody)

bodywingTotal <- (forewingTotal | hindwingTotal) & bodyTotal
sum(bodywingTotal)

wingTotal <- forewingTotal | hindwingTotal

wingOnly <- wingTotal & !mothBodyStripes
sum(wingOnly)

location <- ifelse(wingTotal & !bodyTotal, "Wing", 
                   ifelse(!wingTotal & bodyTotal, "Body", 
                          ifelse(!wingTotal & !bodyTotal, "Neither", "Both")))
Moths$location <- location


#This is forewing OR (logical) hindwing)
#Based off forewing warning score

#Number of aposematic butterflies and where the signals are found
#Choosing dorsal to stay consistent with the moth book, going off dorsalProbabilityWarning
#134 out of 490 butterflys are >4
# NA'S?
sum(dorsalProbablyWarning)

butterflyButterflyWarning <- dorsalProbablyWarning | ventralProbablyWarning

butterflyBodyTotal <- butterflyBodyStripes & butterflyButterflyWarning
sum(butterflyBodyTotal)

dorsalWingTotal <- dorsalWingSpots & dorsalProbablyWarning
sum(butterflyWingTotal)

ventralWingTotal <- ventralWingSpots & ventralProbablyWarning

butterflyWingTotal <- ventralWingTotal | dorsalWingTotal

butterflyWingOnly <- butterflyWingTotal & !butterflyBodyStripes
sum(butterflyWingOnly)


#Re-do with Highly warning coloured species

#Question 1, how many moths and butterflies are highly like to be warning coloured
#Warning colours = Forewing and ventral
#Moths 52
#Butterflies 95

forewingHighTotal <- forewingSpots & forewingHighWarning
sum(forewingHighTotal)
ventralHighTotal <- ventralWingSpots & ventralHighWarning
sum(ventralHighTotal)

#Question 2. Where are these signals found
forewingOnlyLocation <- forewingHighTotal & !mothBodyStripes
sum(forewingOnlyLocation)

forewingBoth <- forewingHighTotal & mothBodyStripes
sum(forewingBoth)

ventralOnlyLocation <- ventralHighTotal & !butterflyBodyStripes
sum(ventralOnlyLocation)

ventralBoth <- ventralHighTotal & butterflyBodyStripes
sum(ventralBoth)

view(ventralBoth)
