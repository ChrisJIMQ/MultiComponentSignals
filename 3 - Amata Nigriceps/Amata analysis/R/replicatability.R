library(readxl)
library(Durga)
library(rptR)

# Testing inter and intra replicability


########################################################################################################
# Read data and convert to long format
#Replicatability between analysers
files <- list.files("../data", "*Intereliability.xlsx", full.names = TRUE)

df <- do.call(rbind, lapply(files, function(file) {
  df <- read_xlsx(file)
  df <- as.data.frame(df)
  df[df == "N/A"] <- NA
  df$leftWing <- as.numeric(df$`Left Orange Wing Pixel #`) / as.numeric(df$`Left Whole Wing Pixel #`)
  df$rightWing <- as.numeric(df$`Right Orange Wing Pixel #`) / as.numeric(df$`Right Whole Wing Pixel #`)
  df$body <- as.numeric(df$`Body Orange Pixel #`) / as.numeric(df$`Whole body pixel count`)
  
  components <- c("leftWing", "rightWing", "body")
  
  stats::reshape(df, direction = "long",
                 varying = components,
                 idvar = "FrameID",
                 v.names = "proportion",
                 timevar = "component",
                 times = components
  )
}))

df$imageID <- paste(df$FrameID, df$component)



########################################################################################################
# Overall measurement reproducibility by different people


par(mfrow = c(3, 2), oma = c(0, 0, 3, 0))

subsets <- list(c("Chris", "kaity"), 
                c("Chris", "Paige"), 
                c("kaity", "Paige"))
for (names in subsets) {
  subset <- df$Analysis %in% names
  DurgaDiff(df, "proportion", "Analysis", groups = names, id.col = "imageID", na.rm = T, ci.conf = .99) |> DurgaPlot(bty = "n") 
  mtext(paste(names, collapse = " and "), 3, line = 2, xpd = NA, adj = 1.4, col = 2)
  repeatability <- rpt(proportion ~ (1 | imageID), "imageID", data = df[subset, ],
                       datatype = "Gaussian", nboot = 200, npermut = 0)
  print(names)
  cat(sprintf("Variance explained by the image: R^2 = %g%%\n", repeatability$R^2 * 100))
  #summary(repeatability)
  plot(repeatability)
}



########################################################################################################
# Threshold reproducibility

df$threshold <- ifelse(df$component == "leftWing", as.numeric(df$`Left Wing Threshold`),
                       ifelse(df$component == "rightWing", as.numeric(df$`Right Wing Threshold`),
                              as.numeric(df$`Body Threshold`)))

par(mfrow = c(3, 1))

for (names in subsets) {
  subset <- df$Analysis %in% names
  
  
  repeatability <- rpt(proportion ~ (1 | threshold), "threshold", data = df[subset, ],
                       datatype = "Gaussian", nboot = 200, npermut = 0)
  print(names)
  cat(sprintf("Variance explained by the image: R^2 = %g%%\n", repeatability$R^2 * 100))
  #summary(repeatability)
  plot(repeatability)
  mtext(paste(names, collapse = " and "), 3, line = 1, xpd = NA, adj = 0, col = 2)
}
# 

#User consistency
PersonData <- function(name) {
  documents <- list.files("../data", paste0("Round.*", name, ".xlsx"), full.names = TRUE)
  
  l <- lapply(documents, function(document) {
    df <- read_xlsx(document)
    df <- as.data.frame(df)
    df[df == "N/A"] <- NA
    df$leftWing <- as.numeric(df$`Left Orange Wing Pixel #`) / as.numeric(df$`Left Whole Wing Pixel #`)
    df$rightWing <- as.numeric(df$`Right Orange Wing Pixel #`) / as.numeric(df$`Right Whole Wing Pixel #`)
    df$body <- as.numeric(df$`Body Orange Pixel #`) / as.numeric(df$`Whole body pixel count`)
    df$RoundReplicate <- substring(document, 14 , 16)
    
    components <- c("leftWing", "rightWing", "body")
    
    df2 <-  stats::reshape(df, direction = "long",
                           varying = components,
                           idvar = "FrameID",
                           v.names = "proportion",
                           timevar = "component",
                           times = components
    )
    df2$GenPhotoID <- seq_len(nrow(df2))
    df2
  })
  do.call(rbind, l)
}

Person <- function(name, df) {
  d <- DurgaDiff(df, "proportion", "RoundReplicate", id.col = "GenPhotoID", na.rm = T, ci.conf = .95) 
  DurgaPlot(d, bty = "n") 
  print(d)
  mtext(name, 3, line = 2, xpd = NA, adj = 1.4, col = 2)
  repeatability <- rpt(proportion ~ (1 | GenPhotoID), "GenPhotoID", data = df,
                       datatype = "Gaussian", nboot = 200, npermut = 0)
  print(name)
  cat(sprintf("Variance explained by the image: R^2 = %g%%\n", repeatability$R^2 * 100))
  #summary(repeatability)
  plot(repeatability)
}

par(mfrow = c(2, 2), oma = c(0, 0, 3, 0))

Chris <- PersonData("Chris")
Rejected <- which(is.na(Chris$pro))
Person("Chris", Chris)
Kaity <- PersonData("Kaity")
Kaity$proportion[Rejected] <- NA
Kaity$proportion[22] <- NA
Person("Kaity", Kaity)
#By removing frames that should not have been done (also huge diff in 22)(For Kaity), r^2 = 70% for both chris and kaity, however systematic difference with kaity was found, as it does not pass 0