ec2024 <- read.csv("https://raw.githubusercontent.com/jcervas/Data/master/Elections/Presidential/Pres%20by%20State/president_state.csv")
head(ec2024)
dim(ec2024)



ec2024$lagging_one <- NA
ec2024$lagging_two <- NA



for (year in seq(1872,2020,4)) {
     lag <- year - 4
     ec2024$lagging_one[ec2024$year %in% year] <- 
          ec2024$dem[ec2024$year %in% lag]

}

for (year in seq(1876,2020,4)) {
     lag2 <- year - 8
     ec2024$lagging_two[ec2024$year %in% year] <- 
          ec2024$dem[ec2024$year %in% lag2]

}


xlim <- ylim <- c(0,1)
xlab <- "Democratic Share of\n Previous Election"
ylab <- "Democratic Share of Recent Election"

par(mfrow=c(1,2))
plot(
     x=ec2024$lagging_one, 
     y=ec2024$dem, 
     xlim=xlim, 
     ylim=ylim,
     xlab=xlab,
     ylab=ylab, 
     main="1872-2020")
reg1 <- lm(ec2024$dem ~ ec2024$lagging_one)
abline(reg1, col="red")
text(
     x=0.8,y=0.1, 
     paste0(
          "Slope: ",round(summary(reg1)$coefficients[2], d=3),
          "\nAdj-R-Sqr: ",round(summary(reg1)$adj.r.squared, d=3)),
     col="red",
     cex=0.85)

ec8420 <- subset(ec2024, year %in% seq(1984,2020,4))

plot(
     x=ec8420$lagging_one, 
     y=ec8420$dem, 
     xlim=xlim, 
     ylim=ylim,
     xlab=xlab,
     ylab=ylab, 
     main="1984-2020")
reg2 <- lm(ec8420$dem ~ ec8420$lagging_one)
abline(reg2, col="red")
text(
     x=0.8,y=0.1, 
     paste0(
          "Slope: ",round(summary(reg2)$coefficients[2], d=3),
          "\nAdj-R-Sqr: ",round(summary(reg2)$adj.r.squared, d=3)), 
     col="red",
     cex=0.85)





