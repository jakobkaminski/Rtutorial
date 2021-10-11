library(readr)
p2 <- read_csv("~/ownCloud/Lehre/M28/Data/p2")

p2[!duplicated(p2$ID),]

write.csv(p2[!duplicated(p2$ID),],"~/ownCloud/Lehre/M28/Data/tablet2.csv")


#test new newer test test
