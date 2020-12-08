## code to prepare `hc` dataset goes here

tsdist <- TSclust::diss(t(last_60), "DTWARP")
names(tsdist) <- colnames(last_60)

save(tsdist,  file = "data/tsdist.RData")
