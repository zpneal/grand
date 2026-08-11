rm(list=ls())
dat <- read.csv("data.csv", header = TRUE)

feedback <- data.frame(item = character(), a = numeric(), b = numeric(), c = numeric(), notes = character())

items <- c("purpose", "style", "notes", 
           "source", "multiple", "system", "location", "date", "data", "citation",
           "mode", "nodes", "boundary", "order", 
           "plex", "direction", "weight", "loops", "edges", "npartite", "hypergraph", "size",
           "method", "ethics",
           "degree", "missing", "unmeasured",
           "symmetrizing", "binarizing", "projecting", "excluding", "aggregating", "imputing",
           "connected", "connectivity", "reciprocity", "change",
           "edit", "other")

for (item in items) {
  col <- which(colnames(dat)==paste0(item, "1"))
  feedback <- rbind(feedback, data.frame(item = substr(item, 1, nchar(item)),
                                         a = sum(dat[,col]=="Agree" | dat[,col]=="Yes"),
                                         b = sum(dat[,col]=="Agree, but I have some minor suggestions"),
                                         c = sum(dat[,col]=="Disagree" | dat[,col]=="No"),
                                         notes = paste(dat[,col+1][which(dat[,col+1]!="")], collapse = "\n\n=====\n\n")))
}

col <- which(colnames(dat)=="comments")
feedback <- rbind(feedback, data.frame(item = "comments",
                                       a = 0,
                                       b = 0,
                                       c = 0,
                                       notes = paste(dat[,col][which(dat[,col]!="")], collapse = "\n\n=====\n\n")))

feedback$agree <- feedback$a
feedback$agree_but <- feedback$b
feedback$disagree <- feedback$c

feedback <- feedback[,c("item", "agree", "agree_but", "disagree", "notes")]

write.csv(feedback, "feedback2.csv", row.names = FALSE)