rm(list=ls())
dat <- read.csv("data.csv", row.names = 1, header = TRUE)

feedback <- data.frame(item = character(), sagree = numeric(), agree = numeric(), neutral = numeric(), disagree = numeric(), sdisagree = numeric(), notes = character())
items <- c("purpose1", "style1", "tree1", "types1", 
           "data.type1", "data.source1", "data.count1", "data.system1", "data.data1",
           "nodes.meaning1", "nodes.count1", "nodes.attribute1",
           "edges.direction1", "edges.weight1", "edges.multiplex1", "edges.structural1", "edges.meaning1", "edges.count1",
           "data.procedure1", "data.ethics1", "data.degree1", "data.nodes1",
           "trans.sym1", "trans.bin1", "trans.proj1", "trans.exclude1", "trans.impute1",
           "connected1", "connectivity1",
           "other1")

for (item in items) {
  col <- which(colnames(dat)==item)
  feedback <- rbind(feedback, data.frame(item = item,
                                         sagree = sum(dat[,col]=="Strongly agree"),
                                         agree = sum(dat[,col]=="Somewhat agree"),
                                         neutral = sum(dat[,col]=="Neither agree nor disagree"),
                                         disagree = sum(dat[,col]=="Somewhat disagree"),
                                         sdisagree = sum(dat[,col]=="Strongly disagree"),
                                         notes = paste(dat[,col+1][which(dat[,col+1]!="")], collapse = "\n\n=====\n\n")))
}

feedback <- rbind(feedback, data.frame(item = "comments",
                                       sagree = 0,
                                       agree = 0,
                                       neutral = 0,
                                       disagree = 0,
                                       sdisagree = 0,
                                       notes = paste(dat$comments[which(dat$comments!="")], collapse = "\n\n=====\n\n")))

feedback <- feedback[, c("item", "sagree", "agree", "neutral", "disagree", "sdisagree", "notes")]
write.csv(feedback, "feedback1.csv", row.names = FALSE)
