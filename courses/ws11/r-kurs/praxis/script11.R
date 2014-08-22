taste <- read.table("Taste.csv", header = TRUE)
attach(taste)

groups =  integer(20)
groups[LIQ*SCR== 0] = "LIQ=SCR=0"
groups[16:20] = "LIQ=SCR=1"
groups[LIQ == 1] = "LIQ = 1"
groups[SCR == 1] = "SCR = 1"

SCORE=c(SCORE,SCORE[ LIQ*SCR==1 ])

pairwise.t.test(SCORE,groups, p.adjust ="fdr")
#LIQ = as.factor(LIQ)
#SCR = as.factor(SCR)

#groups = as.factor(LIQ*SCR)

taste.lm <- lm(SCORE ~ LIQ * SCR)
summary(taste.lm)


