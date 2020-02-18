
rm(list = ls())

library(reshape2)
library(ggplot2)
library(ggthemes)

setwd("/home/gueguema/Documents/_TUTOS/3_R/_PACKAGES/RFate/R_supplements/")

#####################################################################################
tab = read.table("PNE_editParamsC24.csv", header = TRUE, sep = ";")

## ORIGINAL
ggplot(tab, aes(x = group)) +
  geom_segment(aes(xend = group, y = SOIL_MIN, yend = SOIL_MAX)) +
  geom_point(aes(y = SOIL_CONTRIB)) +
  annotate("rect", xmin = 1, xmax = 6
           , ymin = max(tab$SOIL_MIN[grep("^C", tab$group)])
           , ymax = min(tab$SOIL_MAX[grep("^C", tab$group)])
           , fill = "#d95f0e", alpha = 0.5) +
  annotate("rect", xmin = 7, xmax = 16
           , ymin = max(tab$SOIL_MIN[grep("^H", tab$group)])
           , ymax = min(tab$SOIL_MAX[grep("^H", tab$group)])
           , fill = "#31a354", alpha = 0.5) +
  annotate("rect", xmin = 17, xmax = 24
           , ymin = max(tab$SOIL_MIN[grep("^P", tab$group)])
           , ymax = min(tab$SOIL_MAX[grep("^P", tab$group)])
           , fill = "#1c9099", alpha = 0.5) +
  scale_y_continuous(breaks = seq(0.5, 6, 0.5)) +
  theme_fivethirtyeight() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


## MODIFIED
inc = 0.9
dec = 0.9

ggplot(tab, aes(x = group)) +
  geom_segment(aes(xend = group, y = SOIL_MIN + inc, yend = SOIL_MAX - dec)) +
  geom_point(aes(y = SOIL_CONTRIB)) +
  # annotate("rect", xmin = 1, xmax = 6
  #          , ymin = max(tab$SOIL_MIN[grep("^C", tab$group)] + inc)
  #          , ymax = min(tab$SOIL_MAX[grep("^C", tab$group)] - dec)
  #          , fill = "#d95f0e", alpha = 0.5) +
  # annotate("rect", xmin = 7, xmax = 16
  #          , ymin = max(tab$SOIL_MIN[grep("^H", tab$group)] + inc)
  #          , ymax = min(tab$SOIL_MAX[grep("^H", tab$group)] - dec)
  #          , fill = "#31a354", alpha = 0.5) +
  # annotate("rect", xmin = 17, xmax = 24
  #          , ymin = max(tab$SOIL_MIN[grep("^P", tab$group)] + inc)
  #          , ymax = min(tab$SOIL_MAX[grep("^P", tab$group)] - dec)
  #          , fill = "#1c9099", alpha = 0.5) +
  scale_y_continuous(breaks = seq(0.5, 6, 0.5)) +
  theme_fivethirtyeight() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



#####################################################################################
tab = read.table("PNE_editParamsC24.csv", header = TRUE, sep = ";")
tab$group = factor(tab$group, tab$group[order(tab$SOIL_CONTRIB)])
tab = tab[order(tab$group), ]


## ORIGINAL
ggplot(tab, aes(x = group)) +
  geom_hline(yintercept = c(1, 2.25, 3.75, 5), col = "grey40", lty = 2) +
  geom_segment(aes(xend = group, y = SOIL_MIN, yend = SOIL_MAX)) +
  geom_point(aes(y = SOIL_CONTRIB)) +
  annotate("rect", xmin = 1, xmax = 24
           , ymin = max(tab$SOIL_MIN)
           , ymax = min(tab$SOIL_MAX)
           , fill = "#d95f0e", alpha = 0.5) +
  scale_y_continuous(breaks = seq(0.5, 6, 0.5)) +
  theme_fivethirtyeight() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


group1 = 1:7
group2 = 8:15
group3 = 16:24
group1 = which(tab$SOIL_CONTRIB < 2.25)
group2 = which(tab$SOIL_CONTRIB >= 2.25 & tab$SOIL_CONTRIB <= 3.5)
group3 = which(tab$SOIL_CONTRIB > 3.5)

end1 = length(group1)
end2 = length(group1) + length(group2)

ggplot(tab, aes(x = group)) +
  geom_hline(yintercept = c(1, 2.25, 3.75, 5), col = "grey40", lty = 2) +
  geom_segment(aes(xend = group, y = SOIL_MIN, yend = SOIL_MAX)) +
  geom_point(aes(y = SOIL_CONTRIB)) +
  annotate("rect", xmin = 1, xmax = end1
           , ymin = max(tab$SOIL_MIN[group1])
           , ymax = min(tab$SOIL_MAX[group1])
           , fill = "#d95f0e", alpha = 0.5) +
  annotate("rect", xmin = end1 + 1, xmax = end2
           , ymin = max(tab$SOIL_MIN[group2])
           , ymax = min(tab$SOIL_MAX[group2])
           , fill = "#31a354", alpha = 0.5) +
  annotate("rect", xmin = end2 + 1, xmax = nrow(tab)
           , ymin = max(tab$SOIL_MIN[group3])
           , ymax = min(tab$SOIL_MAX[group3])
           , fill = "#1c9099", alpha = 0.5) +
  scale_y_continuous(breaks = seq(0.5, 6, 0.5)) +
  theme_fivethirtyeight() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


## MODIFIED
inc = 0.6
dec = 0.6

ggplot(tab, aes(x = group)) +
  geom_segment(aes(xend = group, y = SOIL_MIN + inc, yend = SOIL_MAX - dec)) +
  geom_point(aes(y = SOIL_CONTRIB)) +
  scale_y_continuous(breaks = seq(0.5, 6, 0.5)) +
  theme_fivethirtyeight() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


group1 = 1:7
group2 = 8:15
group3 = 16:24
group1 = which(tab$SOIL_CONTRIB < 2.25)
group2 = which(tab$SOIL_CONTRIB >= 2.25 & tab$SOIL_CONTRIB <= 3.5)
group3 = which(tab$SOIL_CONTRIB > 3.5)

end1 = length(group1)
end2 = length(group1) + length(group2)

ggplot(tab, aes(x = group)) +
  geom_hline(yintercept = c(1, 2.25, 3.75, 5), col = "grey40", lty = 2) +
    geom_segment(aes(xend = group, y = SOIL_MIN + inc, yend = SOIL_MAX - dec)) +
  geom_point(aes(y = SOIL_CONTRIB)) +
  annotate("rect", xmin = 1, xmax = end1
           , ymin = max(tab$SOIL_MIN[group1] + inc)
           , ymax = min(tab$SOIL_MAX[group1] - dec)
           , fill = "#d95f0e", alpha = 0.5) +
  annotate("rect", xmin = end1 + 1, xmax = end2
           , ymin = max(tab$SOIL_MIN[group2] + inc)
           , ymax = min(tab$SOIL_MAX[group2] - dec)
           , fill = "#31a354", alpha = 0.5) +
  annotate("rect", xmin = end2 + 1, xmax = nrow(tab)
           , ymin = max(tab$SOIL_MIN[group3] + inc)
           , ymax = min(tab$SOIL_MAX[group3] - dec)
           , fill = "#1c9099", alpha = 0.5) +
  scale_y_continuous(breaks = seq(0.5, 6, 0.5)) +
  theme_fivethirtyeight() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

