library(tidyverse)


comp <- read_csv("assessment.csv")

colnames(comp) <- c("gorupid", "itemid", "cat", "title", "priority", "presonal_score", "desired_score", "gap")


comp$cat <- as.factor(comp$cat)
# comp$title <- as.factor(comp$title)


compgath <- comp %>% gather(key = "score_group", value = "ll", presonal_score:gap)

orderedLevels <- compgath %>% group_by(score_group) %>%
  filter(score_group == "gap") %>%
  arrange(ll, desc(priority))  %>% ungroup() %>% select(title)
orderedLevels <- as.vector(orderedLevels$title)

compgath$title
compgath$title <- factor(compgath$title, levels =  as.vector(orderedLevels))

compgath %>%
ggplot( aes(x=title,color=cat,fill=as.factor(priority )), size=.52) + 
  theme_light() +
  guides(fill=guide_legend(title="Priority"), 
         color=FALSE)+ #guide_legend(title=NULL,label = FALSE)) +
  geom_bar(data=subset(compgath,score_group=="gap"), aes(y=ll),alpha =.6, stat="identity") +
  geom_bar(data=subset(compgath,score_group=="presonal_score"), aes(y=ll * (-1)),alpha =.6, stat="identity") +
  geom_hline(aes(yintercept = 0)) + 
  scale_y_continuous(breaks=-4:4,labels=abs(-4:4),limits = c(-5,5) ) +   # breaks=-4:4,labels=abs(-4:4) + 
  ylab("Curent Level (left), Gap (right)") + 
  xlab("Competency") + 
  ggtitle("Competencies Assessment")+ 
  scale_color_brewer(palette = "Set1") + 
  coord_flip()

