library(ggplot2)
df <-read.table(file="VernData.csv",
                header=TRUE,
                sep=",",
                stringsAsFactors=FALSE)

df$VernDuration <- as.factor(df$VernDuration)

cols <- c("darkcyan", "darkgoldenrod2", "brown3", "aquamarine3", "bisque1")

g <- ggplot(data=df,
            mapping=aes(x=VernDuration, y=TimeToFlower, fill=VernDuration))+
  geom_boxplot()

gp <- g + scale_x_discrete(labels=c("0 \n(control)","2","4","6","8")) +
  scale_fill_manual(values=cols) + 
  labs(y = "Days to Flowering", x = "Duration of Vernalization \n (week)") +
  theme(legend.position = "none",
        text = element_text(size = 16, family = "serif"))

print(gp)


g <- ggplot(data=df, aes(x=TimeToFlower)) +  geom_density(alpha = 0.1, aes(fill=VernDuration, colour=VernDuration)) + labs(x="Days to Flowering")

print(g)
