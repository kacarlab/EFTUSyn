setwd("")

library(ggplot2)

# Import data with comma-separated vales; replace with appropriate path and file name
data <- read.table(file = "file.csv", header = TRUE, sep = ",", row.names = 1)

# transpose data
data_transpose <- t(data)
data_dataframe <- as.data.frame(data_transpose)

values <- c(data_dataframe$Founder, data_dataframe$Evolved)
groups <- rep(colnames(data_dataframe), each=3)

data_for_plot <- as.data.frame(cbind(values, groups))

data_for_plot$groups=factor(data_for_plot$groups, levels=c("Founder", "Evolved"))

#For boxplot
ggplot(data_for_plot, aes(x = groups, y = as.numeric(values), fill=groups))+ 
  geom_boxplot(color="black", fill="grey", alpha=0.4)+
  geom_point()+
  theme_bw()+
  theme(legend.position = "none")+
  labs(x = "" , y = "AUC", size=16)+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),axis.text.x = element_text(angle=0))+
  theme(axis.text.x= element_text(size=15, color="black"),axis.title.x=element_text(size=18),
        axis.text.y= element_text(size=15, color="black"),axis.title.y=element_text(size=18))+
  scale_y_continuous(limits=c(0.17,0.27), expand=c(0,0), breaks=c(0.17, 0.19, 0.21, 0.23, 0.25, 0.27))
