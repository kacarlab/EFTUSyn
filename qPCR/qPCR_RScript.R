setwd("")

library(ggplot2)

# Import data with comma-separated vales; replace with appropriate path and file name
data <- read.table(file = "file.csv", header = TRUE, sep = ",", row.names = 1)

data.df <- as.data.frame(data)

data_transpose <- t(data)
data_dataframe <- as.data.frame(data_transpose)

values <- c(data.df$CH6138, data.df$CH6139, data.df$CH6556, data.df$CH6585)
groups <- rep(colnames(data.df), each=3)

data_for_plot <- as.data.frame(cbind(values, groups))

data_for_plot$groups=factor(data_for_plot$groups, levels=c("CH6138", "CH6139", "CH6556", "CH6585"))

ggplot(data_for_plot, aes(x = groups, y = as.numeric(values), fill=groups))+ 
  geom_boxplot(color="black", fill="grey", alpha=0.4)+
  geom_point()+
  scale_color_brewer(palette = "Set1")+
  theme_bw()+
  theme(legend.position = "none")+
  labs(x = "" , y = '', size=16)+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),axis.text.x = element_text(angle=0))+
  theme(axis.text.x= element_text(size=15, color="black"),axis.title.x=element_text(size=18),
        axis.text.y= element_text(size=15, color="black"),axis.title.y=element_text(size=18))+
  scale_y_continuous(limits=c(0,4), expand=c(0,0), breaks=c(0, 0.5, 1.0, 1.5, 2.0, 2.5, 3.0, 3.5, 4.0))

Rel606.vs.dtufA <- t.test(data.df$'Rel606', data.df$'dtufA')
Rel606.vs.CH6138 <- t.test(data.df$'Rel606', data.df$'CH6138')
Rel606.vs.CH6139 <- t.test(data.df$'Rel606', data.df$'CH6139')
Rel606.vs.CH6556 <- t.test(data.df$'Rel606', data.df$'CH6556')
Rel606.vs.CH6585 <- t.test(data.df$'Rel606', data.df$'CH6585')
dtufA.vs.CH6138 <- t.test(data.df$'dtufA', data.df$'CH6138')
dtufA.vs.CH6139 <- t.test(data.df$'dtufA', data.df$'CH6139')
dtufA.vs.CH6556 <- t.test(data.df$'dtufA', data.df$'CH6556')
dtufA.vs.CH6585 <- t.test(data.df$'dtufA', data.df$'CH6585')
CH6138.vs.CH6139 <- t.test(data.df$'CH6138', data.df$'CH6139')
CH6138.vs.CH6556 <- t.test(data.df$'CH6138', data.df$'CH6556')
CH6138.vs.CH6585 <- t.test(data.df$'CH6138', data.df$'CH6585')
CH6139.vs.CH6556 <- t.test(data.df$'CH6139', data.df$'CH6556')
CH6139.vs.CH6585 <- t.test(data.df$'CH6139', data.df$'CH6585')
CH6556.vs.CH6585 <- t.test(data.df$'CH6585', data.df$'CH6556')

p.values <- cbind(Rel606.vs.dtufA$p.value, Rel606.vs.CH6138$p.value, Rel606.vs.CH6139$p.value, Rel606.vs.CH6556$p.value, Rel606.vs.CH6585$p.value, dtufA.vs.CH6138$p.value, dtufA.vs.CH6139$p.value, dtufA.vs.CH6556$p.value, dtufA.vs.CH6585$p.value, CH6138.vs.CH6139$p.value, CH6138.vs.CH6556$p.value, CH6138.vs.CH6585$p.value, CH6139.vs.CH6556$p.value, CH6139.vs.CH6585$p.value, CH6556.vs.CH6585$p.value)
p.values.t <- t(p.values)
p.values.df <- as.data.frame(p.values.t)
row.names(p.values.df) <- c("wtvdA", "wtv38", "wtv39", "wtv56", "wtv85", "dAv38", "dAv39", "dAv56", "dAv85", "38v39", "38v56", "38v85", "39v56", "39v85", "56v85")

write.table(p.values.df, file = "qPCR_wt_norm_pvalues.txt", col.names=F, row.names=T, quote=F, sep='\t')
