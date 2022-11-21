# install.packages("ggplot2")
# install.packages("raster")
# install.packages("rgdal")
# install.packages("classInt")
# install.packages("RColorBrewer")
library(ggplot2)
library(raster)
library(rgdal)
library(classInt)
library(RColorBrewer)
install.packages("gridExtra")
require(gridExtra)
delim = ";"  # or is it "\t" ?
dec = ","    # or is it "," ?
dados <- read.csv("result.csv", header=FALSE, sep=delim, dec=dec, stringsAsFactors=FALSE)
colnames(dados) <- c("time", "type")
class(dados)

dados2 <- read.csv("result_block.csv", header=FALSE, sep=delim, dec=dec, stringsAsFactors=FALSE)
colnames(dados2) <- c("index", "blockSize", "Type")
dados2$blockSize  <- as.numeric(dados2$blockSize)
dados2$index  <- as.numeric(dados2$index)

plotLines <- ggplot(dados2, aes(x = index, y = blockSize, color = Type, group=Type)) + geom_line() +
  ggtitle("Block size growth") +
  ylab("Block Size")+
  xlab("Asset Count")+
  theme(
    # title = element_text(size = 10),
    legend.position = 'top',
    legend.title =  element_text(size = 8),
    legend.text =  element_text(size = 8),
    axis.title.y = element_text(size = 10),
    # axis.ticks = element_blank(),
    axis.text.y = element_text(size = 10),
    # axis.title.y = element_text(size = 20),
    axis.title.x = element_text(size = 10),
    axis.text.x = element_text(size = 10),
    axis.line = element_blank())

png(filename="graph1.png", width = 4.3, height = 3.1,
    units = "in", pointsize = 12, bg = "white", res = 300,
    restoreConsole = TRUE)
print(plotLines)
dev.off()

cor <- c("darkseagreen4", "darkolivegreen4", "darkgreen", "orange")

cor1 <- brewer.pal(n = 8, name = "BrBG")
cor1 <- c(cor1[6],cor1[8],cor1[6],cor1[8])
p <- ggplot(dados, aes(x=type, y=time, fill=type, color =type)) +
  scale_fill_manual(values=cor1) +
  geom_violin(width=1, aes(color = 'red'),lwd=0.1) +
  stat_summary(fun=median, geom="point", size=3, color="white") +
  geom_boxplot( width=0.1, color="darkolivegreen3", alpha=0.2, lwd=0.2, outlier.stroke = 0.5,outlier.size = 0.5) +
  theme_bw() +
  scale_color_manual(values=cor) +
  labs( y = "Response Time (ms)") +
  ggtitle("Response time On-Chain vs Off-Chain") +
  theme(
    legend.position = "none",
    axis.title.y = element_text(size = 10),
    # axis.ticks = element_blank(),
    axis.text.y = element_text(size = 10),
    # axis.title.y = element_text(size = 20),
    axis.title.x = element_blank(),
    axis.text.x = element_text(size = 10),
    axis.line = element_blank())

plotViolinBlockSize <- p + geom_point(lwd=0.2) 


png(filename="graph2.png", width = 4.3, height = 3.1,
    units = "in", pointsize = 12, bg = 300, "white", res = 1200,
    restoreConsole = TRUE)
print(plotViolinBlockSize)
dev.off()

dados3 <- read.csv("result_db.csv", header=FALSE, sep=delim, dec=dec, stringsAsFactors=FALSE)
colnames(dados3) <- c("time", "type")

p3 <- ggplot(dados3, aes(x=type, y=time, fill=type, color =type)) +
  scale_fill_manual(values=cor1) +
  geom_violin(width=1, aes(color = 'red'), lwd=0.2) +
  stat_summary(fun=median, geom="point", size=3, color="white") +
  geom_boxplot( width=0.1, color="darkolivegreen3", alpha=0.2, lwd=0.2,  outlier.stroke = 0.5,outlier.size = 0.5) +
  theme_bw() +
  scale_color_manual(values=cor) +
  labs( y = "Response Time (ms)") +
  ggtitle("Response time CouchDB vs LevelDB") +
  theme(
    legend.position = "none",
    axis.title.y = element_text(size = 10),
    # axis.ticks = element_blank(),
    axis.text.y = element_text(size = 10),
    # axis.title.y = element_text(size = 20),
    axis.title.x = element_blank(),
    axis.text.x = element_text(size = 10),
    axis.line = element_blank())

plotViolinDatabase <- p3 + geom_point(lwd=0.3) 

png(filename="graph3.png", width = 4.3, height = 3.1,
    units = "in", pointsize = 12, bg = "white", res = 300,
    restoreConsole = TRUE)
print(plotViolinDatabase)
dev.off()
