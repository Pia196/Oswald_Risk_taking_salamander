# 1. IMPORT DATA
# mydata

# 2. SIMPLE STATISTICS AND GRAPHICS
# LENGTH DIFFERENCES 2018
mydata1<-subset(mydata, year=="2018")
summary(mydata1$size0)
subset1a<-subset(mydata1, ori=="Pond")
subset1b<-subset(mydata1, ori=="Stream")
hist(mydata1$size0) 
hist(subset1a$size0) 
hist(subset1b$size0) 
summary(subset1a$size0)
summary(subset1b$size0)

# testing for normality: Shapiro-Wilk
shapiro.test(mydata1$size0)
shapiro.test(subset1a$size0)
shapiro.test(subset1b$size0)
# testing for homogeneity of variances: F-Test
var.test(mydata1$size0 ~ mydata1$ori)
# comparison t test
t.test(size0~ori, data=mydata1)

# plotting
library(ggplot2)
library(ggpubr)
plot1<-
  ggplot(data=mydata1, aes(ori,size0))+
  geom_boxplot(aes(fill=ori))+
  scale_fill_manual(values=c("steelblue4", "lightsteelblue3"),
                    name="Origin",
                    breaks=c("Pond", "Stream"),
                    labels=c("Pond", "Stream"))+
  scale_x_discrete(labels=c("Pond (N=48)","Stream (N=55)"))+
  stat_compare_means(method="t.test", label="p.signif",  label.x = 1.5, label.y = 5)+
  theme_classic(base_size=12, base_family="Arial")+
  labs(x="Origin", y="Snout-tail length (cm)")+
  theme(axis.text.x=element_text(family="Arial", size=12, color="black"), 
        axis.text.y=element_text(family="Arial", size=12, color="black"),
        legend.position="none")
ggsave(plot1, filename="plot1", path="C:/Users/localadmin/Desktop", dpi=300, dev='png', height=150, width=150, units="mm")

# LENGTH DIFFERENCES 2019
mydata2<-subset(mydata,year=="2019")
summary(mydata2$size0)
subset2a<-subset(mydata2, ori=="Pond")
subset2b<-subset(mydata2, ori=="Stream")
hist(mydata2$size0) 
hist(subset2a$size0) 
hist(subset2b$size0) 
summary(subset2a$size0)
summary(subset2b$size0)

# testing for normality: Shapiro-Wilk
shapiro.test(mydata2$size0)
shapiro.test(subset2a$size0)
shapiro.test(subset2b$size0)
# testing for homogeneity of variances: F-Test
var.test(mydata2$size0 ~ mydata2$ori)
# comparison Wilcoxon test
wilcox.test(mydata2$size0 ~ mydata2$ori)

# plotting
library(ggplot2)
library(ggpubr)
plot2<-
  ggplot(data=mydata2, aes(ori,size0))+
  geom_boxplot(aes(fill=ori))+
  scale_fill_manual(values=c("steelblue4", "lightsteelblue3"),
                    name="Origin",
                    breaks=c("Pond", "Stream"),
                    labels=c("Pond", "Stream"))+
  stat_compare_means(method="wilcox.test", label="p.signif",  label.x = 1.5, label.y = 5)+
  scale_x_discrete(labels=c("Pond (N=55)","Stream (N=52)"))+
  theme_classic(base_size=12, base_family="Arial")+
  labs(x="Origin", y="Snout-tail length (cm)")+
  theme(axis.text.x=element_text(family="Arial", size=12, color="black"), 
        axis.text.y=element_text(family="Arial", size=12, color="black"),
        legend.position="none")
ggsave(plot2, filename="plot2", path="C:/Users/localadmin/Desktop", dpi=300, dev='png', height=150, width=150, units="mm")


# BOTH YEARS COMBINED
library(gridExtra)
library(grid)
plot3<-
  ggplot(data=mydata, aes(year,size0, fill=ori))+
  geom_boxplot(aes(fill=ori))+
  scale_fill_manual(values=c("steelblue4", "lightsteelblue3"),
                    name="Origin",
                    breaks=c("Pond", "Stream"),
                    labels=c("Pond", "Stream"))+
  scale_x_discrete(labels=c("2018" = "Shelter-emergence (2018)", "2019" = "Shelter-seeking (2019)"))+
  scale_y_continuous(name="Snout-tail length (cm)", limits=c(2.5, 5))+
  stat_compare_means(method="wilcox.test", label="p.signif", size=5)+
  theme_classic(base_size=14, base_family="Arial")+
  labs(x="Year", y="Snout-tail length (cm)")+
  guides(fill=guide_legend(title="Habitat type"))+
  theme(plot.margin = unit(c(0.5,0.5,2,0.5), "cm"),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_text(family="Arial", size=14, color="black"),
        legend.position="none")

setwd("C:/Users/localadmin/Desktop")
png("plot3.png", height=150, width=200, units="mm", res=300);print(Multiplot)
Multiplot<-grid.arrange(plot3, ncol=1)
grid.text("Pond", x = unit(0.26, "npc"), y = unit(0.12, "npc"), gp=gpar(fontfamily="Arial", fontsize=14))
grid.text("(N=48)", x = unit(0.26, "npc"), y = unit(0.09, "npc"), gp=gpar(fontfamily="Arial", fontsize=14))
grid.text("Stream", x = unit(0.41, "npc"), y = unit(0.12, "npc"), gp=gpar(fontfamily="Arial", fontsize=14))
grid.text("(N=55)", x = unit(0.41, "npc"), y = unit(0.09, "npc"), gp=gpar(fontfamily="Arial", fontsize=14))
grid.text("Pond", x = unit(0.664, "npc"), y = unit(0.12, "npc"), gp=gpar(fontfamily="Arial", fontsize=14))
grid.text("(N=55)", x = unit(0.664, "npc"), y = unit(0.09, "npc"), gp=gpar(fontfamily="Arial", fontsize=14))
grid.text("Stream", x = unit(0.813, "npc"), y = unit(0.12, "npc"), gp=gpar(fontfamily="Arial", fontsize=14))
grid.text("(N=52)", x = unit(0.813, "npc"), y = unit(0.09, "npc"), gp=gpar(fontfamily="Arial", fontsize=14))
grid.text("Shelter-emergence (2018)", x = unit(0.336, "npc"), y = unit(0.036, "npc"), gp=gpar(fontfamily="Arial", fontsize=14))
grid.text("Shelter-seeking (2019)", x = unit(0.742, "npc"), y = unit(0.036, "npc"), gp=gpar(fontfamily="Arial", fontsize=14))
dev.off()


# BEHAVIOURAL DIFFERENCES SHELTER-EMERGENCE (2018)
hist(mydata1$tis0)
hist(subset1a$tis0)
hist(subset1b$tis0)
summary(subset1a$tis0)
summary(subset1b$tis0)

# testing for normality: Shapiro-Wilk
shapiro.test(mydata1$tis0)
shapiro.test(subset1a$tis0)
shapiro.test(subset1b$tis0)
# testing for homogeneity of variances: F-Test
var.test(mydata1$tis0 ~ mydata1$ori)
# comparison Wilcoxon test
wilcox.test(mydata1$tis0 ~ mydata1$ori)

# plotting
library(ggplot2)
plot4<-
  ggplot(data=mydata1, aes(ori,tis0))+
  geom_boxplot(aes(fill=ori))+
  scale_fill_manual(values=c("steelblue4", "lightsteelblue3"),
                    name="Origin",
                    breaks=c("Pond", "Stream"),
                    labels=c("Pond", "Stream"))+
  stat_compare_means(method="wilcox.test", label="p.signif",  label.x = 1.485, label.y = 130, size=5)+
  scale_y_continuous(breaks=seq(0,135,30),limits = c(0, 135))+
  theme_classic(base_size=14, base_family="Arial")+
  labs(x="Habitat Type", y="Time in shelter (s)")+
  theme(axis.text.x=element_text(family="Arial", size=14, color="black"), 
        axis.text.y=element_text(family="Arial", size=14, color="black"),
        legend.position="none")
ggsave(plot4, filename="plot4", path="C:/Users/localadmin/Desktop", dpi=300, dev='png', height=150, width=150, units="mm")


#DIFFERENCES OF BEHAVIOUR BY ORIGIN 2019
hist(mydata2$til0)
hist(subset2a$til0)
hist(subset2b$til0)
summary(subset2a$til0)
summary(subset2b$til0)

# testing for normality: Shapiro-Wilk
shapiro.test(mydata2$til0)
shapiro.test(subset2a$til0)
shapiro.test(subset2b$til0)
# testing for homogeneity of variances: F-Test
var.test(mydata2$til0 ~ mydata2$ori)
# comparison Wilcoxon test
wilcox.test(mydata2$til0 ~ mydata2$ori)

# plotting
library(ggplot2)
plot5<-
  ggplot(data=mydata2, aes(ori,til0))+
  geom_boxplot(aes(fill=ori))+
  scale_fill_manual(values=c("steelblue4", "lightsteelblue3"),
                    name="Origin",
                    breaks=c("Pond", "Stream"),
                    labels=c("Pond", "Stream"))+
  stat_compare_means(method="wilcox.test", label="p.signif",  label.x = 1.485, label.y = 130, size=5)+
  scale_y_continuous(breaks=seq(0,135,30), limits = c(0, 135))+
  theme_classic(base_size=14, base_family="Arial")+
  labs(x="Habitat type", y="Time outside shelter (s)")+
  theme(axis.text.x=element_text(family="Arial", size=14, color="black"), 
        axis.text.y=element_text(family="Arial", size=14, color="black"),
        legend.position="none")

ggsave(plot5, filename="plot5", path="C:/Users/localadmin/Desktop", dpi=300, dev='png', height=150, width=150, units="mm")


# CORRELATION SIZE AND SHELTER-EMERGENCE 2018
cor.test(mydata1$tis0, mydata1$size0, method = "spearman")
cor.test(subset1a$tis0, subset1a$size0, method = "spearman")
cor.test(subset1b$tis0, subset1b$size0, method = "spearman")

# plotting
library(extrafont)
font_import(paths = NULL, recursive = TRUE, prompt = TRUE,pattern = NULL)
loadfonts(device = "win")
library(ggplot2)

model1<-lm(tis0 ~ size0, data =mydata1)
coef(model1)
plot6<-
  ggplot(data=mydata1, aes(size0,tis0))+
  geom_point(aes(shape=ori, color=ori))+
  scale_shape_manual(values=c(16, 16),
                     name="Origin",
                     breaks=c("Pond", "Stream"),
                     labels=c("Pond", "Stream"))+
  scale_color_manual(values=c("steelblue4", "lightsteelblue3"),
                     name="Origin",
                     breaks=c("Pond", "Stream"),
                     labels=c("Pond", "Stream"))+
  geom_abline(intercept=coef(model1)[1], slope=coef(model1)[2]) +
  stat_cor(method = "spearman", label.y = 132, label.x=2.75, size=5)+
  scale_y_continuous(breaks=seq(0,135,30), limits = c(0, 135))+
  scale_x_continuous(breaks=seq(2.0,5.0,0.5),limits=c(2.0,5.0))+
  theme_classic(base_size=14, base_family="Arial")+
  labs(x="Snout-tail length (cm)", y="Time in shelter (sec)")+
  theme(axis.text.x=element_text(family="Arial", size=14, color="black"), 
        axis.text.y=element_text(family="Arial", size=14, color="black"),
        legend.position="none")

ggsave(plot6, filename="plot6", path="C:/Users/localadmin/Desktop", dpi=300, dev='png', height=150, width=150, units="mm")


# CORRELATION SIZE AND SHELTER-SEEKING 2019
cor.test(mydata2$til0, mydata2$size0, method = "spearman")
cor.test(subset2a$til0, subset2a$size0, method = "spearman")
cor.test(subset2b$til0, subset2b$size0, method = "spearman")

library(extrafont)
font_import(paths = NULL, recursive = TRUE, prompt = TRUE,pattern = NULL)
loadfonts(device = "win")
library(ggplot2)

plot7<-
  ggplot(data=mydata2, aes(size0,til0))+
  geom_point(aes(shape=ori, color=ori))+
  scale_shape_manual(values=c(16, 16),
                     name="Origin",
                     breaks=c("Pond", "Stream"),
                     labels=c("Pond", "Stream"))+
  scale_color_manual(values=c("steelblue4", "lightsteelblue3"),
                     name="Origin",
                     breaks=c("Pond", "Stream"),
                     labels=c("Pond", "Stream"))+
  stat_cor(method = "spearman", label.y = 132,label.x=2.75,size=5)+
  scale_y_continuous(breaks=seq(0,135,30), limits = c(0, 135))+
  scale_x_continuous(breaks=seq(2,5,0.5), limits=c(2.0,5.0))+
  theme_classic(base_size=14, base_family="Arial")+
  labs(x="Snout-tail length (cm)", y="Time outside shelter (s)")+
  theme(axis.text.x=element_text(family="Arial", size=14, color="black"), 
        axis.text.y=element_text(family="Arial", size=14, color="black"),
        legend.position="none")

ggsave(plot7, filename="plot7", path="C:/Users/localadmin/Desktop", dpi=300, dev='png', height=150, width=150, units="mm")


# MERGE EVERYTHING INTO ONE MULTIPLOT
figa<-plot4+
  theme(plot.margin = unit(c(1.4,0.1,1.6,0.5), "cm"),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

figb<-plot6+
  theme(plot.margin = unit(c(1.4,0.5,1,0), "cm"),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())

figc<-plot5+
  theme(plot.margin = unit(c(1.3,0.1,1.7,0.5), "cm"),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

figd<-plot7+
  theme(plot.margin = unit(c(1.3,0.5,1.1,0), "cm"),
        axis.title.y=element_blank(),
        axis.title.x=element_blank())

library(gridExtra)
library(grid)
setwd("C:/Users/localadmin/Desktop")
png("plot8_multiplot.png", height=150, width=200, units="mm", res=300);print(Multiplot1)
Multiplot1<-grid.arrange(figa, figb, figc, figd, ncol=2)
grid.text("A)", x = unit(0.036, "npc"), y = unit(0.93, "npc"), gp=gpar(fontfamily="Arial", fontsize=14))
grid.text("B)", x = unit(0.5, "npc"), y = unit(0.93, "npc"), gp=gpar(fontfamily="Arial", fontsize=14))
grid.text("C)", x = unit(0.036, "npc"), y = unit(0.47, "npc"), gp=gpar(fontfamily="Arial", fontsize=14))
grid.text("D)", x = unit(0.5, "npc"), y = unit(0.47, "npc"), gp=gpar(fontfamily="Arial", fontsize=14))
grid.text("Pond", x = unit(0.203, "npc"), y = unit(0.592, "npc"), gp=gpar(fontfamily="Arial", fontsize=14))
grid.text("Stream", x = unit(0.375, "npc"), y = unit(0.592, "npc"), gp=gpar(fontfamily="Arial", fontsize=14))
grid.text("(N=48)", x = unit(0.203, "npc"), y = unit(0.561, "npc"), gp=gpar(fontfamily="Arial", fontsize=14))
grid.text("(N=55)", x = unit(0.375, "npc"), y = unit(0.561, "npc"), gp=gpar(fontfamily="Arial", fontsize=14))
grid.text("Pond", x = unit(0.203, "npc"), y = unit(0.097, "npc"), gp=gpar(fontfamily="Arial", fontsize=14))
grid.text("Stream", x = unit(0.375, "npc"), y = unit(0.097, "npc"), gp=gpar(fontfamily="Arial", fontsize=14))
grid.text("(N=55)", x = unit(0.203, "npc"), y = unit(0.067, "npc"), gp=gpar(fontfamily="Arial", fontsize=14))
grid.text("(N=52)", x = unit(0.375, "npc"), y = unit(0.067, "npc"), gp=gpar(fontfamily="Arial", fontsize=14))
grid.text("Habitat type", x = unit(0.288, "npc"), y = unit(0.025, "npc"), gp=gpar(fontfamily="Arial", fontsize=14))
grid.text("Snout-tail length (cm)", x = unit(0.76, "npc"), y = unit(0.025, "npc"), gp=gpar(fontfamily="Arial", fontsize=14))
grid.text("Shelter-emergence", x = unit(0.5, "npc"), y = unit(0.97, "npc"), gp=gpar(fontfamily="Arial", fontsize=14, fontface="bold"))
grid.text("Shelter-seeking", x = unit(0.5, "npc"), y = unit(0.51, "npc"), gp=gpar(fontfamily="Arial", fontsize=14, fontface="bold"))
dev.off()

# 3. 2-PART MODEL (first: binomial model to see, if there's a difference in the probability of moving between shelter and open space; second: run a linear model to investigate if those who moved, differ in the time they moved)

library(lme4)
library(lmerTest)

# RISK_TAKING (2018)
mod1<-glmer(tisbin0~ori+size0+(1|ssite),data=mydata1,family = "binomial")
summary(mod1)
mod2<-lmer(tis0~ori+size0+(1|ssite), data=mydata1[mydata1$tisbin0==1,])
summary(mod2)

# SHELTER-SEEKING (2019)
mod3<-glmer(tilbin0~ori+size0+(1|ssite),data=mydata2,family = "binomial")
summary(mod3)
mod4<-lmer(til0~ori+size0+(1|ssite), data=mydata2[mydata2$tilbin0==1,])
summary(mod4)
