library(readxl)
require(ggplot2)
require(lme4)
require(lmerTest)
library(ggtext)
library(gridExtra)

data = read_excel("D:/下载/Project/code/data for coding.xlsx",sheet = "Sheet1",col_names = TRUE)
data$Tannin[data$Tannin == 0.5] <- "CT:HT = 1:1"
data$Tannin[data$Tannin == 0.0909090909090909] <- "CT:HT = 1:10"
data$Tannin[data$Tannin == 0.909090909090909] <- "CT:HT = 10:1"
data$N[data$N == 30] = "30 kg/(a▪ha) N"
data$N[data$N == 100] = "100 kg/(a▪ha) N"
# convert soil data to categorical factors
data$Soil <- as.factor(data$Soil)
data$N = as.factor(data$N)
data$Tannin = as.factor(data$Tannin)
data$Time = as.factor(data$Time)
# fit the linear regression model and extract the residual 
AP_residuals <- residuals(lmer(AP ~ Tannin*N*Soil + (1|Time), data = data))
BG_residuals <- residuals(lmer(BG ~ Tannin*N*Soil + (1|Time), data = data))
NAG_residuals <- residuals(lmer(NAG ~ Tannin*N*Soil + (1|Time), data = data))
PPO_residuals <- residuals(lmer(PPO ~ Tannin*N*Soil + (1|Time), data = data))
# create data frame
residual_data <- data.frame(Residuals = c(AP_residuals, BG_residuals, NAG_residuals, PPO_residuals),Enzyme = rep(c("AP", "BG", "NAG", "PPO"), each = length(AP_residuals)))
# draw the box section of the residual
ggplot(residual_data, aes(x = Enzyme, y = Residuals)) +
  geom_boxplot() +
  ylab("Residuals") +
  xlab("Enzyme") +
  facet_wrap(~ Enzyme, scales = "free")
# plot the density of the residuals
ggplot(residual_data, aes(x = Residuals)) +
  geom_density(fill = "skyblue", alpha = 0.5) +
  ylab("Density") +
  xlab("Residuals") +
  facet_wrap(~ Enzyme, scales = "free")

# remove any potential outliers
#IQR_AP = quantile(AP_residuals, 0.75)- quantile(AP_residuals, 0.25)
#lower_whisker_AP <- quantile(AP_residuals, 0.25) - 1.5 * IQR_AP
#upper_whisker_AP <- quantile(AP_residuals, 0.75) + 1.5 * IQR_AP
#IQR_BG = quantile(BG_residuals, 0.75)- quantile(BG_residuals, 0.25)
#lower_whisker_BG <- quantile(BG_residuals, 0.25) - 1.5 * IQR_BG
#upper_whisker_BG <- quantile(BG_residuals, 0.75) + 1.5 * IQR_BG
#IQR_NAG = quantile(NAG_residuals, 0.75)- quantile(NAG_residuals, 0.25)
#lower_whisker_NAG <- quantile(NAG_residuals, 0.25) - 1.5 * IQR_NAG
#upper_whisker_NAG <- quantile(NAG_residuals, 0.75) + 1.5 * IQR_NAG
#IQR_PPO = quantile(PPO_residuals, 0.75)- quantile(PPO_residuals, 0.25)
#lower_whisker_PPO <- quantile(PPO_residuals, 0.25) - 1.5 * IQR_PPO
#upper_whisker_PPO <- quantile(PPO_residuals, 0.75) + 1.5 * IQR_PPO
#print(upper_whisker_AP)
#print(lower_whisker_AP)
par(mfrow = c(3,1), mar=c(3,3,1,1))
new1 <- data[abs(AP_residuals) >= -1.510151  & abs(AP_residuals) <= 1.32691 , ]
AP_tannin_model = lmer(AP ~ Tannin*N*Soil + (1|Time), data = new1)
plot(AP_tannin_model)
new2 = data[abs(BG_residuals)>= -1.147233  & abs(BG_residuals)<= 1.07639 , ]
BG_tannin_model = lmer(BG ~ Tannin*N*Soil + (1|Time), data = new2)
plot(BG_tannin_model)
new3 <- data[abs(NAG_residuals)>= -0.9822029 & abs(NAG_residuals)<= 0.8171584 , ]
NAG_tannin_model = lmer(NAG ~ Tannin*N*Soil + (1|Time), data = new3)
plot(NAG_tannin_model)
new4 <- data[abs(PPO_residuals)>= -8.836445 & abs(PPO_residuals)<= 8.473482  , ]
PPO_tannin_model = lmer(PPO ~ Tannin*N*Soil + (1|Time), data = new4)
plot(PPO_tannin_model)

# carry out results
summary(AP_tannin_model)
summary(BG_tannin_model)
summary(NAG_tannin_model)
summary(PPO_tannin_model)

#interaction AP
# 绘制箱线图
colors <- c("#D9E7F4", "#96C3DE", "#4C96C8", "#08519C")
a = data.frame(Soil = "forest", Tannin = "0", AP = 2.3, N = factor("0", levels = levels(new1$N)), label = "p-value of Soilgrassland = 0.04722")
A = ggplot(new1, aes(x = Soil, y = AP, fill = Tannin)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +
  facet_grid(N ~ ., scales = "free_y", space = "free_y") +
  labs(title = "The Impacts of Tannin, N, and Soil on AP Activity",
       x = "N",
       y = "AP Activity")+
   geom_linerange(aes(xmin = -Inf, xmax = 1.5, y = median(subset(new1, N == 0 & Tannin == 0 & Soil == "forest")$AP)),
                      linetype = "dashed", linewidth = 0.7, color = "red")+
   geom_linerange(aes(xmin = 1.5, xmax = 2.5, y = median(subset(new1, N == 0 & Tannin == 0 & Soil == "grassland")$AP)),
                  linetype = "dashed", linewidth = 0.7, color = "red")+
  geom_text(aes(x = 1.5, y = 2.5, label = "*"), size = 7, color = "red")+
  scale_fill_manual(values = colors)+
  geom_text(data = a, aes(label = label), size = 4, fontface = "bold")
ggsave("D:/下载/Project/code/0806/AP.png", plot = A, dpi = 1000, width = 7, height = 7)

#interaction BG
# 绘制箱线图
b1 = ggplot(new2, aes(x = Soil, y = BG, fill = Tannin)) +
  geom_boxplot(alpha = 0.6,outlier.shape = NA) +
  facet_grid(N ~ ., scales = "free_y", space = "free_y") +
  labs(title = "The Impacts of Tannin, N, and Soil on BG Activity",
       x = "Soil",
       y = "BG Activity")+
  geom_linerange(aes(xmin = -Inf, xmax = 1.5, y = median(subset(new2, N == 0 & Tannin == 0 & Soil == "forest")$BG)),
                 linetype = "dashed", linewidth = 0.7, color = "red")+
  geom_linerange(aes(xmin = 1.5, xmax = 2.5, y = median(subset(new2, N == 0 & Tannin == 0 & Soil == "grassland")$BG)),
                 linetype = "dashed", linewidth = 0.7, color = "red")+
  scale_fill_manual(values = colors)

b2 = ggplot(new2[new2$Soil == "grassland",], aes(x = as.factor(N), y = BG)) +
  geom_boxplot(outlier.shape = NA) +
  labs(title = "The Impacts of N on BG Activity in Grassland Soil",
       x = "N",
       y = "BG Activity") +
  geom_hline(yintercept = median(new2$BG[new2$N == 0 & new2$Soil == "grassland"]), linewidth = 0.8, linetype = "dashed", color = "red")+
  geom_text(aes(x = 2, y = 3, label = "*"), size = 10, color = "red")+
  geom_richtext(aes(x = 2, y = 2.8, label = "p-value of **N30:Grassland** = **0.02291**"), size = 4,
                stat = "unique")
b3 = ggplot(new2[new2$Soil == "grassland",], aes(x = as.factor(Tannin), y = BG)) +
  geom_boxplot(outlier.shape = NA) +
  labs(title = "The Impacts of Tannin on BG Activity in Grassland Soil",
       x = "Tannin",
       y = "BG Activity") +
  geom_hline(yintercept = median(new2$BG[new2$Tannin == 0 & new2$Soil == "grassland"]), linewidth = 0.8, linetype = "dashed", color = "red")+
  geom_text(aes(x = 4, y = 3, label = "*"), size = 10, color = "red")+
  geom_richtext(aes(x = 2.3, y = 2.8, label = "p-value of **Tannin(CT:HT = 10:1):Grassland** = **0.00786**"), size = 4,
                stat = "unique")
# 绘制三个图形
B = grid.arrange(b1,b2, b3, ncol = 3)
ggsave("D:/下载/Project/code/0806/BG.png", plot = B, dpi = 1000, width = 21, height = 7)

#interaction NAG
n = data.frame(Soil = "forest", Tannin = "0", NAG = 1.2, N = factor("0", levels = levels(new3$N)), label = "p-value of Soilgrassland = 0.0322")
n1 = ggplot(new3, aes(x = Soil, y = NAG, fill = Tannin)) +
  geom_boxplot(alpha = 0.6,outlier.shape = NA) +
  facet_grid(N ~ ., scales = "free_y", space = "free_y") +
  labs(title = "The Impacts of Tannin, N, and Soil on NAG Activity",
       x = "N",
       y = "NAG Activity")+
  geom_linerange(aes(xmin = -Inf, xmax = 1.5, y = median(subset(new3, N == 0 & Tannin == 0 & Soil == "forest")$NAG)),
                 linetype = "dashed", linewidth = 0.7, color = "red")+
  geom_linerange(aes(xmin = 1.5, xmax = 2.5, y = median(subset(new3, N == 0 & Tannin == 0 & Soil == "grassland")$NAG)),
                 linetype = "dashed", linewidth = 0.7, color = "red")+
  geom_text(aes(x = 1.5, y = 1.4, label = "*"), size = 7, color = "red")+
  scale_fill_manual(values = colors)+
  geom_text(data = n, aes(label = label), size = 4, fontface = "bold")
n2 = ggplot(new3[new3$Soil == "grassland",], aes(x = as.factor(N), y = NAG)) +
  geom_boxplot(outlier.shape = NA) +
  labs(title = "The Impacts of N on NAG Activity in Grassland Soil",
       x = "N",
       y = "NAG Activity") +
  geom_text(aes(x = 2, y = 2, label = "*"), size = 10, color = "red")+
  geom_text(aes(x = 3, y = 2, label = "*"), size = 10, color = "red")+# 添加星号标签
  geom_richtext(aes(x = 2, y = 1.9, label = "p-value of **N30:Grassland** = **0.0107**, 
                    p-value of **N100:Grassland** = **0.0207**"), size = 4, stat = "unique")+
  geom_hline(yintercept = median(new3$NAG[new3$N == 0 & new3$Soil == "grassland"]), linewidth = 0.8, linetype = "dashed", color = "red")
N1 = grid.arrange(n1, n2, ncol = 1)
N2 = grid.arrange(n1, n2, ncol = 2)
ggsave("D:/下载/Project/code/0726/NAG1.png", plot = N1, dpi = 1000, width = 7, height = 10)
ggsave("D:/下载/Project/code/0806/NAG2.png", plot = N2, dpi = 1000, width = 14, height = 7)

#interaction PPO
p = data.frame(Soil = "forest", Tannin = "0", PPO = 28, N = factor("0", levels = levels(new4$N)), label = "p-value of Soilgrassland = 0.049409")
p1 = ggplot(new4, aes(x = Soil, y = PPO, fill = Tannin)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +
  facet_grid(N ~ ., scales = "free_y", space = "free_y") +
  labs(title = "The Impacts of Tannin, N, and Soil on PPO Activity",
       x = "N",
       y = "PPO Activity")+
  geom_linerange(aes(xmin = -Inf, xmax = 1.5, y = median(subset(new4, N == 0 & Tannin == 0 & Soil == "forest")$PPO)),
                 linetype = "dashed", linewidth = 0.7, color = "red")+
  geom_linerange(aes(xmin = 1.5, xmax = 2.5, y = median(subset(new4, N == 0 & Tannin == 0 & Soil == "grassland")$PPO)),
                 linetype = "dashed", linewidth = 0.7, color = "red")+
  geom_text(aes(x = 2, y = 25, label = "*"), size = 7, color = "red")+
  scale_fill_manual(values = colors)+
  geom_text(data = p, aes(label = label), size = 4, fontface = "bold")
#The Impacts of Tannin on PPO Activity in Grassland Soil
p2 = ggplot(new4[new4$Soil == "grassland",], aes(x = as.factor(Tannin), y = PPO)) +
  geom_boxplot(outlier.shape = NA) +
  labs(title = "The Impacts of Tannin on PPO Activity in Grassland Soil",
       x = "Tannin",
       y = "PPO Activity") +
  geom_hline(yintercept = median(new4$PPO[new4$Tannin == 0 & new4$Soil == "grassland"]), linewidth = 0.8, linetype = "dashed", color = "red")+
  geom_text(aes(x = 4, y = 26, label = "*"), size = 7, color = "red")+
  geom_richtext(aes(x = 2.3, y = 26, label = "p-value of **Tannin(CT:HT = 10:1):Grassland** = **2.22e-05**"), size = 4, stat = "unique")+
  coord_cartesian(ylim = c(10, 27))
  
#The Impacts of Tannin on PPO Activity with 100kg/ha N
p3 = ggplot(new4[new4$N == 100,], aes(x = as.factor(Tannin), y = PPO)) +
  geom_boxplot(outlier.shape = NA) +
  labs(title = "The Impacts of Tannin on PPO Activity with 100kg/ha N",
       x = "Tannin",
       y = "PPO Activity") +
  geom_hline(yintercept = median(new4$PPO[new4$Tannin == 0 & new4$N == 100]), linewidth = 0.8, linetype = "dashed", color = "red") +
  geom_text(aes(x = 4, y = 27, label = "*"), size = 7, color = "red") +
  coord_cartesian(ylim = c(10, 27))+
  geom_richtext(aes(x = 2.7, y = 26, label = "p-value of **Tannin(CT:HT = 10:1):N100** = **0.012150**"), size = 4, stat = "unique")
  
P = grid.arrange(p1, p2, p3, ncol = 1)
P2 = grid.arrange(p1, p2, p3, ncol = 3)
ggsave("D:/下载/Project/code/0726/PPO.png", plot = P, dpi = 1000, width = 7, height = 21)
ggsave("D:/下载/Project/code/0806/PPO2.png", plot = P2, dpi = 1000, width = 21, height = 7)
