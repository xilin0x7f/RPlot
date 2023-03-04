## 两组间T检验bar图
# 预设项，两组数据
data_group1 = readxl::read_excel("data/data_group1.xlsx", "AD")
data_group2 = readxl::read_excel("data/data_group2.xlsx", "AD")
name_group1 = "CG"
name_group2 = "EG"
# 最后一定要加斜杠/
save_dir = "pictures/"
significance = read.table("signif.txt")$V1
xlabel = "组别"
ylabel = "AD"
errbar = "sem" # "sd"或者"sem"


# 以下为代码执行部分，谨慎修改
# 删除全是NAN的行
data_group1 = data_group1[apply(data_group1, 1, function(y) any(!is.na(y))), ]
data_group2 = data_group2[apply(data_group2, 1, function(y) any(!is.na(y))), ]
# 删除全是NAN的列
data_group1 = data_group1[, apply(data_group1, 2, function(y) any(!is.na(y)))]
data_group2 = data_group2[, apply(data_group2, 2, function(y) any(!is.na(y)))]
# names(data)[1] = "subject"
# names4plot = names(data)[2:length(names(data))]
# data$group = c(rep("CG", 27), rep("EG", 33))

library("ggplot2")
library("dplyr")
library("ggsignif")

names4plot = names(data_group1)
names4plot == names(data_group2)

# 检查两组数据的列名是否一致，如果不一致则应该检查数据
if (any(names4plot!=names(data_group2))){
  print("error, please check data")
}


data = rbind(data_group1, data_group2)
data$group = c(rep(name_group1, nrow(data_group1)), rep(name_group2, nrow(data_group2)))

for (varIdx in 1:length(names4plot)){
  data[, colnames(data) %in% c(names4plot[varIdx], "group")] %>% 
    group_by(group) %>%
    summarise(mean=mean(get(names4plot[varIdx])),
              sd = sd(get(names4plot[varIdx])),
              sem = sd(get(names4plot[varIdx]))/sqrt(length(get(names4plot[varIdx])))
              ) -> data4plot
  
  options(scipen = 200)
  p = ggplot(data4plot, aes(x=group, y=mean, color=group))+
    geom_bar(stat="identity", width=.5, aes(fill=group), color=c("#ca6181", "#1c86b8"), 
             fill=c("#ca6181", "#1c86b8"), show.legend = F)+
    geom_errorbar(aes(x=group, ymax=mean+get(errbar), ymin=mean-get(errbar)), width=.3, show.legend=F)+
    geom_signif(comparisons=list(c(name_group1, name_group2)),
                annotations = significance[varIdx],
                y_position = 1*max(data4plot$mean+data4plot[errbar]),
                show.legend = F, color="black"
                )+
    scale_y_continuous(expand=c(0, 0))+ # 使y从0开始
    expand_limits(y=c(0, 1.1*max(data4plot$mean+data4plot[errbar])))+
    theme_classic()+
    labs(x=xlabel, 
         y=ylabel,
         title=names4plot[varIdx])+
    theme(plot.title = element_text(hjust = .5, size=16), 
          axis.text = element_text(face="bold", size=10),
          axis.title = element_text(face="bold", size=14)
          )
  p
  
  
  
  ggsave(plot=p, filename=paste0(save_dir, names4plot[varIdx], ".tif"),
         device="tiff", dpi=600, units="in", width=4, height=5)
}

