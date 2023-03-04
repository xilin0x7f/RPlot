# 导入数据
data = readxl::read_excel("data/data_group1.xlsx", "AD")
name_group = c("group1", "group2", "group3", "group4")
number_group = c(9, 4, 5, 9)
errbar = "sem" # "sd"或者"sem"
xlabel = "组别"
ylabel = ""


signif_order <- rep(0, length(name_group)*(length(name_group)-1)/2)
signif_position <- rep(0, length(name_group)*(length(name_group)-1)/2)
idx=0
for (idx1 in 1:(length(name_group)-1)){
  for (idx2 in (idx1+1):length(name_group)){
    idx <- idx + 1
    signif_order[idx] = length(name_group)*(idx2-idx1-1)-(idx2-idx1)*(idx2-idx1-1)/2+idx1
    if (idx1==1){
      signif_position[idx] = 1
    }else{
      signif_position[idx] = signif_position[idx-1]+0.1
    }
  }
}
group = c(rep(name_group[1], number_group[1]))
for (idx in 2:length(name_group)){
  group = c(group, rep(name_group[idx], number_group[idx]))
}

# 删除全空的行和列
data = data[apply(data, 1, function(y) any(!is.na(y))), ]
data = data[, apply(data, 2, function(y) any(!is.na(y)))]

names4analysis = names(data)
data$group = group
varIdx = 1
# 方差分析并事后检验
model = aov(get(names4analysis[varIdx]) ~ group, data=data)
summary(model)
library("lsr")
library("ggplot2")
library("dplyr")
library("ggsignif")
library("ggpubr")
res = lsr::posthocPairwiseT(x=model, p.adjust.methods="bonferroni")
posthoc_p = c(res$p.value)
posthoc_p = posthoc_p[!is.na(posthoc_p)]
comparisons = list()
for (idx1 in 1:(length(name_group)-1)){
  for (idx2 in (idx1+1):length(name_group)){
    comparisons[[length(comparisons)+1]] =  c(name_group[idx1], name_group[idx2])
  }
}
comparisons
posthoc_sign = c()
for (idx in 1:length(posthoc_p)){
  if (posthoc_p[idx]<0.0001){
    posthoc_sign[idx] = "****"
  }else if(posthoc_p[idx]<0.001){
    posthoc_sign[idx] = "***"
  }else if(posthoc_p[idx]<0.01){
    posthoc_sign[idx] = "**"
  }else if(posthoc_p[idx]<0.05){
    posthoc_sign[idx] = "*"
  }else {
    posthoc_sign[idx] = "ns"
  }
}


data[, colnames(data) %in% c(names4analysis[varIdx], "group")] %>% 
  group_by(group) %>%
  summarise(mean=mean(get(names4analysis[varIdx])),
            sd = sd(get(names4analysis[varIdx])),
            sem = sd(get(names4analysis[varIdx]))/sqrt(length(get(names4analysis[varIdx])))
  ) -> data4plot

options(scipen = 200)
p = ggplot(data=data4plot,
           mapping=aes(x=group, y=mean+get(errbar)))+
  geom_bar(data4plot, mapping=aes(x=group, y=mean, color=group, fill=group),
           stat="identity", width=.5, show.legend = F)+
  geom_errorbar(data4plot, mapping=aes(x=group, ymax=mean+get(errbar), ymin=mean-get(errbar)), width=.3, show.legend=F)+
  geom_signif(comparisons=comparisons[signif_oder],
              annotations=posthoc_sign[signif_oder],
              y_position = max(data4plot$mean+data4plot[errbar])*signif_position,
              show.legend = F, color="black", manual=F
  )+
  scale_y_continuous(expand=c(0, 0))+ # 使y从0开始
  expand_limits(y=c(0, (max(signif_position)+0.2)*max(data4plot$mean+data4plot[errbar])))+
  theme_classic()+
  labs(x=xlabel, 
       y=ylabel,
       title=names4analysis[varIdx])+
  theme(plot.title = element_text(hjust = .5, size=16), 
        axis.text = element_text(face="bold", size=10),
        axis.title = element_text(face="bold", size=14)
  )
p



ggsave(plot=p, filename=paste0(save_dir, names4analysis[varIdx], ".tif"), 
       device="tiff", dpi=600, units="in", width=4, height=5)