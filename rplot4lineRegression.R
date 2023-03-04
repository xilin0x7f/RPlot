# r plot for line regression

# 以表1为自变量，以表2为因变量
data1 = readxl::read_excel("量表.xlsx")
data2 = readxl::read_excel("EV_regress_covs.xlsx")
save_dir="p/"
stats_dir = "statsinfo/"

# 删除全是NAN的行
data1 = data1[apply(data1, 1, function(y) any(!is.na(y))), ]
data2 = data2[apply(data2, 1, function(y) any(!is.na(y))), ]
# 删除全是NAN的列
data1 = data1[, apply(data1, 2, function(y) any(!is.na(y)))]
data2 = data2[, apply(data2, 2, function(y) any(!is.na(y)))]

library("ggplot2")
for (idx1 in 1:length(data1)){
  for (idx2 in 1:length(data2)){
    data4lm <- cbind(data1[, idx1], data2[, idx2])
    # 以表2为因变量，表1为自变量
    model <- lm(get(names(data2)[idx2]) ~ get(names(data1)[idx1]), data=data4lm)
    res <- summary(model)
    # 保存统计结果到特定文件
    capture.output(res, cat(sprintf("Pearson's r: %.5f", cor(data4lm)[1, 2])), 
                   file=gsub(":", " ", paste0(stats_dir, names(data1)[idx1], "_", names(data2)[idx2], ".txt")))
    p <- ggplot(data=data4lm, aes(x=get(names(data1)[idx1]), y=get(names(data2)[idx2])))+
      geom_point()+
      geom_smooth(method="lm", se=T, color="#99504e")+
      annotate("text", x=0.9*max(data1[names(data1)[idx1]]), y=max(data2[names(data2)[idx2]]),
               label=sprintf("N: %s\nR squared: %.5f\nP: %.5f\nPearson's r: %.5f", dim(data1)[1], res$r.squared, res$coefficients[2,4], cor(data4lm)[1, 2]),
               hjust=0)+
      scale_y_continuous(expand=c(0, 0))+ # 使y从0开始
      scale_x_continuous(expand=c(0, 0))+
      expand_limits(y=c(0, 1.1*max(data2[names(data2)[idx2]])),
                    x=c(0, 1.1*max(data1[names(data1)[idx1]])))+
      labs(x=names(data1)[idx1], 
           y=names(data2)[idx2])+
      theme_classic()+
      theme(plot.title = element_text(hjust = .5, size=16), 
            axis.text = element_text(face="bold", size=10),
            axis.title = element_text(face="bold", size=14)
      )
    ggsave(plot=p, filename=gsub(":", " ", paste0(save_dir, names(data1)[idx1], "_", names(data2)[idx2], ".tif")),
           device="tiff", dpi=300, units="in", width=12, height=9)
  }
}
