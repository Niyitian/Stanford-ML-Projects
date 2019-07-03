library(caret)
library(ape)

data = read.csv('results_separate.csv',header = TRUE,row.names = 1)
code = read.csv('code.csv',header = TRUE)
data = data[!is.na(data$HDI),]

# ???????????????
cnt = row.names(data)
row.names(code)=code$country_code
name_data = code[cnt,]$name
row.names(data)=name_data

# ???????????????
calc = function(x){
  num_na = sum(is.na(x))
  len = length(x)
  return(num_na/len)
}

rate = apply(data,1,calc)
plot(data$ZlnCHINAGDP,rate)

# ????????????????????????
medianImpute = preProcess(data,method = 'medianImpute')
data = predict(medianImpute,data)

# ??????????????????/????????????
data = subset(data,select = -c(RMSE,MAE,corr,ZlnCHINAGDP,HDI,original_sd,original_mad))

# clustering
out_dist = dist(data,method='euclidean')
out_hclust = hclust(out_dist,method='complete')
plot(out_hclust)

mypal=c("#556270", "#4ECDC4", "#1B676B", "#FF6B6B", "#C44D58")
clus5=cutree(out_hclust, 5)
plot(as.phylo(out_hclust), type="fan", tip.color=mypal[clus5],
     label.offset=1)
text(-290,0,'rich1',cex=1.5,col = '#1B676B')
text(170,-220,'rich2',cex=1.5,col = "#4ECDC4")
text(210,160,'poor',cex=1.5,col = '#556270')

# radar plot
name_1 = names(clus5[clus5==1])
name_2 = names(clus5[clus5==2])
name_3 = names(clus5[clus5==3])
name_4 = clus5[clus5==4]
name_5 = clus5[clus5==5]
data_1 = data[name_1,]
data_2 = data[name_2,]
data_3 = data[name_3,]
data_4 = data[name_4,]
data_5 = data[name_5,]
v_1 = apply(data_1, 2, mean)
v_2 = apply(data_2, 2, mean)
v_3 = apply(data_3, 2, mean)
v_4 = apply(data_4, 2, mean)
v_5 = apply(data_5, 2, mean)

v = rbind(v_1,v_2)
v = rbind(v,v_3)

calc = function(v){
  a = (v[1]-v[2])^2+(v[2]-v[3])^2+(v[1]-v[3])^2
  return(a)
}
dis = apply(v, 2, calc)
num = sort(dis)
num = names(num)[417:425]

v = as.data.frame(v)
library(fmsb)
v = v[,num]
max_v = apply(v, 2, max)+5
min_v = apply(v, 2, min)-5
maxmin = as.data.frame(rbind(max_v,min_v))
v1 = rbind(maxmin,v[1,])
v2 = rbind(maxmin,v[2,])
v3 = rbind(maxmin,v[3,])
radarchart(v1,title = 'poor')
radarchart(v2,title = 'rich2')
radarchart(v3,title = 'rich1')
