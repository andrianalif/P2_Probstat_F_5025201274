Responden <-c(1,2,3,4,5,6,7,8,9)
X <- c(78,75,67,77,70,72,78,74,77)
Y <- c(100,95,70,90,90,90,89,90,100)

#nomor 1
#1a
df <- data.frame(Responden,X,Y)
df

selisih <- df[,3]-df[,2]
selisih

stdv_1a<- sd(selisih)
stdv_1a

#1b
x1bar_1b <- mean(X)
x2bar_1b <- mean(Y)
n1_1b<-length(X)
n2_1b<-length(Y)
S1kdrt_1b <- var(X)
S2kdrt_1b <- var(Y)
Spkdrt_1b <- ((n1_1b-1)*S1kdrt_1b+(n2_1b-1)*S2kdrt_1b)/((n1_1b-1)+(n2_1b-1))
Spkdrt_1b

t_1b<-((x1bar_1b-x2bar_1b)-stdv_1a)/sqrt(Spkdrt_1b*((1/n1_1b)+(1/n2_1b)))
t_1b                                

#1c
xbar = mean(Y)          
mu0 = mean(X)
s = sd(Y)
n = length(Y)
t = (xbar-mu0)/(s/sqrt(n)) 
t

alpha = 0.05 
t.half.alpha = qt(1-alpha/2, df=n-1) 
c(-t.half.alpha, t.half.alpha)

pval <- 2*pt(t, df=n-1)
pval

cat("karena pvalue > 0,05 atau pvalue>alpha maka keputusan gagal tolak H0","\n",
    "tidak ada pengaruh yang signifikan secara statistika dalam hal kadar saturasi oksigen , sebelum dan sesudah melakukan aktivitas")

#2
#2a
cat("H0 : mu <= 20.000","\n","H0 : mu > 20.000")
xbar2 = 23500         
mu02 = 20000
sd2 = 3900
n2 = 100
z2 = (xbar2-mu02)/(sd2/sqrt(n2)) 
z2

alpha2 =0.05
z.alpha2 = qnorm(1-alpha2) 
z.alpha2 

#nomor 2
#2a
cat("saya setuju dengan klaim tersebut karena setelah diuji ternyata tolak H0 (z>Z.alpha) sehingga rata-rata mobil dikemudikan per tahun lebih dari 20.000km")
#2b
cat("seperti yang sudah tertulis diatas nilai 8,974359 merupakan nilai dari zhitung(z2 dalam syntax ini) melebihi nilai ztabel(z.alpha2) sehingga keputusan yang diambil adalah Tolak H0","\n",
    "sehingga disimpulkan bahwa rata-rata mobil dikemudikan pertahun lebih dari 20.000km")
#2c
pval = pnorm(z2, lower.tail=FALSE) 
pval 
cat("Dikarenakan nilai pvalue< alpha(0,05) maka keputusan yang diambil adalah Tolak H0","\n",
    "sehingga disimpulkan bahwa rata-rata mobil dikemudikan pertahun lebih dari 20.000km")

#nomor 3
#3a
cat("H0 : mu = mu0","\n","mu !=(tidak sama dengan) mu0")

#3c
xbar3 = 2.79
mu03 = 3.64
s3 = 1.32
n3 = 27              
t3 = (xbar3-mu03)/(s3/sqrt(n3)) 
t3  

#3d nilai kritis
alpha3 = 0.05 
t.alpha3 = qt(1-alpha3, df=2) 
t.alpha3 

#3e
cat("Keputusan : Gagal Tolak H0")

#3f
cat("Kesimpulan : Tidak ada perbedaan pada rata-rata jumlah saham perusahaan di dua kota tersebut")

#nomor 4
# a)
my_data <- read.delim(file.choose())

my_data$Group <- as.factor(my_data$Group)
my_data$Group = factor(my_data$Group, labels = c("grup1", "grup1", "grup3"))


grup1 <- subset(my_data, Group == "grup1")
grup2 <- subset(my_data, Group == "grup1")
grup3 <- subset(my_data, Group == "grup3")

qqnorm(grup1$Length)

qqnorm(grup2$Length)

qqnorm(grup3$Length)

# berdasarkan plot kuantil normal di atas, tidak ditemukan outlier utama pada homogenitas varians

# b)
bartlett.test(Length ~ Group, data = my_data)

# c)
model1 <- aov(Length ~ Group, data = my_data)
summary(model1)

# d)
# nilai p adalah 0.0013 dimana kurang dari 0.005, sehingga h0 ditolak

# e)
TukeyHSD(model1)

# f)
library("ggplot2")

ggplot(my_data, aes(x = Group, y = Length)) + 
  geom_boxplot(fill = "white", colour = "black") + 
  scale_x_discrete() + xlab("Group") + ylab("Length")

# nomor 5
library(dplyr)
library(multcompView)

gtl <- read.csv(file.choose())

# a)
qplot(x = Temp, y = Light, geom = "point", data = gtl) +
  facet_grid(.~Glass, labeller = label_both)

# b)
gtl$Glass <- as.factor(gtl$Glass)
gtl$Temp_Factor <- as.factor(gtl$Temp)
str(gtl)

gtlaov <- aov(Light ~ Glass*Temp_Factor, data = gtl)
summary(gtlaov)

# c)
data_summary <- group_by(gtl, Glass, Temp) %>%
  summarise(mean=mean(Light), sd=sd(Light)) %>%
  arrange(desc(mean))

print(data_summary)

# d)
tukey <- TukeyHSD(gtlaov)
print(tukey)

# e)
tukey.cld <- multcompLetters4(gtlaov, tukey)
print(tukey.cld)