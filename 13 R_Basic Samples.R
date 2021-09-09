
dir <- "/Users/wrk/Google Drive/BigDataAndDatabase/"
setwd(dir)

5 + (2.3 - 1.125)*3.2/1.1 + 1.23E3
##
2^10


## 数学函数——平方根、指数、对数
sqrt(6.25)
## [1] 2.5
exp(1)
## [1] 2.718282
log10(10000)
## [1] 4
log2(8)
## [1]3

## 数学函数——取整
round(1.1234, 2)
## [1] 1.12
round(-1.9876, 2)
## [1] -1.99
floor(1.1234)
## [1] 1
floor(-1.1234)
## [1] -2
ceiling(1.1234)
## [1] 2
ceiling(-1.1234)
## [1] -1

## 数学函数——三角函数
pi
## [1] 3.141593
sin(pi/6)
## [1] 0.5
cos(pi/6)
## [1] 0.8660254
sqrt(3)/2
## [1] 0.8660254
tan(pi/6)
## [1] 0.5773503

## 简单输出
print(sin(pi/2))
cat("sin(pi/2)=", sin(pi/2), "\n")


## 用sink()函数作运行记录
sink("tmpres01.txt", split=TRUE)
print(sin(pi/6))
print(cos(pi/6))
cat("t(10)的双侧0.05分位数（临界值）=", qt(1 - 0.05/2, 10), "\n")
sink()


## 向量计算与变量赋值
# R语言以向量为最小单位。用<-赋值
x1 <- 1:10
x1

# 一般的向量可以用c()生成
marks <- c(3, 5, 10, 5, 6)
# 用程序设计语言的术语描述， R语言是动态类型的， 
# 其变量的类型不需要预先声明， 运行过程中允许变量类型改变， 
# 实际上变量赋值是一种“绑定”（binding）， 
# 将一个变量的名称（变量名）与实际的一个存储位置联系在一起。 
# 在命令行定义的变量称为全局变量。

# 向量可以和一个标量作四则运算， 结果是每个元素都和这个标量作四则运算
x1 + 200

# 两个等长的向量可以进行四则运算， 相当于对应元素进行四则运算
x2 <- x1 * 3
x2 - x1


# R的许多函数都可以用向量作为自变量， 结果是自变量的每个元素各自的函数值
sqrt(x1)

## 函数曲线示例 
curve(x^2, -2, 2)
curve(sin(x), 0, 2*pi)
abline(h=0)

## 条形图示例
barplot(c("男生"=10, "女生"=7), main="男女生人数")

## 散点图示例
plot(1:10, sqrt(1:10))

## R软件自带的图形示例
demo("graphics")
demo("image")


##  读入表格数据
cityCase <- read.csv("input001.csv", header=TRUE, as.is=TRUE)
print(head(cityCase))

# table() for frequency 
table(cityCase[["Town2"]])
table(cityCase[["Town2"]], cityCase[["Town1"]])
knitr::kable(table(cityCase[["Town2"]], cityCase[["Town1"]]))
summary(cityCase[["Town1"]])
mean(cityCase[["Town1"]])
sd(cityCase[["Town1"]])


