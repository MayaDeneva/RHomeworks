# ф.н. 2MI0700013
# Мая Денева
# Информационни системи
# 1 група, за първа година
# М.Кандиларов

### Задача 1. data frame
tips <- read.csv("tips.csv", header = TRUE, sep = ",")
View(tips)



### Задача 2. Структурата data frame в R съдържа
# еднакъв тип по колони и еднаква дължина по редове



### Задача 3.number of rows and columns
print(nrow(tips))
print(ncol(tips))



### Задача 4.names of columns
names(tips)



### Задача 5. create new column
tips$bill.without.tip <- tips$total_bill - tips$tip
# move column in third place

install.packages("dplyr")
library(dplyr)
tips <- tips %>% relocate(bill.without.tip, .after = tip)
tips <- tips %>% relocate(bill.without.tip, .before = sex)

# Проверете дали стойността на бакшиша може да е включена в стойността на сметката
any(tips$total_bill < tips$tip)

#rename first column
names(tips)[1] <- "bill"

#delete bill.without.tips
tips <- subset(tips, select = -c(bill.without.tip))

#rename rows
rownames(tips) <- paste0("Order", 1:nrow(tips))
View(tips)



### Задача 6. Изкарайте на екрана първите 7 реда и отделно последните 3 реда използвайки функции в R.
# Първите 7 реда
head(tips, n = 7)
# Последните 3 реда
tail(tips, n = 3)



### Задача 7.
# Проверяваме типовете на данните в колоните
sapply(tips, class)
# Данните от колоните bill, tip, size са от числов тип и следователно са количествени, докато данните от колоните
#  sex, smoker и day са качествени.

# Проверка за непрекъснати и дискретни
col_types <- sapply(tips, function(x) is.numeric(x))
ifelse(col_types, "Непрекъснати", "Дискретни")

#Трансформирайте колоните с качествени данни до колони с факторни променливи..
tips$sex <- factor(tips$sex)
tips$smoker <- factor(tips$smoker)
tips$day <- factor(tips$day)



### Задача 8.
# Заредете колоните в паметта
write.csv(tips, "my_tips.csv", row.names = TRUE)
tips <- read.csv("my_tips.csv")

# Изведете дескриптивни статистики за всяка една от променливите
summary(tips)

## (a) Колко е минималната, максималната и средната заплатена сметка?
#Миниална - 3.07, Максимална - 50.81, Средна - 19.79

## (b) 75(b) 75% от сметките са били на стойност по-малка от колко?
q <- quantile(tips$bill, 0.75)
q
# 75% от сметките са били на стойност по-малка от 24.1275
## (c) 0.75-тия квантил се нарича още 3-ти квантил..
##(d) На колко от масите в заведението е имало пушачи?
table(tips$smoker)
#На 93 маси.

##(e) Кой е най-често срещания ден от седмицата за посещаване на заведението?
day_counts <- table(tips$day)

most_common_day <- names(day_counts)[which.max(day_counts)]
most_common_day

# Получаваме "Sat" - събота.

## (f) Колко от посещенията на заведението са направени за вечеря?

table(tips$time)["Dinner"]

# Получаваме 176



### Задача 9.
## (a) Вариационния ред на стойността на сметките.
var_range <- range(tips$bill)
cat("Вариационен ред на стойността на сметките: ", var_range[1], "-", var_range[2], "\n")

## (b) Ранга на 15-тото наблюдение от стойността на сметките.
rank(tips$bill)[15]

## (c) Data framе-a tips подреден по стойностите в колоната size. Без да използвате допълнителни библиотеки.
tips <- tips[order(tips$size),]
View(tips)

## (d) Сортирайте data framе-a tips първо по стойностите в колоната size и при наличие на повтарящи се стойности в колоната,
# сортирайте по колоната time.
# По-горе за пета задача инсталирах библиотеката dplyr.
library(dplyr)
tips <- tips %>% arrange(size, time)
View(tips)

## 30% от сметките са били на стойност по-малка от колко?
value <- quantile(tips$bill, 0.3)
cat("30% от сметките са били на стойност по-малка от:", value)

## (f) 1% от бакшишите са били на стойност по-голяма от колко?
tip_99_perc <- quantile(tips$tip, probs = 0.99)
cat("1% от бакшишите са били на стойност по-голяма от ",tip_99_perc)

##(g) Само стойностите на долния и горния hinge за бакшишите..
lower_hinge <- quantile(tips$tip, 0.25)
upper_hinge <- quantile(tips$tip, 0.75)

print(paste("Lower hinge:", lower_hinge))
print(paste("Upper hinge:", upper_hinge))

## (h) Стандартното отклонение на стойността на сметките.
sd(tips$bill)  # 8.902412

##(i) Дисперсията на стойността на бакшишите.
var(tips$tip)  # 1.914455

##(j) Най-често колко души са присъствали на една маса? (Сортирайте таблицата за
#да получите резултата)

# Извличаме броя на гостите на масата
table_size <- table(tips$size)
table_size

# Сортираме таблицата по брой гости в низходящ ред
sorted_table_size <- sort(table_size, decreasing = TRUE)
# Извеждаме най-често срещания брой гости на маса
head(names(sorted_table_size), n = 1)



### Задача 10. Оценете вероятностите на следните събития в R. Напишете в коментари кои
# вероятности оценявате и как ги пресмятате с формули.

## (a) ’На случайно избрана маса в заведението да има пушач’;
#Това е вероятността да се избере маса, на която има поне един пушач. Нека броят на масите, на които има поне един пушач,
# е n и общия брой на масите в заведението е N. Тогава вероятността да се избере маса с пушач е P = n / N.

# намираме броя на масите, на които има пушач
n_smokers <- nrow(filter(tips, smoker == "Yes"))

# намираме общия брой на масите
n_tables <- nrow(tips)

# пресмятаме вероятността
P_smoker <- n_smokers / n_tables

# извеждаме резултата
cat("Вероятността да се избере маса с пушач е", P_smoker)

## (b) ’На случайно избрана маса в заведението да има поне 4ма души’;

# намираме броя маси с поне с поне 4ма души
at_least_four <- nrow(filter(tips, size >= 4))
# p = брой маси с поне 4 човека / броя на всички маси
p_at_least_four <- at_least_four / n    # 0.1885246

## (c) ’На случайно избрана маса от 3ма души да има пушач’;
three_people_smoker <- nrow(filter(tips, size == 3 & smoker == "Yes")) / nrow(filter(tips, size == 3))
three_people_smoker

## (d) ’Случайно избрана поръчка да е от събота вечер’;

saturday_dinner <- nrow(filter(tips, day == "Sat" & time == "Dinner"))
p_saturday_dinner <- saturday_dinner / n_tables

## (e) ’Случайно избрана поръчка в събота да е за вечеря’;
# намираме всички поръчки в събота
saturday <- nrow(filter(tips, day == "Sat"))
# разделяме благоприятните случаи - съботната поръчка да е за вечеря, на общия брой поръчки в събота
p_saturday_order_is_for_dinner <- saturday_dinner / saturday

## (f) ’На случайно избрана маса за вечеря да плати мъж’;
# благоприятни случаи: маси с мъже
men_tables = nrow(filter(tips, sex == "Male"))
p_man_pays <- men_tables / n_tables

## (g) ’На случайно избрана маса, на която е платил мъж, да са вечеряли’;
# Пресмятаме с условна вероятност P(A|B) = P(A ∩ B) / P(B), където събитието A е "Масата, на която е платил мъж, е вечеряла"
# а събитието B e "Мъж е платил на случайно избрана маса"

male_pays_dinner = nrow(filter(tips, sex == "Male", time == "Dinner"))
p_male_pays_dinner = male_pays_dinner / men_tables
p_male_pays_dinner

## (h) ’На случайно избрана маса да плати мъж и клиентите да са вечеряли’; Мисля че това е същото като горното



### Задача 11. Изведете:
## (a) редовете на масите, на които е имало най-много лица и са дошли за обяд (не използвайте константа за максимума);
tips<- read.csv("my_tips.csv")
#вземаме само поръчките направени по обяд
lunch_data <- filter(tips, time == "Lunch")

#подреждаме таблицата по брой лица
arrange(lunch_data,size)
# вземаме най-големия брой
max_customers <- lunch_data$size[1]
#извеждаме първите няколко реда, които отговарят на условието
top <- lunch_data %>% filter(size == max_customers)
View(top)

## (b) броя на масите, на които сметката заедно с бакшиша е била по-голяма от $20 и на която са били пушачи, за обяд.

nrow(filter(tips, bill > 20, smoker == "Yes", time == "Lunch"))



### Задача 12.
# зареждаме нужната библиотека
library(ggplot2)

# групираме данните по ден от седмицата и изчисляваме общия брой на клиентите за всеки ден
daily_totals <- tips %>% 
  group_by(day) %>% 
  summarize(total_customers = n())

# създаваме стълбчата диаграма
ggplot(daily_totals, aes(x = day, y = total_customers)) + 
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Натоварване на заведението по дни от седмицата", 
       x = "Ден от седмицата", 
       y = "Брой клиенти")



### Задача 13.


ggplot(tips, aes(x = time, fill = sex)) +
  geom_bar() +
  labs(title = "Тенденция при пол и време на посещение",
       x = "Време на посещение",
       y = "Брой",
       fill = "Пол")



## Задача 14.
# Пресмятане на честотната таблица за пола на платеца в различните дни и части от деня
gender_table <- table(tips$day, tips$time, tips$sex)

# Извеждане на честотната таблица
gender_table

# Създаване на графика за разпределението на платците по пола в различните части от деня
ggplot(data=tips, aes(x=time, fill=sex)) + 
  geom_bar(position="dodge") +
  facet_wrap(~day) +
  labs(title="Разпределение на платците по пола в различните дни и части от деня",
       x="Време на посещение", y="Брой")
# В петък на обяд 3-ма мъже са платили сметката.


##Задача 15.

ggplot(tips[tips$time == "Dinner",], aes(x=bill)) +
  geom_histogram(aes(y=..density..), binwidth=2, color="black", fill="white") +
  xlim(0, 60) + ylim(0, 0.25) + 
  xlab("Total Bill ($)") + ylab("Density") +
  ggtitle("Histogram of Total Bill for Dinner") +
  
  # Добавяме графика на оценка на плътността в червено
  geom_density(color="red", size=1)

#  Хистограмата показва, че има по-голям брой сметки с по-малки стойности, 
#а графиката на оценка на плътността показва, че вероятността за сметки с по-голяма
#стойност е по-ниска. Не е симетрична.


### Задача 16.
#boxplot
ggplot(tips, aes(x = "", y = tip, fill = time)) +
  geom_boxplot() +
  labs(x = NULL, y = "Tip", title = "Boxplot of Evening Tips")
#интерквартилен размах
iqr <- IQR(tips$tip)
iqr
#Линията в средата на кутията показва медианата, 
#горната и долната част на кутията показват границите на първи (25%) и трети (75%) квартили.
# Линиите на boxplot-a се разпростират до последните нормални стойности, които се намират в разстояние, 
# равно на 1.5 пъти интерквартилното разстояние (IQR) от първия и третия квартил. 
# Наблюдават се outliers като точки извън линиите. Първите два най-малки outlier-а са със стойност 6.5 и 6.7


### Задача 17.
boxplot(tip ~ sex, data = tips, xlab = "Пол", ylab = "Бакшиш", fill = sex, main = "Големина на бакшиша в зависимост от пола на платеца")
# Мъжете са дали малко по-висок бакшиш от жените. 




### Задача 18.

install.packages("lattice")
library(lattice)
tips <- read.csv("my_tips.csv")
# create a lattice plot showing the tip amount by time of day and gender
xyplot(tip ~ time | sex, data = tips, col = "red", pch = 20,
       xlab = "Time of Day", ylab = "Tip Amount", main = "Бакшиш в зависимост от време на посещение и пол")



### Задача 19.
# начертаване на scatter plot
library(ggplot2)
ggplot(data = tips, aes(x = bill, y = tip)) +
  geom_point()

# добавяне на регресионна права
ggplot(data = tips, aes(x = bill, y = tip)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

#Корелация на Пирсън
cor(tips$bill, tips$tip, method = "pearson")
#Корелация на Спирман
cor.test(tips$bill, tips$tip, method = "spearman")

#Корелацията на Пирсън между стойността на сметката и стойността на бакшиша е 0.6757341,
# А на Спирман - 0.6789681, които са почти еднакви по стойност. Това означава, че
#по-големите стойности на сметката имат тенденция да съответстват на по-големи стойности на бакшиша.

#За да извлечем оценките на коефициентите на линейната функция можем да използваме функцията lm() в R.
model <- lm(tip ~ bill, data=tips)
summary(model)
#От тази информация можем да извлечем оценките на коефициентите на линейната функция, които са:
#intercept: 0.920270
#bill: 0.105025

#Така уравнението на линейния модел ще изглежда като:
  
tip = 0.920270 +  0.105025 * bill
#Ако сметката е била $25, каква е очакваната стойност на бакшиша, на базата на линейния модел?
expected_tip <- 0.92 + 0.105 * 25
expected_tip

#Получаваме 3.545895


