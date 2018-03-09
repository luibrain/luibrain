# Esly russkie bukvy ne otobrajautsa: File -> Reopen with encoding... UTF-8

# Используйте UTF-8 как кодировку по умолчанию!
# Установить кодировку в RStudio: Tools -> Global Options -> General, 
#  Default text encoding: UTF-8

# ..............................................................................
# Математическое моделирование: Практика 3
#   Параметрические классификаторы для бинарной Y
#      * логистическая регрессия
#      * линейный дискриминантный анализ (LDA)
#      * квадатичный дискриминантный анализ (QDA)
#      * ROC-кривая
# ..............................................................................

library('ISLR')
library('GGally')
library('MASS')

my.seed <- 345
train.percent <- 0.75
options("ggmatrix.progress.bar" = FALSE)

# Исходные данные: набор Default -----------------------------------------------
?Default
head(Default)
str(Default)

# графики разброса
ggp <- ggpairs(Default)
print(ggp, progress = FALSE)


set.seed(my.seed)
inTrain <- sample(seq_along(Default$default),
                  nrow(Default)*train.percent)
df <- Default[inTrain, ]
# фактические значения на обучающей выборке
Факт <- df$default


# Строим модели, чтобы спрогнозировать default ---------------------------------

# LDA ==========================================================================
model.lda <- lda(default ~ balance, data = Default[inTrain, ])
model.lda

# прогноз: вероятности принадлежности классу 'Yes' (дефолт)
p.lda <- predict(model.lda, df, 
                 type = 'response')
Прогноз <- factor(ifelse(p.lda$posterior[, 'Yes'] > 0.5, 
                         2, 1),
                  levels = c(1, 2),
                  labels = c('No', 'Yes'))

# матрица неточностей
conf.m <- table(Факт, Прогноз)
conf.m

# чувствительность
conf.m[2, 2] / sum(conf.m[2, ])
# специфичность
conf.m[1, 1] / sum(conf.m[1, ])
# верность
sum(diag(conf.m)) / sum(conf.m)


# QDA ==========================================================================
model.qda <- qda(default ~ balance, data = Default[inTrain, ])
model.qda
# прогноз: вероятности принадлежности классу 'Yes' (дефолт)
p.qda <- predict(model.qda, df, type = 'response')
Прогноз <- factor(ifelse(p.qda$posterior[, 'Yes'] > 0.5, 
                         2, 1),
                  levels = c(1, 2),
                  labels = c('No', 'Yes'))

# матрица неточностей
conf.m <- table(Факт, Прогноз)
conf.m

# чувствительность
conf.m[2, 2] / sum(conf.m[2, ])
# специфичность
conf.m[1, 1] / sum(conf.m[1, ])
# верность
sum(diag(conf.m)) / sum(conf.m)


# Подбор границы отсечения вероятностей классов --------------------------------

# ROC-кривая  ===========================================================

# считаем 1-SPC и TPR для всех вариантов границы отсечения
x <- NULL    # для (1 - SPC)
y <- NULL    # для TPR
x1 <- NULL
y1 <- NULL 
# заготовка под матрицу неточностей
tbl <- as.data.frame(matrix(rep(0, 4), 2, 2))
rownames(tbl) <- c('fact.No', 'fact.Yes')
colnames(tbl) <- c('predict.No', 'predict.Yes')
# вектор вероятностей для перебора
p.vector <- seq(0, 1, length = 501)
# цикл по вероятностям отсечения
for (p in p.vector){
  # прогноз
  Прогноз <- factor(ifelse(p.lda$posterior[, 'Yes'] > p, 
                           2, 1),
                    levels = c(1, 2),
                    labels = c('No', 'Yes'))
  
  # фрейм со сравнением факта и прогноза
  df.compare <- data.frame(Факт = Факт, Прогноз = Прогноз)
  
  # заполняем матрицу неточностей
  tbl[1, 1] <- nrow(df.compare[df.compare$Факт == 'No' & df.compare$Прогноз == 'No', ])
  tbl[2, 2] <- nrow(df.compare[df.compare$Факт == 'Yes' & df.compare$Прогноз == 'Yes', ])
  tbl[1, 2] <- nrow(df.compare[df.compare$Факт == 'No' & df.compare$Прогноз == 'Yes', ])
  tbl[2, 1] <- nrow(df.compare[df.compare$Факт == 'Yes' & df.compare$Прогноз == 'No', ])
  
  # считаем характеристики
  TPR <- tbl[2, 2] / sum(tbl[2, 2] + tbl[2, 1])
  y <- c(y, TPR)
  SPC <- tbl[1, 1] / sum(tbl[1, 1] + tbl[1, 2])
  x <- c(x, 1 - SPC)
}
for (p in p.vector){
  # прогноз
  Прогноз1 <- factor(ifelse(p.qda$posterior[, 'Yes'] > p, 
                           2, 1),
                    levels = c(1, 2),
                    labels = c('No', 'Yes'))
  
  # фрейм со сравнением факта и прогноза
  df.compare <- data.frame(Факт = Факт, Прогноз1 = Прогноз1)
  
  # заполняем матрицу неточностей
  tbl[1, 1] <- nrow(df.compare[df.compare$Факт == 'No' & df.compare$Прогноз1 == 'No', ])
  tbl[2, 2] <- nrow(df.compare[df.compare$Факт == 'Yes' & df.compare$Прогноз1 == 'Yes', ])
  tbl[1, 2] <- nrow(df.compare[df.compare$Факт == 'No' & df.compare$Прогноз1 == 'Yes', ])
  tbl[2, 1] <- nrow(df.compare[df.compare$Факт == 'Yes' & df.compare$Прогноз1 == 'No', ])
  
  # считаем характеристики
  TPR <- tbl[2, 2] / sum(tbl[2, 2] + tbl[2, 1])
  y1 <- c(y1, TPR)
  SPC <- tbl[1, 1] / sum(tbl[1, 1] + tbl[1, 2])
  x1 <- c(x1, 1 - SPC)
}
# строим ROC-кривую
par(mar = c(5, 5, 1, 1))
# кривая
plot(x, y, type = 'l', col = 'blue', lwd = 1,
     xlab = '(1 - SPC)', ylab = 'TPR', 
     xlim = c(0, 1), ylim = c(0, 1))
lines(x1, y1, type = 'l', col = 'red', lwd = 1,
     xlab = '(1 - SPC)', ylab = 'TPR', 
     xlim = c(0, 1), ylim = c(0, 1))
# прямая случайного классификатора
abline(a = 0, b = 1, lty = 3, lwd = 2)
# точка для вероятности 0.5
points(x[p.vector == 0.5], y[p.vector == 0.5], pch = 16)
text(x[p.vector == 0.5], y[p.vector == 0.5], 'p = 0.5(lda)', pos = 4)
# точка для вероятности 0.2
points(x[p.vector == 0.2], y[p.vector == 0.2], pch = 16)
text(x[p.vector == 0.2], y[p.vector == 0.2], 'p = 0.2(lda)', pos = 4)

# точка для вероятности 0.5
points(x1[p.vector == 0.5], y1[p.vector == 0.5], pch = 16)
text(x1[p.vector == 0.5], y1[p.vector == 0.5], 'p = 0.5(qda)', pos = 4)
# точка для вероятности 0.2
points(x1[p.vector == 0.2], y1[p.vector == 0.2], pch = 16)
text(x1[p.vector == 0.2], y1[p.vector == 0.2], 'p = 0.2(qda)', pos = 4)


df1 <- Default[-inTrain, ]
# фактические значения на обучающей выборке
Факт1 <- df1$default

# LDA ==========================================================================
model.lda <- lda(default ~ balance, data = Default[-inTrain, ])
model.lda

# прогноз: вероятности принадлежности классу 'Yes' (дефолт)
p.lda <- predict(model.lda, df1, 
                 type = 'response')
Прогноз <- factor(ifelse(p.lda$posterior[, 'Yes'] > 0.5, 
                         2, 1),
                  levels = c(1, 2),
                  labels = c('No', 'Yes'))

# QDA ==========================================================================
model.qda <- qda(default ~ balance, data = Default[-inTrain, ])
model.qda
# прогноз: вероятности принадлежности классу 'Yes' (дефолт)
p.qda <- predict(model.qda, df1, type = 'response')
Прогноз <- factor(ifelse(p.qda$posterior[, 'Yes'] > 0.5, 
                         2, 1),
                  levels = c(1, 2),
                  labels = c('No', 'Yes'))


# Подбор границы отсечения вероятностей классов --------------------------------

# ROC-кривая  ===========================================================

# считаем 1-SPC и TPR для всех вариантов границы отсечения
x <- NULL    # для (1 - SPC)
y <- NULL    # для TPR
x1 <- NULL
y1 <- NULL 
# заготовка под матрицу неточностей
tbl <- as.data.frame(matrix(rep(0, 4), 2, 2))
rownames(tbl) <- c('fact.No', 'fact.Yes')
colnames(tbl) <- c('predict.No', 'predict.Yes')
# вектор вероятностей для перебора
p.vector <- seq(0, 1, length = 501)
# цикл по вероятностям отсечения
for (p in p.vector){
  # прогноз
  Прогноз <- factor(ifelse(p.lda$posterior[, 'Yes'] > p, 
                           2, 1),
                    levels = c(1, 2),
                    labels = c('No', 'Yes'))
  
  # фрейм со сравнением факта и прогноза
  df1.compare <- data.frame(Факт1 = Факт1, Прогноз = Прогноз)
  
  # заполняем матрицу неточностей
  tbl[1, 1] <- nrow(df1.compare[df1.compare$Факт1 == 'No' & df1.compare$Прогноз == 'No', ])
  tbl[2, 2] <- nrow(df1.compare[df1.compare$Факт1 == 'Yes' & df1.compare$Прогноз == 'Yes', ])
  tbl[1, 2] <- nrow(df1.compare[df1.compare$Факт1 == 'No' & df1.compare$Прогноз == 'Yes', ])
  tbl[2, 1] <- nrow(df1.compare[df1.compare$Факт1 == 'Yes' & df1.compare$Прогноз == 'No', ])
  
  # считаем характеристики
  TPR <- tbl[2, 2] / sum(tbl[2, 2] + tbl[2, 1])
  y <- c(y, TPR)
  SPC <- tbl[1, 1] / sum(tbl[1, 1] + tbl[1, 2])
  x <- c(x, 1 - SPC)
}
for (p in p.vector){
  # прогноз
  Прогноз1 <- factor(ifelse(p.qda$posterior[, 'Yes'] > p, 
                            2, 1),
                     levels = c(1, 2),
                     labels = c('No', 'Yes'))
  
  # фрейм со сравнением факта и прогноза
  df1.compare <- data.frame(Факт1 = Факт1, Прогноз1 = Прогноз1)
  
  # заполняем матрицу неточностей
  tbl[1, 1] <- nrow(df1.compare[df1.compare$Факт == 'No' & df1.compare$Прогноз1 == 'No', ])
  tbl[2, 2] <- nrow(df1.compare[df1.compare$Факт == 'Yes' & df1.compare$Прогноз1 == 'Yes', ])
  tbl[1, 2] <- nrow(df1.compare[df1.compare$Факт == 'No' & df1.compare$Прогноз1 == 'Yes', ])
  tbl[2, 1] <- nrow(df1.compare[df1.compare$Факт == 'Yes' & df1.compare$Прогноз1 == 'No', ])
  
  # считаем характеристики
  TPR <- tbl[2, 2] / sum(tbl[2, 2] + tbl[2, 1])
  y1 <- c(y1, TPR)
  SPC <- tbl[1, 1] / sum(tbl[1, 1] + tbl[1, 2])
  x1 <- c(x1, 1 - SPC)
}
# строим ROC-кривую
par(mar = c(5, 5, 1, 1))
# кривая
plot(x, y, type = 'l', col = 'blue', lwd = 1,
     xlab = '(1 - SPC)', ylab = 'TPR', 
     xlim = c(0, 1), ylim = c(0, 1))
lines(x1, y1, type = 'l', col = 'red', lwd = 1,
      xlab = '(1 - SPC)', ylab = 'TPR', 
      xlim = c(0, 1), ylim = c(0, 1))
# прямая случайного классификатора
abline(a = 0, b = 1, lty = 3, lwd = 2)
# точка для вероятности 0.5
points(x[p.vector == 0.5], y[p.vector == 0.5], pch = 16)
text(x[p.vector == 0.5], y[p.vector == 0.5], 'p = 0.5(lda)', pos = 4)
# точка для вероятности 0.2
points(x[p.vector == 0.2], y[p.vector == 0.2], pch = 16)
text(x[p.vector == 0.2], y[p.vector == 0.2], 'p = 0.2(lda)', pos = 4)

# точка для вероятности 0.5
points(x1[p.vector == 0.5], y1[p.vector == 0.5], pch = 16)
text(x1[p.vector == 0.5], y1[p.vector == 0.5], 'p = 0.5(qda)', pos = 4)
# точка для вероятности 0.2
points(x1[p.vector == 0.2], y1[p.vector == 0.2], pch = 16)
text(x1[p.vector == 0.2], y1[p.vector == 0.2], 'p = 0.2(qda)', pos = 4)


# На самом деле графики очень похожи, но необходимо все же выбрать, какая модель лучше - lda или qda. В основном, можно заметить, что кривые синего цвета повышают чувствительно модели немного лучше, чем кривые красного цвета. Следовательно, метод qda является наиболее благоприятным.