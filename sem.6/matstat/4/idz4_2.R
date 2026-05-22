# ============================================================
# Исходные данные
# ============================================================

Y <- c(
  61.60, 62.53, 62.33, 58.60, 58.85, 61.72,
  26.02, 28.73, 31.66, 30.14, 31.18, 29.83,
  37.85, 39.79, 35.99, 41.22, 35.60, 35.08,
  68.71, 69.99, 68.13, 70.77, 67.83, 68.89,
  33.62, 29.98, 30.44, 29.33, 30.13, 29.88,
  34.79, 33.14, 33.39, 33.44, 33.02, 34.95,
  76.78, 78.65, 76.25, 76.27, 73.33, 73.30,
  43.02, 39.74, 44.16, 43.05, 48.55, 41.72
)

A <- c(
  1,1,1,1,1,1,
  1,1,1,1,1,1,
  1,1,1,1,1,1,
  1,1,1,1,1,1,
  2,2,2,2,2,2,
  2,2,2,2,2,2,
  2,2,2,2,2,2,
  2,2,2,2,2,2
)

B <- c(
  1,1,1,1,1,1,
  2,2,2,2,2,2,
  3,3,3,3,3,3,
  4,4,4,4,4,4,
  1,1,1,1,1,1,
  2,2,2,2,2,2,
  3,3,3,3,3,3,
  4,4,4,4,4,4
)

dat <- data.frame(
  Y = Y,
  A = factor(A),
  B = factor(B)
)

alpha <- 0.05
h <- 1.70

# ============================================================
# a) Двухфакторная модель
# ============================================================

model_full <- lm(Y ~ A * B, data = dat)
summary(model_full)
sigma2_hat <- summary(model_full)$sigma^2
cat("Несмещенная оценка дисперсии =", sigma2_hat, "\n")
# SSE
SSE <- sum(residuals(model_full)^2)
# SSR
SSR <- sum((fitted(model_full) - mean(dat$Y))^2)
# SST
SST <- sum((dat$Y - mean(dat$Y))^2)
cat("SSE =", SSE, "\n")
cat("SSR =", SSR, "\n")
cat("SST =", SST, "\n")

# ============================================================
# b) График взаимодействия
# ============================================================

interaction.plot(
  x.factor = dat$B,
  trace.factor = dat$A,
  response = dat$Y,
  type = "b",
  pch = 19,
  col = c("blue", "red"),
  xlab = "Фактор B",
  ylab = "Среднее Y",
  trace.label = "Фактор A"
)

library(ggplot2)

# Убедимся, что факторы являются категориями
dat$A <- factor(dat$A)
dat$B <- factor(dat$B)

# Построение графика
ggplot(dat, aes(x = A, y = Y, group = B, color = B)) +
  
  # 1. Слой с исходными точками (разбросаны горизонтально, чтобы не слипались)
  geom_jitter(width = 0, size = 2, alpha = 0.7) +
  
  # 2. Слой с линиями, соединяющими средние значения
  stat_summary(geom = "line", fun = mean, size = 1) +
  
  # 3. Слой с крупными точками средних значений (поверх линий)
  stat_summary(geom = "point", fun = mean, size = 3) +
  
  # Оформление
  labs(
    title = "Зависимость Y от уровня фактора A при фиксированном значении фактора B",
    x = "Уровень фактора A",
    y = "Значение Y",
    color = "Уровень фактора B"
  ) +
  theme_bw() +  # Белый фон с сеткой
  theme(panel.grid.minor = element_blank()) # Убираем мелкую сетку
# ============================================================
# c) Анализ остатков
# ============================================================

res <- residuals(model_full)


# гистограмма с шагами
hist_obj <- hist(
  res,
  probability = TRUE,
  breaks = seq(min(res), max(res) + h, by = h),
  col = "lightgray",
  main = "Гистограмма остатков",
  xlab = "Остатки",
  ylim = c(0, 0.25)
)
print(hist_obj$counts)
counts_table <- data.frame(
  Интервал = paste0("[", head(hist_obj$breaks, -1), ", ", tail(hist_obj$breaks, -1), "]"),
  Частота  = hist_obj$counts
)
print(counts_table, row.names = FALSE)

curve(
  dnorm(
    x,
    mean = mean(res),
    sd = sd(res)
  ),
  add = TRUE,
  col = "red",
  lwd = 2
)

# Остатки модели
res <- residuals(model_full)
k <- 5
breaks <- quantile(
  res,
  probs = seq(0, 1, length.out = k + 1)
)
breaks
table(cut(res, breaks = breaks, include.lowest = TRUE))
# Гистограмма просто масштаб другой
hist(
  res,
  probability = TRUE,
  breaks = breaks,
  col = "lightgray",
  main = "Гистограмма остатков",
  xlab = "Остатки"
)

# Нормальная плотность
curve(
  dnorm(
    x,
    mean = mean(res),
    sd = sd(res)
  ),
  add = TRUE,
  col = "red",
  lwd = 2
)

# ============================================================
# Гистограмма остатков: равная ширина h, но не менее 5 набл.
# в каждом столбце. Используется для критерия хи-квадрат.
# ============================================================

# --- входные данные -----------------------------------------
res <- residuals(model_full)   
min_count <- 5                 # минимум наблюдений в столбце

# --- 1. строим равноширокие интервалы шириной h -------------
r_min <- min(res)
r_max <- max(res)

# сдвигаем левую границу чуть левее минимума
left  <- floor(r_min / h) * h
right <- ceiling(r_max / h) * h

breaks_eq <- seq(left, right, by = h)

# крайние границы уходят в ±Inf для корректного охвата хвостов
breaks_inf <- c(-Inf, breaks_eq[-c(1, length(breaks_eq))], +Inf)

# --- 2. считаем частоты -------------------------------------
counts <- table(cut(res, breaks = breaks_inf, include.lowest = TRUE))
cat("Частоты по равноширокими интервалам (до объединения):\n")
print(counts)

# --- 3. объединяем крайние интервалы, пока в них < min_count -
# Объединяем слева
while (length(counts) > 2 && counts[1] < min_count) {
  breaks_inf <- breaks_inf[-2]                        # убираем вторую границу
  counts <- table(cut(res, breaks = breaks_inf, include.lowest = TRUE))
}
# Объединяем справа
while (length(counts) > 2 && counts[length(counts)] < min_count) {
  n_br <- length(breaks_inf)
  breaks_inf <- breaks_inf[-(n_br - 1)]              # убираем предпоследнюю
  counts <- table(cut(res, breaks = breaks_inf, include.lowest = TRUE))
}

cat("\nЧастоты после объединения хвостов:\n")
print(counts)
cat("Число интервалов k =", length(counts), "\n")

# --- 4. гистограмма -----------------------------------------
# Для корректного отображения заменяем ±Inf конечными значениями
breaks_plot <- breaks_inf
breaks_plot[1]                <- min(res) - 0.01
breaks_plot[length(breaks_plot)] <- max(res) + 0.01

hist(
  res,
  breaks    = breaks_plot,
  probability = TRUE,
  col       = "lightgray",
  border    = "white",
  main      = paste0("Гистограмма остатков (h = ", h, ")"),
  xlab      = "Остатки",
  ylab      = "Плотность"
)
# 1. Сохраняем результат hist() в объект
hist_obj <- hist(res,
                 breaks      = breaks_hist,
                 probability = TRUE,
                 col         = "lightgray",
                 border      = "black",
                 main        = "Гистограмма остатков полиномиальной модели",
                 xlab        = "Остатки",
                 ylim        = c(0, 0.8))

# 2. Теперь можно смотреть частоты:
print(hist_obj$counts)

# 3. Удобный вывод в виде таблицы (интервал + количество)
counts_table <- data.frame(
  Интервал = paste0("[", head(hist_obj$breaks, -1), ", ", tail(hist_obj$breaks, -1), "]"),
  Частота  = hist_obj$counts
)
print(counts_table, row.names = FALSE)

# нормальная кривая
curve(
  dnorm(x, mean = mean(res), sd = sd(res)),
  add = TRUE, col = "red", lwd = 2
)

# границы интервалов (кроме ±Inf) — вертикальные линии
inner_breaks <- breaks_inf[is.finite(breaks_inf)]
abline(v = inner_breaks, col = "steelblue", lty = 2, lwd = 1)

legend("topright",
       legend = c("Нормальная плотность", "Границы интервалов"),
       col    = c("red", "steelblue"),
       lty    = c(1, 2), lwd = c(2, 1), bty = "n")

# --- 5. таблица для критерия Пирсона ------------------------
mu_hat    <- mean(res)
sigma_hat <- sd(res)
n         <- length(res)
k         <- length(counts)

# теоретические вероятности
p_j <- numeric(k)
for (j in seq_len(k)) {
  lo <- breaks_inf[j]
  hi <- breaks_inf[j + 1]
  p_j[j] <- pnorm(hi, mu_hat, sigma_hat) -
    pnorm(lo, mu_hat, sigma_hat)
}

nu_j  <- as.numeric(counts)
np_j  <- n * p_j
chi2_terms <- (nu_j - np_j)^2 / np_j
chi2_obs   <- sum(chi2_terms)

m  <- 2          # оцениваемых параметров (μ, σ)
df_chi <- k - 1 - m
chi2_crit <- qchisq(1 - 0.02, df_chi)   # α = 0.02 из условия
p_value   <- pchisq(chi2_obs, df_chi, lower.tail = FALSE)

tbl <- data.frame(
  interval   = names(counts),
  nu_j       = nu_j,
  p_j        = round(p_j, 7),
  np_j       = round(np_j, 5),
  contrib    = round(chi2_terms, 7)
)
cat("\nТаблица критерия Пирсона:\n")
print(tbl)
cat(sprintf(
  "\nchi2_набл = %.4f,  df = %d,  chi2_крит = %.4f,  p-value = %.4f\n",
  chi2_obs, df_chi, chi2_crit, p_value
))
if (chi2_obs > chi2_crit) {
  cat("Вывод: H0 ОТВЕРГАЕТСЯ — отклонение от нормальности значимо.\n")
} else {
  cat("Вывод: H0 не отвергается — остатки согласуются с N(μ, σ²).\n")
}

# ============================================================
# d) Дисперсионный анализ
# ============================================================

anova(model_full)
q <- lm(Y~as.factor(A)*as.factor(B),data = dat)    # Ò Û˜ÂÚÓÏ ‚ÒÂı ‚Á‡ËÏÓ‰ÂÈÒÚ‚ËÈ
q12 <- lm(Y~as.factor(A)+as.factor(B),data = dat)  # ‡‰‰ËÚË‚Ì‡ˇ, ÓÚÒÛÚÒÚ‚ÛÂÚ ¿*¬
q1 <- lm(Y~as.factor(B),data = dat)                # ÓÚÒÛÚÒÚ‚ÛÂÚ ¿
q2 <- lm(Y~as.factor(A),data = dat)                # ÓÚÒÛÚÒÚ‚ÛÂÚ B
q0 <- lm(Y~1,data = dat)                           # ÓÚÒÛÚÒÚ‚ÛÂÚ ‚ÒÂ


q12a <- anova(q12,q)
nms <- names(q12a)
a.o.v <- matrix(unlist(q12a),nrow = 2,ncol = 6)[2,3:6]
names(a.o.v) <- nms[3:6]
rdf <- q12a$Res.Df[2] #36 = n - (d1*d2) = 48 - 4*3
xal <- qf(1 - alpha, a.o.v[1], rdf)
a.o.v <- c("H_(12)", a.o.v, xal)
AOV <- as.data.frame(t(a.o.v))
names(AOV) <- c("H",nms[3:6],"x_alpha")


q1a <- anova(q1,q)
a.o.v <- matrix(unlist(q1a),nrow = 2,ncol = 6)[2,3:6]
names(a.o.v) <- nms[3:6]
xal <- qf(1 - alpha, a.o.v[1], rdf)
a.o.v <- c("H_(1)",a.o.v, xal)
AOV <- rbind(AOV, a.o.v)


q2a <- anova(q2,q)
a.o.v <- matrix(unlist(q2a),nrow = 2,ncol = 6)[2,3:6]
names(a.o.v) <- nms[3:6]
xal <- qf(1 - alpha, a.o.v[1], rdf)
a.o.v <- c("H_(2)", a.o.v, xal)
AOV <- rbind(AOV, a.o.v)


q0a <- anova(q0,q)
a.o.v <- matrix(unlist(q0a),nrow = 2,ncol = 6)[2,3:6]
names(a.o.v) <- nms[3:6]
xal <- qf(1 - alpha, a.o.v[1], rdf)
a.o.v <- c("H_(0)", a.o.v, xal)
AOV <- rbind(AOV,a.o.v)


a.o.v <- c("Err", rdf, q12a$RSS[2], NA, NA, NA)
AOV <- rbind(AOV, a.o.v)
AOV$MRSS <- as.numeric(AOV$`Sum of Sq`) / as.numeric(AOV$Df)
AOV1 <- AOV[, c(1, 3, 2, 7, 4, 6, 5)]
AOV1




# ============================================================
# e) AIC и BIC
# ============================================================

model_add <- lm(Y ~ A + B, data = dat)
model_A   <- lm(Y ~ A, data = dat)
model_B   <- lm(Y ~ B, data = dat)
model_0   <- lm(Y ~ 1, data = dat)

tab_ic <- data.frame(
  Model = c(
    "A*B",
    "A+B",
    "A",
    "B",
    "Const"
  ),
  AIC = c(
    AIC(model_full),
    AIC(model_add),
    AIC(model_A),
    AIC(model_B),
    AIC(model_0)
  ),
  BIC = c(
    BIC(model_full),
    BIC(model_add),
    BIC(model_A),
    BIC(model_B),
    BIC(model_0)
  )
)

print(tab_ic)


