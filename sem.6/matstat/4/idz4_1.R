# ============================================================
# ИДЗ 4, Часть 1, Вариант 11
# α = 0.02,  h = 0.69
# ============================================================

y <- c(11.90, 12.18, 12.37, 12.96, 12.52, 11.11, 12.42, 13.59, 13.67, 12.80,
       12.32, 12.62, 11.58, 13.42, 11.89, 11.76, 12.73, 12.25, 12.96, 12.15,
       13.78, 12.71, 13.95, 12.66, 13.08, 12.47, 12.19, 12.18, 12.09, 11.27,
       11.99, 12.86, 12.19, 12.87, 12.52, 13.33, 12.41, 13.09, 12.85, 12.30,
       13.60, 13.10, 11.65, 13.47, 12.36, 12.74, 11.88, 12.91, 11.50, 12.50)

x <- c(4, 3, 3, 3, 1, 4, 2, 1, 0, 2, 2, 2, 5, 2, 3, 3, 3, 3, 3, 2,
       2, 2, 1, 3, 2, 3, 2, 4, 3, 4, 5, 2, 3, 3, 4, 1, 4, 3, 2, 3,
       2, 2, 2, 2, 4, 2, 4, 2, 4, 4)

alpha <- 0.02
h     <- 0.69
n     <- length(y)
dat   <- data.frame(Y = y, X = x)

# ============================================================
# a) ЛИНЕЙНАЯ МОДЕЛЬ
# ============================================================

Y  <- as.matrix(y)
x0 <- rep(1, n)

# Матричный МНК: β̂ = (X'X)^{-1} X'Y
Xl  <- matrix(c(x0, x), nrow = 2, byrow = TRUE)   # 2×50
Sl  <- Xl %*% t(Xl)                                # X X'  (2×2)
S1l <- solve(Sl)
bhatl <- S1l %*% Xl %*% Y

cat("=== a) Линейная модель ===\n")
cat("β̂1 (сдвиг)  =", bhatl[1], "\n")
cat("β̂2 (масштаб) =", bhatl[2], "\n")

# Проверка через lm()
q0 <- lm(Y ~ 1,        data = dat)   # нулевая (константная)
q1 <- lm(Y ~ X,        data = dat)   # линейная
cat("Через lm:", coef(q1))

# График
x_range <- c(min(x), max(x))
y_lin   <- bhatl[1] + bhatl[2] * x_range

plot(x, y, pch = 16, cex = 1.2,
     main = "Линейная и квадратичная регрессия",
     xlab = "Ковариата X", ylab = "Наблюдаемая Y")
lines(x_range, y_lin, col = "blue", lwd = 2)

# ============================================================
# b) ПОЛИНОМИАЛЬНАЯ (КВАДРАТИЧНАЯ) МОДЕЛЬ
# ============================================================

X2 <- matrix(c(x0, x, x^2), nrow = 3, byrow = TRUE)  # 3×50
S2  <- X2 %*% t(X2)
S12 <- solve(S2)
bhat2 <- S12 %*% X2 %*% Y

cat("\n=== b) Полиномиальная модель ===\n")
cat("β̂1 =", bhat2[1], "\n")
cat("β̂2 =", bhat2[2], "\n")
cat("β̂3 =", bhat2[3], "\n")

q2  <- lm(Y ~ I(X) + I(X^2), data = dat)
q2s <- summary(q2)
cat("\nsummary(q2):\n"); print(q2s)
coef(q2)
# Кривая квадратичной модели
x_seq <- seq(min(x), max(x), length.out = 500)
y_quad <- bhat2[1] + bhat2[2]*x_seq + bhat2[3]*x_seq^2
lines(x_seq, y_quad, col = "red", lwd = 2)
legend("topleft",
       legend = c("Линейная", "Квадратичная", "Наблюдения"),
       col    = c("blue", "red", "black"),
       lty    = c(1, 1, NA), pch = c(NA, NA, 16))

# Вершина параболы
x_vertex <- -bhat2[2] / (2 * bhat2[3])
cat("Вершина параболы X* =", x_vertex, "\n")



# ============================================================
# c) χ²-критерий Пирсона (ровно по 10 наблюдений в группе)
# ============================================================

q2  <- lm(Y ~ X + I(X^2), data = dat)
res <- residuals(q2)

mu_start <- mean(res)

# выборочное стандартное отклонение остатков
sigma_start <- sd(res)

cat("Начальные значения:\n")
cat("mu_start =", mu_start, "\n")
cat("sigma_start =", sigma_start, "\n")

# ------------------------------------------------------------
# Минимизация статистики χ²
# ------------------------------------------------------------

# Функция статистики Пирсона
chi2_fn <- function(params){
  
  mu    <- params[1]
  sigma <- params[2]
  
  # sigma должна быть положительной
  if(sigma <= 0){
    return(Inf)
  }
  
  # вероятности по интервалам
  p <- diff(
    pnorm(
      brk,
      mean = mu,
      sd   = sigma
    )
  )
  
  # ожидаемые частоты
  expected <- n * p
  
  # защита от деления на ноль
  expected[expected < 1e-10] <- 1e-10
  
  # статистика χ²
  sum((nu - expected)^2 / expected)
}

# Минимизация χ² с помощью optim
opt <- optim(
  par    = c(mu_start, sigma_start),
  fn     = chi2_fn,
  method = "Nelder-Mead"
)

# ------------------------------------------------------------
# Оценки параметров
# ------------------------------------------------------------

mu_hat    <- opt$par[1]
sigma_hat <- opt$par[2]

cat("\nОценки после минимизации:\n")
cat("mu_hat =", mu_hat, "\n")
cat("sigma_hat =", sigma_hat, "\n")


# --- Гистограмма ---
# 1. Сохраняем результат hist() в объект
hist_obj <- hist(res,
                 breaks      = breaks_hist,
                 probability = TRUE,
                 col         = "lightgray",
                 border      = "black",
                 main        = "Гистограмма остатков полиномиальной модели",
                 xlab        = "Остатки",
                 ylim        = c(0, 0.8))
curve(dnorm(x, mean = mu_hat, sd = sigma_hat),
      add = TRUE, col = "red", lwd = 2)
# 2. Теперь можно смотреть частоты:
print(hist_obj$counts)

# 3. Удобный вывод в виде таблицы (интервал + количество)
counts_table <- data.frame(
  Интервал = paste0("[", head(hist_obj$breaks, -1), ", ", tail(hist_obj$breaks, -1), "]"),
  Частота  = hist_obj$counts
)
print(counts_table, row.names = FALSE)



#--------------гистограмма по квантилям-----------------
k <- 5
breaks <- quantile(
  res,
  probs = seq(0, 1, length.out = k + 1)
)
breaks
table(cut(res, breaks = breaks, include.lowest = TRUE))
# Гистограмма
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

# --- Упорядоченные остатки ---
res_sorted <- sort(res)
cat("Упорядоченные остатки:\n")
print(round(res_sorted, 5))

# ============================================================
# Границы через упорядоченные остатки:
# между 10-м и 11-м, между 20-м и 21-м, между 30-м и 31-м,
# между 40-м и 41-м элементами
# ============================================================

k_final  <- 5
obs_per  <- n / k_final   # = 10

# Внутренние границы — середины между соседними остатками
b1 <- (res_sorted[10] + res_sorted[11]) / 2
b2 <- (res_sorted[20] + res_sorted[21]) / 2
b3 <- (res_sorted[30] + res_sorted[31]) / 2
b4 <- (res_sorted[40] + res_sorted[41]) / 2

brk <- c(-Inf, b1, b2, b3, b4, +Inf)

cat(sprintf("\nВнутренние границы интервалов:\n"))
cat(sprintf("b1 = %.5f\n", b1))
cat(sprintf("b2 = %.5f\n", b2))
cat(sprintf("b3 = %.5f\n", b3))
cat(sprintf("b4 = %.5f\n\n", b4))

# --- Наблюдаемые частоты (ровно по 10) ---
nu <- numeric(k_final)
for (i in 1:k_final) {
  if (i == 1) {
    nu[i] <- sum(res <= brk[2])
  } else if (i == k_final) {
    nu[i] <- sum(res > brk[k_final])
  } else {
    nu[i] <- sum(res > brk[i] & res <= brk[i+1])
  }
}

cat("Наблюдаемые частоты nu (должно быть ровно 10):\n")
print(nu)

# --- Теоретические вероятности через N(mu, sigma) ---
p0 <- numeric(k_final)
for (i in 1:k_final) {
  if (i == 1) {
    p0[i] <- pnorm(brk[2], mean = mu_hat, sd = sigma_hat)
  } else if (i == k_final) {
    p0[i] <- 1 - pnorm(brk[k_final], mean = mu_hat, sd = sigma_hat)
  } else {
    p0[i] <- pnorm(brk[i+1], mean = mu_hat, sd = sigma_hat) -
      pnorm(brk[i],   mean = mu_hat, sd = sigma_hat)
  }
}

np0 <- n * p0

cat("\nОжидаемые частоты np:\n")
print(round(np0, 5))

# ============================================================
# Статистика χ²
# ============================================================

res_std  <- (nu - np0) / sqrt(np0)
res2_val <- res_std^2
chi2_obs <- sum(res2_val)

df_chi   <- k_final - 1 - 2      # 5 - 1 - 2 = 2
chi_crit <- qchisq(1 - alpha, df_chi)
p_value  <- pchisq(chi2_obs, df_chi, lower.tail = FALSE)

# ============================================================
# Таблица Пирсона
# ============================================================

ranges <- character(k_final)
for (i in 1:k_final) {
  ranges[i] <- if (i == 1) {
    sprintf("(-Inf; %.3f]",         brk[2])
  } else if (i == k_final) {
    sprintf("(%.3f; +Inf)",         brk[k_final])
  } else {
    sprintf("(%.3f; %.3f]",         brk[i], brk[i+1])
  }
}

cat("\n")
cat("=================================================================\n")
cat("Таблица критерия Пирсона χ²\n")
cat("=================================================================\n")
cat(sprintf("%-3s  %-22s  %4s  %10s  %7s  %10s  %10s\n",
            "i", "range", "nu_i", "p_i", "np_i",
            "(v-np)/√np", "(v-np)²/np"))
cat(strrep("-", 77), "\n")

for (i in 1:k_final) {
  cat(sprintf("%-3d  %-22s  %4d  %10.7f  %7.5f  %10.5f  %10.7f\n",
              i,
              ranges[i],
              nu[i],
              p0[i],
              np0[i],
              res_std[i],
              res2_val[i]))
}

cat(strrep("-", 77), "\n")
cat(sprintf("%-3s  %-22s  %4d  %10.7f  %7.5f  %10s  %10.7f\n",
            "", "Итого", sum(nu), sum(p0), sum(np0), "χ²_набл =", chi2_obs))

# ============================================================
# Итоги
# ============================================================

cat("\n=================================================================\n")
cat("Результаты критерия χ²\n")
cat("=================================================================\n")
cat(sprintf("Число интервалов  k  = %d\n",          k_final))
cat(sprintf("Степени свободы   df = k-1-2 = %d\n",  df_chi))
cat(sprintf("χ²_набл              = %.5f\n",         chi2_obs))
cat(sprintf("χ²_крит (α = %.2f)   = %.5f\n",        alpha, chi_crit))
cat(sprintf("p-value              = %.5f\n",         p_value))
cat("\n")

if (chi2_obs > chi_crit) {
  cat(sprintf(
    "Вывод: Отвергаем H0 на уровне значимости α = %.2f.\n", alpha))
  cat("Остатки НЕ согласуются с нормальным распределением.\n")
} else {
  cat(sprintf(
    "Вывод: Нет оснований отвергать H0 на уровне значимости α = %.2f.\n",
    alpha))
  cat("Остатки согласуются с нормальным распределением.\n")
}


# ============================================================
# d) ДОВЕРИТЕЛЬНЫЕ ИНТЕРВАЛЫ ДЛЯ β2 и β3
# ============================================================
library(ellipse)

p  <- 3; df <- n - p
beta_hat <- coef(q2)
b2_hat   <- beta_hat[2]
b3_hat   <- beta_hat[3]

B_mat   <- q2s$cov.unscaled
V_mat   <- q2s$sigma^2 * B_mat
Sigma_23 <- V_mat[2:3, 2:3]
se2 <- sqrt(V_mat[2,2])
se3 <- sqrt(V_mat[3,3])

# ── 1. Частные t-интервалы ──────────────────────────────────
t_crit   <- qt(1 - alpha/2, df)
ci_indiv <- confint(q2, level = 1 - alpha)[2:3, ]
cat("Частные ДИ (индивидуальный t):\n"); print(round(ci_indiv, 8))

# ── 2. Совместный эллипс (Шеффе) ───────────────────────────
q       <- 2
F_joint <- qf(1 - alpha, q, df)
S_scheffe <- sqrt(q * F_joint)
cat(sprintf("Поправка Шеффе: S = %.6f\n", S_scheffe))

# Уровень для ellipse(): пересчитываем в хи-квадрат
chi2_level <- pchisq(q * F_joint, df = q)
ellipse_pts <- ellipse(Sigma_23, centre = c(b2_hat, b3_hat), level = chi2_level)

# ── 3. Метод Бонферрони ─────────────────────────────────────
k           <- 2
t_crit_bonf <- qt(1 - alpha / (2 * k), df)
cat(sprintf("t_крит (Бонферрони): %.6f\n", t_crit_bonf))

bonf_b2 <- c(b2_hat - t_crit_bonf * se2, b2_hat + t_crit_bonf * se2)
bonf_b3 <- c(b3_hat - t_crit_bonf * se3, b3_hat + t_crit_bonf * se3)

# ── 4. Метод Шеффе для β3 ──────────────────────────────────
# Проекция эллипса на ось β3: a = (0, 1)
scheffe_b3 <- c(b3_hat - S_scheffe * se3, b3_hat + S_scheffe * se3)
cat(sprintf("Шеффе для β3: [%.6f, %.6f]\n", scheffe_b3[1], scheffe_b3[2]))

# ── 4b. Метод Шеффе для β2 ───────────────────────────────── 
scheffe_b2 <- c(b2_hat - S_scheffe * se2, b2_hat + S_scheffe * se2)
cat(sprintf("Шеффе для β2: [%.6f, %.6f]\n", scheffe_b2[1], scheffe_b2[2]))

# ── 5. Метод Шеффе для ψ = β2 + β3 ─────────────────────────
a_psi   <- c(1, 1)
psi_hat <- sum(a_psi * c(b2_hat, b3_hat))
se_psi  <- sqrt(as.numeric(t(a_psi) %*% Sigma_23 %*% a_psi))
scheffe_psi <- c(psi_hat - S_scheffe * se_psi, psi_hat + S_scheffe * se_psi)
cat(sprintf("Шеффе для ψ=β2+β3: [%.6f, %.6f]\n", scheffe_psi[1], scheffe_psi[2]))

# ── 6. Сравнительная таблица для β3 ────────────────────────
t_indiv_b3 <- c(b3_hat - t_crit * se3, b3_hat + t_crit * se3)

comparison <- data.frame(
  Метод   = c("Индивидуальный t", "Бонферрони", "Шеффе"),
  Нижняя  = c(t_indiv_b3[1], bonf_b3[1], scheffe_b3[1]),
  Верхняя = c(t_indiv_b3[2], bonf_b3[2], scheffe_b3[2]),
  Ширина  = c(diff(t_indiv_b3), diff(bonf_b3), diff(scheffe_b3))
)
print(comparison, row.names = FALSE, digits = 6)

# ── 7. График: эллипс + прямоугольник Бонферрони ────────────
x_range <- range(c(ellipse_pts[,1], bonf_b2)) * 1.15
y_range <- range(c(ellipse_pts[,2], bonf_b3)) * 1.2

plot(ellipse_pts, type = "l", lwd = 2, col = "steelblue",
     xlim = x_range, ylim = y_range,
     main = paste0("Совместные ДИ ", (1-alpha)*100, "%"),
     xlab = expression(beta[2]), ylab = expression(beta[3]))

rect(bonf_b2[1], bonf_b3[1], bonf_b2[2], bonf_b3[2],
     border = "firebrick", lwd = 2, lty = 2)

# Проекция Шеффе для β3 (вертикальный отрезок при β2 = b2_hat)
tick <- (bonf_b2[2] - bonf_b2[1]) * 0.02
segments(b2_hat, scheffe_b3[1], b2_hat, scheffe_b3[2], col = "darkgreen", lwd = 3)
segments(b2_hat - tick, scheffe_b3[1], b2_hat + tick, scheffe_b3[1], col = "darkgreen", lwd = 3)
segments(b2_hat - tick, scheffe_b3[2], b2_hat + tick, scheffe_b3[2], col = "darkgreen", lwd = 3)

abline(v = 0, h = 0, lty = 2, col = "grey60")
points(b2_hat, b3_hat, pch = 16, col = "red", cex = 1.4)

legend("topright",
       legend = c("Эллипс (Шеффе)", "Прямоугольник Бонферрони",
                  "Проекция Шеффе для β3", "β̂"),
       col    = c("steelblue", "firebrick", "darkgreen", "red"),
       lty    = c(1, 2, 1, NA), pch = c(NA, NA, NA, 16),
       lwd = 2, bty = "n")

# ── 8. График: сравнение интервалов для β3 ─────────────────
plot(1, type = "n",
     xlim = range(c(t_indiv_b3, bonf_b3, scheffe_b3)) * c(0.95, 1.05),
     ylim = c(0.5, 3.5), axes = FALSE,
     xlab = expression(beta[3]), ylab = "",
     main = "Сравнение ДИ для β3")
axis(1); axis(2, at = 1:3, labels = c("Шеффе", "Бонферрони", "Индивид. t"))
grid()

bounds <- rbind(t_indiv_b3, bonf_b3, scheffe_b3)
colors <- c("steelblue", "firebrick", "darkgreen")
for (i in 1:3) {
  segments(bounds[i,1], i, bounds[i,2], i, col = colors[i], lwd = 3)
  points(b3_hat, i, pch = 16, col = "red", cex = 1.2)
}
abline(v = 0, lty = 2, col = "grey60")
legend("topright",
       legend = c("Индивид. t", "Бонферрони", "Шеффе", "β̂₃"),
       col = c("steelblue", "firebrick", "darkgreen", "red"),
       lwd = c(3,3,3,NA), pch = c(NA,NA,NA,16), bty = "n")
cat("\n====================================================\n")
cat("d) Доверительные интервалы\n")
cat("====================================================\n")

# Основные параметры
p  <- 3
df <- n - p
cat(sprintf("n  = %d\n", n))
cat(sprintf("p  = %d\n", p))
cat(sprintf("df = %d\n", df))
cat(sprintf("Уровень доверия = %.2f\n", 1 - alpha))
beta_hat <- coef(q2)
b2_hat <- beta_hat[2]
b3_hat <- beta_hat[3]
cat("\nОценки коэффициентов:\n")
cat(sprintf("beta2_hat = %.6f\n", b2_hat))
cat(sprintf("beta3_hat = %.6f\n", b3_hat))

# Ковариационная матрица
B_mat <- q2s$cov.unscaled
V_mat <- q2s$sigma^2 * B_mat
Sigma_23 <- V_mat[2:3, 2:3]

# Стандартные ошибки
se2 <- sqrt(V_mat[2,2])
se3 <- sqrt(V_mat[3,3])
se_table <- data.frame(
  parameter = c("beta2", "beta3"),
  SE = c(se2, se3)
)
cat("\nСтандартные ошибки:\n")
se_table$SE <- round(se_table$SE, 8)
print(se_table, row.names = FALSE)

# Частные доверительные интервалы
ci_individual <- confint(
  q2,
  level = 1 - alpha
)
ci_table <- data.frame(
  parameter = c("beta2", "beta3"),
  lower = ci_individual[2:3,1],
  upper = ci_individual[2:3,2]
)
cat("\nЧастные доверительные интервалы:\n")
ci_table$lower <- round(ci_table$lower, 8)
ci_table$upper <- round(ci_table$upper, 8)

print(ci_table, row.names = FALSE)

# t-критическое
t_crit <- qt(
  1 - alpha/2,
  df
)
cat("\nКритическое значение t:\n")
cat(sprintf("t_(1-alpha/2, df) = %.6f\n", t_crit))

# Совместная доверительная область
Sigma_inv <- solve(Sigma_23)

cat("\nКовариационная матрица для (β2, β3):\n")
print(round(Sigma_23, 8))
cat("\nОбратная ковариационная матрица:\n")
print(round(Sigma_inv, 8))

# F-критическое
F_joint <- qf(
  1 - alpha,
  df1 = 2,
  df2 = df
)

cat("\nКритическое значение F:\n")
cat(sprintf("F_(1-alpha; 2, %d) = %.6f\n", df, F_joint))


# Уравнение доверительного эллипса
cat("\nСовместная доверительная область:\n")
cat("(b - b_hat)^T * Sigma^(-1) * (b - b_hat)")
cat(sprintf(" <= %.6f\n", 2 * F_joint))


# График эллипса
library(ellipse)
ellipse_pts <- ellipse(
  Sigma_23,
  centre = c(b2_hat, b3_hat),
  level  = 1 - alpha
)
plot(
  ellipse_pts,
  type = "l",
  lwd = 2,
  main = paste0(
    "Совместный ДИ ",
    (1-alpha)*100,
    "%"
  ),
  xlab = expression(beta[2]),
  ylab = expression(beta[3])
)
points(
  b2_hat,
  b3_hat,
  pch = 16,
  col = "red"
)
abline(v = 0, lty = 2, col = "black") # Вертикальная линия x = 0
abline(h = 0, lty = 2, col = "black") # Горизонтальная линия y = 0


# МЕТОД БОНФЕРРОНИ 
cat("\n--- Метод Бонферрони ---\n")

k <- 2  # количество параметров
alpha_bonf <- alpha / k
t_crit_bonf <- qt(1 - alpha_bonf / 2, df)
cat(sprintf("Критическое t (Бонферрони): t_(1-alpha/(2*k), df) = %.6f\n", t_crit_bonf))

bonf_lower_b2 <- b2_hat - t_crit_bonf * se2
bonf_upper_b2 <- b2_hat + t_crit_bonf * se2
bonf_lower_b3 <- b3_hat - t_crit_bonf * se3
bonf_upper_b3 <- b3_hat + t_crit_bonf * se3

cat("\nСовместный ДИ методом Бонферрони (уровень доверия =", 1 - alpha, "):\n")
bonf_table <- data.frame(
  Параметр = c("beta[2]", "beta[3]"),  
  Нижняя_граница = c(bonf_lower_b2, bonf_lower_b3),
  Верхняя_граница = c(bonf_upper_b2, bonf_upper_b3),
  stringsAsFactors = FALSE
)
print(bonf_table, row.names = FALSE, digits = 7)

# МЕТОД ШЕФФЕ
cat("\n--- Метод Шеффе ---\n")

a_vec <- c(1, 1)
v_hat <- sum(a_vec * c(b2_hat, b3_hat))
se_v <- sqrt(t(a_vec) %*% Sigma_23 %*% a_vec)
cat(sprintf("Оценка ψ = β2 + β3: %.6f\n", v_hat))
cat(sprintf("Стандартная ошибка SE(ψ): %.6f\n", se_v))

q <- 2
S_scheffe <- sqrt(q * F_joint)
cat(sprintf("Поправка Шеффе: S = sqrt(q * F) = %.6f\n", S_scheffe))

scheffe_lower <- v_hat - S_scheffe * se_v
scheffe_upper <- v_hat + S_scheffe * se_v

cat("\nСовместный ДИ методом Шеффе для ψ = β2 + β3 (уровень доверия =", 1 - alpha, "):\n")
scheffe_table <- data.frame(
  Линейная_комбинация = "ψ = β₂ + β₃",
  Нижняя_граница = scheffe_lower,
  Верхняя_граница = scheffe_upper,
  stringsAsFactors = FALSE
)
print(scheffe_table, row.names = FALSE, digits = 7)

# СРАВНЕНИЕ ШИРИНЫ ИНТЕРВАЛОВ
cat("\n--- Сравнение ширины интервалов ---\n")
cat(sprintf("Ширина ДИ для β2 (Бонферрони): %.6f\n", bonf_upper_b2 - bonf_lower_b2))
cat(sprintf("Ширина ДИ для β3 (Бонферрони): %.6f\n", bonf_upper_b3 - bonf_lower_b3))
cat(sprintf("Ширина ДИ для ψ=β2+β3 (Шеффе):  %.6f\n", scheffe_upper - scheffe_lower))

#----------------------переделка-------------------------
library(ellipse)

# ── Совместный эллипс (= метод Шеффе) ──────────────────────────────────────
# Критическое значение F-распределения
q   <- 2
F_joint <- qf(1 - alpha, q, df)
cat(sprintf("F_крит (Шеффе/эллипс): F_(1-alpha, %d, %d) = %.6f\n", q, df, F_joint))

# Граница эллипса: (beta - beta_hat)^T Sigma^{-1} (beta - beta_hat) <= q * F
# ellipse() ожидает корреляционную/ковариационную матрицу и строит
# множество точек на границе эллипсоида уровня (1-alpha) для нормального вектора.
# Чтобы получить область { b : (b - b_hat)^T Sigma^{-1} (b - b_hat) <= q*F },
# передаём Sigma_23 и level = 1 - alpha (ellipse использует chi2/F внутри).
# НО: ellipse(..., level=p) строит контур хи-квадрат с 2 степенями свободы,
# т.е. границу q*F_joint нужно пересчитать в уровень хи-квадрат:
chi2_level <- pchisq(q * F_joint, df = q)

ellipse_pts <- ellipse(
  Sigma_23,
  centre = c(b2_hat, b3_hat),
  level  = chi2_level          # соответствует q * F_(1-alpha, q, df)
)

# ── Метод Бонферрони ────────────────────────────────────────────────────────
k            <- 2
alpha_bonf   <- alpha / k
t_crit_bonf  <- qt(1 - alpha_bonf / 2, df)
cat(sprintf("t_крит (Бонферрони): %.6f\n", t_crit_bonf))

bonf_lower_b2 <- b2_hat - t_crit_bonf * se2
bonf_upper_b2 <- b2_hat + t_crit_bonf * se2
bonf_lower_b3 <- b3_hat - t_crit_bonf * se3
bonf_upper_b3 <- b3_hat + t_crit_bonf * se3

cat(sprintf("β2 Бонферрони: [%.6f, %.6f]\n", bonf_lower_b2, bonf_upper_b2))
cat(sprintf("β3 Бонферрони: [%.6f, %.6f]\n", bonf_lower_b3, bonf_upper_b3))

# ============================================================
# ШЕФФЕ ДЛЯ beta3
# ============================================================

# Вектор для beta3
a_beta3 <- c(0, 1)

# Оценка beta3
psi_beta3 <- b3_hat

# SE(beta3)
se_beta3_scheffe <- sqrt(
  t(a_beta3) %*% Sigma_23 %*% a_beta3
)

# Интервал Шеффе
scheffe_beta3_lower <-
  psi_beta3 - S_scheffe * se_beta3_scheffe

scheffe_beta3_upper <-
  psi_beta3 + S_scheffe * se_beta3_scheffe

cat("\nИнтервал Шеффе для beta3:\n")
cat(sprintf(
  "[%.6f ; %.6f]\n",
  scheffe_beta3_lower,
  scheffe_beta3_upper
))
# Вертикальный отрезок Шеффе для beta3
segments(
  x0 = b2_hat,
  y0 = scheffe_beta3_lower,
  x1 = b2_hat,
  y1 = scheffe_beta3_upper,
  col = "darkgreen",
  lwd = 3
)

# Засечки сверху/снизу
segments(
  x0 = b2_hat - 0.015,
  y0 = scheffe_beta3_lower,
  x1 = b2_hat + 0.015,
  y1 = scheffe_beta3_lower,
  col = "darkgreen",
  lwd = 3
)

segments(
  x0 = b2_hat - 0.015,
  y0 = scheffe_beta3_upper,
  x1 = b2_hat + 0.015,
  y1 = scheffe_beta3_upper,
  col = "darkgreen",
  lwd = 3
)

# ── График 1: эллипс + прямоугольник Бонферрони ────────────────────────────
plot_joint <- function(show_scheffe = FALSE) {
  # Общий диапазон осей
  x_range <- range(c(ellipse_pts[,1], bonf_lower_b2, bonf_upper_b2)) * c(1.15, 1.15)
  y_range <- range(c(ellipse_pts[,2], bonf_lower_b3, bonf_upper_b3)) * c(1.5,  1.15)
  
  plot(
    ellipse_pts, type = "l", lwd = 2, col = "steelblue",
    xlim = x_range, ylim = y_range,
    main = if (show_scheffe)
      paste0("Совместные ДИ ", (1-alpha)*100, "%: эллипс, Бонферрони, Шеффе")
    else
      paste0("Совместные ДИ ", (1-alpha)*100, "%: эллипс и Бонферрони"),
    xlab = expression(beta[2]),
    ylab = expression(beta[3])
  )
  
  # Прямоугольник Бонферрони
  rect(
    bonf_lower_b2, bonf_lower_b3,
    bonf_upper_b2, bonf_upper_b3,
    border = "firebrick", lwd = 2, lty = 2
  )
  
  # Шеффе = тот же эллипс, но явно отмечаем отдельным цветом
  if (show_scheffe) {
    lines(ellipse_pts, col = "darkorange", lwd = 2, lty = 3)
  }
  
  # Оси координат
  abline(v = 0, lty = 2, col = "grey50")
  abline(h = 0, lty = 2, col = "grey50")
  
  # Оценка параметров
  points(b2_hat, b3_hat, pch = 16, col = "red", cex = 1.4)
  
  # Легенда
  if (show_scheffe) {
    legend(
      "topright",
      legend = c(
        "Эллипс (совм. ДИ / Шеффе)",
        "Прямоугольник Бонферрони"
      ),
      col    = c("steelblue", "firebrick", "red"),
      lty    = c(1, 2, NA),
      pch    = c(NA, NA, 16),
      lwd    = 2,
      bty    = "n"
    )
  } else {
    legend(
      "topright",
      legend = c("Эллипс (совм. ДИ)", "Прямоугольник Бонферрони"),
      col    = c("steelblue", "firebrick", "red"),
      lty    = c(1, 2, NA),
      pch    = c(NA, NA, 16),
      lwd    = 2,
      bty    = "n"
    )
  }
}

# Рисунок 1: эллипс + Бонферрони
plot_joint(show_scheffe = FALSE)

# Рисунок 2: все три (эллипс = Шеффе + Бонферрони)
plot_joint(show_scheffe = TRUE)

# ── Метод Шеффе: пояснение и вывод ─────────────────────────────────────────
# Совместная доверительная область Шеффе — это тот же эллипс.
# Дополнительно покажем, что точка (0,0) вне эллипса:
beta_test  <- c(0, 0)
diff_vec   <- c(b2_hat, b3_hat) - beta_test
Sigma_inv  <- solve(Sigma_23)
stat_joint <- as.numeric(t(diff_vec) %*% Sigma_inv %*% diff_vec)
threshold  <- q * F_joint

cat(sprintf("\nСтатистика для (0,0): %.4f, порог q*F = %.4f\n", stat_joint, threshold))
cat(sprintf("(0,0) вне эллипса Шеффе: %s\n",
            ifelse(stat_joint > threshold, "ДА → H0 отвергается", "НЕТ")))

cat(sprintf("\nПоправка Шеффе: S = sqrt(q * F) = sqrt(%d * %.4f) = %.6f\n",
            q, F_joint, sqrt(q * F_joint)))

# ============================================================================
# МЕТОД ШЕФФЕ ДЛЯ ОТДЕЛЬНОГО ПАРАМЕТРА β3
# ============================================================================
cat("\n--- Метод Шеффе: интервал только для β3 ---\n")

# Поправка Шеффе (та же, что и для эллипса)
q <- 2
S_scheffe <- sqrt(q * F_joint)  # F_joint уже вычислен ранее
cat(sprintf("Поправка Шеффе: S = sqrt(%d * %.4f) = %.6f\n", 
            q, F_joint, S_scheffe))

# Интервал для β3
scheffe_b3_lower <- b3_hat - S_scheffe * se3
scheffe_b3_upper <- b3_hat + S_scheffe * se3

cat("\nСовместный ДИ методом Шеффе для β3 (уровень доверия =", 1 - alpha, "):\n")
scheffe_b3_table <- data.frame(
  Параметр = "beta[3]",
  Нижняя_граница = scheffe_b3_lower,
  Верхняя_граница = scheffe_b3_upper,
  stringsAsFactors = FALSE
)
print(scheffe_b3_table, row.names = FALSE, digits = 7)

# ============================================================================
# СРАВНЕНИЕ: три типа интервалов для β3
# ============================================================================
cat("\n--- Сравнение интервалов для β3 ---\n")

# Индивидуальный t-интервал
t_indiv <- qt(1 - alpha/2, df)
ci_t_lower <- b3_hat - t_indiv * se3
ci_t_upper <- b3_hat + t_indiv * se3

# Бонферрони (уже вычислен)
# bonf_lower_b3, bonf_upper_b3

# Шеффе (только что вычислен)
# scheffe_b3_lower, scheffe_b3_upper

comparison <- data.frame(
  Метод = c("Индивидуальный t", "Бонферрони", "Шеффе"),
  Нижняя = c(ci_t_lower, bonf_lower_b3, scheffe_b3_lower),
  Верхняя = c(ci_t_upper, bonf_upper_b3, scheffe_b3_upper),
  Ширина = c(
    ci_t_upper - ci_t_lower,
    bonf_upper_b3 - bonf_lower_b3,
    scheffe_b3_upper - scheffe_b3_lower
  )
)
print(comparison, row.names = FALSE, digits = 6)
# График: сравнение интервалов для β3
y_positions <- c(3, 2, 1)  # позиции для трех методов
x_lower <- c(ci_t_lower, bonf_lower_b3, scheffe_b3_lower)
x_upper <- c(ci_t_upper, bonf_upper_b3, scheffe_b3_upper)

plot(
  1, 1, type = "n",
  xlim = range(c(x_lower, x_upper)) * c(0.95, 1.05),
  ylim = c(0.5, 3.5),
  axes = FALSE,
  xlab = expression(beta[3]),
  ylab = "",
  main = "Сравнение доверительных интервалов для β3"
)
axis(1)
axis(2, at = 1:3, labels = c("Шеффе", "Бонферрони", "Индивид. t"))
grid()

# Отрезки (используем векторы одинаковой длины)
segments(x0 = x_lower, y0 = y_positions, 
         x1 = x_upper, y1 = y_positions,
         col = c("darkgreen", "firebrick", "steelblue"), 
         lwd = 3)

# Точечные оценки (повторяем b3_hat три раза)
points(rep(b3_hat, 3), y_positions, pch = 16, col = "red", cex = 1.2)

# Вертикальная линия в 0
abline(v = 0, lty = 2, col = "grey50")

legend("topright",
       legend = c("Индивидуальный t", "Бонферрони", "Шеффе", "Оценка"),
       col = c("darkgreen", "firebrick", "steelblue", "red"),
       lwd = c(3, 3, 3, NA),
       pch = c(NA, NA, NA, 16),
       bty = "n")

# ============================================================
# f) ВЫБОР МОДЕЛИ: AIC и BIC
# ============================================================

cat("\n=== f) AIC и BIC ===\n")
aic_vals <- AIC(q0, q1, q2)
bic_vals <- BIC(q0, q1, q2)
rownames(aic_vals) <- rownames(bic_vals) <- c("Y~1 (константная)", "Y~X (линейная)", "Y~X+X² (квадратичная)")
cat("\nAIC:\n"); print(aic_vals)
cat("\nBIC:\n"); print(bic_vals)
best_aic <- rownames(aic_vals)[which.min(aic_vals$AIC)]
best_bic <- rownames(bic_vals)[which.min(bic_vals$BIC)]
cat("\nЛучшая модель по AIC:", best_aic, "\n")
cat("Лучшая модель по BIC:", best_bic, "\n")