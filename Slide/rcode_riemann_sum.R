

x <- seq(-1.5, 1, by = 0.1)
f <- function(x){
  y <- -0.5 * x^2 + 2
  y
}
y <- f(x)

# Set-up
cairo_pdf("fig3_1.pdf", height = 4, width = 4)
par(mar = c(3, 4, 2, 1))
plot(y ~ x, type = "l", xlim = c(-1.2, 0.2), ylim = c(0, 2.2), xlab = "", ylab = "", axes = FALSE)
axis(1, at = c(-1, 0), labels = c("a", "b"), cex.lab = 0.8)
abline(h = 0, col = 8)
abline(h = 0, col = 8)
segments(-1, 0, -1, f(-1), col = 2)
segments(0, 0, 0, f(0), col = 2)
box()
dev.off()

# First rectangle
cairo_pdf("fig3_2.pdf", height = 4, width = 4)
par(mar = c(3, 4, 2, 1))
plot(y ~ x, type = "l", xlim = c(-1.2, 0.2), ylim = c(0, 2.2), xlab = "", ylab = "", axes = FALSE)
axis(1, at = c(-1, -0.9, 0), labels = c("a", "a+h", "b"), cex.lab = 0.8)
mtext("a+h", side = 1, line = 1, at = -0.9, cex = 0.8)
axis(2, at = c(f(-1)), labels = c("f(a)"), las = 2, cex.lab = 0.8)
abline(h = 0, col = 8)
segments(-1, 0, -1, f(-1), col = 2)
segments(0, 0, 0, f(0), col = 2)
segments(-1.6, f(-1), -1, f(-1), col = 4, lty = 2)
polygon(x = c(-1, -0.9, -0.9, -1), y = c(0, 0, f(-1), f(-1)), border = 4,
        col = rgb(0, 0, 1, alpha = 0.1))
box()
dev.off()

# Add second rectangle
cairo_pdf("fig3_3.pdf", height = 4, width = 4)
par(mar = c(3, 4, 2, 1))
plot(y ~ x, type = "l", xlim = c(-1.2, 0.2), ylim = c(0, 2.2), xlab = "", ylab = "", axes = FALSE)
axis(1, at = c(-1, -0.9, -0.8, 0), labels = c("a", "", "", "b"), cex.lab = 0.8)
mtext("a+2h", side = 1, line = 1, at = -0.8, cex = 0.8)
axis(2, at = c(f(-0.9)), labels = c("f(a+h)"), las = 2, cex.lab = 0.8)
abline(h = 0, col = 8)
segments(-1, 0, -1, f(-1), col = 2)
segments(0, 0, 0, f(0), col = 2)
segments(-1.6, f(-0.9), -0.9, f(-0.9), col = 4, lty = 2)
polygon(x = c(-1, -0.9, -0.9, -1), y = c(0, 0, f(-1), f(-1)), border = 4,
        col = rgb(0, 0, 1, alpha = 0.1))
polygon(x = c(-0.9, -0.8, -0.8, -0.9), y = c(0, 0, f(-0.9), f(-0.9)), border = 4,
        col = rgb(0, 0, 1, alpha = 0.1))
box()
dev.off()

# h = 0.1
cairo_pdf("fig3_4.pdf", height = 4, width = 4)
par(mar = c(3, 4, 2, 1))
plot(y ~ x, type = "l", xlim = c(-1.2, 0.2), ylim = c(0, 2.2), xlab = "", ylab = "", axes = FALSE)
axis(1, at = c(-1, 0), labels = c("a", "b"))
abline(h = 0, col = 8)
segments(-1, 0, -1, f(-1), col = 2)
segments(0, 0, 0, f(0), col = 2)
for (i in 0:9){
  x.vals <- c(-1 + 0.1 * i, -1 + 0.1 * (i + 1))
  y.val <- f(-1 + 0.1 * i)
  polygon(x = c(x.vals, rev(x.vals)), y = c(0, 0, y.val, y.val), border = 4,
          col = rgb(0, 0, 1, alpha = 0.1))
}
box()
dev.off()

# h = 0.05
cairo_pdf("fig3_5.pdf", height = 4, width = 4)
par(mar = c(3, 4, 2, 1))
plot(y ~ x, type = "l",xlim = c(-1.2, 0.2),  ylim = c(0, 2.2), xlab = "", ylab = "", axes = FALSE)
axis(1, at = c(-1, 0), labels = c("a", "b"))
abline(h = 0, col = 8)
segments(-1, 0, -1, f(-1), col = 2)
segments(0, 0, 0, f(0), col = 2)
for (i in 0:19){
  x.vals <- c(-1 + 0.05 * i, -1 + 0.05 * (i + 1))
  y.val <- f(-1 + 0.05 * i)
  polygon(x = c(x.vals, rev(x.vals)), y = c(0, 0, y.val, y.val), border = 4,
          col = rgb(0, 0, 1, alpha = 0.1))
}
box()
dev.off()

# h = 0.01
cairo_pdf("fig3_6.pdf", height = 4, width = 4)
par(mar = c(3, 4, 2, 1))
plot(y ~ x, type = "l", xlim = c(-1.2, 0.2), ylim = c(0, 2.2), xlab = "", ylab = "", axes = FALSE)
axis(1, at = c(-1, 0), labels = c("a", "b"))
abline(h = 0, col = 8)
segments(-1, 0, -1, f(-1), col = 2)
segments(0, 0, 0, f(0), col = 2)
for (i in 0:99){
  x.vals <- c(-1 + 0.01 * i, -1 + 0.01 * (i + 1))
  y.val <- f(-1 + 0.01 * i)
  polygon(x = c(x.vals, rev(x.vals)), y = c(0, 0, y.val, y.val), border = 4,
          col = rgb(0, 0, 1, alpha = 0.1))
}
box()
dev.off()
