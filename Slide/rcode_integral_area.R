
x <- seq(-1.5, 1, by = 0.1)
f <- function(x){
  y <- x^2 + 1
  y
}
y <- f(x)

# Set-up
cairo_pdf("fig3_7.pdf", height = 4, width = 4)
par(mar = c(3, 4, 2, 1))
plot(y ~ x, type = "l", ylim = c(0, 3.5), axes = FALSE, xlab = "", ylab = "")
axis(1, at = c(-1, 0.5), labels = c("a", "b"))
segments(-1, 0, -1, f(-1))
segments(0.5, 0, 0.5, f(0.5))
x.vals <- seq(-1, 0.5, by = 0.01)
polygon(x = c(x.vals, rev(x.vals)), y = c(rep(0, length(x.vals)), f(rev(x.vals))), border = NA,
        col = rgb(0, 1, 0, alpha = 0.2))
abline(h = 0, col = 8)
box()
dev.off()

# S(b+h) - S(b)
cairo_pdf("fig3_8.pdf", height = 4, width = 4)
par(mar = c(3, 4, 2, 1))
plot(y ~ x, type = "l", ylim = c(0, 3.5), axes = FALSE, xlab = "", ylab = "")
axis(1, at = c(-1, 0.5, 0.7), labels = c("a", "b", "b+h"))
mtext("b+h", side = 1, line = 1, at = 0.7, cex = 0.8)
segments(-1, 0, -1, f(-1))
segments(0.5, 0, 0.5, f(0.5))
segments(0.7, 0, 0.7, f(0.7))
x.vals <- seq(0.5, 0.7, by = 0.01)
polygon(x = c(x.vals, rev(x.vals)), y = c(rep(0, length(x.vals)), f(rev(x.vals))), border = NA,
        col = rgb(0, 0, 1, alpha = 0.2))
abline(h = 0, col = 8)
box()
dev.off()

# S(b+h) - S(b) > h * f(b)
cairo_pdf("fig3_9.pdf", height = 4, width = 4)
par(mar = c(3, 4, 2, 1))
plot(y ~ x, type = "l", ylim = c(0, 3.5), axes = FALSE, xlab = "", ylab = "")
axis(1, at = c(-1, 0.5, 0.7), labels = c("a", "b", "b+h"))
mtext("b+h", side = 1, line = 1, at = 0.7, cex = 0.8)
axis(2, at = c(f(0.5)), labels = c("f(b)"), las = 2)
segments(-1, 0, -1, f(-1))
segments(0.5, 0, 0.5, f(0.5))
segments(0.7, 0, 0.7, f(0.7))
polygon(x = c(0.5, 0.7, 0.7, 0.5), y = c(0, 0, f(0.5), f(0.5)), border = NA,
        col = rgb(0, 0, 1, alpha = 0.2))
segments(-1.6, f(0.5), 0.7, f(0.5), col = 4, lty = 2)
abline(h = 0, col = 8)
box()
dev.off()

# S(b+h) - S(b) < h * f(b+h)
cairo_pdf("fig3_10.pdf", height = 4, width = 4)
par(mar = c(3, 4, 2, 1))
plot(y ~ x, type = "l", ylim = c(0, 3.5), axes = FALSE, xlab = "", ylab = "")
axis(1, at = c(-1, 0.5, 0.7), labels = c("a", "b", "b+h"))
mtext("b+h", side = 1, line = 1, at = 0.7, cex = 0.8)
axis(2, at = c(f(0.7)), labels = c("f(b+h)"), las = 2)
segments(-1, 0, -1, f(-1))
segments(0.5, 0, 0.5, f(0.5))
segments(0.7, 0, 0.7, f(0.7))
polygon(x = c(0.5, 0.7, 0.7, 0.5), y = c(0, 0, f(0.7), f(0.7)), border = NA,
        col = rgb(0, 0, 1, alpha = 0.2))
segments(-1.6, f(0.7), 0.7, f(0.7), col = 4, lty = 2)
abline(h = 0, col = 8)
box()
dev.off()



