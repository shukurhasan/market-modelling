# ------------------------------------------------------------
# Cournot duopoly: a toy model
# Blog post: market-modelling.blogpost.com
# Tested with: R 4.5.2
# Note: all numbers are illustrative, not real company data.
# ------------------------------------------------------------

a <- 100   # price when nothing is sold
b <- 1     # price falls by b for each extra unit sold
cost <- 20    # cost to produce one unit

# A firm's profit-maximising output, given its rival's output
best_response <- function(q_other) {
  pmax((a - cost - b * q_other) / (2 * b), 0)
}

# Equilibrium for n identical firms
q_each <- function(n) (a - cost) / (b * (n + 1))
price_n <- function(n) a - b * n * q_each(n)
profit_each <- function(n) (price_n(n) - cost) * q_each(n)

# Key points for two firms
cournot_q <- q_each(2)                 # about 26.7 each
collude_q <- (a - cost) / (4 * b)      # 20 each (monopoly output, split)

# Chart 1: best-response curves
q <- seq(0, 80, by = 1)
plot(q, best_response(q), type = "l", lwd = 2, col = "steelblue",
     xlim = c(0, 80), ylim = c(0, 80),
     xlab = "Firm A output", ylab = "Firm B output",
     cex.lab = 1.5, cex.axis = 1.2,
     main = "Best responses and the Cournot equilibrium",
     cex.main = 2)
lines(best_response(q), q, lwd = 2, col = "firebrick")
points(cournot_q, cournot_q, pch = 19, cex = 1.5)
points(collude_q, collude_q, pch = 17, cex = 1.5)
text(cournot_q + 0.5, cournot_q + 2, "Cournot Equilibrium", pos = 4, cex = 1.5)
text(collude_q, collude_q - 1, "Collusion", pos = 1, cex = 1.5)
legend(x = 50, y = 80, bty = "n", lwd = 2, col = c("steelblue", "firebrick"),
       legend = c("B's best response to A", "A's best response to B"), cex = 1.5)

# Chart 2: more firms, lower prices and profits
n <- 1:10
plot(n, price_n(n), type = "b", pch = 19, col = "steelblue", lwd = 2,
     ylim = c(0, 100), xlab = "Number of firms",
     ylab = "Dollars", cex.lab = 1.5, cex.axis = 1.2,
     main = "Number of firms vs market price",
     cex.main = 2)
abline(h = cost, lty = 3, lwd = 1.5, col = "darkgreen")
text(2, cost + 3, "unit cost", cex = 1.5)
text(9, cost + 12, "market price", cex = 1.5)


