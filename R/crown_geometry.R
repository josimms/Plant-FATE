crown <- function(z, H, m, n, r_0) {
  out1 = r_0 * m * n * (z/H)^(n-1) * (1 - (z/H)^n)^(m-1)
  out2 = -r_0 * m * n * (z/H)^(n-1) * (1 - (z/H)^n)^(m-1)
  data.table::data.table(radius = c(out1, out2),
                         height = c(z, z))
}

tree = crown(0:30, 30, 2.0, 1.1, 15)

plot(tree$radius, tree$height, type = "l", col = "darkgreen")
