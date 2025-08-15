crown <- function(z, H, m, n, r_0) {
  out1 = r_0 * m * n * (z/H)^(n-1) * (1 - (z/H)^n)^(m-1)
  out2 = -r_0 * m * n * (z/H)^(n-1) * (1 - (z/H)^n)^(m-1)
  data.table::data.table(radius = c(out1, out2),
                         height = c(z, z))
}

count = 1
tree = NULL
for (z in seq(1, 30, length.out = 20)) {
  tree[[count]] = crown(z, 30, 2.0, 1.1, 40)
  count = count + 1
}

whole_tree <- do.call(rbind, tree)

plot(whole_tree$radius, whole_tree$height)


