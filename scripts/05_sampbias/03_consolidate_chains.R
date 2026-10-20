# 05c. Consolidate the 16 chains -> results/sampbias_results.csv (R-hat, ESS, half-distances)
# Consolida as cadeias do sampbias: R-hat (cadeias divididas), ESS total, meia-distancia agrupada e IC 95%
# v6 = v5 com o site.data v4. v5 (com instituicoes): nao reabre o raster salvo no .rds (terra nao sobrevive a saveRDS/readRDS entre sessoes sem wrap());
#     registros/celulas vem do CSV que cada cadeia ja salvou na hora em que rodou.
O <- "results/sampbias_runs/"
files <- list.files(O, pattern = "^sampbias_v6_.*_final\\.rds$")
pat <- "^sampbias_v6_([a-z]+)_([AB])_([0-9.]+)deg_it([0-9.e+]+)_seed([0-9]+)_final\\.rds$"
info <- do.call(rbind, lapply(files, function(f) { m <- regmatches(f, regexec(pat, f))[[1]]
  data.frame(file = f, csv = sub("\\.rds$", ".csv", f), realm = m[2], variant = m[3], res = m[4], iters = m[5], seed = m[6]) }))

rhat <- function(mat) {                       # mat: draws x chains; R-hat com cadeias divididas ao meio
  n <- nrow(mat); h <- n %/% 2; sp <- cbind(mat[1:h, , drop = FALSE], mat[(h + 1):(2 * h), , drop = FALSE])
  nn <- nrow(sp); W <- mean(apply(sp, 2, var)); B <- nn * var(colMeans(sp))
  sqrt(((nn - 1) / nn * W + B / nn) / W) }
ess1 <- function(x) { n <- length(x); a <- acf(x, plot = FALSE, lag.max = min(500, n - 1))$acf[-1]
  k <- which(a < 0.05)[1]; if (is.na(k)) k <- length(a); n / (1 + 2 * sum(a[seq_len(k - 1)])) }

out <- list()
for (g in unique(paste(info$realm, info$variant, sep = "_"))) {
  ii <- info[paste(info$realm, info$variant, sep = "_") == g, ]
  bs <- lapply(paste0(O, ii$file), readRDS); n <- min(sapply(bs, function(b) nrow(b$bias_estimate)))
  facs <- sub("^w_", "", grep("^w_", names(bs[[1]]$bias_estimate), value = TRUE))
  c1 <- read.csv(paste0(O, ii$csv[1]))[1, ]   # registros/celulas ja calculados no momento em que a cadeia 1 rodou
  for (fc in facs) {
    mat <- sapply(bs, function(b) b$bias_estimate[[paste0("w_", fc)]][seq_len(n)])
    w <- as.vector(mat); h <- log(2) / w
    out[[length(out) + 1]] <- data.frame(realm = ii$realm[1], variant = ii$variant[1], res_deg = ii$res[1],
      iterations_per_chain = ii$iters[1], chains = nrow(ii), draws_pooled = length(w),
      sites_in_class = c1$sites_in_class, records_counted = c1$records_counted,
      cells_with_records = c1$cells_with_records, cells_total = c1$cells_total,
      factor = fc, half_distance_km = round(median(h)), half_lo = round(quantile(h, .025)), half_hi = round(quantile(h, .975)),
      rate_at_100km = round(median(exp(-w * 100)), 3), Rhat = round(rhat(mat), 3),
      ESS_total = round(sum(apply(mat, 2, ess1))), row.names = NULL)
  }
}
res <- do.call(rbind, out)
res$convergiu <- ifelse(res$Rhat < 1.05 & res$ESS_total > 400, "sim", "NAO")
write.csv(res, "results/sampbias_results.csv", row.names = FALSE)
print(res[, c("realm", "variant", "factor", "half_distance_km", "half_lo", "half_hi", "rate_at_100km", "Rhat", "ESS_total", "convergiu")], row.names = FALSE)
