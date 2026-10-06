# 01. Fix site coordinates in the compiled site table
# Input : data/raw/site_data_raw.csv  (compiled table as received; personal e-mails removed)
# Output: data/intermediate/site_data_coordinates_fixed.csv, logs/LOG_coordinates_site_data.csv
# Every change is logged with the value before and after. Steps:
#  1) BISC: latitude and longitude swapped.
#  2) One-row offset: from spreadsheet row 1997 to the end, latitude/longitude were one row below
#     (each site carried the coordinates of the site above). Coordinates are moved up one row;
#     the last site (Yanachaga-Chemillén, TEAM) is left without coordinates.
#  3) Three ReefCheck sites in Príncipe (Gulf of Guinea, outside the study region) removed.
#  4) Obvious typing errors (sign or decimal point).
#  5) Country label errors (coordinates correct): "Dominican Republic" -> Dominica; "Aruba" -> Trinidad and Tobago (Tobago).
#  6) PELD-IAFA rows 737-738: coordinates copied from rows 3830-3831 (same sites, plausible coordinates).
#  7) Typing errors: Mike's Maze 2 latitude (24.97 was the latitude of Mike's Reef, Bahamas); Tobacco Caye longitude (-86 -> -88).
#  8) Sites whose coordinates do not match the locality and cannot be corrected -> no coordinates
#     (kept in the table, excluded from spatial analyses): PPBioTermites rows 4125-4128, 4130, 4131; PMP-RNCE Trecho C (995).
# "linha_planilha" is the row number in the original spreadsheet (header = row 1) and links every log entry to the source.
dir.create("data/intermediate", showWarnings = FALSE, recursive = TRUE); dir.create("logs", showWarnings = FALSE)
s <- read.csv("data/raw/site_data_raw.csv", strip.white = TRUE, check.names = FALSE)
s$linha_planilha <- seq_len(nrow(s)) + 1
log <- data.frame()
anota <- function(l, acao, antes, depois) log <<- rbind(log, data.frame(linha_planilha = s$linha_planilha[l],
  Network.ID = s$Network.ID[l], Site.Name = s$Site.Name[l], acao = acao, antes = antes, depois = depois))
L <- function(x) match(x, s$linha_planilha)
coord <- function(l) paste(s$Site.Latitude[l], s$Site.Longitude[l])

# 1) BISC
b <- which(s$Network.ID == "BISC")
anota(b, "lat/lon swapped", coord(b), paste(s$Site.Longitude[b], s$Site.Latitude[b]))
tmp <- s$Site.Latitude[b]; s$Site.Latitude[b] <- s$Site.Longitude[b]; s$Site.Longitude[b] <- tmp

# 2) one-row offset
i <- which(s$linha_planilha >= 1997)
lat_novo <- c(s$Site.Latitude[i[-1]], NA); lon_novo <- c(s$Site.Longitude[i[-1]], NA)
anota(i, "coordinates moved up one row (one-row offset)", coord(i), paste(lat_novo, lon_novo))
s$Site.Latitude[i] <- lat_novo; s$Site.Longitude[i] <- lon_novo

# 3) Príncipe
pr <- which(s$Network.ID == "ReefCheck" & s$Site.Country == "Sao Tome and Principe")
anota(pr, "removed (Príncipe, Gulf of Guinea)", coord(pr), "")

# 4) typing errors
fix <- data.frame(linha = c(203, 3003, 4129, 4129, 4467),
  col = c("Site.Latitude", "Site.Latitude", "Site.Latitude", "Site.Longitude", "Site.Latitude"),
  novo = c(-0.6859, -2.5, -3, -59.9766, -10.566),
  motivo = c("ForestGEO Yasuní: misplaced decimal point (-6.859 -> -0.6859)", "Alter do Chão: latitude sign",
             "Campus da UFAM (Manaus): latitude sign", "Campus da UFAM (Manaus): longitude sign", "Chico Mendes: latitude sign"))
for (k in seq_len(nrow(fix))) { l <- L(fix$linha[k]); anota(l, fix$motivo[k], s[[fix$col[k]]][l], fix$novo[k]); s[[fix$col[k]]][l] <- fix$novo[k] }

# 5) country labels
dom <- which(s$Site.Country == "Dominican Republic" & s$Site.Longitude > -62 & s$Site.Longitude < -61 & s$Site.Latitude > 15 & s$Site.Latitude < 16)
anota(dom, "country: Dominican Republic -> Dominica", "Dominican Republic", "Dominica"); s$Site.Country[dom] <- "Dominica"
tob <- which(s$Site.Country == "Aruba" & s$Site.Longitude > -61 & s$Site.Longitude < -60.4 & s$Site.Latitude > 11 & s$Site.Latitude < 11.5)
anota(tob, "country: Aruba -> Trinidad and Tobago", "Aruba", "Trinidad and Tobago"); s$Site.Country[tob] <- "Trinidad and Tobago"
s <- s[-pr, ]

# 6) PELD-IAFA
for (k in 1:2) {
  de <- L(c(3830, 3831)[k]); para <- L(c(737, 738)[k]); stopifnot(s$Site.Name[de] == s$Site.Name[para])
  anota(para, paste("PELD-IAFA: coordinates copied from row", s$linha_planilha[de], "(same site)"), coord(para), coord(de))
  s$Site.Latitude[para] <- s$Site.Latitude[de]; s$Site.Longitude[para] <- s$Site.Longitude[de]
}
# 7) typing errors
fix <- data.frame(linha = c(2067, 1298), col = c("Site.Latitude", "Site.Longitude"), novo = c(17.973278, -88.058472),
  motivo = c("Mike's Maze 2: latitude (24.97 was the latitude of Mike's Reef, Bahamas)", "Tobacco Caye: longitude (-86 -> -88)"))
for (k in seq_len(nrow(fix))) { l <- L(fix$linha[k]); anota(l, fix$motivo[k], s[[fix$col[k]]][l], fix$novo[k]); s[[fix$col[k]]][l] <- fix$novo[k] }
# 8) unverifiable coordinates
na <- L(c(4125, 4126, 4127, 4128, 4130, 4131, 995))
anota(na, "no coordinates: reported coordinates do not match the locality and cannot be corrected", coord(na), "NA NA")
s$Site.Latitude[na] <- NA; s$Site.Longitude[na] <- NA

write.csv(s, "data/intermediate/site_data_coordinates_fixed.csv", row.names = FALSE)
write.csv(log, "logs/LOG_coordinates_site_data.csv", row.names = FALSE)
cat("01 | sites:", nrow(s), "| with coordinates:", sum(!is.na(s$Site.Latitude) & !is.na(s$Site.Longitude)), "| logged changes:", nrow(log), "\n")
