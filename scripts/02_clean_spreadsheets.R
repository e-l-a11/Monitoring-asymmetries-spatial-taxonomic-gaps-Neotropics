# 02. Clean and harmonize the spreadsheets -> data/*.csv (the tables used in every analysis)
# Inputs : data/raw/*_raw.csv (compiled tables as received; personal e-mails, links to private files and the
#          lifespan-source column were removed before publication) and data/intermediate/site_data_coordinates_fixed.csv (script 01)
# Outputs: data/*.csv, logs/LOG_cleaning_spreadsheets.csv, logs/summary_spreadsheets.csv
# Steps (all logged): trim spaces; fix shifted cells in variable.data; harmonize Network.ID across tables (reference:
# network.data); remove e-mails; remove identical duplicated rows; fill missing taxonomy from other records of the same species;
# apply the species corrections in data/raw/species_corrections.csv (names, higher taxonomy, implausible lifespans).
R <- "data/raw/"
dir.create("data", showWarnings = FALSE); dir.create("logs", showWarnings = FALSE)
ler <- function(f) read.csv(f, check.names = FALSE, colClasses = "character", na.strings = character(0))
log <- data.frame()
anota <- function(sheet, action, n, detail = "") log <<- rbind(log, data.frame(sheet, action, n, detail))

P <- list(network        = ler(paste0(R, "network_data_raw.csv")),
          site           = ler("data/intermediate/site_data_coordinates_fixed.csv"),
          variable       = ler(paste0(R, "variable_data_raw.csv")),
          site_variables = ler(paste0(R, "site_variables_data_raw.csv")),
          authors        = ler(paste0(R, "authors_data_raw.csv")),
          publications   = ler(paste0(R, "publications_data_raw.csv")),
          groups_time    = ler(paste0(R, "groups_time_raw.csv")))
n0 <- sapply(P, nrow)

# 1) leading/trailing spaces in cells and double spaces in column names
for (k in names(P)) {
  x <- P[[k]]; antes <- sum(sapply(x, function(c) sum(grepl("^\\s|\\s$", c))))
  x[] <- lapply(x, trimws); names(x) <- gsub("\\. ", ".", trimws(names(x)))
  P[[k]] <- x; anota(k, "leading/trailing spaces removed (cells)", antes)
}

# 2) variable.data: unnamed columns and shifted rows
v <- P$variable; names(v)[7] <- "Variable.Record.Method"            # column name used in the GitHub version
ex <- which(names(v) == "")                                          # columns 12-14: leftovers from shifted rows
r1 <- which(grepl("^http", v[[ex[1]]]))                                # MarineGEO, PPBioCS: the 2nd reference is in URL and the link in the next column
v$Variable.Protocol.Reference[r1] <- paste(v$Variable.Protocol.Reference[r1], v$Variable.Protocol.URL[r1], sep = " | ")
v$Variable.Protocol.URL[r1] <- v[[ex[1]]][r1]
anota("variable", "protocol reference/URL recovered from unnamed columns (shifted rows)", length(r1), paste(unique(v$Network.ID[r1]), collapse = ", "))
r2 <- which(v[[ex[3]]] != "" & v$Variable.Protocol.Reference == "")   # BDCTamar Turtle.N: reference two columns to the right
v$Variable.Protocol.Reference[r2] <- v[[ex[3]]][r2]; v$Variable.Protocol.URL[r2] <- "na"
anota("variable", "protocol reference recovered from unnamed columns", length(r2), paste(v$Network.ID[r2], v$Variable.ID[r2]))
v <- v[, -ex]
# PPBio Amp.rept: reptiles/amphibians flagged as abiotic, with the metric shifted to Group_especific
r3 <- which(v$Network.ID == "PPBio" & v$Variable.ID == "Amp.rept" & v$Variable.Type == "abiotic" & v$Metrics == "")
v$Variable.Type[r3] <- "biotic"; v$Metrics[r3] <- v$Group_especific[r3]; v$Group_especific[r3] <- "na"
anota("variable", "PPBio Amp.rept: abiotic -> biotic; metric 'abundance' moved back to Metrics", length(r3))
r4 <- which(v$Network.ID == "BDCTamar" & v$Variable.ID == "Turtle.Interac" & v$Group_especific == "woody plants")
v$Group_especific[r4] <- "marine turtles"; anota("variable", "BDCTamar Turtle.Interac: Group_especific 'woody plants' -> 'marine turtles'", length(r4))
r5 <- which(v$Network.ID == "PPBioCS" & v$Group_especific %in% c("soil variables", "water variables"))
v$Group_especific[r5] <- "na"; anota("variable", "PPBioCS: Group_especific holding the Metrics value -> 'na'", length(r5))
P$variable <- v

# publications: unnamed column with one note -> Publication.OBS
p <- P$publications; names(p)[names(p) == ""] <- "Publication.OBS"; P$publications <- p

# site: unnamed column = links to internal Google Drive sheets (already removed from the public data in data/raw)
s <- P$site; vaz <- which(names(s) == "")
if (length(vaz)) { anota("site", "unnamed column removed (links to internal sheets)", sum(s[[vaz]] != "")); P$site <- s[, -vaz] }

# 3) harmonized Network.ID (reference = network.data)
mapa <- c("PPbioAN ou PBPaAg" = "PBPaAg",   # " PBPaAg" (with a space) was already fixed in step 1
  "Instituto Amazónico de Investigaciones Científicas SINCHI" = "SINCHI",
  "LEV_Com_PalmFixed" = "LEV_ComPalmFixed", "Lev_ComPalmVar" = "LEV_ComPalmVar",
  "PDBFF" = "PBDFF", "PELD FORR" = "PELDFORR", "Planta" = "Plant", "PPBioSinop" = "PPBio.Sinop",
  "PPBio Am Oc" = "PPBio AmOc", "LB.CS" = "LEB.CS", "OPWALL-ECU" = "OPWALL-ECU-T",
  "Bio.M.A" = "Bio.M.A.", "fundamazonia" = "FundAmazonia", "igarapes" = "Igarapes",
  "monitora.mar.pr" = "Monitora.Mar.Pr", "Monitora.mar.pr" = "Monitora.Mar.Pr", "pmcc" = "PMCC",
  "monitoramento_barcarena" = "Monitoramento_Barcarena", "PPBIOMA" = "PPBioMA", "Rede ripária" = "Rede Ripária")
names(mapa) <- trimws(names(mapa))
n <- P$network; i <- grepl("^PPRebioUnião \\(", n$Network.ID)
n$Network.OBS[i] <- paste0(ifelse(n$Network.OBS[i] %in% c("", "na"), "", paste0(n$Network.OBS[i], " | ")), sub("^PPRebioUnião \\((.*)\\)$", "\\1", n$Network.ID[i]))
n$Network.ID[i] <- "PPRebioUnião"; anota("network", "Network.ID 'PPRebioUnião (note...)' -> 'PPRebioUnião' (note moved to Network.OBS)", sum(i))
P$network <- n
for (k in names(P)) {
  id <- P[[k]]$Network.ID; j <- id %in% names(mapa)
  for (de in unique(id[j])) anota(k, "Network.ID harmonized", sum(id == de), paste(de, "->", mapa[[de]]))
  P[[k]]$Network.ID[j] <- mapa[id[j]]
}
# authors: one row with two projects -> two rows
a <- P$authors; j <- which(a$Network.ID == "POPA APAS and POPA FNT")
if (length(j)) { b <- a[j, ]; a$Network.ID[j] <- "POPA APAS"; b$Network.ID <- "POPA FNT"; a <- rbind(a, b)
  anota("authors", "'POPA APAS and POPA FNT' split into two rows (POPA APAS and POPA FNT)", length(j)) }
P$authors <- a

# 4) e-mails
tira <- list(network = c("Network.Email", "Network.Admin.Email"), site = "Site.Contact.Email", authors = "Author.Email")
for (k in names(tira)) { t <- intersect(tira[[k]], names(P[[k]])); if (length(t)) { P[[k]] <- P[[k]][, setdiff(names(P[[k]]), t)]; anota(k, "e-mail column removed", length(t), paste(t, collapse = ", ")) } }
re <- "[[:alnum:]._%+-]+@[[:alnum:].-]+\\.[[:alpha:]]{2,}"
for (k in names(P)) for (c in names(P[[k]])) { j <- grepl(re, P[[k]][[c]])
  if (any(j)) { P[[k]][[c]][j] <- gsub(re, "[e-mail removed]", P[[k]][[c]][j]); anota(k, "e-mail within text replaced", sum(j), c) } }

# 5) groups_time: without the column of lifespan sources
g <- P$groups_time
if ("source.time" %in% names(g)) anota("groups_time", "column source.time (lifespan sources) removed", 1)
P$groups_time <- g[, names(g) != "source.time"]
# taxonomy "na" filled in when the same species has the full taxonomy in another record
g <- P$groups_time; tx <- c("kingdom", "phylum", "class", "order", "family", "genus")
comp <- g[g$phylum != "na", c("species", tx)]; comp <- comp[!duplicated(comp$species), ]
j <- which(g$phylum == "na" & g$species %in% comp$species)
g[j, tx] <- comp[match(g$species[j], comp$species), tx]
anota("groups_time", "taxonomy 'na' filled in from another record of the same species", length(j), paste(unique(g$species[j]), collapse = ", "))
P$groups_time <- g

# 5b) species name, taxonomy and lifespan corrections (data/raw/species_corrections.csv: one row per correction,
#     with its reason and source). Order: lifespan by original name -> names -> higher taxonomy -> fish class ->
#     a single maximum lifespan per species -> one record per species and project
g <- P$groups_time; cc <- ler(paste0(R, "species_corrections.csv"))
L <- cc[cc$lifespan_max != "", ]
for (i in seq_len(nrow(L))) { j <- g$species == L$species_original[i]
  novo <- ifelse(L$lifespan_max[i] == "NA", "na", L$lifespan_max[i])
  g$lifespan.max[j] <- novo; g$`lifespan (years)`[j] <- novo
  anota("groups_time", paste0("lifespan corrected (", L$reason[i], ")"), sum(j), paste0(L$species_original[i], " -> ", novo, ifelse(L$source[i] != "", paste0(" [", L$source[i], "]"), ""))) }
N <- cc[cc$species_corrected != "", ]
for (i in seq_len(nrow(N))) { j <- g$species == N$species_original[i]
  g$species[j] <- N$species_corrected[i]; g$genus[j] <- sub(" .*", "", N$species_corrected[i])
  anota("groups_time", paste0("name corrected (", N$reason[i], ")"), sum(j), paste(N$species_original[i], "->", N$species_corrected[i])) }
T <- cc[cc$kingdom != "", ]
for (i in seq_len(nrow(T))) { alvo <- ifelse(T$species_corrected[i] != "", T$species_corrected[i], T$species_original[i])
  j <- g$species == alvo; g[j, c("kingdom", "phylum", "class", "order", "family")] <- T[i, c("kingdom", "phylum", "class", "order", "family")]
  anota("groups_time", "higher taxonomy corrected", sum(j), alvo) }
j <- g$phylum == "Chordata" & g$class == "na" & g$order %in% c("Characiformes", "Perciformes", "Siluriformes", "Cyprinodontiformes", "Tetraodontiformes", "Synbranchiformes")
g$class[j] <- "Actinopterygii"; anota("groups_time", "fish class 'na' -> Actinopterygii", sum(j), paste(unique(g$species[j]), collapse = ", "))
v <- suppressWarnings(as.numeric(g$lifespan.max)); mx <- tapply(v, g$species, function(x) if (all(is.na(x))) NA else max(x, na.rm = TRUE))
novo <- mx[g$species]; muda <- !is.na(novo) & (is.na(v) | v != novo)
anota("groups_time", "maximum lifespan harmonized across records of the same species (largest value)", sum(muda), paste(unique(g$species[muda]), collapse = ", "))
g$lifespan.max[muda] <- format(novo[muda], trim = TRUE, drop0trailing = TRUE)
d <- duplicated(g[, c("species", "Network.ID")])
anota("groups_time", "repeated records of the same species in the same project (after name corrections) removed", sum(d), paste(unique(g$species[d]), collapse = ", "))
P$groups_time <- g[!d, ]

# 6) repeated rows (identical in every column, after the corrections above)
for (k in names(P)) { d <- duplicated(P[[k]]); if (any(d)) {
  t <- sort(table(P[[k]]$Network.ID[d]), decreasing = TRUE)
  anota(k, "repeated rows removed", sum(d), paste(names(t), t, sep = ": ", collapse = "; ")); P[[k]] <- P[[k]][!d, ] } }

# 7) final check and output
stopifnot(!any(grepl("utm_source", tolower(unlist(P)))))   # no link with tracking parameters
nomes <- c(network = "network_data", site = "site_data", variable = "variable_data", site_variables = "site_variables_data",
           authors = "authors_data", publications = "publications_data", groups_time = "groups_time")
for (k in names(P)) write.csv(P[[k]], file.path("data", paste0(nomes[[k]], ".csv")), row.names = FALSE, fileEncoding = "UTF-8")
resumo <- data.frame(sheet = names(P), rows_before = n0, rows_after = sapply(P, nrow), columns = sapply(P, ncol))
write.csv(log, "logs/LOG_cleaning_spreadsheets.csv", row.names = FALSE); write.csv(resumo, "logs/summary_spreadsheets.csv", row.names = FALSE)
print(resumo, row.names = FALSE)
ids <- lapply(P, function(x) unique(x$Network.ID))
cat("\nNetwork.ID not yet in network.data:\n")
for (k in setdiff(names(ids), "network")) { f <- setdiff(ids[[k]], ids$network); if (length(f)) cat(" ", k, ":", paste(f, collapse = " | "), "\n") }
vb <- P$variable[P$variable$Variable.Type == "biotic", ]
cat("Projects with a biotic variable and no site:", paste(setdiff(unique(vb$Network.ID), ids$site), collapse = " | "), "\n")
cat("Projects with a site and no variable:", paste(setdiff(ids$site, ids$variable), collapse = " | "), "\n")
cat("Projects (Network.ID) in network.data:", length(ids$network), "| with site:", length(ids$site), "| sites:", nrow(P$site),
    "| unique references:", length(unique(P$publications$Publication.Reference)), "\n")

# 8) habitat class of each site (terrestrial / freshwater / marine; rules in the column "regra"), with harmonized IDs
cl <- read.csv("data/raw/site_habitat_class_raw.csv", fileEncoding = "UTF-8-BOM", check.names = FALSE, colClasses = "character")
cl <- cl[cl$linha_planilha %in% P$site$linha_planilha, ]; cl$Network.ID <- P$site$Network.ID[match(cl$linha_planilha, P$site$linha_planilha)]
write.csv(cl, "data/site_habitat_class.csv", row.names = FALSE)
cat("02 | site_habitat_class:", nrow(cl), "sites\n"); print(table(cl$classe))
