# 02. Clean and harmonize the spreadsheets -> data/*.csv (the tables used in every analysis)
# Inputs : data/raw/*_raw.csv (compiled tables as received; personal e-mails, links to private files and the
#          lifespan-source column were removed before publication) and data/intermediate/site_data_coordinates_fixed.csv (script 01)
# Outputs: data/*.csv, logs/LOG_cleaning_spreadsheets.csv, logs/summary_spreadsheets.csv
# Steps (all logged): trim spaces; fix shifted cells in variable.data; harmonize Network.ID across tables (reference:
# network.data); remove e-mails; remove identical duplicated rows; fill missing taxonomy from other records of the same species.
# (Comments below are in Portuguese, as in the original working script.)
R <- "data/raw/"
dir.create("data", showWarnings = FALSE); dir.create("logs", showWarnings = FALSE)
ler <- function(f) read.csv(f, check.names = FALSE, colClasses = "character", na.strings = character(0))
log <- data.frame()
anota <- function(planilha, acao, n, detalhe = "") log <<- rbind(log, data.frame(planilha, acao, n, detalhe))

P <- list(network        = ler(paste0(R, "network_data_raw.csv")),
          site           = ler("data/intermediate/site_data_coordinates_fixed.csv"),
          variable       = ler(paste0(R, "variable_data_raw.csv")),
          site_variables = ler(paste0(R, "site_variables_data_raw.csv")),
          authors        = ler(paste0(R, "authors_data_raw.csv")),
          publications   = ler(paste0(R, "publications_data_raw.csv")),
          groups_time    = ler(paste0(R, "groups_time_raw.csv")))
n0 <- sapply(P, nrow)

# 1) espaços nas pontas e espaços duplos nos nomes das colunas
for (k in names(P)) {
  x <- P[[k]]; antes <- sum(sapply(x, function(c) sum(grepl("^\\s|\\s$", c))))
  x[] <- lapply(x, trimws); names(x) <- gsub("\\. ", ".", trimws(names(x)))
  P[[k]] <- x; anota(k, "espaços nas pontas removidos (células)", antes)
}

# 2) variable.data: colunas sem nome e linhas deslocadas
v <- P$variable; names(v)[7] <- "Variable.Record.Method"            # nome que a coluna tem na versão do GitHub
ex <- which(names(v) == "")                                          # colunas 12-14: sobra de linhas deslocadas
r1 <- which(grepl("^http", v[[ex[1]]]))                                # MarineGEO, PPBioCS: a 2ª referência está em URL e o link na coluna seguinte
v$Variable.Protocol.Reference[r1] <- paste(v$Variable.Protocol.Reference[r1], v$Variable.Protocol.URL[r1], sep = " | ")
v$Variable.Protocol.URL[r1] <- v[[ex[1]]][r1]
anota("variable", "referência/URL do protocolo trazidas das colunas sem nome (linhas deslocadas)", length(r1), paste(unique(v$Network.ID[r1]), collapse = ", "))
r2 <- which(v[[ex[3]]] != "" & v$Variable.Protocol.Reference == "")   # BDCTamar Turtle.N: referência 2 colunas à direita
v$Variable.Protocol.Reference[r2] <- v[[ex[3]]][r2]; v$Variable.Protocol.URL[r2] <- "na"
anota("variable", "referência do protocolo trazida das colunas sem nome", length(r2), paste(v$Network.ID[r2], v$Variable.ID[r2]))
v <- v[, -ex]
# PPBio Amp.rept: répteis/anfíbios marcados como abióticos, com a métrica deslocada para Group_especific
r3 <- which(v$Network.ID == "PPBio" & v$Variable.ID == "Amp.rept" & v$Variable.Type == "abiotic" & v$Metrics == "")
v$Variable.Type[r3] <- "biotic"; v$Metrics[r3] <- v$Group_especific[r3]; v$Group_especific[r3] <- "na"
anota("variable", "PPBio Amp.rept: abiotic -> biotic; métrica 'abundance' voltou para Metrics", length(r3))
r4 <- which(v$Network.ID == "BDCTamar" & v$Variable.ID == "Turtle.Interac" & v$Group_especific == "woody plants")
v$Group_especific[r4] <- "marine turtles"; anota("variable", "BDCTamar Turtle.Interac: Group_especific 'woody plants' -> 'marine turtles'", length(r4))
r5 <- which(v$Network.ID == "PPBioCS" & v$Group_especific %in% c("soil variables", "water variables"))
v$Group_especific[r5] <- "na"; anota("variable", "PPBioCS: Group_especific com o valor de Metrics -> 'na'", length(r5))
P$variable <- v

# publications: coluna sem nome com 1 observação -> Publication.OBS
p <- P$publications; names(p)[names(p) == ""] <- "Publication.OBS"; P$publications <- p

# site: coluna sem nome = links para planilhas internas do Google Drive (já removida dos dados públicos em data/raw)
s <- P$site; vaz <- which(names(s) == "")
if (length(vaz)) { anota("site", "coluna sem nome removida (links para planilhas internas)", sum(s[[vaz]] != "")); P$site <- s[, -vaz] }

# 3) Network.ID unificados (referência = network.data)
mapa <- c("PPbioAN ou PBPaAg" = "PBPaAg",   # " PBPaAg" (com espaço) já foi corrigido no passo 1
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
n$Network.ID[i] <- "PPRebioUnião"; anota("network", "Network.ID 'PPRebioUnião (observação...)' -> 'PPRebioUnião' (observação movida para Network.OBS)", sum(i))
P$network <- n
for (k in names(P)) {
  id <- P[[k]]$Network.ID; j <- id %in% names(mapa)
  for (de in unique(id[j])) anota(k, "Network.ID unificado", sum(id == de), paste(de, "->", mapa[[de]]))
  P[[k]]$Network.ID[j] <- mapa[id[j]]
}
# authors: uma linha com dois projetos -> duas linhas
a <- P$authors; j <- which(a$Network.ID == "POPA APAS and POPA FNT")
if (length(j)) { b <- a[j, ]; a$Network.ID[j] <- "POPA APAS"; b$Network.ID <- "POPA FNT"; a <- rbind(a, b)
  anota("authors", "'POPA APAS and POPA FNT' separado em duas linhas (POPA APAS e POPA FNT)", length(j)) }
P$authors <- a

# 4) e-mails
tira <- list(network = c("Network.Email", "Network.Admin.Email"), site = "Site.Contact.Email", authors = "Author.Email")
for (k in names(tira)) { t <- intersect(tira[[k]], names(P[[k]])); if (length(t)) { P[[k]] <- P[[k]][, setdiff(names(P[[k]]), t)]; anota(k, "coluna de e-mail removida", length(t), paste(t, collapse = ", ")) } }
re <- "[[:alnum:]._%+-]+@[[:alnum:].-]+\\.[[:alpha:]]{2,}"
for (k in names(P)) for (c in names(P[[k]])) { j <- grepl(re, P[[k]][[c]])
  if (any(j)) { P[[k]][[c]][j] <- gsub(re, "[e-mail removido]", P[[k]][[c]][j]); anota(k, "e-mail dentro do texto substituído", sum(j), c) } }

# 5) groups_time: sem a coluna de fontes da longevidade
g <- P$groups_time
if ("source.time" %in% names(g)) anota("groups_time", "coluna source.time (fontes da longevidade) removida", 1)
P$groups_time <- g[, names(g) != "source.time"]
# taxonomia "na" preenchida quando a mesma espécie tem a taxonomia completa em outro registro
g <- P$groups_time; tx <- c("kingdom", "phylum", "class", "order", "family", "genus")
comp <- g[g$phylum != "na", c("species", tx)]; comp <- comp[!duplicated(comp$species), ]
j <- which(g$phylum == "na" & g$species %in% comp$species)
g[j, tx] <- comp[match(g$species[j], comp$species), tx]
anota("groups_time", "taxonomia 'na' preenchida com a de outro registro da mesma espécie", length(j), paste(unique(g$species[j]), collapse = ", "))
P$groups_time <- g

# 6) linhas repetidas (idênticas em todas as colunas, depois das correções acima)
for (k in names(P)) { d <- duplicated(P[[k]]); if (any(d)) {
  t <- sort(table(P[[k]]$Network.ID[d]), decreasing = TRUE)
  anota(k, "linhas repetidas removidas", sum(d), paste(names(t), t, sep = ": ", collapse = "; ")); P[[k]] <- P[[k]][!d, ] } }

# 7) conferência final e gravação
stopifnot(!any(grepl("utm_source", tolower(unlist(P)))))   # nenhum link com parâmetros de rastreamento
nomes <- c(network = "network_data", site = "site_data", variable = "variable_data", site_variables = "site_variables_data",
           authors = "authors_data", publications = "publications_data", groups_time = "groups_time")
for (k in names(P)) write.csv(P[[k]], file.path("data", paste0(nomes[[k]], ".csv")), row.names = FALSE, fileEncoding = "UTF-8")
resumo <- data.frame(planilha = names(P), linhas_antes = n0, linhas_depois = sapply(P, nrow), colunas = sapply(P, ncol))
write.csv(log, "logs/LOG_cleaning_spreadsheets.csv", row.names = FALSE); write.csv(resumo, "logs/summary_spreadsheets.csv", row.names = FALSE)
print(resumo, row.names = FALSE)
ids <- lapply(P, function(x) unique(x$Network.ID))
cat("\nNetwork.ID que ainda não estão na network.data:\n")
for (k in setdiff(names(ids), "network")) { f <- setdiff(ids[[k]], ids$network); if (length(f)) cat(" ", k, ":", paste(f, collapse = " | "), "\n") }
vb <- P$variable[P$variable$Variable.Type == "biotic", ]
cat("Projetos com variável biótica e sem site:", paste(setdiff(unique(vb$Network.ID), ids$site), collapse = " | "), "\n")
cat("Projetos com site e sem variável:", paste(setdiff(ids$site, ids$variable), collapse = " | "), "\n")
cat("Projetos (Network.ID) na network.data:", length(ids$network), "| com site:", length(ids$site), "| sites:", nrow(P$site),
    "| referências únicas:", length(unique(P$publications$Publication.Reference)), "\n")

# 8) classe de habitat de cada site (terrestrial / freshwater / marine; regras na coluna "regra"), com os IDs harmonizados
cl <- read.csv("data/raw/site_habitat_class_raw.csv", fileEncoding = "UTF-8-BOM", check.names = FALSE, colClasses = "character")
cl <- cl[cl$linha_planilha %in% P$site$linha_planilha, ]; cl$Network.ID <- P$site$Network.ID[match(cl$linha_planilha, P$site$linha_planilha)]
write.csv(cl, "data/site_habitat_class.csv", row.names = FALSE)
cat("02 | site_habitat_class:", nrow(cl), "sites\n"); print(table(cl$classe))
