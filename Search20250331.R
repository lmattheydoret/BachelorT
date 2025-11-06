# tiabkey: search in title, abstract, and keywords.
tiabkey <- function(ris=NULL, searchWords=NULL) {
    idxLs <- list()
    for(j in searchWords) {
        idxTitle <- grepl(pattern=j, x=ris[,"title"], ignore.case = TRUE)
        idxAbstract <- grepl(pattern=j, x=ris[,"abstract"], ignore.case = TRUE)
        idxKeywords <- grepl(pattern=j, x=ris[,"keywords"], ignore.case = TRUE)
        idxj <- idxTitle | idxAbstract | idxKeywords
        # Lege index-vektor idxj in idxLs Liste ab
        idxLs[[j]] <- idxj
    }
    return(idxLs)
}

# tiab: search in title and abstract.
tiab <- function(ris=NULL, searchWords=NULL) {
    idxLs <- list()
    for(j in searchWords) {
        idxTitle <- grepl(pattern=j, x=ris[,"title"], ignore.case = TRUE)
        idxAbstract <- grepl(pattern=j, x=ris[,"abstract"], ignore.case = TRUE)
        idxj <- idxTitle | idxAbstract
        # Lege index-vektor idxj in idxLs Liste ab
        idxLs[[j]] <- idxj
    }
    return(idxLs)
}

# -------------------------------------------

# install.packages("synthesisr", dependencies = TRUE)
# Online help and examples:
# https://cran.r-project.org/web/packages/synthesisr/vignettes/synthesisr_vignette.html
# library(synthesisr)

# ----------------------------------------------
# Step 1: Read all of your downloaded RIS files and adjust your path
# ----------------------------------------------
# PATH <- "/Users/leoniematthey/Desktop/Recherche/"

FILES <- paste0(PATH, list.files(PATH))

imported_files <- synthesisr::read_refs(
    filename = FILES,
    return_df = TRUE)
# Quick check, number of rows and columns (use command dim(), = dimension)
dim(imported_files)

# Check: Rows of each RIS file = c(485, 255, 370)
sum(485, 255, 370)

# Want to see the column names?
colnames(imported_files)
# What do the column names mean? See here: https://en.wikipedia.org/wiki/RIS_(file_format)
# On this wiki page, scroll down to the table, where the first column is "Tag". Example: Y1 = year/date

# -------------------------------------------
# Step 2: Remove duplicates (use title, that is, if a title exists more than once in the dataset, remove all except for one entry).
# -------------------------------------------
imported_filesUnique <- imported_files[!duplicated(tolower(imported_files$title)),]
# How many rows exist after having removed duplicated titles?
dim(imported_filesUnique)
# How many rows have been removed?
nrow(imported_files) - nrow(imported_filesUnique)
rownames(imported_filesUnique) <- 1:nrow(imported_filesUnique)

imported_filesUnique$title

# -------------------------------------------
# Step 3: Narrowing down your search is 'different' now, because (almost) all titles include ADHD and women. So, for example, look for special topics, like prison, suicide, ...
# -------------------------------------------

# Select single words which you think MUST occur in the title and abstract.
# Search specific words (use custom function 'tiab') within the current search hits
idxList <- tiab(ris=imported_filesUnique, searchWords = c(
    "attention deficit hyperactivity disorder",
    "adhd", "diagnos", "women", "woman", "female"))
# Quick check: how many search hits of each specific word?
lapply(idxList, function(x) length(which(x)))

# Step 3a: Combine what you did in step 3, to get to narrower search results:
# Select all titles, which contain in title or abstract:
# attention deficit hyperactivity disorder OR adhd
# AND
# diagnos
# AND
# women OR woman OR female
idxConnect <- (idxList$`attention deficit hyperactivity disorder` | idxList$adhd) &
    idxList$diagnos &
    (idxList$women | idxList$woman | idxList$female)
# How many search hits does this yield:
length(which(idxConnect)) #  79 hits.

# -------------------------------------------
# Step 4: Copy paste the relevant columns to excel, so that you can easily read the titles, maybe also check out the abstract.
# -------------------------------------------

# Select certain columns only:
cbind(colnames(imported_filesUnique))
# Select from the 52 column names: database, publication_type, title, journal, keywords, doi, A1, Y1, author, year, abstract
selectColnames <- colnames(imported_filesUnique)[c(1, 3, 5, 6, 11, 12, 16, 21, 31, 37, 40)]

# all 447 unique search hits:
excelUnordered447 <- imported_filesUnique[,selectColnames]
# 79 search hits from narrowed down search
excelUnordered79 <- imported_filesUnique[idxConnect,selectColnames]

# Own function, with which to quickly merge Y1 and year into one column, then order that column from newest to oldes date (decreasing order)
orderYear <- function(excel=NULL) {
    year.y1 <- gsub("\\//", "", excel$Y1)
    year.year <- excel$year
    yearMat <- matrix(c(year.y1, year.year), ncol=2)
    yearAllMerged <- apply(yearMat, 1, function(x) {
        if(all(is.na(x))) {
            NA
        } else {
            x[!is.na(x)]
        }
    })
    yearAll <- as.numeric(unlist(yearAllMerged))
    excel$YEAR <- yearAll
    return(excel[order(excel$YEAR, decreasing = TRUE),])
}

excel <- orderYear(excel=excelUnordered447)
excel <- orderYear(excel=excelUnordered79)

# For Mac (copy to clipboard, then paste in excel)
clip <- pipe("pbcopy", "w")
write.table(excel, sep="\t", dec=".", col.names=TRUE, row.names=FALSE, file = clip)
close(clip)
