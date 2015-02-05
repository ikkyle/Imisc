Mode <- function(x) {
  ux <- unique(x[!is.na(x)])
  ux[which.max(tabulate(match(x, ux)))]
}

# sql must calculate the "diff" match criteria, can call them anything, just sort correctly
# target id must be called "main_id"
# matching id must be called "match_id"
# requires the seq.id function and Mode function
# seq.len indicates how many of each top matches to limit the random sampling to.
#  this can be adjusted depending on the number of good matches to improve the matches made. 
#  too high and there will be poorer matches made, too low and there won't be enough variation in the randomness
#  when seq.len == NULL (the default) the sequence length is computed based on the number of distinct top 
#  results ther are per main_id. This is usefull if there isn't a lot of variability in the matches 
#  ex: all top 3 matches have the same exact data, so the mean length of top 3 distinct matches are taken and 
#  that is used as the seq.len (3 is a constant used in the function, this may change)
# n.votes is the number of times to loop to get votes
# Add RANDOM() into the ORDER BY clause as the last value to randomize the order of subjects

### this one is the main function that finds votes ###
.make.votes2 <- function(sqlstr, n.votes, seq.len, df1,df0,name1,name0,...){
  
  match <- vector(mode='list', length=n.votes)
  match2 <- vector(mode='list', length=n.votes)
  match3 <- vector(mode='list', length=n.votes)
  match3.agg <- vector(mode='list', length=n.votes)
  data <- vector(mode='list', length=n.votes)
  sbjs <- vector(mode='list', length=n.votes)
  
  db <- dbConnect(SQLite(), dbname=':memory:')
  
  dbSendQuery(db, paste0('DROP TABLE IF EXISTS ', name1))
  dbSendQuery(db, paste0('DROP TABLE IF EXISTS ', name0))
  
  dbWriteTable(db, name1, df1, row.names=FALSE)
  try(dbWriteTable(db, name0, df0, row.names=FALSE), silent=TRUE)
  qry <- dbSendQuery(db, sqlstr)
  match0 <- fetch(qry)
  dbClearResult(qry)
  
  match0$seq0 <- seq.id(match0$main_id)  
  match0$seq1 <- seq.id(match0[,names(match0) != 'match_id'])
  match0 <- subset(match0, seq1 <= 3)
  match0 <- aggregate(seq0 ~ main_id,
                      data=match0, 
                      FUN=max)
  seq.len2 <- ifelse(is.null(seq.len), round(mean(match0$seq0),0), seq.len)
  
  pb <- txtProgressBar(min=0, max=n.votes, style=3)
  
  for(i in 1:n.votes){
    
    qry <- dbSendQuery(db, sqlstr)
    match[[i]] <- fetch(qry)
    dbClearResult(qry)
    
    sbjs[[i]] <- data.frame(unique(match[[i]]$main_id))  
    names(sbjs[[i]]) <- 'main_id'
    
    match[[i]] <- data.frame(cbind(match[[i]], seq.id(match[[i]]$main_id)))    
    names(match[[i]])[ncol(match[[i]])] <- 'sequence'
    
    match2[[i]] <- subset(match[[i]], match[[i]]$sequence <= seq.len2)
    
    match3[[i]] <- match2[[i]][sample(1:nrow(match2[[i]]), replace=TRUE),]
    
    match3[[i]] <- subset(match3[[i]], !duplicated(match3[[i]]$match_id))
    match3[[i]] <- merge(sbjs[[i]], match3[[i]], by='main_id', all.x=TRUE)
    
    match3.agg[[i]] <- aggregate(sequence ~ main_id, 
                                 data=match3[[i]], FUN=min)
    names(match3.agg[[i]])[2] <- 'seq2'
    
    match3[[i]] <- merge(match3[[i]], match3.agg[[i]], by='main_id', all.x=TRUE)
    
    data[[i]] <- subset(match3[[i]], sequence == seq2 | is.na(match_id))
    data[[i]] <- data[[i]][order(data[[i]]$main_id), c('main_id', 'match_id')]
    
    data[[i]] <- subset(data[[i]], !duplicated(main_id))
    
    names(data[[i]])[2] <- paste('match_id', i, sep='_')
    
    setTxtProgressBar(pb, i)
    
  }
  
#   dbSendQuery(db, paste0('DROP TABLE IF EXISTS ', name1))
#   dbSendQuery(db, paste0('DROP TABLE IF EXISTS ', name0))
   dbDisconnect(db)
  
  return(data)
}


### the outside fuction that saves out the data created by make.votes() ###
find.match <- function(sqlstr, n.votes=100, seq.len=NULL, df1, df0, ...){
  
  if(regexpr('ORDER BY', sqlstr, ignore.case=TRUE) < 0) stop('SQL statement does not contain ORDER BY clause')
  if(regexpr('RANDOM\\(\\)', sqlstr, ignore.case=TRUE) < 0) stop('SQL statement does not contain RANDOM() in the ORDER BY clause. Please add "RANDOM()" as the last ordering criteria.')
  
  name1 <- as.character(substitute(df1))
  name0 <- as.character(substitute(df0))
  
  data <- .make.votes2(sqlstr=sqlstr, n.votes=n.votes, 
                       seq.len=seq.len, df1=df1, df0=df0,
                       name1=name1, name0=name0)
  
  stopifnot(n.votes > 1)
  
  ids <- data[[1]]$main_id
  votes <- data[[1]][,2]
  
  for(j in (2:length(data))){
    votes <- cbind(votes, data[[j]][,2])
  }
  
  votes <- cbind(ids, votes)
  
  final.vote1 <- apply(votes[,2:ncol(votes)], 1, Mode)
  votes <- apply(votes, c(1,2), FUN=function(x){(ifelse(x %in% final.vote1, NA, x))})
  final.vote2 <- apply(votes[,2:ncol(votes)], 1, Mode)
  votes <- apply(votes, c(1,2), FUN=function(x){(ifelse(x %in% final.vote2, NA, x))})
  final.vote3 <- apply(votes[,2:ncol(votes)], 1, Mode)
  votes <- apply(votes, c(1,2), FUN=function(x){(ifelse(x %in% final.vote3, NA, x))})
  final.vote4 <- apply(votes[,2:ncol(votes)], 1, Mode)
  votes <- apply(votes, c(1,2), FUN=function(x){(ifelse(x %in% final.vote4, NA, x))})
  final.vote5 <- apply(votes[,2:ncol(votes)], 1, Mode)
  
  final.vote <- as.matrix(cbind(final.vote1, final.vote2, final.vote3, final.vote4, final.vote5))
  matches <- data.frame(unique(data.frame(cbind(votes[,1], final.vote))))
  
  names(matches) <- c('main_id', 'match_id1', 'match_id2', 'match_id3', 'match_id4', 'match_id5') 
  
  matches$final.match <- ifelse(duplicated(matches$match_id1), matches$match_id2, matches$match_id1)
  matches$final.match <- ifelse(duplicated(matches$final.match), matches$match_id3, matches$final.match)
  matches$final.match <- ifelse(duplicated(matches$final.match), matches$match_id4, matches$final.match)
  matches$final.match <- ifelse(duplicated(matches$final.match), matches$match_id5, matches$final.match)
  
  match.data <- list(match.pairs = matches[,c('main_id', 'final.match')], 
                     match.list = matches[,c('main_id', 'match_id1', 'match_id2', 
                                             'match_id3', 'match_id4', 'match_id5')])
  
  return(match.data)
}


