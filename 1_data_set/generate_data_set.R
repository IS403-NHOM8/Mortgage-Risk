suppressPackageStartupMessages(library(data.table))
ROOT <- 'C:/Users/24521/Downloads/Mortgage-Risk/1_data_set'
RAW <- file.path(ROOT, 'data', 'raw', 'freddie_mac')
OUT <- file.path(ROOT, 'data', 'processed', 'freddie_mac')
dir.create(OUT, recursive=TRUE, showWarnings=FALSE)
VINTAGES <- 2010:2022
SNAPSHOTS <- 2011:2022
CHUNK_SIZE <- 25000L      
PERF_CUTOFF <- 202301L
WRITE_MASTER <- TRUE      
CENSORING_POLICY <- 'exclude' 
DATA_MODE <- 'PAPER_REPLICATION' 
if(DATA_MODE != 'PAPER_REPLICATION') stop('Only PAPER_REPLICATION is implemented')

O_NAMES <- c('credit_score','date_first_payment','first_time_homebuyer',
             'date_maturity','MSA','insurance_percent','nr_units','occupancy',
             'original_combined_loan_to_value','original_dept_to_income','original_upb',
             'original_loan_to_value','original_interest_rate','channel','prepayment_penalty',
             'amortization_type','property_state','property_type','postal_code','loan_number',
             'loan_purpose','loan_term','number_of_borrowers','seller_name','super_conforming',
             'prerelief_loan_number','program_indicator','relief_refinance',
             'valuation_method','interest_only','vantagescore4')
P_NAMES <- c('loan_number','period','current_upb','current_loan_delinquency_status',
             'loan_age','remaining_months','date_defect_settlement','modified',
             'zero_balance_code','date_zero_balance','current_interest_rate',
             'current_deferred_UPB','DDLPI','mi_recoveries','net_sale_proceeds',
             'non_mi_recoveries','expenses','legal_costs','maintenance_costs',
             'taxes_and_insurance','miscellaneous_expenses','actual_loss',
             'modification_cost','step_modification','deferred_payment_plan',
             'estimated_loan_to_value','zero_balance_removal_UPB','delinquent_interest',
             'delinquent_desaster','assistance_code','month_modification_cost',
             'interest_bearing_UPB','mi_cancellation','servicer_name','bankruptcy_cramdown_costs')
stopifnot(length(O_NAMES)==31L, length(P_NAMES)==35L)

fail <- function(...) stop(..., call.=FALSE)
as_num <- function(x) suppressWarnings(as.numeric(x))
clean_num <- function(x, missing=character()) {
  x <- trimws(as.character(x)); x[x %in% c('',missing)] <- NA_character_
  as_num(x)
}
month_date <- function(x, label='date') {
  x <- trimws(as.character(x)); x[x==''] <- NA_character_
  malformed <- !is.na(x) & !grepl('^[0-9]{6}([0-9]{2})?$',x)
  if(any(malformed)) fail('Malformed ',label,': ',paste(head(unique(x[malformed]),3),collapse=', '))
  eight <- !is.na(x) & nchar(x)==8L
  if(any(eight)) {
    full <- as.Date(x[eight], '%Y%m%d')
    if(anyNA(full) || any(format(full,'%Y%m%d')!=x[eight])) fail('Invalid YYYYMMDD in ',label)
  }
  six <- substr(x,1L,6L)
  out <- as.Date(paste0(six,'01'),'%Y%m%d')
  if(any(!is.na(x) & is.na(out))) fail('Invalid YYYYMM in ',label)
  out
}
month_index <- function(d) as.integer(format(d,'%Y'))*12L + as.integer(format(d,'%m'))
month_shift <- function(d, offset) {
  k <- month_index(d)+offset
  as.Date(sprintf('%04d-%02d-01',(k-1L)%/%12L,(k-1L)%%12L+1L))
}
parse_lines <- function(lines, positions, names_vec, description) {
  widths <- lengths(regmatches(lines, gregexpr('|',lines,fixed=TRUE)))+1L
  bad <- which(widths!=length(names_vec))
  if(length(bad)) fail('Schema mismatch ',description,': expected ',length(names_vec),
                       ', observed ',widths[bad[1L]])
  dt <- fread(text=paste(lines,collapse='\n'), sep='|', header=FALSE,
              select=positions, colClasses='character', na.strings=NULL,
              showProgress=FALSE, quote='')
  if(nrow(dt)!=length(lines)) fail('Row count mismatch reading ',description)
  setnames(dt,names_vec[positions])
  for(j in names(dt)) set(dt,j=j,value=trimws(dt[[j]]))
  dt
}
zip_member <- function(zip_file, kind, vintage) {
  listing <- unzip(zip_file,list=TRUE)$Name
  regex <- if(kind=='orig') sprintf('^sample_orig_%d\\.txt$',vintage) else
    sprintf('^sample_(perf|svcg)_%d\\.txt$',vintage)
  hit <- listing[grepl(regex,basename(listing),ignore.case=TRUE)]
  if(length(hit)!=1L || any(grepl('(^/|\\.\\./|\\\\)',hit)))
    fail('Invalid or missing ',kind,' member in ',basename(zip_file))
  hit
}
read_orig <- function(zip_file, member, cols) {
  con <- unz(zip_file,member,open='r'); on.exit(close(con),add=TRUE)
  ll <- readLines(con,warn=FALSE)
  parse_lines(ll,cols,O_NAMES,basename(zip_file))
}

zipc <- fread(file.path(RAW,'ZIP_centroids.csv'))
if(!all(c('ZIP3','X_centroid','Y_centroid') %in% names(zipc))) fail('Bad ZIP_centroids schema')
zipc[, ZIP3:=sprintf('%03d',as.integer(ZIP3))]
zipc[, `:=`(X_centroid=as_num(X_centroid),Y_centroid=as_num(Y_centroid))]
zipc <- unique(zipc[is.finite(X_centroid)&is.finite(Y_centroid)&
                      X_centroid > -125 & X_centroid < -66 &
                      Y_centroid > 24 & Y_centroid < 50 & ZIP3!='001',
                    .(ZIP3,X_centroid,Y_centroid)])
if(anyDuplicated(zipc$ZIP3)) fail('Conflicting ZIP3 coordinates')

fred <- fread(file.path(RAW,'MORTGAGE30US.csv'))
if('observation_date' %in% names(fred) && !'DATE' %in% names(fred))
  setnames(fred,'observation_date','DATE')
if(!all(c('DATE','MORTGAGE30US') %in% names(fred))) fail('Bad FRED schema')
fred[,`:=`(DATE=as.Date(DATE),MORTGAGE30US=as_num(MORTGAGE30US))]
fred <- fred[!is.na(DATE)&is.finite(MORTGAGE30US)][order(DATE)]
if(!nrow(fred)||anyDuplicated(fred$DATE)) fail('Invalid or duplicate FRED dates')

macro <- fread(file.path(RAW,'macro_data.csv'),sep=';')
if(!all(c('state_long','state_short','year') %in% names(macro))) fail('Bad macro_data schema')
macro_vars <- setdiff(names(macro),c('state_long','state_short','year'))
if(length(macro_vars)!=9L) warning('Expected nine original macro columns; got ',length(macro_vars))
macro[,`:=`(year=as.integer(year),state_short=trimws(as.character(state_short)))]
if(anyDuplicated(macro[,.(year,state_short)])) fail('Duplicate macro state-year')
for(col in macro_vars) set(macro,j=col,value=as_num(macro[[col]]))

by_snapshot <- setNames(vector('list',length(SNAPSHOTS)),as.character(SNAPSHOTS))
snapshot_audit <- list(); exceptions <- list(); exit_reason_audit <- list(); seen_ids <- character()
for(v in VINTAGES) {
  message('vintage ',v)
  zp <- file.path(RAW,sprintf('sample_%d.zip',v))
  if(!file.exists(zp)) fail('Missing ZIP ',zp)
  orig_member <- zip_member(zp,'orig',v)
  perf_member <- zip_member(zp,'perf',v)
  if(v==2022L) { 
    message('  vintage 2022: verified members; no loans eligible for Jan 2022')
    next
  }
  ocols <- c(1:13,16:23)
  o <- read_orig(zp,orig_member,ocols)
  setnames(o,'loan_number','loan_id')
  if(anyDuplicated(o$loan_id)||any(o$loan_id %in% seen_ids))
    fail('Duplicate origination loan ID at vintage ',v)
  seen_ids <- c(seen_ids,o$loan_id)
  o <- o[property_type=='SF' & amortization_type=='FRM' & as_num(loan_term)==360]
  o[,ZIP3:=substr(postal_code,1L,3L)]
  o <- o[grepl('^[0-9]{3}$',ZIP3)]
  o <- merge(o,zipc,by='ZIP3',all=FALSE,sort=FALSE)
  if(!nrow(o)) next
  
  annual_chunks <- list(); event_chunks <- list(); zero_chunks <- list()
  ra_chunks <- list(); early_chunks <- list(); chunk_number <- 0L
  month_state <- data.table(loan_id=character(),yr=integer(),month_mask=integer())
  setkey(month_state,loan_id,yr)
  pc <- c(1:5,8:11)
  con <- unz(zp,perf_member,open='r')
  processed <- 0L
  repeat {
    lines <- readLines(con,n=CHUNK_SIZE,warn=FALSE)
    if(!length(lines)) break
    processed <- processed+length(lines); chunk_number <- chunk_number+1L
    p <- parse_lines(lines,pc,P_NAMES,sprintf('%s chunk %d',basename(zp),processed))
    rm(lines)
    setnames(p,'loan_number','loan_id')
    p <- p[loan_id %in% o$loan_id]
    if(!nrow(p)) next
    p[, period_date:=month_date(period,'performance period')]
    if(anyNA(p$period_date) ||
       any(format(p$period_date, '%Y%m') != p$period))
      fail('Invalid performance period in vintage ',v)
    p <- p[as.integer(format(period_date,'%Y%m')) < PERF_CUTOFF]
    if(!nrow(p)) next
    if(anyDuplicated(p,by=c('loan_id','period')))
      fail('Duplicate loan-month within chunk in vintage ',v)
    p[,`:=`(yr=as.integer(format(period_date,'%Y')),
            mo=as.integer(format(period_date,'%m')))]
    incoming <- p[, .(month_mask=Reduce(bitwOr, bitwShiftL(1L,unique(mo)-1L))),
                  by=.(loan_id,yr)]
    prior <- month_state[incoming,on=.(loan_id,yr),x.month_mask]
    prior[is.na(prior)] <- 0L
    if(any(bitwAnd(prior,incoming$month_mask)!=0L))
      fail('Duplicate loan-month across chunks in vintage ',v)
    fresh_keys <- incoming[!month_state,on=.(loan_id,yr)]
    month_state[incoming,on=.(loan_id,yr),
                month_mask:=bitwOr(month_mask,i.month_mask)]
    if(nrow(fresh_keys)) {
      month_state <- rbindlist(list(month_state,fresh_keys),use.names=TRUE)
      setkey(month_state,loan_id,yr)
    }
    rm(incoming,prior,fresh_keys)
    status <- p$current_loan_delinquency_status
    recognized <- is.na(status) | status %in% c('','RA','XX') | grepl('^[0-9]{1,2}$',status)
    if(any(!recognized)) fail('Unknown delinquency code vintage ',v,': ',
                              paste(head(unique(status[!recognized]),5),collapse=', '))
    number_status <- suppressWarnings(as.integer(status))
    if(any(!is.na(number_status) & !(number_status %in% 0:99)))
      fail('Out of range delinquency status vintage ',v)
    early_chunks[[length(early_chunks)+1L]] <- unique(p[
      as_num(loan_age) %in% 0:3 & modified=='',.(loan_id)])
    dc <- p[!is.na(number_status) & number_status>=3L,
            .(date_default=min(period_date)),by=loan_id]
    if(nrow(dc)) event_chunks[[length(event_chunks)+1L]] <- dc
    zz <- month_date(p$date_zero_balance,'zero-balance date')
    if(any(!is.na(zz))) {
      p[, zero_date:=zz]
      zero_chunks[[length(zero_chunks)+1L]] <- p[!is.na(zero_date),
                                                 .(date_zero_balance=min(zero_date),
                                                   zero_balance_code=as.character(zero_balance_code[which.min(zero_date)])),by=loan_id]
    }
    if(any(status=='RA',na.rm=TRUE)) ra_chunks[[length(ra_chunks)+1L]] <-
      p[current_loan_delinquency_status=='RA',.(ra_date=min(period_date)),by=loan_id]
    p[,`:=`(current_upb=clean_num(current_upb),
            current_interest_rate=clean_num(current_interest_rate))]
    annual_chunks[[length(annual_chunks)+1L]] <- p[order(period_date),
                                                   .SD[.N],by=.(loan_id,yr),.SDcols=c('period_date','current_upb','current_interest_rate')]
    if(chunk_number %% 32L == 0L) {
      compact <- rbindlist(annual_chunks)
      setorder(compact,loan_id,yr,period_date)
      annual_chunks <- list(compact[, .SD[.N],by=.(loan_id,yr)])
      rm(compact)
    }
    rm(p,dc,zz)
  }
  close(con)
  if(!length(annual_chunks)) fail('No servicing rows vintage ',v)
  if(anyDuplicated(month_state,by=c('loan_id','yr')))
    fail('Duplicate month-state key vintage ',v)
  gc(verbose=FALSE)
  reduce_min <- function(ll, field) {
    if(!length(ll)) {
      ans <- data.table(loan_id=character(), date_value=as.Date(character()))
      setnames(ans,'date_value',field); return(ans)
    }
    ans <- rbindlist(ll)[, .(value=min(get(field))),by=loan_id]
    setnames(ans,'value',field); ans
  }
  empty_test <- reduce_min(list(),'date_default')
  stopifnot(identical(names(empty_test),c('loan_id','date_default')),
            inherits(empty_test$date_default,'Date'),nrow(empty_test)==0L)
  rm(empty_test)
  early <- if(length(early_chunks)) unique(rbindlist(early_chunks)$loan_id) else character()
  d <- reduce_min(event_chunks,'date_default')
  if(length(zero_chunks)) {
    z <- rbindlist(zero_chunks)
    setorder(z,loan_id,date_zero_balance)
    z <- z[, .SD[1L],by=loan_id]
  } else {
    z <- reduce_min(list(),'date_zero_balance')
    z[,zero_balance_code:=character()]
  }
  ra <- reduce_min(ra_chunks,'ra_date')
  a_year <- rbindlist(annual_chunks)
  setorder(a_year,loan_id,yr,period_date)
  a_year <- a_year[, .SD[.N],by=.(loan_id,yr)]
  if(anyDuplicated(a_year,by=c('loan_id','yr'))) fail('Duplicate annual state vintage ',v)
  g <- copy(month_state)
  g[,`:=`(n_months_observed=vapply(month_mask,function(mask)
    sum(bitwAnd(mask,bitwShiftL(1L,0:11))!=0L),integer(1L)),
    max_reporting_gap=vapply(month_mask,function(mask) {
      mm <- which(bitwAnd(mask,bitwShiftL(1L,0:11))!=0L)
      if(length(mm)<2L) return(0L)
      as.integer(max(diff(mm)-1L))
    },integer(1L)))]
  g[,month_mask:=NULL]
  setorder(a_year,loan_id,yr)
  rm(annual_chunks,event_chunks,zero_chunks,ra_chunks,early_chunks,month_state)
  gc(verbose=FALSE)
  
  o <- o[loan_id %in% early]
  if(!nrow(o)) {rm(o,a_year,d,z,ra,g);gc(verbose=FALSE);next}
  for(event_table in list(d,z,ra)) {
    if(anyDuplicated(event_table$loan_id)) fail('Duplicate loan IDs in event table vintage ',v)
    n_before_merge <- nrow(o)
    o <- merge(o,event_table,by='loan_id',all.x=TRUE,sort=FALSE)
    if(nrow(o)!=n_before_merge)
      fail('Row multiplication while merging loan events vintage ',v)
  }
  if(!('ra_date' %in% names(o)) || !inherits(o$ra_date,'Date'))
    fail('Missing or invalid ra_date after RA merge vintage ',v)
  if(!('date_default' %in% names(o)) || !inherits(o$date_default,'Date') ||
     !('date_zero_balance' %in% names(o)) ||
     !inherits(o$date_zero_balance,'Date'))
    fail('Invalid default / zero-balance event columns vintage ',v)
  o[,`:=`(date_first_payment=month_date(date_first_payment,'first payment'),
          date_maturity=month_date(date_maturity,'maturity'))]
  if(!inherits(o$date_maturity,'Date')) fail('Invalid normalized maturity vintage ',v)
  o[,zero_balance_code:=trimws(as.character(zero_balance_code))]
  o[is.na(zero_balance_code)|zero_balance_code=='',zero_balance_code:=NA_character_]
  o[,exit_reason:=fifelse(!is.na(date_zero_balance),
                          fifelse(is.na(zero_balance_code),'ZERO_BALANCE_CODE_MISSING',
                                  paste0('ZERO_BALANCE_CODE_',zero_balance_code)),
                          fifelse(!is.na(date_maturity),'SCHEDULED_MATURITY_ONLY',
                                  'NO_RECORDED_ZERO_OR_MATURITY'))]
  o[,ra_class:='NO_RA']
  o[!is.na(ra_date),ra_class:='RA_WITHOUT_RECORDED_ZERO']
  o[!is.na(ra_date)&!is.na(date_zero_balance)&date_zero_balance<ra_date,
    ra_class:='ZERO_BEFORE_RA']
  o[!is.na(ra_date)&!is.na(date_zero_balance)&date_zero_balance>ra_date,
    ra_class:='RA_BEFORE_ZERO']
  o[!is.na(ra_date)&!is.na(date_zero_balance)&date_zero_balance==ra_date,
    ra_class:='RA_ZERO_SAME_MONTH']
  o[!is.na(ra_date)&!is.na(date_default)&date_default>ra_date,
    ra_class:='RA_BEFORE_DEFAULT']
  o[!is.na(ra_date)&!is.na(date_default)&date_default==ra_date,
    ra_class:='DEFAULT_RA_SAME_MONTH']
  o[!is.na(ra_date)&!is.na(date_default)&date_default<ra_date,
    ra_class:='DEFAULT_BEFORE_RA']
  o[,event_order:='NO_DEFAULT_ZERO_PAIR']
  o[!is.na(date_default)&!is.na(date_zero_balance)&
      date_default<date_zero_balance,event_order:='DEFAULT_BEFORE_ZERO']
  o[!is.na(date_default)&!is.na(date_zero_balance)&
      date_default>date_zero_balance,event_order:='ZERO_BEFORE_DEFAULT']
  o[!is.na(date_default)&!is.na(date_zero_balance)&
      date_default==date_zero_balance,event_order:='DEFAULT_ZERO_SAME_MONTH']
  flagged <- o[ra_class!='NO_RA' &
                 (is.na(date_default)|ra_date<=date_default|
                    ra_class %in% c('ZERO_BEFORE_RA','RA_ZERO_SAME_MONTH'))]
  if(nrow(flagged)) exceptions[[length(exceptions)+1L]] <- flagged[,
                                                                   .(vintage=v,snapshot=NA_integer_,loan_id,issue='RA_REVIEW',
                                                                     detail=ra_class,event_date=ra_date,zero_balance_code,exit_reason,
                                                                     date_zero_balance,date_default,event_order,
                                                                     raw_default=NA_integer_,label_status=NA_character_,
                                                                     exclusion_reason=NA_character_)]
  zero_summary <- o[!is.na(date_zero_balance),
                    .(loans=.N,with_RA=sum(!is.na(ra_date)),
                      zero_before_default=sum(event_order=='ZERO_BEFORE_DEFAULT'),
                      same_month_default=sum(event_order=='DEFAULT_ZERO_SAME_MONTH')),
                    by=.(zero_balance_code,exit_reason)]
  if(nrow(zero_summary)) {
    zero_summary[,vintage:=v]
    exit_reason_audit[[length(exit_reason_audit)+1L]] <- zero_summary
  }
  o[,loan_start:=month_shift(date_first_payment,-1L)]
  ends <- pmin(as_num(o$date_default),as_num(o$date_zero_balance),
               as_num(o$date_maturity),na.rm=TRUE)
  ends[!is.finite(ends)] <- NA_real_
  o[,loan_end:=as.Date(ends,origin='1970-01-01')]
  o[, original_value:=100*clean_num(original_upb)/clean_num(original_loan_to_value,'999')]
  for(col in c('credit_score','insurance_percent','original_dept_to_income',
               'original_combined_loan_to_value','original_upb','original_loan_to_value',
               'original_interest_rate','nr_units','number_of_borrowers')) {
    sentinel <- switch(col,credit_score='9999',insurance_percent='999',
                       original_dept_to_income='999',original_combined_loan_to_value='999',
                       original_loan_to_value='999',nr_units='99',number_of_borrowers='99',character())
    set(o,j=col,value=clean_num(o[[col]],sentinel))
  }
  o[!is.finite(original_value)|original_value<=0,original_value:=NA_real_]
  o[,first_time_homebuyer:=fifelse(first_time_homebuyer=='Y','1',
                                   fifelse(first_time_homebuyer=='N','0',NA_character_))]
  o[,MSA:=fifelse(is.na(MSA)|MSA=='','0','1')]
  o[!occupancy %in% c('P','S','I') | is.na(occupancy),occupancy:=NA_character_]
  o[!loan_purpose %in% c('C','N','P') | is.na(loan_purpose),loan_purpose:=NA_character_]
  o[!nr_units %in% 1:4,nr_units:=NA_real_]
  o[number_of_borrowers>=2,number_of_borrowers:=2]
  if(any(o$number_of_borrowers<1,na.rm=TRUE)) fail('Invalid borrower count vintage ',v)
  for(y in SNAPSHOTS[SNAPSHOTS>v]) {
    snapshot <- as.Date(sprintf('%04d-01-01',y))
    horizon <- as.Date(sprintf('%04d-01-01',y+1L))
    a <- o[!is.na(loan_start)&loan_start<snapshot &
             (is.na(loan_end)|loan_end>=snapshot)]
    n_active <- nrow(a)
    if(!n_active) next
    last <- a_year[yr<y, .SD[.N],by=loan_id]
    n_before_merge <- nrow(a)
    a <- merge(a,last[,.(loan_id,latest_date=period_date,current_upb,
                         current_interest_rate)],by='loan_id',all.x=TRUE,sort=FALSE)
    if(nrow(a)!=n_before_merge) fail('Row multiplication in latest servicing join ',y)
    fallback <- is.na(a$latest_date)
    a[fallback,`:=`(latest_date=loan_start,current_upb=original_upb,
                    current_interest_rate=original_interest_rate)]
    fresh <- !is.na(a$latest_date)&a$latest_date>=month_shift(snapshot,-6L)
    n_stale <- sum(!fresh)
    a <- a[fresh]
    if(!nrow(a)) next
    a[,raw_default:=as.integer(!is.na(date_default)&date_default>=snapshot&
                                 date_default<horizon)]
    a[,default:=raw_default]
    followup <- a_year[yr==y,.(loan_id,last_followup=period_date)]
    n_before_merge <- nrow(a)
    a <- merge(a,followup,by='loan_id',all.x=TRUE,sort=FALSE)
    if(nrow(a)!=n_before_merge) fail('Row multiplication in followup join ',y)
    qa <- g[yr==y,.(loan_id,n_months_observed,max_reporting_gap)]
    a <- merge(a,qa,by='loan_id',all.x=TRUE,sort=FALSE)
    if(nrow(a)!=n_before_merge) fail('Row multiplication in gap audit join ',y)
    december <- as.Date(sprintf('%d-12-01',y))
    exit <- (!is.na(a$date_zero_balance)&a$date_zero_balance>=snapshot&
               a$date_zero_balance<horizon) |
      (!is.na(a$date_maturity)&a$date_maturity>=snapshot&
         a$date_maturity<horizon)
    a[,`:=`(label_status='uncertain_negative',exclusion_reason=NA_character_)]
    a[raw_default==1L,label_status:='positive']
    a[raw_default==0L & exit,label_status:='negative_after_exit']
    a[raw_default==0L & !exit & !is.na(last_followup) &
        last_followup>=december,label_status:='negative_observed']
    same_month <- a$event_order=='DEFAULT_ZERO_SAME_MONTH' &
      !is.na(a$date_default) & a$date_default>=snapshot & a$date_default<horizon
    a[same_month,label_status:='ambiguous_same_month']
    contradictory <- a$event_order=='ZERO_BEFORE_DEFAULT' &
      ((!is.na(a$date_default)&a$date_default>=snapshot&a$date_default<horizon) |
         (!is.na(a$date_zero_balance)&a$date_zero_balance>=snapshot&
            a$date_zero_balance<horizon))
    a[contradictory & raw_default==1L,
      `:=`(label_status='conflicting_positive',
           exclusion_reason='ZERO_BEFORE_DEFAULT_POSITIVE')]
    a[contradictory & raw_default==0L,
      `:=`(label_status='uncertain_negative',
           exclusion_reason='ZERO_BEFORE_DEFAULT_NEGATIVE')]
    a[label_status=='uncertain_negative' & is.na(exclusion_reason),
      exclusion_reason:='INSUFFICIENT_FOLLOWUP_NEGATIVE']
    review <- a[!is.na(exclusion_reason) | same_month |
                  (ra_class!='NO_RA' & !is.na(ra_date) &
                     ra_date>=snapshot & ra_date<horizon)]
    if(nrow(review)) exceptions[[length(exceptions)+1L]] <- review[,
                                                                   .(vintage=v,snapshot=y,loan_id,issue='LABEL_OR_CHRONOLOGY_REVIEW',
                                                                     detail=ra_class,event_date=date_default,date_zero_balance,
                                                                     zero_balance_code,exit_reason,event_order,raw_default,label_status,
                                                                     exclusion_reason,n_months_observed,max_reporting_gap)]
    excluded_uncertain_negatives <- sum(!is.na(a$exclusion_reason)&
                                          a$raw_default==0L)
    excluded_conflicting_positives <- sum(!is.na(a$exclusion_reason)&
                                            a$raw_default==1L)
    excluded_RA_chronology <- sum(contradictory &
                                    a$ra_class!='NO_RA')
    n_same_month <- sum(same_month)
    n_ra_horizon <- sum(a$ra_class!='NO_RA' & !is.na(a$ra_date) &
                          a$ra_date>=snapshot & a$ra_date<horizon)
    if(CENSORING_POLICY!='exclude') fail('Unknown censoring policy')
    a <- a[is.na(exclusion_reason)]
    if(anyNA(a$default) || any(a$default != a$raw_default))
      fail('Lost or changed raw numeric default at snapshot ',y)
    if(!nrow(a)) {
      snapshot_audit[[length(snapshot_audit)+1L]] <- data.table(
        vintage=v,snapshot=y,active_before_freshness=n_active,
        filtered_stale=n_stale,
        excluded_uncertain_negatives=excluded_uncertain_negatives,
        excluded_conflicting_positives=excluded_conflicting_positives,
        excluded_RA_chronology=excluded_RA_chronology,
        ambiguous_same_month_retained=0L,
        RA_events_in_horizon=n_ra_horizon,
        remaining=0L,defaults=0L)
      next
    }
    historical_rate <- fred[DATE<snapshot,tail(MORTGAGE30US,1L)]
    if(length(historical_rate)!=1L) fail('FRED has no rate before ',snapshot)
    a[,`:=`(current_loan_to_value=100*current_upb/original_value,
            ir_spread=current_interest_rate-historical_rate,
            n_months=month_index(snapshot)-month_index(loan_start)-1L,
            date_versioning=snapshot)]
    a[!is.finite(current_loan_to_value),current_loan_to_value:=NA_real_]
    m <- macro[year==y-1L,c('state_short',macro_vars),with=FALSE]
    n_before_merge <- nrow(a)
    a <- merge(a,m,by.x='property_state',by.y='state_short',all.x=TRUE,sort=FALSE)
    if(nrow(a)!=n_before_merge) fail('Row multiplication in macro join ',y)
    unmatched <- sum(!a$property_state %in% m$state_short)
    X_cols <- c('X_centroid','Y_centroid','credit_score','occupancy','nr_units',
                'loan_purpose','first_time_homebuyer','MSA','insurance_percent',
                'original_dept_to_income','original_combined_loan_to_value','original_upb',
                'number_of_borrowers','date_versioning','current_loan_to_value',
                'ir_spread','n_months','current_upb',macro_vars)
    if(anyDuplicated(a$loan_id)) fail('Duplicate loan at snapshot ',y)
    dt <- a[,c('loan_id','default',X_cols),with=FALSE]
    by_snapshot[[as.character(y)]][[length(by_snapshot[[as.character(y)]])+1L]] <- dt
    snapshot_audit[[length(snapshot_audit)+1L]] <- data.table(
      vintage=v,snapshot=y,active_before_freshness=n_active,
      n_reporting_gaps=sum(a$max_reporting_gap>0L,na.rm=TRUE),
      largest_reporting_gap=if(all(is.na(a$max_reporting_gap))) NA_integer_ else max(a$max_reporting_gap,na.rm=TRUE),
      negative_observed=sum(a$label_status=='negative_observed'),
      negative_after_exit=sum(a$label_status=='negative_after_exit'),
      filtered_stale=n_stale,
      excluded_uncertain_negatives=excluded_uncertain_negatives,
      excluded_conflicting_positives=excluded_conflicting_positives,
      excluded_RA_chronology=excluded_RA_chronology,
      ambiguous_same_month_retained=n_same_month,
      RA_events_in_horizon=n_ra_horizon,
      remaining=nrow(dt),defaults=sum(dt$default),macro_unmatched=unmatched)
  }
  rm(o,a_year,d,z,ra,early,g); gc(verbose=FALSE)
}

parts <- list(); master_path <- file.path(OUT,'loan_year_master.csv')
if(WRITE_MASTER && file.exists(master_path)) unlink(master_path)
for(y in SNAPSHOTS) {
  dt <- rbindlist(by_snapshot[[as.character(y)]],use.names=TRUE,fill=TRUE)
  if(!nrow(dt)) fail('No snapshot observations in ',y)
  if(anyDuplicated(dt$loan_id)) fail('Duplicate loan IDs within snapshot ',y)
  setorder(dt,loan_id)
  parts[[as.character(y)]] <- dt
  if(WRITE_MASTER) fwrite(dt,master_path,append=y!=min(SNAPSHOTS),
                          col.names=y==min(SNAPSHOTS),dateTimeAs='ISO')
  all_dt <- rbindlist(parts,use.names=TRUE,fill=TRUE)
  X <- as.data.frame(all_dt[,setdiff(names(all_dt),c('loan_id','default')),with=FALSE])
  cat_cols <- c('occupancy','nr_units','loan_purpose','first_time_homebuyer',
                'MSA','number_of_borrowers')
  for(k in cat_cols) X[[k]] <- as.character(X[[k]])
  Y <- as.integer(all_dt$default)
  ID <- as.character(all_dt$loan_id)
  if(anyNA(Y) || nrow(X)!=length(Y) || length(ID)!=length(Y)) fail('Output alignment error')
  AUX <- list(snapshot_year=y,cumulative=TRUE,vintage_min=2010L,
              vintage_max_eligible=y-1L,vintage_max_archive_checked=2022L,
              label_definition='first_numeric_03plus',label_horizon_months=12L,
              data_mode=DATA_MODE,ambiguous_same_month_policy='retain_03plus_and_audit',
              conflicting_positive_policy='exclude_strictly_after_zero',
              imputation='TRAIN_ONLY_REQUIRED',
              censoring_policy=CENSORING_POLICY,reporting_cutoff=PERF_CUTOFF,
              retrospective_macro=TRUE)
  saveRDS(list(Y=Y,X=X,ID=ID,AUX=AUX),
          file.path(OUT,sprintf('data_window_%d.rds',y)),compress='gzip')
  message('saved cumulative ',y,' observations=',length(Y),' defaults=',sum(Y))
  rm(all_dt,X,Y,ID);gc(verbose=FALSE)
}
if(length(snapshot_audit)) fwrite(rbindlist(snapshot_audit,fill=TRUE),
                                  file.path(OUT,'generator_snapshot_audit.csv'))
if(length(exit_reason_audit)) fwrite(rbindlist(exit_reason_audit,fill=TRUE),
                                     file.path(OUT,'generator_exit_reason_audit.csv'))
exception_path <- file.path(OUT,'generator_exceptions.csv')
if(length(exceptions)) fwrite(rbindlist(exceptions,fill=TRUE),exception_path)
else if(file.exists(exception_path)) unlink(exception_path)
message('Finished. Inspect exceptions, exit-reason counts, and split-specific preprocessing.')
