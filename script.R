pacman::p_load(data.table,pbapply,magrittr,glue)
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
# helper FX ----
rand_vect=function(N, M, sd = 1, pos.only = TRUE) {
  vec <- rnorm(N, M/N, sd)
  if (abs(sum(vec)) < 0.01) vec <- vec + 1
  vec <- round(vec / sum(vec) * M)
  deviation <- M - sum(vec)
  for (. in seq_len(abs(deviation))) {
    vec[i] <- vec[i <- sample(N, 1)] + sign(deviation)
  }
  if (pos.only) while (any(vec < 0)) {
    negs <- vec < 0
    pos  <- vec > 0
    vec[negs][i] <- vec[negs][i <- sample(sum(negs), 1)] + 1
    vec[pos][i]  <- vec[pos ][i <- sample(sum(pos ), 1)] - 1
  }
  vec
}
rangE=function(v){max(v)-min(v)}
# fixed ----
no_reports=1e5 #rough STR est. per year
no_days=365-(8*12)-20 # removes weekend and holidays
days=seq(no_days)
no_analyst=20
no_leaves=24
# no. of simulations ----
N=1e4
# simulations ----
## simulation (1) no STR if leaves >= 3 days (fully exploit) ----
sim1=function(){
  randomLeave=function(){
    t=sample(seq(31-3),1)
    c(t,t+1,t+2)
  }
  leaveDays=function(){
    replicate(no_leaves/3,randomLeave(),simplify=F)%>%
      lapply(seq(length(.)),function(i,dt){
        dt[[i]]+(31*(i-1))
      },dt=.)%>%
      unlist}
  workingDays=replicate(no_analyst,days[!days%in%leaveDays()],simplify=F)
  dailySTR=rand_vect(no_days,no_reports)
  lapply(days,function(d){
    strAssigned=dailySTR[d]/sum(sapply(workingDays,function(x)d%in%x))
    sapply(seq(no_analyst),function(a){
      ifelse(d%in%workingDays[[a]],strAssigned,0)
    })
  })%>%
    lapply(seq(no_analyst),function(i,res){
      sapply(res,function(x)x[[i]])},res=.
    )%>%
    sapply(.,sum)%>%
    rangE
}
pbreplicate(N,sim1())%>%
  saveRDS(glue("res_sim1_{N}_times.rds"))
## simulation (2) no STR if leaves >= 1 day ----
sim2=function(){
  workingDays=replicate(no_analyst,days[!days%in%sample(seq(no_days),no_leaves)],simplify=F)
  dailySTR=rand_vect(no_days,no_reports)
  lapply(days,function(d){
    strAssigned=dailySTR[d]/sum(sapply(workingDays,function(x)d%in%x))
    sapply(seq(no_analyst),function(a){
      ifelse(d%in%workingDays[[a]],strAssigned,0)
    })
  })%>%
    lapply(seq(no_analyst),function(i,res){
      sapply(res,function(x)x[[i]])},res=.
    )%>%
    sapply(.,sum)%>%
    rangE
}
pbreplicate(N,sim2())%>%
  saveRDS(glue("res_sim2_{N}_times.rds"))
# results ----
rds=list.files(pattern="rds")
p1=readRDS(rds[1])%>%hist
p2=readRDS(rds[2])%>%hist
xlim=c(min(c(p1$breaks,p2$breaks)),max(c(p1$breaks,p2$breaks)))
ylim=c(min(c(p1$counts,p2$counts)),max(c(p1$counts,p2$counts)))
title="green=1d,blue=3d"
plot(p1,col=rgb(0,0,1,1/4),xlim=xlim,ylim=ylim,main=title)
plot(p2,col=rgb(1,0,0,1/4),xlim=xlim,ylim=ylim,add=T,main=title)
