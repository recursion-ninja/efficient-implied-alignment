### relationship between string length, leaf-set size and runtime

### set up work directory and load library----
args <- commandArgs(trailingOnly=TRUE)
if (length(args)==0) {
  stop("You must provide the input path and output file", call.=FALSE)
} else if (length(args)==1) {
  stop("You must provide the output file", call.=FALSE)
} 

#current_warning <- getOption("warn") #suppress unnecessary warning msg
#options(warn = -1)
#options(warn = current_warning)

#install.packages("ggplot2") #if the package is not yet installed
install.packages("stargazer")
#suppressWarnings(library(ggplot2))
suppressWarnings(library(stargazer))

### best case data----
## read data
best <- read.csv(paste0(args[1], "/pathological-12.preorder.csv"),
                 colClasses = c("NULL", "NULL", NA, NA, NA),
                 col.names = c("null1", "null2","n", "k", "z"),
                 header = FALSE)
head(best) #sanity check
sapply(best, class) #check column class

## plot n and z, by k
#k_factor <- factor(best$k, levels=c("25", "50", "100", "200", "400"))
#ggplot(data=best, aes(x=log2(n), y=log2(z),
#                       group=k_factor, col=k_factor)) +
#      geom_point() +
#      geom_line(size=1) +
#      labs(title="Runtime in best case data",
#           x="Leaf-set size", y="Runtime (milliseconds)",
#           col="String length",
#           caption="Note: both leaf-set size and runtime are on log2 scale") +
#      scale_x_continuous(labels=c("2"="4", "3"="8", "4"="16",
#                                  "5"="32", "6"="64")) +
#      scale_color_brewer(palette="Set3") +
#      theme(plot.title=element_text(hjust=0.5, size=18),
#            plot.caption=element_text(hjust=0, face="italic"))
#ggsave("log-runtime_best.jpeg", width=20, height=10, unit="cm")
#rm(k_factor)

## build linear regression model
mod_best <- lm(data=best, log2(z)~log2(n)+log2(k))
summary(mod_best)

### worst case data----
worst <- read.csv(paste0(args[1], "/pathological-31.preorder.csv"),
                  colClasses = c("NULL", "NULL", NA, NA, NA),
                  col.names = c("null1", "null2","n", "k", "z"),
                  header = FALSE)
sapply(worst, class)
head(worst)
mod_worst <- lm(data=worst, log2(z)~log2(n)+log2(k))
summary(mod_worst)

### fungi data----
fungi <- read.csv(paste0(args[1], "/fungi-11.preorder.csv"),
                  colClasses = c("NULL", "NULL", NA, NA, NA),
                  col.names = c("null1", "null2","n", "k", "z"),
                  header = FALSE)
sapply(fungi, class)
mod_fungi <- lm(data=fungi, log2(z)~log2(n)+log2(k))
summary(mod_fungi)

## metazoa data----
metazoa <- read.csv(paste0(args[1], "/metazoa-11.preorder.csv"),
                colClasses = c("NULL", "NULL", NA, NA, NA),
                col.names = c("null1", "null2","n", "k", "z"),
                header = FALSE)
sapply(metazoa, class)
mod_metazoa <- lm(data=metazoa, log2(z)~log2(n)+log2(k))
summary(mod_metazoa)

### format all models, extract coefficients and adj. r2----
# specify the output file to be .tex
stargazer(mod_best, mod_worst, mod_fungi, #mod_metazoa,
          type="latex",
          title="Regression coefficients of leaf-set size and string length on runtime",
          column.labels = c("Best", "Worst", "Fungi", "Metazoa"),
          covariate.labels = c("$\\log_2$ (String count $\\;n$)",
                               "$\\log_2$ (String length $k$)"),
          dep.var.labels = "$\\log_2$ (Runtime)",
          #keep=c("n", "k"),
          omit = "Constant",
          se=NULL, report="vc",
          keep.stat = c("n", "adj.rsq"),
          omit.table.layout = "n",
          align=TRUE, #Requires \usepackage{dcolumn} in LaTeX preamble
          #omit.stat = c("f", "ser", "rsq"),
          out=args[2]) #"runtime-regression.tex")

