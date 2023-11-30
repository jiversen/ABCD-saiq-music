print('HI')

print(commandArgs())

print(tryCatch(sys.frame(1)$filename, error=function(cond) return(NULL) ))



