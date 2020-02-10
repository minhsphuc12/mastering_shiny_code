i <- 1
paste("Generation ",i,sep="")

i <- 1
Output <- c()
Output[i] <- i+1
parse(text=(paste("Output[",i,"]*50",sep="")))

eval(parse(text=(paste("Output[",i,"]*50",sep=""))))

Output <- c()
Results <- c()
for(i in 1:10){
    Output[i] <- i+1
    Results[i] <- eval(parse(text=(paste("Output[",i,"]*i",sep=""))))
}
abc = 1
parse(text = 'abc') %>% eval
expr(abc) %>% eval

#