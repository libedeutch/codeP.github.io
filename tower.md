# Idea
Calculate the bricks needed for both increasing and decreasing order. 

```R
increase<- function(statues,len,diffs){
  num = 0;
  for(i in 1:length(diffs)){
    if(diffs[i]>=0){
      num = num + statues[i+1]-1-statues[i];
      statues[i] = statues[i+1]-1 ;
    }
    if(diffs[i]<0){
      num = num + statues[i]+1 - statues[i+1];
      statues[i+1] = statues[i]+1;
    }
  }
  tmp = statues[1] + len-1;
  gap = statues[len] - tmp;
  if(gap<=0){
    num = num - gap;
  }
  else{
    num = num + len-1 ;
  }
  
  return (num);
  
}

statues <- c(6,2,3,8)
len = length(statues)
diffs <- diff(statues)
decrease(statues, len, diffs)
increase(statues, len, diffs)


decrease <- function(statues,len,diffs){
  num = 0;
  for(i in length(diffs):1){
    if(diffs[i]<0){
      num = num + statues[i]-1-statues[i+1];
      statues[i+1] = statues[i]-1 ;
    }
    if(diffs[i]>=0){
      num = num + statues[i+1]+1 - statues[i];
      statues[i] = statues[i-1]+1;
    }
  }
  tmp = statues[len] + len-1;
  gap = statues[1] - tmp;
  if(gap<=0){
    num = num - gap;
  }
  else{
    num = num + len-1 ;
  }
  
  return (num);
}
```
