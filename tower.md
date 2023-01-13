# Idea
Calculate the bricks needed for both increasing and decreasing order. </br>
For the increasing case, starting from the final lowest location i= 1</br>
Compare the adjacent brickss i and i+1; i.e.,  first and second brick, if the second one is larger, direction is desirable, brick 1 increases to brick 2 -1 or brick 2 increase to brick 1 +1; </br>
After the steps, all bricks are processed except the last one, if last one is higher than expected, all 1, to n-1 bricks increase brick[n]- tartget[n];
else last is increased to expected </br>
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
