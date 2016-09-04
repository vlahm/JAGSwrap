# **title**

### subtitle (semi-description)
### **Description**
Description text - can be multi-line
### **Contents**
1. _func_name_ for doing x y and z
2. _other_func_ logical; for doing more stuff
x.  Installation
y.  Contact the author

---
### **1. func_name**
#### Do stuff if things happen
```R
matfilter(x, mar=2, cond=NA, fun=NULL, thresh=1, 
          na.rm=TRUE, filter=TRUE)
```
+ `x` a matrix or data.frame
+ `mar` [numeric] the margin along which the function will be applied - either 1 for rows or 2 for columns
+ `cond` [character] the completion of a Boolean expression, e.g. ">= 0", which is passed to `apply`


#### **_Example Usage_**
```R
> matfilter(df, mar=2, fun=is.na, thresh=0.3)
$column_proportions
   a    b    c 
0.00 0.25 0.50 

  a  b
1 2  1
2 2  2
3 3 NA
4 3  1
```
---


### **6. Installation**
```R
# devtools lets you install packages from GitHub:
install.packages('devtools')
library(devtools)

# then it's this simple:
install_github('vlahm/manipulateR')
library(manipulateR)
```
---
### **7. Contact the author**
Mike Vlah: 
+ vlahm13@gmail[dot]com
+ linkedin.com/in/michaelvlah (figure out how to add links to github md)
