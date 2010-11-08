# ##
# spa1_q1_tn248.R
# This script will compile scores, grades, and ranks for 
# an arbitrary number of students and their exam answers.
# It will also spot cheating by pairwise comparing students'
# answers and results.
# ##

# ##
# Created 14 Oct 2010
# Last updated 24 Oct 2010
# tn248@cam.ac.uk
# ##

num.students <- 12 # Adjustable
num.questions <- 30 #Adjustable
 
crib <- cbind(c(1:100),read.table("crib.dat"))
colnames(crib)<- c("qn","response")
rubric <- read.table("grade.txt",header=TRUE)

# initialize everything
student.index <-1:num.students
correct <-rep(NA,num.students)
rank<-rep(NA,num.students)

students <- lapply(1:num.students,function(x){read.table(paste("student", toString(x),".dat",sep=""),header=TRUE)})

correct <- sapply(1:num.students,function(x){sum(students[[x]]$response == crib[students[[x]]$qn,]$response)})

number <- sapply(1:num.students,function(x){floor(100*correct[[x]]/num.questions)})

alpha.grades <- sapply(1:num.students,function(x){rubric$grade[which((rubric$min <= number[[x]]) == (number[[x]] <= rubric$max), TRUE)]})

results <- data.frame(student=student.index,score=correct,grade=alpha.grades,rank=rank)

# assign ranks
results.sort<- results[order(results$score),]
results.sort$rank<-c(12:1)

# sort it back
results <- results.sort[order(results.sort$student),]
results
summary(results$score)

###
# cheater catcher function
###
# usage: cheaterkiller(i,j), where i and j are 
# the numbers of the suspected students
# 
# I had considered preloading all students before using 
# this function, but I think this is more memory efficient.

cheaterkiller <- function(i,j){ 
	
student.i <- read.table(paste("student", toString(i),".dat",sep=""),header=TRUE)
student.j <- read.table(paste("student", toString(j),".dat",sep=""),header=TRUE)

# which questions did they both attempt and how similar are the answers?
similarqs <- intersect(student.i$qn,student.j$qn)

#order by questions
student.i <- student.i[order(student.i$qn),]
student.j <- student.j[order(student.j$qn),]

#one-line to compare students' answers
similaras <- student.i$response[which(student.i$qn %in% similarqs)] == student.j$response[which(student.j$qn %in% similarqs)]

return(sum(similaras))
	}
	
###
# Finding the cheater; only calculates lower diagonal of matrix
###

cheatermatrix <- matrix(rep(0,num.students^2),num.students,num.students)
x <- cbind(rep(1:num.students, each=num.students),rep(1:num.students, num.students))
x <- x[x[,1] <= x[,2], ]
cheatermatrix[lower.tri(cheatermatrix,diag=TRUE)]<-mapply(cheaterkiller,x[,1],x[,2])
cheatermatrix