
orthnorm <- function(Matrix) {
  identical(round(t(Matrix) %*% Matrix,10), diag(ncol(Matrix)))
}

proj <- function(u,v){
  return(sum(u*v)/(sum(u*u))*u)
}

normalize <- function(Matrix)
  return(Matrix/sqrt(rowSums(Matrix*Matrix)))

Gram_Shmidt_Orthogon <- function(V,U,index1,index2,n)
{
  print(proj(U[index2,],V[index1,]))
  U[index1,]=U[index1,]-proj(U[index2,],V[index1,])
  if(index2<(index1-1)){
    index2= index2+1
    return(Gram_Shmidt_Orthogon(V,U,index1,index2,n))
  }
  else if(index1<n){
    index1=index1+1
    return(Gram_Shmidt_Orthogon(V,U,index1,1,n))
  }
  else return(U)
}

N = as.integer(readline("Set dimensionality of a space: "))
x=c()
for(i in 1:(N*N))
  x=append(x,as.numeric(readline("Give vector element: ")))
V = t(matrix(x,N))
print("Input set of vectors:")
print(V)
print("Is the given set of vectors orthonormal?")
print(orthnorm(V))
U = Gram_Shmidt_Orthogon(V,V,2,1,N)
print("After orthogonalization:")
print(U)
U=normalize(U)
print("Normalized set of vectors:")
print(U)
print("Check if received set is orthonormal:")
print(orthnorm(U))
