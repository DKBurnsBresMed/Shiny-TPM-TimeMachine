


# FUNCTION: TPM_time_machine
# Use     : change the time scale of a transition matrix
# Method  : take the nth root of the eigenvalues as a diagonal and multiply
#           with the eigenvectors V.D.V(-1)
# Source  : Med Decis Making. 2016 November ; 36(8): 952–964. doi:10.1177/0272989X16656165

TPM_time_machine <- function(MAT, nth_root) {
  
  # compute eigenvalues and eigenvectors
  eig <- eigen(MAT)
  
  # Compile the eigenvalues into a diagonal and take nth root of values (D)
  D <- diag(eig$values^(1/nth_root))
  
  # V.D.V(-1) = re-scaled transitions, see source above
  eig$vectors %*% D %*% solve(eig$vectors)
  
}



# EXAMPLE

# P <-
#   structure(
#     c(0.714, 0, 0, 0.0619, 0.5728, 0, 0.2241, 0.4272, 1),
#     .Dim = c(3L, 3L),
#     .Dimnames = list(NULL, NULL)
#   )
# 
# 
# TPM_time_machine(P,12)
# 
# # [,1]           [,2]       [,3]
# # [1,] 0.9723177 0.00775546 0.01992686
# # [2,] 0.0000000 0.95462672 0.04537328
# # [3,] 0.0000000 0.00000000 1.00000000
# # 

#
#
#
#
#

# function b = stochroot(A,p)
# Ap = mpower(A,p);
# n=length(A);
# b = zeros(n,n);
# for j=1:n
# a = real(Ap(j,:));
# while true
# if sum(a)==1 && min(a)>=0
# b(j,:) = a;
# break;
# end
# lambda = (sum(a)-1)/n;
# x = a - lambda*ones(1,n);
# if min(x)>=0
# b(j,:)=x; break;
# end
# for k=1:n
# x(k) = max(0,x(k));
# end
# a = x;
# end
# end





#
#
#
#
#