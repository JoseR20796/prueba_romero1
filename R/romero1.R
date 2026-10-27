#Ingresar una matriz y calcular la diagonal principal y secundaria
#Creado por: Jose Romero
#Fecha: 26-10-2026
analisis_matriz=function(){
  cat("Ingrese el numero de filas: ")
  fila=readLines(n=1)
  fila=as.numeric(fila)
  cat("Ingrese el numero de columnas: ")
  columna=readLines(n=1)
  columna=as.numeric(columna)

  if(fila!=columna){
    stop(paste("Error: La matriz no es cuadrada.",
               "Ingresó", fila, "filas y", columna, "columnas.",
               "Ambos valores deben ser iguales."))
  }
  n=fila
  A=matrix(0,nrow = n, ncol = n)
  cat(paste0("\n Ingrese los valores de la matriz elemento por elemento (Aij):\n"))
  for (i in 1:n) {
    for (j in 1:n) {
      cat(paste0("Ingrese el valor para A[", i, ",", j, "]: "))
      valor=readLines(n=1)
      valor=as.numeric(valor)
      if(is.na(valor)){
        stop(paste("Error: El valor ingresado para A[", i, ",", j, "] no es un número válido."))
      }
      A[i, j]=valor
    }
  }
  diag_principal = diag(A)
  diag_secundaria = A[cbind(1:n, n:1)]
  cat("\n--- RESULTADOS ---\n")
  cat("Matriz ingresada:\n")
  print(A)

  cat("\nDiagonal principal:\n")
  print(diag_principal)

  cat("\nDiagonal secundaria:\n")
  print(diag_secundaria)
}
analisis_matriz()
