package systolic

import chisel3._
import java.sql.Driver

class MatMul extends Systolic {
  val N1, N2, N3 = 1 
  val i = Iterator(0, N1)
  val j = Iterator(0, N2)
  val k = Iterator(0, N3)

  val A = Input(i, k)
  val B = Input(k, j)
  val C = Input(i, j) 

  val a, b = Local(16)
  val c = Local(32) 
  
  a(i, 0, k) := A(i, k)
  b(0, j, k) := B(k, j) 
  c(i, j, 0) := 0 

  a(i, j, k) := a(i, j-1, k)
  b(i, j, k) := b(i-1, j, k) 
  c(i, j, k) := c(i, j, k-1) + (a(i, j-1, k) * b(i-1, j, k))

  c(i, j) := c(i, j, N3)
  spcaceTimeTransform(Seq(
    Seq(1, 0, 0),
    Seq(0, 1, 0), 
    Seq(1, 1, 1)))
}

object  SystolicMain extends App {
  val tmp = new MatMul
}