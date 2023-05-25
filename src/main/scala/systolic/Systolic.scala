package systolic

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import chisel3._
import chisel3.util._
import systolic.Util._

class Systolic {

  class Iterator(low: Int, high: Int, val diff: Int = 0) {
    def +(i: Int): Iterator = new Iterator(low, high, i) 
    def -(i: Int): Iterator = this.+(-i)  
  }

  object Iterator{
    def apply(low: Int, high: Int) : Iterator = {
      bounds += ((low, high))
      new Iterator(low, high)
    }
  }

  class Input(it1: Iterator, it2: Iterator) {
    def apply(its: Iterator*) = {
      new IRef(this, its)
    }
  }

  object Input {
    def apply(it1: Iterator, it2: Iterator) = new Input(it1, it2)
  }

  class Output(it1: Iterator, it2:Iterator) {
    def apply(its: Iterator*) = {
      new ORef(this, its)
    }
  }

  object Output {
    def apply(it1: Iterator, it2: Iterator) = new Output(it1, it2)
  }
  
  class Local(val weidth: Int) {
    var input: IRef = null 
    val calculation: Expr = null 
    var output: ORef = null 

    def apply(its: Iterator*): Ref = {
      new Ref(this, its.map(_.diff), its.map(_ => false), its, Seq.fill(its.size)(0))      
    }

    def apply(it1: Iterator, num: Int, it2: Iterator): Ref = {
      new Ref(this, Seq(it1.diff, num, it2.diff), Seq(false, true, false), Seq(it1, null, it2), Seq(0, num, 0))
    }

    def apply(num: Int, it1: Iterator, it2: Iterator): Ref = {
      new Ref(this, Seq(num, it1.diff, it2.diff), Seq(true, false, false), Seq(null, it1, it2), Seq(num, 0, 0))
    }

    def apply(it1: Iterator, it2: Iterator, num: Int): Ref = {
      new Ref(this, Seq(it1.diff, it2.diff, num), Seq(false, false, true), Seq(it1, it2, null), Seq(0, 0, num))
    }

    def apply(it1: Iterator, it2: Iterator, nums: Tuple2[Int, Int]): Ref = {
      new Ref(this, Seq(it1.diff, it2.diff, nums._1), Seq(false, false, true), Seq(it1, it2, null), Seq(0, 0, nums._2))
    }

    def apply(nums: Tuple2[Int, Int], it1: Iterator, it2: Iterator): Ref = {
      new Ref(this, Seq(nums._1, it1.diff, it2.diff), Seq(true, false, false), Seq(null, it1, it2), Seq(nums._2, 0, 0))
    }

    def apply(it1: Iterator, nums: Tuple2[Int, Int], it2: Iterator): Ref = {
      new Ref(this, Seq(it1.diff, nums._1, it2.diff), Seq(false, true, false), Seq(it1, null, it2), Seq(0, nums._2, 0))
    }
  }

  object Local {
    def apply(width: Int): Local = {
      val result = new Local(width)
      locals += result 
      result 
    }
  }
  

  val bounds = ArrayBuffer.empty[Tuple2[Int, Int]]
  val locals = ArrayBuffer.empty[Local]
  val fixed_directions = mutable.Map.empty[Local, Seq[Int]]

  var mod: Module = null
  
  // 填充fixed_directions
  def fix(local: Local): Unit = {
    fixed_directions(local) = Seq(0, 0)
  }

  def flowR(local: Local): Unit =  {
    fixed_directions(local) = Seq(0, 1) 
  }

  def flowD(local: Local): Unit = {
    fixed_directions(local) = Seq(1, 0)
  }

  def low(local:Local, dir: Tuple2[Int, Int]): Unit = {
    fixed_directions(local) = Seq(dir._1, dir._2)
  }
  
  def getSpcaceTimeTransform: Seq[Seq[Seq[Int]]] = {
    // 把6个Seq展开传递进comb函数
    val possibilities = comb(Seq.fill(6)(Seq(-1, 0, 1)): _*)
    val dirs = fixed_directions.map{
      case (local, xy_d) => dependencyvecs(local.calculation).collect{}
      val it_d =  
    }
  }
}