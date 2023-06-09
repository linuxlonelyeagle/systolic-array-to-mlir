package systolic

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import chisel3._
import chisel3.util._
import systolic.Util._

class Systolic {
  var mod: Module = null
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
  
  class Local(val width: Int) {
    var input: IRef = null 
    var calculation: Expr = null 
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
  
 /*def getSpcaceTimeTransform: Seq[Seq[Seq[Int]]] = {
    // 把6个Seq展开传递进comb函数
    val possibilities = comb(Seq.fill(6)(Seq(-1, 0, 1)): _*)
    val dirs = fixed_directions.map{
      case (local, xy_d) => dependencyvecs(local.calculation).collect{case (l, d) if l == local => d }.head
      
    }
  
  }*/
  def spcaceTimeTransform(matrix: Seq[Seq[Int]]) : Unit = {
    val P = matrix.init
    val s = Seq(matrix.last)
    //println(s"Det: ${det(matrix)}")
    val domain = comb(bounds.map(t => t._1 to t._2).toSeq:_*) 
    //println(s"domain: ${domain}")
    val domain_verts = comb(bounds.map(t => Seq(t._1, t._2)).toSeq:_*)
    //println(s"domain_verts: ${domain_verts}")
    // domain_to_range 算出每一个迭代运行的时间点
    val domain_to_range = domain.map(d => d -> matmul(matrix, Seq(d).transpose).flatten).toMap
    //println(s"domain_to_range: ${domain_to_range}")
    // proj_verts 让三维，降低到二维的空间，算出需要多少个pe 
    val proj_verts = (for (v <- domain_verts) yield matmul(P, Seq(v).transpose).flatten).distinct
    // 输出所有的pe 
    println("The vertices of your systolic array are:")
    proj_verts.foreach(v => println(s"\t${v}"))
    val inputSeqs = ArrayBuffer.empty[Tuple4[Systolic#Input, Int, Seq[Int], Seq[Int]]]
    // 这里算出输入矩阵每一个输入的位置，包括了元素在原来矩阵的位置，还有输入的时间，还有通过那一个pe进行输入
    for (it_vec <- domain) {
      val xyt = matmul(matrix, Seq(it_vec).transpose).flatten
      var letter = 'a'
      for (l <- locals.filter(_.input != null)) {
        val l_it = it_vec.lazyZip(l.input.localRef.direction).lazyZip(l.input.localRef.fiexd).toList.map{ case(i, d, f) => if (f) d else i}
        if (l_it == it_vec) {
          // 这里算出的应该是输入矩阵在原矩阵中的位置
          val in_coord = l.input.its.map(it => (l.input.localRef.its zip it_vec).collect{case(jt, i) if it == jt => i}).flatten
          // println(s"\t\t$letter: $in_coord")
          inputSeqs += ((l.input.input, xyt.last, xyt.init, in_coord))
        }
        letter = (letter.toInt + 1).asInstanceOf[Char]
      }
    }   
    var letter = 'a'
    for ( l <- locals.filter(_.input != null)) {
      println(s"Your input pattern for ${letter} is:")
      val input_seq = inputSeqs.filter(l.input.input == _._1).distinct.sortWith(_._2 < _._2)
      input_seq.foreach {
        case (_, t, cell, in) => println(s"\tAt time $t, (${in.foldLeft("")((acc, x) => s"$acc, $x") drop 2}) is input to cell (${cell.foldLeft("")((acc, x) => s"$acc, $x") drop 2})")
      }
      println()
      letter = (letter.toInt + 1).asInstanceOf[Char]
    }
    val outputSeqs = ArrayBuffer.empty[Tuple4[Systolic#Output, Int, Seq[Int], Seq[Int]]]
    for (it_vec <- domain) {
      val xyt = matmul(matrix, Seq(it_vec).transpose).flatten
      for (l <- locals.filter(_.output != null)) {  
        val l_it = it_vec.lazyZip(l.output.localRef.direction).lazyZip(l.output.localRef.fiexd).toList.map { case(i, d, f) => if (f) d else i }
        if (l_it == it_vec) {
          val out_coord = l.output.its.map(it => (l.output.localRef.its zip it_vec).collect{ case (jt, i) if it == jt => i}.head)
          outputSeqs += ((l.output.output, xyt.last, xyt.init, out_coord))         
        }
      }
    }
    for (l <- locals.filter(_.output != null)) {
      val output_seq = outputSeqs.filter(l.output.output == _._1).distinct.sortWith(_._2 < _._2)
      println(s"Your output pattern for $letter is:")
      output_seq.foreach {
        case (_, t, cell, out) => println(s"\tAt time $t, (${out.foldLeft("")((acc, x) => s"$acc, $x") drop 2}) is output from cell (${cell.foldLeft("")((acc, x) => s"$acc, $x") drop 2})")
      }
      println()
      letter = (letter.toInt + 1).asInstanceOf[Char]
    }

    val ind = locals.zipWithIndex.toMap
    val dep_vecs = locals.map(l => l -> dependencyvecs(l.calculation)).toMap
    val stationary_locals = locals.filter(l => matmul(P, Seq(dep_vecs(l).find(_._1 == l).get._2).transpose).flatten.forall(_ == 0))
    //println(s"dep_vecs: ${dep_vecs}")
    class PE extends Module {
      val io = IO(new Bundle {
        val ins = chisel3.Input(MixedVec(locals.map(l => chisel3.Input(UInt(l.width.W))).toSeq))
        val outs = chisel3.Output(MixedVec(locals.map(l => chisel3.Input(UInt(l.width.W))).toSeq))        
      })
      val regs = locals.map(l => Reg(UInt(l.width.W)))
      val wires = locals.map(l => Wire(UInt(l.width.W)))
      // 初始化寄存器里面的值
      regs.zipWithIndex.foreach { case (r, i) => 
        r := ("h" + (i + 97).asInstanceOf[Char]).U 
      } 

      for ((loc, i) <- ind) {
        wires(i) := Converter.visit(loc.calculation, (ind.keys zip regs).toMap)
      } 
      
      // 连接 stationary 寄存器
      for ((_, i) <- ind.filter( t => stationary_locals.contains(t._1))) {
        regs(i) := wires(i)
      }   

      // 连接 non-stationary 寄存器
      for ((_, i) <- ind.filter( t => !stationary_locals.contains(t._1))) {
        regs(i) := io.ins(i)
      }
       
      (io.outs zip wires).foreach { case (out, w) 
        => out := w 
      }
    }

    class Mesh extends Module {
     val space = domain_to_range.map(_._2.init).toSeq.distinct
      val pes = space.map(s => s -> Module(new PE)).toMap
      val inputs, outputs = ArrayBuffer.empty[UInt]
      val was_connected = ArrayBuffer.empty[UInt]
      pes.values.foreach(_.io.ins.foreach(_:= DontCare))
      for ((loc, i) <- ind) {
        val it_dir = dep_vecs(loc).collect{case (l, d) if l == loc => d}.head
        val space_dir = matmul(P, Seq(it_dir).transpose).flatten

        pes.foreach{case (s, pe) =>
          val adj_coord = (s zip space_dir).map(t => t._1 - t._2)
          if (pes.contains(adj_coord)) {
            val adj = pes(adj_coord)
            pe.io.ins(i) := adj.io.outs(i)
            if(s != adj_coord)
              was_connected += adj.io.outs(i)
          } else {
            // Special case when it has to be connected to global inputs
            val new_input = Wire(UInt(pe.io.ins(i).getWidth.W))
            pe.io.ins(i) := new_input
            inputs += new_input
          }
        }
      }

      for ((loc, i) <- ind) {
        pes.filter(t => !was_connected.contains(t._2.io.outs(i))).foreach{case (_, pe) =>
          val new_output = Wire(UInt(pe.io.outs(i).getWidth.W))
          new_output := pe.io.outs(i)
          outputs += new_output
        }
      }

      val io = IO(new Bundle {
        val ins = chisel3.Input(MixedVec(inputs.map(_.cloneType).toList))
        val outs = chisel3.Output(MixedVec(outputs.map(_.cloneType).toList))
      })
      (inputs zip io.ins).foreach(t => t._1 := t._2)
      (io.outs zip outputs).foreach(t => t._1 := t._2)
    }
    mod = new Mesh
  }  
}