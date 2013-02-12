import BIDMat.{Mat, FMat, DMat, IMat, CMat, BMat, CSMat, SMat, SDMat, GMat, GIMat, GSMat, HMat}
import BIDMat.MatFunctions._
import BIDMat.SciFunctions._
import BIDMat.Solvers._
import BIDMat.Plotting._


object testbidmat {
def main(args: Array[String]): Unit = {
 println("before")
 Mat.noMKL=true 
 val a = rand(30,30)
 println("after")
 print(a)
 }
}