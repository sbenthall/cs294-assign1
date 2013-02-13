/*	Filename: BayesBuild.scala
	Author: David Greis/ Sebastian Benthall
	CS_294 Assignment One
*/
import java.lang.Class
import java.io.File
import scala.io._

import BIDMat.{Mat, FMat, DMat, IMat, CMat, BMat, CSMat, SMat, SDMat, GMat, GIMat, GSMat, HMat}
import BIDMat.MatFunctions._
import BIDMat.SciFunctions._
import BIDMat.Solvers._
import BIDMat.Plotting._


/*TODOs
-Need to randomly sample 9/10 of the data

*/

class Dictionary {
  var master = scala.collection.mutable.Map.empty[String,Int];
}

class DocObject(file: String) {
	
  /*This thing should know the dictionary*/
  val rawtex = scala.io.Source.fromFile(file).mkString;
  val Linelist = rawtex.split(" ");
  
  /*Dictionary Updater function; takes dictionary obj, updates
   its keys, and creates a value vector for a particular document*/
  def dictUpdater(dict: Dictionary){
    for (word <- Linelist){
      if (dict.master.contains(word)){
	dict.master(word) = dict.master(word) + 1;
      }
      else {
	dict.master = dict.master + (word -> 1)
      }	
    }
  }
  
  /* Takes the values of populated dictionary, separates values
   and prepares them to be added onto the sparse matrix
   MAKE SURE dictUpdater is used first before exporting values*/
  def matrixExporter(dict: Dictionary) : Iterable[Int] = {
    var exportList = dict.master.values;
    return exportList;
  }
  
  /*Reset dictionary values (counts) to zero*/
  def dictresetter(dict: Dictionary){
    dict.master.transform((String,Int) => 0);
  }
}

class Matrix {
  Mat.noMKL=true 
  var master = col(0);

  def loglikelihood(indices : IMat) : FMat = { 
    //slice matrix
    val sliced = master(?,indices)

    val likelihoods = (sum(sliced,2) + 1) /@ (sum(sum(sliced)) + master.nr);

    return ln(likelihoods)
  }

  def width : Int = { master.ncols }
	
  def matrixUpdater(vector: Iterable[Int]) ={
    var newdoclength = vector.size;
    var currmatlength = master.nrows;
    var diff = newdoclength - currmatlength;

    master = col(vector.toArray) \ (master on zeros(diff,width))
  }
}

object bayesBuild{
  //val main_dir = "../src/main/resources/txt_sentoken"
  val main_dir = "/home/sb/ischool/cs294/assign1/bayesBuild/Test";
  val classes = List("neg","pos");

  def buildMatrix(c : String): Matrix = { 
      val class_dir = main_dir + "/" + c;
      println(class_dir)
      val doclist = new File(class_dir).listFiles();

      var dict = new Dictionary();
      
      var mat = new Matrix();

      for (doc <- doclist){
        var pathlist = List(class_dir, doc.getName());
        var filepath = pathlist.mkString("/");
        var currdoc = new DocObject(filepath);
        currdoc.dictUpdater(dict);
        var add = currdoc.matrixExporter(dict);
        mat.matrixUpdater(add)
        
        currdoc.dictresetter(dict);
      }

    return mat;
  }

  /* classifier takes in a document and the likelihood vectors
   * and returns a classification (an index into the list of loglihoods*/
  def classify(testmat : FMat, priors: List[Float], loglikelihoods : List[BIDMat.FMat]): Int = { 
     return 0;
  }

  def main(args: Array[String]) {
    var mats = classes.map(buildMatrix);
    val numPanes = 4
    val paneSize = 5
    val windowPane = (x:Int) => irow((x - 1) * paneSize + 1 to x * paneSize)

    println(windowPane(4))

    for(m <- mats){ 
      println(m.loglikelihood(windowPane(3)))
    }

    for(w <- 1 to numPanes){ 

      var priors = mats.map( _.width.toFloat / mats.map(_.width).sum);
      println(priors)

      val train_window = ((1 to numPanes) diff List(w)).map(windowPane).reduceLeft((x,y) => x \ y);
      val test_window = windowPane(w)

      println(mats.map(x => x.loglikelihood(train_window)));

      println(classify(windowPane(w), priors, mats.map(x => x.loglikelihood(windowPane(1)))));
    }

  }
}
