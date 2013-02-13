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

  def likelihood() : FMat = {
    //with laplace smoothing
    print("have to split out stuff for validation")
    return (sum(master,2) + 1) /@ (sum(sum(master)) + master.nr);
  }

  def loglikelihood() : FMat = { 
    return ln(this.likelihood())

  }

  def width : Int = { master.ncols }
	
  def matrixUpdater(vector: Iterable[Int]) ={
    var newdoclength = vector.size;
    var currmatlength = master.nrows;
    var diff = newdoclength - currmatlength;

    var newmaster = col(vector.toArray)

    /*If new doc is longer than existing matrix
     we need to augment the existing matrix*/
    if (diff > 0){
      println("diff = "+diff)
      /*Create filler array to augment existing columns*/
      var diff_fill = new Array[Float](diff)

      //newmaster = master on zeros(diff,width)
      //print(newmaster)
      for (w <- 0 to width-1) {
	/* Create array for each column of existing matrix*/
	var currdoc_arr = new Array[Float] (0)
	for (x <- 0 to currmatlength-1){
	  var e_arr = Array(master(x,w));
	  currdoc_arr = Array.concat(currdoc_arr,e_arr);
	}
        
	var coltoadd = col(Array.concat(currdoc_arr,diff_fill));
	newmaster = newmaster\coltoadd;
      }	
    }
    if (diff == 0){
      newmaster = newmaster\master
    }
    master = newmaster
    
  }
}

object bayesBuild{
  //val main_dir = "../../review_polarity/txt_sentoken/pos"
  val main_dir = "/home/sb/ischool/cs294/assign1/bayesBuild/Test/";
  val classes = List("neg","pos");

  def buildMatrix(c : String): Matrix = { 
      val class_dir = main_dir + "/" + c;
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
<<<<<<< HEAD
  def classify(indices : FMat, mat : Matrix, priors: List[Double], loglikelihoods : List[BIDMat.FMat]): Int = { 
=======
  def classify(testmat : FMat, priors: List[Double], loglikelihoods : List[BIDMat.FMat]]): Int = { 
>>>>>>> 5e8011628bdc5fcb930a2ef22ddaf7561dfc6c89
  
     return 0;
  }

  def main(args: Array[String]) {
    var mats = classes.map(buildMatrix);

    var priors = mats.map( _.width.toFloat / mats.map(_.width).sum);
    println(priors)
    for(m <- mats){ 
      println(m.loglikelihood)

      
    }

  }
}
