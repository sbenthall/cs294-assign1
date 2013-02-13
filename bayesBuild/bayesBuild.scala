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

    //compute with laplace smoothing
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

  //val main_dir = "../../review_polarity/txt_sentoken/pos"
  val main_dir = "Test";
  val classes = List("pos","neg");
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

class Classifier {
  
  def scoremaker(testmat: FMat, loglikelihoods: FMat): FMat ={
    /*Unclear whether this matrix multiplication is most efficient.
    We could also transpose the testmat.*/
    var liklihoods_t = loglikelihoods.t;
    var resultmat = liklihoods_t*testmat;
    return resultmat;
  }

  def classify(testmat : FMat, priors: List[Double], loglikelihoods : List[BIDMat.FMat]): FMat = { 
    var pos_vector = scoremaker(testmat,loglikelihoods(0)) + priors(0);
    var neg_vector = scoremaker(testmat,loglikelihoods(1)) + priors(1);
    var result = pos_vector > neg_vector;
    return result;
  }
  
}




  def main(args: Array[String]) {
    var mats = classes.map(buildMatrix);
    var priors = mats.map( _.width.toFloat / mats.map(_.width).sum);
    println(priors)
    val numPanes = 4
    val paneSize = 5
    val windowPane = (x:Int) => irow((x - 1) * paneSize + 1 to x * paneSize)
    println(windowPane(4))
    for(m <- mats){ 
      println(m.loglikelihood(windowPane(3)))
    }

    for(w <- 1 to numPanes){ 

      val train_window = ((1 to numPanes) diff List(w)).map(windowPane).reduceLeft((x,y) => x \ y);
      //these are always going to be ln(.5) each...
      val alldocs = mats.map(_.width).sum;

      var logpriors = List(ln(.5),ln(.5));
      println(logpriors)

      val test_window = windowPane(w)

      println(mats.map(x => x.loglikelihood(train_window)));

      //println(classify(windowPane(w), logpriors, mats.map(x => x.loglikelihood(windowPane(1)))));
    }

  }
}
