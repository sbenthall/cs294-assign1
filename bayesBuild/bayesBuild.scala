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
	dict.master += (word -> 1)
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

object Classifier {
  
  def scoremaker(testmat: FMat, loglikelihoods: FMat): FMat ={
    /*Unclear whether this matrix multiplication is most efficient.
    We could also transpose the testmat.*/
    var likelihoods_t = loglikelihoods.t;
    println(likelihoods_t);
    println(likelihoods_t.nc)
    println(testmat.nr)
    var resultmat = likelihoods_t*testmat;
    return resultmat;
  }

  def classify(testmat : FMat, logpriors: List[Double], loglikelihoods : List[BIDMat.FMat]): FMat = { 
    var pos_vector = scoremaker(testmat,loglikelihoods(0)) + logpriors(0);
    var neg_vector = scoremaker(testmat,loglikelihoods(1)) + logpriors(1);
    var result = pos_vector > neg_vector;
    return result;
  }
  
}

class FMeasurer {
  /* The methods of this class assume that the matrices fed to them are resultmats from the
  classify function of the Classifier class. It assumes that the parameter passed as 'posmat' 
  is the vector of predicted valuesof documents that IN REALITY are positive. Likewise, 'negmat'
  is presumed to be the same for documents that IN REALITY are negative.
  */

  def trueposcalc(posmat: FMat, negmat: FMat){
    /*to get tp, I need to know from testset how it's made. this takes in result from classifier
    I need to count the number of 1's that should be 1 and 0s that should be 0

    Steps:
    count how many 1s are in posmat, how many 0s are in negmat
    sum and return
    */
  }

  def falsenegcalc(posmat: FMat, negmat: FMat){
    /*to get fn... what is a false negative in this case? It's something that it says is false
    but is really true. what does it mean to be false? Says it's negative. But really it's positive.
    So I need to count these. This is a count of the entries in the resultmat that are 0 when 
    they should be 1 or 1 when they should be 0.
    
    Steps:

    Count up 0s in posmat
    Count up 1s in negmat 
    sum and return
    */
  }

  def falseposcalc(posmat: FMat, negmat: FMat){
    /* to get fp, count 1s in result mat that should be 0's (or 0s that should be 1)

    Steps:
    Count up 1s in posmat
    Count up 0s in negmat
    sum and return
    */
  }

  
  def calcF_1(){
    /* F = 2PR/(P+R)
    PR = tp/(tp + fp)
    R = tp/(tp + fn)
    
  Steps:
  use other functions in class to calculate tp,fn,fp
  Calculate PR, R, and finally F
  return F
    */
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
      //use same window on both data sets for 10-fold validation
      val train_window = ((1 to numPanes) diff List(w)).map(windowPane).reduceLeft((x,y) => x \ y);
      val loglikelihoods = mats.map(_.loglikelihood(train_window))

      //these are always going to be ln(.5) each...
      //yeah ok this is unnecessary
      var logpriors = List(math.log(.5),math.log(.5));

      val test_window = windowPane(w);
      mats.map(x => Classifier.classify(x.master(?,test_window),logpriors,loglikelihoods));


      println(mats.map(x => x.loglikelihood(train_window)));

      //println(classify(windowPane(w), logpriors, mats.map(x => x.loglikelihood(windowPane(1)))));
    }

  }
}
