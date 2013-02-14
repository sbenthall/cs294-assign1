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

  //TODO: make sparse!
  var master : SMat = null;

  def loglikelihood(indices : IMat) : FMat = { 
    //slice matrix
    val sliced = master(?,indices)

    //compute with laplace smoothing
    val likelihoods = (sum(sliced,2) + 1) /@ (sum(sum(sliced)) + master.nr);

    return ln(likelihoods)
  }

  def width : Int = { master.ncols }
	
  def matrixUpdater(vector: Iterable[Int]) ={
    if(master == null){
      master = sparse(col(vector.toArray))
    } else { 
      var diff = vector.size - master.nrows;

      master = (master on sparse(zeros(diff,width))) \ sparse(col(vector.toArray));
    }    
  }
}

object Classifier {
  
  def scoremaker(testmat: SMat, loglikelihoods: FMat): FMat ={
    /*Unclear whether this matrix multiplication is most efficient.
    We could also transpose the testmat.*/
    var likelihoods_t = loglikelihoods.t;
    var resultmat = likelihoods_t*testmat;
    return resultmat;
  }

  def classify(testmat : SMat, logpriors: List[Double], loglikelihoods : List[BIDMat.FMat]): FMat = { 
    var pos_vector = scoremaker(testmat,loglikelihoods(0)) + logpriors(0);
    var neg_vector = scoremaker(testmat,loglikelihoods(1)) + logpriors(1);
    var result = pos_vector > neg_vector;
    return result;
  }
  
}

object bayesBuild{

  //val main_dir = "../../review_polarity/txt_sentoken/pos"
  val main_dir = "Test";
  val classes = List("pos","neg");

  //reuse this dictionary to keep vocabulary list

      
  def buildMatrix(): Matrix = { 
    var dict = new Dictionary();
    
    var mat = new Matrix();
    
    for(c <- classes){ 
      val class_dir = main_dir + "/" + c;
      val doclist = new File(class_dir).listFiles();
    
      for (doc <- doclist){
        var pathlist = List(class_dir, doc.getName());
        var filepath = pathlist.mkString("/");
        var currdoc = new DocObject(filepath);
        currdoc.dictUpdater(dict);
        var add = currdoc.matrixExporter(dict);
        mat.matrixUpdater(add)
      
        currdoc.dictresetter(dict);        
      }
    }
    
    return mat;
  }

  /* pos_res = results of classification on documents that are actually positive
   * neg_res = results of classification on documents that are actually negative
   * F = 2PR/(P+R)
     PR = tp/(tp + fp)
     R = tp/(tp + fn)
    
    Steps:
    use other functions in class to calculate tp,fn,fp
    Calculate PR, R, and finally F
    return F
   */
  def calcF_1(pos_res : FMat, neg_res : FMat) : Float = {
    val tp = sum(pos_res)(0);
    val fp = sum(neg_res)(0);
    val fn = sum(1 - pos_res)(0);

    val p = tp / (tp + fp);
    val r = tp / (tp + fn);

    return (2 * p * r) / (p + r);
  }

  def main(args: Array[String]) {
    flip;
    val bigmat = buildMatrix();

    val pos_mat = new Matrix();
    pos_mat.master = bigmat.master(?,0 to (bigmat.width / 2) - 1);

    val neg_mat = new Matrix();
    neg_mat.master = bigmat.master(?, (bigmat.width / 2 to bigmat.width - 1));

    var mats = List(pos_mat,neg_mat)

    var priors = mats.map( _.width.toFloat / mats.map(_.width).sum);

    // parameters for n-fold validation
    val numPanes = 4
    val paneSize = 5
    val windowPane = (x:Int) => irow((x - 1) * paneSize to x * paneSize - 1)

    var f1s : List[Float] = List();

    for(w <- 1 to numPanes){ 
      //use same window on both data sets for 10-fold validation
      val train_window = ((1 to numPanes) diff List(w)).map(windowPane).reduceLeft((x,y) => x \ y);
      val loglikelihoods = mats.map(_.loglikelihood(train_window))

      //these are always going to be ln(.5) each...
      //yeah ok this is unnecessary
      var logpriors = List(math.log(.5),math.log(.5));

      val test_window = windowPane(w);

      val classified = mats.map(x => Classifier.classify(x.master(?,test_window),logpriors,loglikelihoods));

      f1s = calcF_1(classified(0),classified(1)) :: f1s;
    }

    println(f1s);
    val avg_f1 = f1s.sum / f1s.length;
    println(avg_f1);

    //compute time in gigaflops
    val ff = gflop;
    println(ff);
  }
}
