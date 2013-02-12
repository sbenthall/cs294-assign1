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



/*Library objects store a list of the files from a directory*/
class Library(dir: String){
	/*Get all documents in a list*/
	val doclist = new File(dir).listFiles();
		/*for (file <- new File(dir).listFiles) { 
			println(file.getName()); 
		}
		*/
	
}

class Dictionary {
	var master = scala.collection.mutable.Map.empty[String,Int]
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
	val master = col(0);
	def matrixsayhi(){
		println(master);
	}
	
	def matrixUpdater(vector: Iterable[Int]) ={
		var newdoclength = vector.size;
		var currmatlength = master(?,0).length;
		var diff = newdoclength - currmatlength;
		/*If new doc is longer than existing matrix
		we need to augment the existing matrix*/
		if (diff > 0){
			println("diff = "+diff)
			/*Create filler array to augment existing columns*/
			var diff_fill_int = Array.fill(diff)(0);
			var diff_fill = new Array[Float] (0);
			for (e <- diff_fill_int){
					var e_float = Array(e.toFloat);
					diff_fill = Array.concat(diff_fill,e_float); 
				}

			/* Take vector from DocObject and turn it into an Array*/
			var vectorlist = new Array[Float] (0);
			for (e <- vector){
				var e_arr = Array(e.toFloat);
				vectorlist = Array.concat(vectorlist,e_arr);
			}
		
			/*Take existing columns, augment them, and bind them to master
			First prepare each column*/	
			var newmaster = col(vectorlist);
			var width = master(0,?).length;

			for (w <- 0 to width-1) {
				/* Create array for each column of existing matrix*/
				var currdoc_arr = new Array[Float] (0)
				for (x <- 0 to currmatlength-1){
					println("master is (x: "+x+", y: "+w+") = " +master(x,w));
					var e_arr = Array(master(x,w));
					currdoc_arr = Array.concat(currdoc_arr,e_arr);
				}
				println(currdoc_arr.getClass());
				println(diff_fill.getClass());

				var coltoadd = col(Array.concat(currdoc_arr,diff_fill));
				println(coltoadd.length)
				println(newmaster.length)
				print(newmaster);				
				newmaster = newmaster\coltoadd;
				println(newmaster);
				
			}
			
		}

	}

	/*for (x <- vectorlist){ println(x)}*/


	/*Pseudo-code:
	Take input from doc
	compare length with existing matrix
	if it is longer{
		take all existing rows of matrix
		append length of vector - existing matrix row of 0s
		bind the matrix together
	}

	*/

	
}


object bayesBuild{
	/*val main_dir = "../../review_polarity/txt_sentoken/pos"*/
	val main_dir = "Test/"
	def main(args: Array[String]) {
		val lib = new Library(main_dir);
		var dict = new Dictionary();
		var mat = new Matrix();
		mat.matrixsayhi();
		for (doc <- lib.doclist){
				var pathlist = List(main_dir, doc.getName());
				var filepath = pathlist.mkString("");
				println(filepath);
				var currdoc = new DocObject(filepath);
				currdoc.dictUpdater(dict);
				var add = currdoc.matrixExporter(dict);
				mat.matrixUpdater(add)
				

				currdoc.dictresetter(dict);


		}


	}
}
