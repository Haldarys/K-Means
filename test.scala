import scala.io.StdIn

object test
{
  def main(args:Array[String]): Unit =
  {
    var entree:Int = 0;
    while(entree <= 0 || entree > 1000)
    {
      println("Entrez un nombre de clusters entre 1 et 1000");
      entree = StdIn.readInt();
    }

    val kmean = new Kmeans(entree);
    println("Statistiques de la base de donnée :")
    println("Moyennes : " + kmean.moyenneBase(0) + " " + kmean.moyenneBase(1) + " " + kmean.moyenneBase(2) + " " + kmean.moyenneBase(3));
    println("Variances : " + kmean.variance(0) + " " + kmean.variance(1) + " " + kmean.variance(2) + " " + kmean.variance(3));
    println("Ecart type : " + kmean.ecartType(0) + " " + kmean.ecartType(1) + " " + kmean.ecartType(2) + " " + kmean.ecartType(3));
    println("");
    
    kmean.algoKmean();
    kmean.verifClasses();
  }
}
