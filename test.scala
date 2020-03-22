object test
{
  def main(args:Array[String]): Unit =
  {
    val kmean = new Kmeans(4);
    println(kmean);
    println("" + kmean.moyenneBase(0) + " " + kmean.moyenneBase(1) + " " + kmean.moyenneBase(2) + " " + kmean.moyenneBase(3));
    println("" + kmean.variance(0) + " " + kmean.variance(1) + " " + kmean.variance(2) + " " + kmean.variance(3))
    println("" + kmean.ecartType(0) + " " + kmean.ecartType(1) + " " + kmean.ecartType(2) + " " + kmean.ecartType(3))

    kmean.algoKmean(20);
    kmean.verifClasses();
  }
}
