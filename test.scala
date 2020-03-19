object test
{
  def main(args:Array[String]): Unit =
  {
    val kmean = new Kmeans(3);
    println((1));
    println(kmean);
    println("" + kmean.moyenne(0) + " " + kmean.moyenne(1) + " " + kmean.moyenne(2) + " " + kmean.moyenne(3));
    println("" + kmean.variance(0) + " " + kmean.variance(1) + " " + kmean.variance(2) + " " + kmean.variance(3))
    println("" + kmean.ecartType(0) + " " + kmean.ecartType(1) + " " + kmean.ecartType(2) + " " + kmean.ecartType(3))
  }
}
