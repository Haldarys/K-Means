object test
{
  def main(args:Array[String]): Unit =
  {
    val kmean = new Kmeans(3);
    println((1));
    println(kmean);
    println("" + kmean.moyenneBase(0) + " " + kmean.moyenneBase(1) + " " + kmean.moyenneBase(2) + " " + kmean.moyenneBase(3));
    println("" + kmean.variance(0) + " " + kmean.variance(1) + " " + kmean.variance(2) + " " + kmean.variance(3))
    println("" + kmean.ecartType(0) + " " + kmean.ecartType(1) + " " + kmean.ecartType(2) + " " + kmean.ecartType(3))

    val t1 = new Tuple(Array(0.5,0.5,0.5,0.5),"");
    val cl = new Cluster(Array(1,2,3,4,5),t1);
    val moy:Array[Double] = kmean.moyenneCluster(cl);
    for(i <- 0 to 3)
    {
      println(moy(i));
    }
  }
}
