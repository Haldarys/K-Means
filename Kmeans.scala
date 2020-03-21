import scala.util.Random
import scala.math.pow
import scala.math.sqrt

class Kmeans(private val K:Int){

  private var clusters:Array[Cluster] = Array();
  private val base:BDD = new BDD(); //Contient un tableau de tuples
  this.creerClusters();

  def moyenneBase(nb:Int): Double= //moyenne du type de valeur choisi
  {
    val valeurs:Array[Tuple] = this.base.getDonnees();
    var total:Double = 0;
    for(tuple <- valeurs)
    {
      total = total + tuple.getValeur(nb);
    }
  return total/valeurs.length.toDouble;
  }

  def variance(nb:Int): Double=
  {
    val valeurs:Array[Tuple] = this.base.getDonnees();
    val moyenne:Double = this.moyenneBase(nb);
    var total:Double = 0;
    for(tuple <- valeurs)
    {
      total = total + pow((tuple.getValeur(nb) - moyenne), 2);
    }
    return total/valeurs.length;
  }

  def ecartType(nb:Int): Double=
  {
    return sqrt(variance(nb));
  }

  def creerClusters(): Unit =
  {
    //Crée chaque cluster avec des valeurs aléatoires
    val rd:Random = new Random();

    var sepalLength:Double = 0;
    var sepalWidth:Double = 0;
    var petalLength:Double = 0;
    var petalWidth:Double = 0;

    for(i <- 1 to this.K)
    {
      sepalLength = constantes.SLMin + rd.nextDouble() * (constantes.SLMax - constantes.SLMin);
      sepalWidth = constantes.SWMin + rd.nextDouble() * (constantes.SWMax - constantes.SWMin);
      petalLength = constantes.PLMin + rd.nextDouble() * (constantes.PLMax - constantes.PLMin);
      petalWidth = constantes.PWMin + rd.nextDouble() * (constantes.PWMax - constantes.PWMin);
      val centroide = new Tuple( Array(sepalLength, sepalWidth, petalLength, petalWidth), ""); //Randomiser les valeurs avec le min et max
      this.clusters = this.clusters :+ new Cluster(Array(), centroide);
    }
    //Remplit les clusters par rapport aux distances des centroides aux tuples
    this.assignerTupleCluster();
  }

  def moyenneCluster(cl:Cluster): Array[Double]=
  {
    val tab:Array[Int] = cl.getId();
    val donnees:Array[Tuple] = this.base.getDonnees();
    val nbVal:Int = donnees(0).getValeurs().length;
    var res:Array[Double] = new Array(nbVal);
    var total:Array[Double] = Array.fill[Double](nbVal)(0);
    for(i <- tab) //Parcoure les tuples du centroide et recupere l'id stocké dans i
    {
      for(j <- 0 to nbVal-1) //Visite les différentes valeurs du tuple pour en calculer les moyenne
      {
        total(j) = total(j) + donnees(i).getValeur(j);
      }
    }
    for(i <- 0 to nbVal-1) //Divise par le total pour calculer les moyennes
    {
      res(i) = total(i)/tab.length;
    }
    return res;
  }

  def recalculerCentroide(cl:Cluster): Unit=
  {
    val moyennes:Array[Double] = this.moyenneCluster(cl);
    cl.setCentroide(new Tuple(moyennes,""));
  }

  def actualiserClusters(): Unit=
  {
    for(i <- this.clusters){
      recalculerCentroide(i);
      //assignerTupleCluster();
    }
  }

  def assignerTupleCluster(): Unit=
  {
    //Fonction qui checke la distance de chaque tuples aux centroide et les ajoute dans le cluster correspondant
    val donnees:Array[Tuple] = this.base.getDonnees();
    var count:Int = 0;

    for(cl <- this.clusters) //Vide tout les clusters
    {
      cl.viderDonnees();
    }

    for(tuple <- donnees) //Parcoure toute les données
    {
      var idMin:Int = 0;
      var valMin:Double = this.clusters(0).getCentroide().distance(tuple);
      for(i <- 1 to this.clusters.length-1) //Teste pour chaque centroide sa distance à la donnée
      {
        var centroide:Tuple = this.clusters(i).getCentroide();
        if(centroide.distance(tuple) < valMin)
        {
          valMin = centroide.distance(tuple);
          idMin = i;
        }
      }
      this.clusters(idMin).addDonnee(idMin);
      count = count + 1;
    }
  }

  override def toString(): String=
  {
    var s:String = "Kmeans :\n";
    s = s + this.base;
    println(this.clusters);
    for(i <- this.clusters)
    {
      s = s + i
    }
    // N'affiche pas les clusters ???
    return s;
  }

}
