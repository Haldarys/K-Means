class Cluster(private val idDonnees:Array[Int], private var centroide:Tuple)
{

  def getCentroide(): Tuple=
  {
    return this.centroide;
  }

  def setCentroide(c:Tuple): Unit=
  {
    this.centroide = c;
  }

  override def toString(): String=
  {
    var s:String = "Cluster : \n";
    s = s + "Centroide : " + this.centroide + "\n";
    s = s + "[";
    for(i <- this.idDonnees)
    {
      s = s + i + " | ";
    }
    s = s + "]" + "\n";
    return s;
  }
}
