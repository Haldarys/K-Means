class Tuple(private val valeurs:Array[Double], private val classe:String)
{

  def getValeur(nb:Int): Double=
  {
    return this.valeurs(nb);
  }

  def getValeurs(): Array[Double]=
  {
    return this.valeurs;
  }

  def getClasse(): String=
  {
    return this.classe;
  }

  override def toString(): String =
  {
    var s:String = "Tuple : [";
    for(i <- this.valeurs)
    {
      s = s + i + " | ";
    }
    s = s + this.classe + " ]" + "\n";
    return s;
  }
}
