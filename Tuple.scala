import scala.math.pow
import scala.math.sqrt

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

  def distance(autre:Tuple): Double=
  {
    var res:Double = 0;
    for(i <- 0 to this.valeurs.length-1)
    {
      res = res + pow(autre.getValeur(i) - this.valeurs(i), 2);
    }
    return sqrt(res);
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
