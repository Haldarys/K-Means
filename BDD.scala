import scala.io.Source

class BDD()
{

  private val donnees:Array[Tuple] = lireDonnees("iris.data");

  def lireDonnees(fichier:String): Array[Tuple] =
  {
    var tab:Array[Tuple] = Array();
    for (line <- Source.fromFile(fichier).getLines) {
	     var ligne:Array[String] = line.split(",");
       tab = tab :+ new Tuple(ligne.dropRight(1).map(_.toDouble) , ligne(ligne.length-1));
    }
    return tab;
  }

  def getDonnees(): Array[Tuple] =
  {
    return this.donnees;
  }

  override def toString(): String =
  {
    var s:String = "BDD : \n"
    var j:Int = 0;
    for(i <- this.donnees)
    {
      s = s + j + " : " + i;
      j = j + 1;
    }
    return s;
  }


}
