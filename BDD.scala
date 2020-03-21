import scala.io.Source

class BDD()
{

  private val donnees:Array[Tuple] = lireDonnees("iris.data");

  def lireDonnees(fichier:String): Array[Tuple] = //Renvoie les données du fichier sous la forme d'un tableau de Tuples
  {
    var tab:Array[Tuple] = Array();
    for (line <- Source.fromFile(fichier).getLines) {
	     var ligne:Array[String] = line.split(","); //Récupère une ligne et la transforme en un tableau de valeurs
       tab = tab :+ new Tuple(ligne.dropRight(1).map(_.toDouble) , ligne(ligne.length-1)); //Crée le tuple et l'ajoute au tableau en séparant les valeurs du nom de la classe
    }
    return tab;
  }

  def getDonnees(): Array[Tuple] = //Renvoie le tableau de données entier
  {
    return this.donnees;
  }

  def getDonnee(nb:Int): Tuple=
  {
    return this.donnees(nb);
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
