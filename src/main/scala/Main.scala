import scala.math._

object Main extends App {

  import OrgsSorter._

  var params = parseCmdLineParams
  val orgs = io.Source.fromFile(params.srcFile).getLines().drop(1).map(lineToOrganizationData).toList

  val sortedOrgs = orgs.sortWith(compareWithTarget(params.length, params.width))

  println(sortedOrgs take 3 mkString "\n")

  /**
   * Широту и долготу ожидает в градусах, переводит в радианы
   */
  def lineToOrganizationData(line: String) = {
    try {
      val items = line.split('|')
      OrganizationData(items(0), items(1), toRadians(items(2) toDouble), toRadians(items(3) toDouble))
    } catch {
      case e: Exception =>
        println(s"cant parse organization data form src file, expected something like: 'Пульс, сеть аптек|Лунная, 8|83.055853116107|54.749178597202' but was: '$line'")
        throw e
    }
  }

  /**
   * Широту и долготу ожидает в градусах, переводит в радианы
   */
  def parseCmdLineParams = {
    try {
      CmdLineParams(args(0), toRadians(args(1) toDouble), toRadians(args(2) toDouble))
    } catch {
      case e: Exception =>
        println(s"cant parse command-line parameters, expected something like: 'apteki.csv 82.933484 55.091131' but was: '${args.mkString(" ")}'")
        throw e
    }
  }
}

/**
 * @param length долгота в радианах
 * @param width широта в радианах
 */
case class CmdLineParams(srcFile: String, length: Double, width: Double)

/**
 * @param length долгота в радианах
 * @param width широта в радианах
 */
case class OrganizationData(name: String, address: String, length: Double, width: Double) {
  override def toString = s"$name|$address"
}

object OrgsSorter {

  val RADIUS_OF_THE_EARTH = 6372795;

  /**
   * @return ближайшую организацию к указанным долготе и широте
   */
  def compareWithTarget(targetLength: Double, targetWidth: Double) = {
    (o1: OrganizationData, o2: OrganizationData) =>
      val dist1 = calcDistance(o1.length, o1.width, targetLength, targetWidth)
      val dist2 = calcDistance(o2.length, o2.width, targetLength, targetWidth)
      dist1 < dist2
  }

  /**
   * Координаты ожидает в радианах
   */
  def calcDistance(length1: Double, width1: Double, length2: Double, width2: Double) = {
    RADIUS_OF_THE_EARTH * acos(sin(width1) * sin(width2) + cos(width1) * cos(width2) * cos(length1 - length2))
  }

}