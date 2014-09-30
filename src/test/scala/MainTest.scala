import org.scalatest._

class MainTest extends FlatSpec with Matchers {

  import Main._
  import OrgsSorter._

  "OrgsSorter" should "with some organization as target parameter return this organization at first place" in {

    val data = """Адонис, ООО, аптека|Зорге, 25|82.902529628935|54.935396634793
                 |Аптека, ООО Вектор-Фарм|Кольцово пос, 12а|83.185112379498|54.940230106559
                 |Аптека №163, ОАО|Кольцово пос, 25а|83.189865171123|54.943291466784
                 |Аптека, ООО Медовая компания Сибирь|Геодезическая, 23|82.911829340008|54.986635569741
                 |Сибирское Здоровье, ООО, аптека|Плахотного, 74/2|82.847964420681|54.986299701344
                 |Сибирь-Фарм, аптека|Гидромонтажная, 50|82.978398028667|54.860945509599
                 |Витана, аптечная сеть|Комсомольская, 19|83.302859560299|54.642989879376
                 |Витана, аптечная сеть|Индустриальный микрорайон, 24|83.302476474196|54.625007087376""".stripMargin

    val orgs = data.split('\n').map(lineToOrganizationData)
    val sibFarm = orgs(5)
    val sortedOrgs = orgs.sortWith(compareWithTarget(sibFarm.length, sibFarm.width))

    sortedOrgs(0).name should be(sibFarm.name)
  }

}
