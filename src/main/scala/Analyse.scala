import scala.io.Source

case class Train(
                  month: Int,
                  shop_id: Int,
                  item_id: Int,
                  item_price: Double,
                  item_cnt: Double,
                  cat_id: Int,
                )


object Analyse extends App {

  val msis = TrainPreprocessing.read()
    .sortBy(s => (s.item_id, s.shop_id, s.month))

  val months = msis.groupBy(m => m.month).keys.toList.sorted
  println(s"months: ${months.size} [${months.mkString(",")}]")
  val items = msis.groupBy(m => m.item_id).keys.toList.sorted
  val il = items.take(10).mkString(",")
  val ir = items.takeRight(10).mkString(",")
  println(s"items: ${items.size} [$il ... $ir]")
  val shops = msis.groupBy(m => m.shop_id).keys.toList.sorted
  println(s"shops: ${shops.size} [${shops.mkString(",")}]")

  println()
  msis.take(10).foreach(m => println(m))
}