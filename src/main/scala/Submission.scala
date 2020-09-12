import scala.io.Source

case class Train(
                  month: Int,
                  shop_id: Int,
                  item_id: Int,
                  item_price: Double,
                  item_cnt: Double,
                  cat_id: Int,
                )


object Submission extends App {

  def meanItems(items: Iterable[Train]): Double = {
    val seqItems = items.toSeq
    val mean = seqItems.map(i => i.item_cnt).sum / seqItems.size
    math.max(0.0, mean)
  }


  TrainPreprocessing.read()
    .groupBy(st => (st.item_id, st.shop_id))
    .map(t => (t._1, meanItems(t._2)))
    .toSeq
    .sortBy(t => t._2)
    .foreach { case (k, m) => println(s"$k => $m") }


}