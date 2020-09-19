import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers

class LocalTesterSuite extends AnyFunSuite with Matchers {

  Seq(
    (213908, 45, 18975),
    (213909, 45, 14153),
    (213910, 45, 14899),
    (213911, 45, 9199),
  ).foreach { case (id, shopId, itemId) =>
    test(s"mapping shop item id to submission Id $id") {
      Util.submissionIdToShopItemId(id) mustBe ShopItemId(shopId, itemId)
      Util.shopItemIdToSubmissionId(ShopItemId(shopId = shopId, itemId = itemId)) mustBe Some(id)
    }
  }

  test("59 3838 must be 2.0") {
    val id = Util.shopItemIdToSubmissionId(ShopItemId(59, 3838)).getOrElse(-1)
    LocalTester.test(Seq(SubmissionDs(id, 2.0))) mustBe 0.0 +- 0.0001
  }
  test("25 20949 must be 410.0") {
    val id = Util.shopItemIdToSubmissionId(ShopItemId(25, 20949)).getOrElse(-1)
    LocalTester.test(Seq(SubmissionDs(id, 410.0))) mustBe 0.0 +- 0.0001
  }
  test("mean of multiple submissions") {
    ???
  }

  /*
  (ShopItemId(12,11370),List(TrainDs(33,ShopItemId(12,11370),511.219033159842,123.0,9)))
  (ShopItemId(12,11373),List(TrainDs(33,ShopItemId(12,11373),382.224936476734,2253.0,9)))
  (ShopItemId(25,20949),List(TrainDs(33,ShopItemId(25,20949),5.0,461.0,71)))
  (ShopItemId(28,20949),List(TrainDs(33,ShopItemId(28,20949),5.0,229.0,71)))
  (ShopItemId(42,20949),List(TrainDs(33,ShopItemId(42,20949),5.0,446.0,71)))
  */
}
