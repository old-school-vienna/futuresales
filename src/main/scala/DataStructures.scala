case class TrainDs(
                    month: Int,
                    shopItemId: ShopItemId,
                    itemPrice: Double,
                    itemCnt: Double,
                    catId: Int,
                  )

case class ShopItemId(
                       shopId: Int,
                       itemId: Int,
                     )

case class TestDs(
                   id: Int,
                   shopItemId: ShopItemId,
                 )


case class SubmissionDs(
                         id: Int,
                         itemCnt: Double,
                       )

