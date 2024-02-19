//> using scala 3.3
import scala.math.Ordering.Implicits.seqOrdering

@main
def runTests() =
  val rates = Seq(Rate("M1", "Military"), Rate("M2", "Military"), Rate("S1", "Senior"), Rate("S2", "Senior"))
  val cabinPrices = Seq(
    CabinPrice("CA", "M1", 200.00),
    CabinPrice("CA", "M2", 250.00),
    CabinPrice("CA", "S1", 225.00),
    CabinPrice("CA", "S2", 260.00),
    CabinPrice("CB", "M1", 230.00),
    CabinPrice("CB", "M2", 260.00),
    CabinPrice("CB", "S1", 245.00),
    CabinPrice("CB", "S2", 270.00)
  )
  val result = Problem1.getBestGroupPrices(rates, cabinPrices)
  println("Best Cabin Prices:")
  println(result.mkString("\n"))

  val promotions = Seq(
    Promotion("P1", Seq("P3")),       // P1 is not combinable with P3
    Promotion("P2", Seq("P4", "P5")), // P2 is not combinable with P4 and P5
    Promotion("P3", Seq("P1")),       // P3 is not combinable with P1
    Promotion("P4", Seq("P2")),       // P4 is not combinable with P2
    Promotion("P5", Seq("P2"))        // P5 is not combinable with P2
  )
  val valid = Problem2.allCombinablePromotions(promotions)
  println("\nAll Promotion Combinations:")
  println(valid.mkString("\n"))

  val validP1 = Problem2.combinablePromotions("P1", promotions)
  println("\nPromotion Combinations for promotionCode=”P1”:")
  println(validP1.mkString("\n"))

  val validP3 = Problem2.combinablePromotions("P3", promotions)
  println("\nPromotion Combinations for promotionCode=”P3”:")
  println(validP3.mkString("\n"))

object Problem1:
  def getBestGroupPrices(rates: Seq[Rate], prices: Seq[CabinPrice]): Seq[BestGroupPrice] =
    val allGroupPrices = for
      // Get all combinations of Rate and CabinPrice
      rate <- rates
      cp <- prices
      // where the rateCodes are the same
      if rate.rateCode == cp.rateCode
    yield BestGroupPrice(cp.cabinCode, rate.rateCode, cp.price, rate.rateGroup)

    // We want the best price per group and cabin type, so group by those fields
    val bestGroupPrices = allGroupPrices
      .groupBy(p => (p.cabinCode, p.rateGroup))
      // then sort each group by price and get the lowest
      .map { case (key, group) => group.sortBy(_.price).head }
    // sort the resulting list by price
    bestGroupPrices.toList.sortBy(_.price)

case class Rate(rateCode: String, rateGroup: String)
case class CabinPrice(cabinCode: String, rateCode: String, price: BigDecimal)
case class BestGroupPrice(cabinCode: String, rateCode: String, price: BigDecimal, rateGroup: String)

object Problem2:
  def allCombinablePromotions(allPromotions: Seq[Promotion]): Seq[PromotionCombo] =
    // get all possible combinations of promos
    val all = (2 to allPromotions.length)
      .flatMap(allPromotions.combinations)

    // Filter out invalid combinations
    val valid = all.filter: combo =>
      // get invalid codes for the promos
      val invalidCodes = combo.flatMap(_.notCombinableWith).distinct

      // combo is valid if none of its promo codes are in the invalid list
      // i.e. all of its promo codes are not in the invalid list
      combo.forall(promo => !invalidCodes.contains(promo.code))

    // We now have all possible combinations of valid promos, but some are subsets of others, so remove those
    // i.e. Keep the ones that are not a subset
    val noSubSets = valid.filterNot: combo =>
      // sets are always subsets of themselves, so only check for subset if combo != otherCombo
      valid.exists(otherCombo => combo != otherCombo && combo.toSet.subsetOf(otherCombo.toSet))

    // convert to PromotionCombo
    noSubSets
      .map(promos => PromotionCombo(promos.map(_.code)))
      .sortBy(_.promotionCodes)

  def combinablePromotions(promotionCode: String, allPromotions: Seq[Promotion]): Seq[PromotionCombo] =
    val validCombos = allCombinablePromotions(allPromotions)
    validCombos.filter(_.promotionCodes.contains(promotionCode))

case class Promotion(code: String, notCombinableWith: Seq[String])
case class PromotionCombo(promotionCodes: Seq[String])