package com.marcom.sales

import org.scalatest.{FunSuite, Matchers, WordSpec}
import org.scalactic._

/**
  * Created by dev on 07/04/16.
  */
class CheckoutTest extends WordSpec with Matchers {

     "a checkout" should {
          "produce error on null items" in new CheckoutFixture {
               val items:List[String] = null
               val result = checkout.price(items)
               result should be (Bad(One("the list of items must not be null")))
          }
          "produce error on empty list" in new CheckoutFixture {
               val items = List[String]()
               val result = checkout.price(items)
               result should be (Bad(One("the list of items must not be empty")))
          }
          "produce error on an invalid item" in new CheckoutFixture {
               val items = List("plum")
               val result = checkout.price(items)
               result should be (Bad(One("the list contains invalid items")))
          }
          "generate a valid price" in new CheckoutFixture {
               val items = List("apple","orange")
               val result = checkout.price(items)
               result should be (Good((85)))
          }
          "generate a valid discount price for an even basket of apples" in new CheckoutFixture {
               val items = List("apple","apple")
               val result = checkout.price(items)
               result should be (Good((60)))
          }
          "generate a valid discount price for an odd basket of apples" in new CheckoutFixture {
               val items = List("apple","apple","apple")
               val result = checkout.price(items)
               result should be (Good((120)))
          }
          "generate a valid discount price for a basket of oranges" in new CheckoutFixture {
               val items = List("orange","orange","orange")
               val result = checkout.price(items)
               result should be (Good((50)))
          }
          "generate a valid discount price for an odd basket of oranges" in new CheckoutFixture {
               val items = List("orange","orange")
               val result = checkout.price(items)
               result should be (Good((50)))
          }

     }

}

trait CheckoutFixture {
    val checkout = new Checkout
}
