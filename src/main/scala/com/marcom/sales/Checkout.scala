package com.marcom.sales

import org.scalactic._
import Accumulation._

/**
  * Created by dev on 07/04/16.
  */
class Checkout extends Requirements {

     val prices = Map("apple" -> 60 , "orange" -> 25)

     def price(items:List[String]) : Int Or One[ErrorMessage] = {
           items match {
                case null => Bad(One("the list of items must not be null"))

                case items if items.size == 0 => Bad(One("the list of items must not be empty"))

                case items => doPrice(items)
           }
     }

     def doPrice(items:List[String]) : Int Or One[ErrorMessage] = {
         if(items.exists(invalid(_))) Bad(One("the list contains invalid items")) else findPrice(items)
     }

     def findPrice(items:List[String]) = Good(items.map(prices(_)).reduce(_ + _))

     def invalid(item:String) = item == null || (item != "apple" && item != "orange")

     def price(item:String) = 1

}
