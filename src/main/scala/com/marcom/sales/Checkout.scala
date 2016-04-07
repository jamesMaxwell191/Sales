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

                case items => priceItems(items)
           }
     }

     def priceItems(items:List[String]) : Int Or One[ErrorMessage] = {
         if(items.exists(invalid(_))) Bad(One("the list contains invalid items")) else findPriceOf(items)
     }

     def findPriceOf(items:List[String]) = {
         val apples = priceApples(items.filter(_ == "apple").size)
          val oranges = priceOranges(items.filter(_ == "orange").size)
          Good(apples + oranges)
     }

     def invalid(item:String) = item == null || (item != "apple" && item != "orange")

     def priceApples(n:Int):Int = {
           n match {
              case  n if n%2 == 0 =>  n/2 * prices("apple")

              case n => (n -1)/2 * prices("apple") + prices("apple")
           }
     }

     def priceOranges(n:Int):Int = {
          n match {
               case  n if n%3 == 0 =>  n/3 * 2 * prices("orange")

               case n => (n -(n%3))/3 * prices("orange") + (n % 3) *prices("orange")
          }
     }


}
