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

                case items => priceItems(items)
           }
     }

     def priceItems(items:List[String]) : Int Or One[ErrorMessage] = {
         val m = items.groupBy(i => i).mapValues(_.size)
         val result = m.foldLeft[Int Or One[ErrorMessage]](Good(0)){ (i,t) => i match {
           case Bad(x) => Bad(x)
           case Good(x) => t match {
             case (null,c) => Bad(One("the list contains null values"))
             case ("apple",c) => Good(x + priceApples(c))
             case ("orange",c) => Good(x + priceOranges(c))
             case ( str,c) => Bad(One(s"unknown fruit $str"))
           }
         }


         }

         result
     }

     def priceApples(n:Int):Int = {
           n match {
              case  n if n%2 == 0 =>  n/2 * prices("apple")

              case n => (n -1)/2 * prices("apple") + prices("apple")
           }
     }

     def priceOranges(n:Int):Int = {
          n match {
               case  n if n%3 == 0 =>  n/3 * 2 * prices("orange")

               case n => (n -(n%3))/3 * 2 * prices("orange") + (n % 3) *prices("orange")
          }
     }


}
