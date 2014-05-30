package code 
package snippet 

import net.liftweb.util._
import net.liftweb.common._
import java.util.Date
import code.lib._
import Helpers._
import net.liftweb.http.S

class HelloWorld {
  lazy val date: Box[Date] = DependencyFactory.inject[Date] // inject the date
  def howdy = {
    Thread.sleep(1000)
    S.warning("Slow!")
    "#time *" #> date.map(_.toString)
  }

  def fastRender = {
    S.warning("Fast render!")
    "#message *" #> "Made it quick"
  }

}

