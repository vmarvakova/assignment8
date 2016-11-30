// Part 1 about Regular Expression Matching
//==========================================

abstract class Rexp
case object ZERO extends Rexp
case object ONE extends Rexp
case class CHAR(c: Char) extends Rexp
case class ALT(r1: Rexp, r2: Rexp) extends Rexp   // alternative
case class SEQ(r1: Rexp, r2: Rexp) extends Rexp   // sequence
case class STAR(r: Rexp) extends Rexp             // star


// some convenience for typing in regular expressions

import scala.language.implicitConversions
import scala.language.reflectiveCalls

def charlist2rexp(s: List[Char]): Rexp = s match {
  case Nil => ONE
  case c::Nil => CHAR(c)
  case c::s => SEQ(CHAR(c), charlist2rexp(s))
}
implicit def string2rexp(s: String): Rexp = charlist2rexp(s.toList)

implicit def RexpOps (r: Rexp) = new {
  def | (s: Rexp) = ALT(r, s)
  def % = STAR(r)
  def ~ (s: Rexp) = SEQ(r, s)
}

implicit def stringOps (s: String) = new {
  def | (r: Rexp) = ALT(s, r)
  def | (r: String) = ALT(s, r)
  def % = STAR(s)
  def ~ (r: Rexp) = SEQ(s, r)
  def ~ (r: String) = SEQ(s, r)
}

def nullable (r: Rexp) : Boolean = r match {
  case ZERO => false
  case ONE => true
  case CHAR(_) => false
  case ALT(r1, r2) => nullable(r1) || nullable(r2)
  case SEQ(r1, r2) => nullable(r1) && nullable(r2)
  case STAR(_) => true

}


def der (c: Char, r: Rexp) : Rexp = r match {
  case ZERO => ZERO
  case ONE => ZERO
  case CHAR(d) => if (c == d) ONE else ZERO
  case ALT(r1, r2) => ALT(der(c, r1), der(c, r2))
  case SEQ(r1, r2) =>
    if (nullable(r1)) ALT(SEQ(der(c, r1), r2), der(c, r2))
    else SEQ(der(c, r1), r2)
  case STAR(r) => SEQ(der(c, r), STAR(r))
}

def simp(r: Rexp): Rexp = r match {

  case ALT(r1, r2) => {
    val one = simp(r1)
    val two = simp(r2)
    (one, two) match {
      case (ZERO, _) => (two)
      case (_, ZERO) => (one)
      case _ => if (one == two) (one)
      else (ALT (one, two))
    }
  }
  case SEQ(r1, r2) => {
    val one = simp(r1)
    val two = simp(r2)
    (one, two) match {
      case (ZERO, _) => (ZERO)
      case (_, ZERO) => (ZERO)
      case (ONE, _) => (two)
      case (_, ONE) => (one)
      case _ => (SEQ(one,two))
    }
  }

  case r => r
}

def ders (s: List[Char], r: Rexp) : Rexp = s match {
  case Nil => r
  case c::s => ders(s, simp(der(c, r)))
}

def matcher(r: Rexp, s: String) : Boolean = nullable(ders(s.toList, r))


def replace(r: Rexp, s1: String, s2: String): String =  {
  if((nullable(r))){
    println("Not null")
  }else println("null")


  s1
  //case ALT(STAR(SEQ(CHAR('a'),CHAR('a'))),SEQ(CHAR('b'),CHAR('b')) )=> s1.replaceAllLiterally(s1,s2)
  //  case ALT(STAR(SEQ(CHAR('a'),CHAR('a'))), SEQ(CHAR('b'),CHAR('b'))) =>
  // s1.toString.replaceAll(ALT(STAR(SEQ(CHAR('a'),CHAR('a'))),SEQ(CHAR('b'),CHAR('b')) ).toString,s2)

}

val EVILs = ALT(STAR(SEQ(CHAR('a'),CHAR('a'))),SEQ(CHAR('b'),CHAR('b')) )
replace(EVILs,"aabbbaaaaaaabaaaaabbaaaabb ","c")

val EVIL = SEQ(STAR(STAR(CHAR('a'))), CHAR('b'))
println(matcher(EVIL, "a" * 1000 ++ "b"))
println(matcher(EVIL, "a" * 1000))
def time_needed[T](i: Int, code: => T) = {
  val start = System.nanoTime()
  for (j <- 1 to i) code
  val end = System.nanoTime()
  (end - start)/(i * 1.0e9)
}

for (i <- 1 to 5000001 by 500000) {
  println(i + " " + "%.5f".format(time_needed(2, matcher(EVIL, "a" * i))))
}