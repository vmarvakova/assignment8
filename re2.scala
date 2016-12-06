// Part 2 about Regular Expression Matching
//==========================================

// copy over all code from re.scala

// (2a) Complete the function iterT which needs to
// be tail-recursive(!) and takes an integer n, a
// function f and an x as arguments. This function
// should iterate f n-times starting with the
// argument x.


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
//
//def replace(r: Rexp, s1: String, s2: String): String =  {
//  if((nullable(r))){
//    println("Not null")
//  }else println("null")
//  s1
//}

import scala.annotation.tailrec

@tailrec
def iterT[A](n: Int, f: A => A, x: A): A = {
  if (n==0) x
  else {
    iterT(n-1,f,f(x))
  }
}

// (2b) Complete the size function for regular
// expressions

def size(r: Rexp): Int = r match {
  case ZERO => 1
  case ONE => 1
  case CHAR(c) => 1
  case ALT(r1,r2) => 1 + size(r1) + size(r2)
  case SEQ(r1,r2) => 1 + size(r1) + size(r2)
  case STAR(r) => 1 + size(r)
}

// two testcases about the sizes of simplified and
// un-simplified derivatives

val EVIL = SEQ(STAR(STAR(CHAR('a'))), CHAR('b'))
size(iterT(20, (r: Rexp) => der('a', r), EVIL))        // should produce 7340068
size(iterT(20, (r: Rexp) => simp(der('a', r)), EVIL))  // should produce 8

// (2c) Complete the fixpoint function below.

//@tailrec
def fixpT[A](f: A => A, x: A): A = {
  if (f(x) == x) x
  else fixpT(f,f(x))
}

//def ctest(n: Long): Long =
//  if (n == 1) 1 else
//  if (n % 2 == 0) n / 2 else 3 * n + 1
//
//fixpT(ctest, 97L)
//fixpT(ctest, 871L)
//fixpT(ctest, 77031L)


def ders (s: List[Char], r: Rexp) : Rexp = s match {
  case Nil => r
  case c::s => ders(s, simp(der(c, r)))
}

def matcher(r: Rexp, s: String) : Boolean = nullable(ders(s.toList, r))

def foo(s: String): String = {
  if (matcher("a", s)) "a" else
  if (matcher("aa" ~ STAR("aa"), s)) s.take(s.length / 2)
  else "a" ++ s * 3
}
fixpT(foo, "a" * 97)
fixpT(foo, "a" * 871)

