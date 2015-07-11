package kenbot.yowfree

import kenbot.yowfree.Free.liftF
import scalaz.Functor
import scala.collection.mutable

// This example is based off the one in Runar Bjarnason's "Dead Simple Dependency Injection" talk.
// Highly recommended viewing.
//
// http://www.youtube.com/watch?v=ZasXwtTRkio


case class Key(id: String)

case class Value(value: String) {

  def +(amount: Int): Value =
    Value((value.toInt + amount).toString)
}


/**
 * Exercise 3a. Instruction set
 *
 * Implement ADT cases as subtypes of KVS, based on the following commands:
 *
 * def put(key: Key, value: Value): Unit
 * def get(key: Key): Value
 * def delete(key: Key): Unit
 *
 * There is a simple mechanical translation from here to an ADT:
 * - Arguments in the function become arguments in the data case
 *
 * - A return type of Unit is represented as a Next value. The interpreter, 
 * when it's gone and done its nasty effectful business, can continue with this value.
 *
 * - Any other return type R is represented as a function R => Next. Why? It's like we're asking
 * the interpreter for a value; if it fishes out an R value from effect-land,
 * then it can plug it in and continue.
 *
 * eg.  def foo(b: Banana): Tangerine =====> case class Foo[A](b: Banana, f: Tangerine => A)
 * def squish(b: Banana): Unit   =====> case class Squish[A](b: Banana, next: A)
 * def pluck(): Durian           =====> case class Pluck[A](f: Durian => A)
 *
 */
case class Get[Next](key: Key, f: Value => Next) extends KVS[Next]
// implement me
case class Put[Next](key: Key, value: Value, next: Next) extends KVS[Next]
// implement me
case class Delete[Next](key: Key, next: Next) extends KVS[Next]
// implement me

// ADT translation of fantasy API
sealed trait KVS[+Next] {

  /**
   * Exercise 3b. Implement map, so KVS can be a functor.
   *
   * This is also a boring mechanical translation of the data cases.  
   */
  def map[B](f: Next => B): KVS[B] = this match {
    case Get(key, h) => Get(key, x => f(h(x)))
    case Put(key, value, next) => Put(key, value, f(next))
    case Delete(key, next) => Delete(key, f(next))
  }
}

object KVS {
  type Script[A] = Free[KVS, A]

  implicit val functor: Functor[KVS] = new Functor[KVS] {
    def map[A, B](kvs: KVS[A])(f: A => B): KVS[B] = kvs map f
  }


  /**
   * Exercise 3c. Lifting functions
   *
   * Implement functions that take regular input, but return
   * KVS instances lifted into the Free monad.
   *
   * Again, this is a boring mechanical translation of our original DSL functions.
   *
   * Consider:
   * - What functions do we already know that can lift a functor into the Free monad?
   * - For Unit-returning functions like delete(), what can we pass into the "Next" slot
   * to make it type-check as a Script[Unit]?
   * - For value-returning functions like get(), what can we pass into the "Value => Next" slot
   * to make Script[Value] typecheck?
   *
   */
  def get(key: Key): Script[Value] = Free.liftF(Get(key, identity))

  def put(key: Key, value: Value): Script[Unit] = Free.liftF(Put(key, value, ()))

  def delete(key: Key): Script[Unit] = Free.liftF(Delete(key, ()))


  // Now we can write pure scripts using free monads!  Naturally, we'll exercise great
  // restraint with our newfound powers.


  val larceny: Script[Unit] = for {
    accountId <- get(Key("swiss-bank-account-id"))
    accountKey = Key(accountId.value)
    _ <- modify(accountKey, _ + 1000000)
    _ <- put(Key("bermuda-airport"), Value("getaway car"))
    _ <- delete(Key("tax-records"))
  } yield ()


  /**
   * Exercise 3d. Composing operations
   *
   * It's a bit tiresome to write get-and-put every time we want to 
   * steal large sums of money and skip the country.
   *
   * Write a composite function "modify", that Gets the value 
   * of the key, applies the modification and Puts it back to the store.
   *
   * Change the "larceny" script, so it uses "modify" rather than get-and-put 
   * to modify the account balance.
   *
   */
  def modify(key: Key, f: Value => Value): Script[Unit] = for {
    amount <- get(key)
    _ <- put(key, f(amount))
  } yield ()


  /**
   * Exercise 3e.  Pure interpreter
   *
   * Write an interpreter, that recursively accumulates results into the immutable Map, returning the 
   * final result. 
   *
   * Get, Put, and Delete should all do what you would expect. 
   *
   * Hint: Pattern matching
   */
  def interpretPure[A](script: Script[A], dataStore: Map[Key, Value]): Map[Key, Value] = script match {
    case Return(()) => dataStore
    case Return(Get(key, f: (Value => Script[_]))) => interpretPure(f(dataStore(key)), dataStore)
    case Return(Put(key, value, next: Script[_])) => interpretPure(next, dataStore + (key -> value))
    case Return(Delete(key, next: Script[_])) => interpretPure(next, dataStore - key)
    case Suspend(next) => interpretPure(Return(next), dataStore)
    case e => ???
  }


  /**
   * Exercise 3f. Effectful interpreter
   *
   * Write an interpreter, that reads each instruction in the script and 
   * mutates the given data store in place, returning unit.
   *
   * Get, Put and Delete should all do what you what expect.
   */
  def interpretImpure[A](script: Script[A], dataStore: mutable.Map[Key, Value]): Unit = script match {
    case Return(Get(key, f: (Value => Script[_]))) => interpretImpure(f(dataStore(key)), dataStore)
    case Return(Put(key, value, next: Script[_])) =>
      dataStore += (key -> value)
      interpretImpure(next, dataStore)
    case Return(Delete(key, next: Script[_])) =>
      dataStore -= key
      interpretImpure(next, dataStore)
    case Suspend(next) =>
      interpretImpure(Return(next), dataStore)
    case Return(_) =>
  }
}


