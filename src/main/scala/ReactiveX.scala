import java.io.IOException

import rx.lang.scala.Notification.{OnCompleted, OnError, OnNext}
import rx.lang.scala.observables.SyncOnSubscribe
import rx.lang.scala.subjects.{BehaviorSubject, PublishSubject, ReplaySubject}
import rx.lang.scala.{Notification, Observable}
import utils._

import scala.concurrent.duration._

object ObservablePLay extends App {

  print(
    Observable.just(1, 2, 3, 4, 5),
    "just"
  )

  print(
    Observable.interval(200 millis).take(5),
    "interval", 2 seconds
  )

  print(
    Observable.from(1 to 5).slidingBuffer(4, 2),
    "from"
  )


  def printObservable[T](o: Observable[T]): Unit = {
    o.materialize.subscribe(n => n match {
      case OnNext(v) => println("Got value " + v)
      case OnCompleted => println("Completed")
      case OnError(err) => println("Error: " + err.getMessage)
    })
  }

  val o1 = Observable.interval(100 millis).take(3)
  val o2 = Observable.error(new IOException("Oops"))
  printObservable(o1)
  printObservable(o2)
  Thread.sleep(500)
}


object PublishSubjectPlay extends App {
  // Subject that, once an Observer has subscribed, emits all subsequently observed items to the subscriber.
  val subject = PublishSubject[String]()

  print(subject, "Publish Subject - Observer 1")
  // observer1 will receive all onNext and onCompleted events
  subject.onNext("one")
  subject.onNext("two")

  print(subject, "Publish Subject - Observer 2")
  // observer2 will only receive "three" and onCompleted
  subject.onNext("three")
  subject.onCompleted()
}

object ReplaySubjectPlay extends App {
  // Subject that, once an Observer has subscribed, emits all so far observed items to the subscriber.
  val subject = ReplaySubject[String]()

  print(subject, "Replay Subject - Observer 1")
  // observer1 will receive all onNext and onCompleted events
  subject.onNext("one")
  subject.onNext("two")

  print(subject, "Replay Subject - Observer 2")
  // observer2 will receive all onNext and onCompleted events
  subject.onNext("three")
  subject.onCompleted()
}

object BehaviorSubjectPlay extends App {
  // On subscribe emits the item most recently emitted by the source Observable and then continues to emit any other items emitted later
  val subject = BehaviorSubject[String]()

  print(subject, "Behavior Subject - Observer 1")
  // observer1 will receive all onNext and onCompleted events
  subject.onNext("one")
  subject.onNext("two")

  print(subject, "Behavior Subject - Observer 2")
  // observer2 will receive previous "two" and all subsequent events
  subject.onNext("three")
  subject.onCompleted()
}

object ObservableCreateSyncOnSubscribeStateLess extends App {
  val o = Observable.create(SyncOnSubscribe.stateless(
    next = () => Notification.OnNext(math.random),
    onUnsubscribe = () => println("I have stopped generating random numbers for this subscriber")
  ))
  print(o.take(10))
}

object ObservableCreateSyncOnSubscribeWithState extends App {
  val o = Observable.create(SyncOnSubscribe(() => 0)(
    next = i => {
      if (i < 2)
      // Check if the state has reached 2 yet, if not, we emit the current state and add 1 to the state
        (Notification.OnNext(i), i + 1)
      else
      // Otherwise we signal completion
        (Notification.OnCompleted, i)
    },
    onUnsubscribe = i => println(s"I have stopped generating random numbers for this subscriber, final state: $i")
  ))
  print(o)
}

object utils {

  def print[T](observable: Observable[T], context: String = "Item", sleep: Duration = 1 second) = {
    observable.subscribe(item => println(s"$context - $item"))
    Thread.sleep(sleep.toMillis)
  }
}
