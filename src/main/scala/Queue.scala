trait Queue
{
  var queue:List[Double] = List.empty
  var front: Int = -1
  var rear: Int = -1

  def enqueue(item: Double): String =
  {
    if(rear == -1 && front == -1)
    {
      front = front + 1
      rear = rear + 1
      queue = queue ::: List(item)
      "Item is enqueued."
    }
    else
    {
      rear = rear + 1
      queue = queue ::: List(item)
      "Item is enqueued.."
    }
  }

  def dequeue(item: Int): String =
  {
    if(front == -1 && rear == -1)
    {
      "Queue Underflow.."
    }
    else if(front == rear )
    {
      queue = queue.drop(item)
      front = -1
      rear = -1
      "Item dequeued.."
    }
    else
    {
      queue = queue.drop(1)
      front = front + 1
      "Item dequeued.."
    }
  }
  def getQueue: List[Double] =
  {
    queue
  }
}

class DoubleQueue extends Queue
{
  override def enqueue(item: Double): String =
  {
    if(rear == -1 && front == -1)
    {
      front = front + 1
      rear = rear + 1
      val ItemDouble = 2 * item
      queue = queue ::: List(ItemDouble)
      "Item is enqueued.."
    }
    else
    {
      rear = rear + 1
      val ItemDouble = 2 * item
      queue = queue ::: List(ItemDouble)
      "Item is enqueued.."
    }
  }
}
class SquareQueue extends Queue
{
  override def enqueue(item: Double): String =
  {
    if(rear == -1 && front == -1)
    {
      front = front + 1
      rear = rear + 1
      val ItemSquare = item * item
      queue = queue ::: List(ItemSquare)
      "Item is enqueued.."
    }
    else
    {
      rear = rear + 1
      val ItemSquare = item * item
      queue = queue ::: List(ItemSquare)
      "Item is enqueued.."
    }
  }
}
object QueueObject extends App
{
  val Object1 = new DoubleQueue
  val Object2 = new SquareQueue
  println(Object1.enqueue(2))
  println(Object1.enqueue(3))
  println(Object1.enqueue(4))
  println(Object1.getQueue)
  println(Object2.enqueue(5))
  println(Object2.enqueue(6))
  println(Object2.enqueue(7))
  println(Object2.getQueue)
  println(Object2.dequeue(5))
  println(Object2.getQueue)
}
