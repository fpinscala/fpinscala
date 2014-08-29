  /*
   * Exercise 5: Implement a non-blocking read from an asynchronous file channel.
   * We'll just give the basic idea - here, we construct a `Future`
   * by reading from an `AsynchronousFileChannel`, a `java.nio` class
   * which supports asynchronous reads.
   */

  import java.nio._
  import java.nio.channels._

  def read(file: AsynchronousFileChannel,
           fromPosition: Long,
           numBytes: Int): Par[Either[Throwable, Array[Byte]]] =
    Par.async { (cb: Either[Throwable, Array[Byte]] => Unit) =>
      val buf = ByteBuffer.allocate(numBytes)
      file.read(buf, fromPosition, (), new CompletionHandler[Integer, Unit] {
        def completed(bytesRead: Integer, ignore: Unit) = {
          val arr = new Array[Byte](bytesRead)
          buf.slice.get(arr, 0, bytesRead)
          cb(Right(arr))
        }
        def failed(err: Throwable, ignore: Unit) =
          cb(Left(err))
      })
    }
  
  // note: We can wrap `read` in `Free[Par,A]` using the `Suspend` constructor
