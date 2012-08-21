/* 
The correctness of this implementation requires only that the `ExecutorService` begins executing tasks in the order they are submitted. This enables us to safely call `innerF.get`. (You may want to try proving to yourself that this cannot deadlock)
*/
def fork[A](p: => Par[A]): Par[A] = {
  es => {
    val latch = new CountDownLatch(1)
    var result: Option[A] = None
    var innerF: Future[A] = null
    var resultF: Future[_] = null
    val f = es.submit(new Runnable { 
      def run = { 
        innerF = p(es)
        resultF = es.submit(new Runnable {
          def run = { result = Some(innerF.get); latch.countDown }    
        })
      }
    })
    new Future[A] {
      def get = { latch.await; result.get }
      def get(timeout: Long, units: TimeUnit) = {
        latch.await(timeout, units)
        result.get
      }
      def isDone = latch.getCount == 0
      def cancel(b: Boolean) = {
        isCancelled = 
          isCancelled ||
          f.cancel(b) || 
          (innerF != null && innerF.cancel(b)) || 
          (resultF != null && resultF.cancel(b))
        isCancelled
      }
      var isCancelled = false 
    }
  }
}