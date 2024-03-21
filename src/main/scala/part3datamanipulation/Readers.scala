package part3datamanipulation

object Readers {

  /*
   - configuration file => initial data structure
   - a DB layer
   - an HTTP layer
   - a business logic layer
  */

  case class Configuration(dbUserName: String, dbPassword: String, hist: String, port: Int, nThreads: Int, emailReplyTo: String)
  case class DbConnection(userName: String, password: String) {
    def getOrderStatus(orderId: Long): String = "dispatched" // select * from the db table and return the status of the orderID
    def getLastOrderId(userName: String): Long = 542  // select max(orderId) from table where username = username
  }
  case class HttpService(host: String, port: Int) {
    def start(): Unit = println("Server started") // this would start the actual server
  }

  //bootstrap
  val config = Configuration("daniel", "rockthejvm1!", "localhost", 1234, 8, "mahesh.kndpl@gmail.com")
  //cats Reader
  import cats.data.Reader
  val dbReader: Reader[Configuration, DbConnection] = Reader(conf => DbConnection(conf.dbUserName, conf.dbPassword))
  val dbConn = dbReader.run(config)

  //Reader[I, O]
  val orderStatusReader = dbReader.map(dbConn => dbConn.getOrderStatus(88))
  val orderStatus = orderStatusReader.run(config)

  def getLastOrderStatus(userName: String) = {
    val usersLastOrderId =
      dbReader
        .map(conn => conn.getLastOrderId(userName))
        .flatMap(lastOrderId => dbReader.map(_.getOrderStatus(lastOrderId)))

    /* Identical
    val userOrderFor = for {
      lasOrderId <- dbReader.map(_.getLastOrderId(userName))
      orderStatus <- dbReader.map(_.getOrderStatus(lasOrderId))
    } yield orderStatus*/

    usersLastOrderId.run(config)
  }

  /*
   Pattern
   1. you create the initial data structure
   2. you create a reader which specifies how that data structure will be manipulated later
   3. you can then map & flatMap the reader to produce derived information
   4. when you need the final piece of information, you call run on the reader with the initial data structure
  */

  case class EmailService(emailReplyTo: String) {
    def sendEmail(address: String, contents: String) = s"From: $emailReplyTo; to: $address >>> $contents"
  }

  //Todo
  def emailUser(userName: String, userEmail: String) = {
    //fetch the status of their last order
    // email them with the Email Service: "Your last order has the status: (status)"
    val emailReader: Reader[Configuration, EmailService] = Reader(conf => EmailService(conf.emailReplyTo))
    (for {
      lastOrderId <- dbReader.map(_.getLastOrderId(userName))
      orderStatus <- dbReader.map(_.getOrderStatus(lastOrderId))
      response <- emailReader.map(_.sendEmail(userEmail, s"Your last order has the status: $orderStatus"))
    } yield response).run(config)
  }

  // TODO 2: what programming pattern do Readers remind you of?
  // Dependency injection!

  def main(args: Array[String]): Unit = {
    println(getLastOrderStatus("daniel"))
    println(emailUser("daniel", "daniel@rtjvm.com"))
  }
}
