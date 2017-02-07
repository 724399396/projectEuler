import org.joda.time.DateTimeConstants

object SundayCheck extends App {

  import com.github.nscala_time.time.Imports._

  var start = DateTime.parse("1901-01-01")
  val end = DateTime.parse("2000-12-31")
  var count = 0

  while(start.isBefore(end)) {
    if (start.getDayOfWeek == DateTimeConstants.SUNDAY
      && start.getDayOfMonth == 1) {
      count = count + 1
    }
    start = start.plusDays(1)
  }

  println(count)
}
