import nomo._

import scala.io.Source
import java.io.File
import sys.process._

object Master {
  
  val P = Configurations.stringParsers[Unit]
  implicit val u = ()
  import P._
  import Errors.msg

  val spaces = takeWhile(c => c == ' ' || c == '\t') >> unit(()) 
  val nl = single('\n')
  val line = takeWhile(_ != '\n').map(_.get) << nl
  val blank = line.filter(_.trim.isEmpty, msg("expected blankline"))
  val blanks = blank.many
  val nonblank = line.filter(!_.trim.isEmpty, msg("expected non-blank line")).map(s => if (s.trim == ".") "" else s)
  val nonblanks = nonblank.many1.map(_.mkString("\n"))

  def token[A](p: Parser[A]) = (p << spaces)
  implicit def toParser(s: String) = token(word(s) | fail(msg("expected: '" + s + "'")) >> unit(()))

  // prompt should not include the sys.error("todo"), that will be 
  case class Question(label: String, prompt: Option[String], hints: List[String], answer: String, explanation: Option[String])
  case class Suite(header: Option[String], questions: List[Question], footer: Option[String])
  case class Example(label: String, content: String)
  case class Examples(header: Option[String], get: List[Example], footer: Option[String])
  case class Chapter(label: String, examples: Examples, suites: List[Suite])
  case class Book(chapters: List[Chapter])

  val open = token("#[") | fail(msg("expected '#['"))
  val close = token("]#") | fail(msg("expected ']#'"))
  // todo: implement def untilMatch(s: F): Parser[MonotypicW[F,I]]
  // reads until the given string is a prefix of the remaining input, or until EOF
  // if this never occurs
  def section(keyword: String, trim: Boolean = false): Parser[String] = 
    spaces >> token(keyword) >>! 
    word("#[") >> (if (trim) takeWhile(_.isWhitespace) else spaces >> blanks) >> 
    takeThrough("]#").map(txt => trimBlanks(txt.get)) << 
    blanks scope (msg(keyword))  

  def namedSection[A](keyword: String, trim: Boolean = false)(f: String => Parser[A]): Parser[A] = 
    spaces >> token(keyword) >>! 
    takeUntil("#[").map(_.get.trim).
    filter(!_.isEmpty, msg("expected nonempty section name")).
    flatMap(s => word("#[") >> (if (trim) takeWhile(_.isWhitespace) else spaces >> blanks) >> f(s) << 
      spaces << close) << 
    blanks scope (msg(keyword)) 
 
  val question: Parser[Question] = 
    namedSection("question") { q => 
      section("prompt").optional ++
      section("hint", true).many ++  
      section("answer") ++
      section("explanation", true).optional map { 
      case p ++ h ++ a ++ e => Question(q,p,h,a,e) }
    }

  /** Trim leading and trailing blank lines. */
  def trimBlanks(s: String): String = { 
    val p = ((spaces >> nl >> spaces).many >> takeWhile(_ => true))
    p(p(s).get.get.reverse).get.get.reverse
  }


  val suite: Parser[Suite] = 
    section("header").optional ++
    question.many1 ++
    section("footer").optional map {
    case h ++ a ++ e => Suite(h,a,e)
  } scope (msg("suite")) 

  val example: Parser[Example] = 
    namedSection("example") { l => takeUntil("]#") map (txt => Example(l,trimBlanks(txt.get))) }

  val examples: Parser[Examples] = 
    section("header").optional ++
    example.many1 ++
    section("footer").optional map {
    case h ++ a ++ e => {
      val r = Examples(h,a,e)
      r
    }
  }
    
  val chapter: Parser[Chapter] = 
    namedSection("chapter") { t => 
      examples ++ 
      suite.many1 map { case es ++ s => Chapter(t,es,s) }
    }

  def include(baseDir: String): Parser[Chapter] = 
    (spaces >> "include") >> takeWhile(!_.isWhitespace) map ( 
    f => chapter.scope(msg("file: " + baseDir+"/"+f))(readFile(baseDir+"/"+f))) mapStatus (
    _.flatMap(_.status)) scope (msg("include")) 

  def part(baseDir: String): Parser[List[Chapter]] = 
    namedSection("part") { l => include(baseDir).delimit1(anyOf(" \t\r\n").many) << blanks } 

  def book(baseDir: String): Parser[Book] = 
    namedSection("book") { l => part(baseDir).many1.map(cs => Book(cs.flatten)) }
  
  def readFile(file: String): String = 
    Source.fromFile(file).getLines.mkString("\n")

  def write(srcBaseDir: String, includesBaseDir: String, book: Book): Unit = {
    new File(srcBaseDir + "/exercises").mkdirs
    new File(srcBaseDir + "/answers").mkdirs
    new File(srcBaseDir + "/examples").mkdirs
    val bookRoot = { 
      val p = System.getProperty("bookRoot")
      if (p eq null) includesBaseDir + "/includes/"
      else p + "/includes/"
    }
    new File(bookRoot + "/examples").mkdirs
    new File(bookRoot + "/exercises").mkdirs
    new File(bookRoot + "/answers").mkdirs
    emitHints(includesBaseDir, book) 
    emitBook(srcBaseDir, includesBaseDir, book) 
  }

  def formatSuite(s: Suite, f: Question => String): String =
    s.header.map(_ + "\n\n").getOrElse("") + 
    s.questions.map(f).mkString("\n\n") + "\n" + 
    s.footer.map(_ + "\n").getOrElse("")

  def leadingWhitespace(s: String) = s.takeWhile(_.isWhitespace)

  // returns (label, examples, exercises, answers, exercises by name, examples by name)  
  def formatChapter(chapter: Chapter): (String, String, String, String, List[(String,String)], List[(String,String)], List[(String,String)]) = {

    def needsClosing(q: Question) = 
      q.prompt.getOrElse("").trim.endsWith("{")

    def formatExercise(q: Question): String = 
      q.prompt.map(p => 
        takeUntil("{")(p).get + 
          "\n"+leadingWhitespace(p)+"  sys.error(\"todo\")").
        getOrElse("")

    def formatAnswer(q: Question): String =
      q.prompt.map(p => 
        p + "\n" + q.answer + 
        (if (needsClosing(q)) "\n"+leadingWhitespace(p) + "}" else "")).
        getOrElse(q.answer)

    (chapter.label, 
      chapter.examples.header.getOrElse("") + "\n\n" + 
        chapter.examples.get.map(_.content).mkString("\n\n") + "\n\n" + 
        chapter.examples.footer.getOrElse(""),
      chapter.suites.map(formatSuite(_, formatExercise)).mkString("\n"),
      chapter.suites.map(formatSuite(_, formatAnswer)).mkString("\n"),
      chapter.suites.flatMap(s => s.questions.map(q => (q.label, formatExercise(q)))),
      chapter.suites.flatMap(s => s.questions.map(q => (q.label, formatAnswer(q)))),
      chapter.examples.get.map(s => (s.label,s.content)))
  }

  def emitChapter(srcBaseDir: String, includesBaseDir: String, chapter: Chapter): Unit = {
    def packageDecl(sub: String) = "package fpinscala." + sub + "\n\n"
    val (label, examples, exercises, answers, exercisesByName, answersByName, examplesByName) = formatChapter(chapter)
    write(packageDecl("examples") + examples, srcBaseDir + "/examples/" + label + ".scala")
    write(packageDecl("exercises") + exercises, srcBaseDir + "/exercises/" + label + ".scala")
    write(packageDecl("answers") + answers, srcBaseDir + "/answers/" + label + ".scala")
    val bookRoot = { 
      val p = System.getProperty("bookRoot")
      if (p eq null) includesBaseDir + "/includes"
      else p + "/includes"
    }
    def programListing(s: String): String = 
      """<programlisting xml:space="preserve">%s</programlisting>""".   
      format(scala.xml.Utility.escape(s))

    exercisesByName.foreach { case (name,e) => 
      write(e, 
      bookRoot + "/exercises/" + label + "." + name) 
    }
    answersByName.foreach { case (name,e) => 
      write(e, 
      bookRoot + "/answers/" + label + "." + name) 
    }
    examplesByName.foreach { case (name,e) => 
      write(e, 
      bookRoot + "/examples/" + label + "." + name) 
    }
  }

  def emitBook(srcBaseDir: String, includesBaseDir: String, book: Book): Unit = 
    book.chapters.foreach(emitChapter(srcBaseDir,includesBaseDir,_))

  def emitHints(baseDir: String, book: Book): Unit = {
    val hints: List[(Int,List[(List[String], Int)])] = 
      book.chapters.zipWithIndex.map { case (chapter,n) => 
        (n, chapter.suites.flatMap(_.questions.map(_.hints)).zipWithIndex)
      }
    val hintsByQuestion: List[List[String]] = 
      hints.flatMap { case (n,ch) => ch.map { case (hs, ex) => formatHints(n,ex,hs) }}
    val maxLevel = hintsByQuestion.map(_.length).max 
    val hintsByLevel = 
      (0 until maxLevel) map (i => hintsByQuestion.filter(_.length > i).map(_(i)))

    hintsByLevel.zipWithIndex foreach { case (hs,i) => 
      val f = baseDir + "/hints"+i+".markdown" 
      val html = baseDir + "/hints"+i+".html" 
      val header = "% Hints - level " + i + "\n\n"
      write(header + hs.mkString("\n\n"), f)
      "pandoc --table-of-contents -s -f markdown -t html" #< new File(f) #> new File(html) !
    }
  }

  def formatHints(chapter: Int, exercise: Int, hints: List[String]): List[String] = 
  hints.map(h =>
  """
  |### %d.%d ###
  |
  |%s 
  """.stripMargin.format(chapter+1,exercise+1,h))

  def write(content: String, file: String): Unit = {
    print("writing to: " + file)
    val out = new java.io.PrintWriter(new File(file))
    try { out.print(content) }
    finally { out.close; println("...done") }
  }

  def run(file: String, srcBaseDir: String, includesBaseDir: String): Unit = {
    val b = book(new java.io.File(file).getParent).scope(msg("file: " + file))(readFile(file)).get
    write(srcBaseDir, includesBaseDir, b)
  }
  
  def main(args: Array[String]): Unit = {
    if (args.length < 1) println("supply a .book file") 
    else if (args.length == 1) run(args(0), "src/main/scala/fpinscala", "src/main/resources")
    else run(args(0), args(1), args(2))
  } 
}


