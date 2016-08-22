package rst2md

import better.files._
import java.io.{File => JFile}

import laika.api.Transform
import laika.factory.RendererFactory
import laika.io.Output
import laika.parse.css.Styles.StyleDeclarationSet
import laika.parse.markdown.Markdown
import laika.parse.rst._
import laika.render.TextWriter
import laika.tree.Elements._
import laika.io._

import scala.io.Codec

// rst extensions
case class Note (content: Seq[Block], options: Options = NoOpt) extends Block with BlockContainer[Note]
case class Warning(content: Seq[Block], options: Options = NoOpt) extends Block with BlockContainer[Warning]
case class IncludeCode(content: Seq[Block], options: Options = NoOpt) extends Block with BlockContainer[IncludeCode] {

  val include: String = {
    content.flatMap { case Paragraph(spans, _) =>
      spans.flatMap { case Text(text, _) =>
        text
      }
    }.mkString("")
  }

  private val tagPos = include.lastIndexOf('#')
  val path = include.take(tagPos)
  val tag = include.drop(tagPos + 1)

}
case class IncludeCode2(content: Seq[Block], options: Options = NoOpt) extends Block with BlockContainer[IncludeCode2]


object ParadoxMarkdown extends RendererFactory[TextWriter] {
  override def fileSuffix: String = "md"
  override def newRenderer(output: Output, root: Element, render: Element => Unit, styles: StyleDeclarationSet): (TextWriter, (Element) => Unit) = {
    val out = new TextWriter(output.asFunction, render, root, ". ")
    (out, renderElement(out))
  }

  private def renderElement (out: TextWriter)(elem: Element): Unit = {
    elem match {

      case RootElement(content) =>
        out << content

      case Title(content, _) =>
        out << "# " << content << "\n\n"

      case Header(level, content, _) =>
        val prefix = "##" * level + " "
        out << prefix << content << "\n\n"

      case Section(header, content, _) =>
        out << header << content

      case Paragraph(content, _) =>
        out << content << "\n\n"

      case Strong(content, _) =>
        out << "**" << content << "**"

      case QuotedBlock(content,attr,_)    =>
        content.foreach { block =>
          out << ">" << block << "\n"
        }

      case BulletListItem(content, _, _) =>
        out << " * " << content << "\n"

      case EnumListItem(content, _, _, _) =>
        // TODO not correct yet
        out << "**" << content.head << "** " << content.tail << "\n"

      case CodeBlock(language, content, _) =>
        out << "```\n" << content << "\n```\n\n"

      case LiteralBlock(content, _) =>
        out << "```\n" << content << "\n```\n\n"

      case Literal(content, _) =>
        out << "`" << content << "`"

      case Text(text, _) =>
        out << text

      case InternalLinkTarget(_) =>
        // TODO what do we do with these and the toc?
        out << ""


      case InternalLink(content, ref, _, _) =>
        out << "@ref:[" << content << "](" << ref << ")"

      case ExternalLink(content, url, _, _) =>
        out << "[" << content << "](" << url << ")"



      // our custom thingies/not covered by md
      case inc: IncludeCode =>
        val name = inc.path.drop(inc.path.lastIndexOf('/') + 1)
        out << "@@snip [" << name << "](" << inc.path << ") { #" << inc.tag << " }\n\n"

      case IncludeCode2(content, _) =>
        // TODO not implemented
        out << "@@snip [Todo.scala](" << content << "{ #todo }"

      case Note(content, _) =>
        out << "**Note:**\n" << content << "\n\n"

      case Warning(content, _) =>
        out << "**Warning:**\n" << content << "\n\n"


      // catchalls
      case sc: SpanContainer[_]           =>
        out << "Missing conversion: " << sc.getClass.toString
        out << sc.content

      case tc: TextContainer              =>
        out << "Missing conversion: " << tc.getClass.toString
        out << tc.content

      case ec: ElementContainer[_,_]      =>
        out << "Missing conversion: " << ec.getClass.toString
        out << ec.content

      case e                              =>
        out << s"unknown element: $e"
    }
  }
}



object Main extends App {
  if (args.length != 2) {
    println("Usage: rst2md srcdir destdir")
    sys.exit(2)
  }

  val srcDir = args(0).toFile
  val destDir = args(1).toFile
  assert(srcDir.isDirectory)
  assert(destDir.isDirectory || !destDir.exists)
  if (destDir.exists) destDir.delete()
  destDir.createDirectories()


  implicit val codec:Codec = Codec.UTF8


  val akkaRst = {

    import laika.parse.rst.Directives._
    import laika.parse.rst.Directives.Parts._

    import laika.parse.rst.TextRoles._
    import laika.parse.rst.ext._

    ReStructuredText withBlockDirectives(
      BlockDirective("note") {blockContent.map(Note(_))},
      BlockDirective("warning") {blockContent.map(Warning(_))},
      BlockDirective("includecode") {blockContent.map(IncludeCode(_))},
      BlockDirective("includecode2") {blockContent.map(IncludeCode2(_))}
    )
  }

  Transform from akkaRst to ParadoxMarkdown fromDirectory srcDir.toJava toDirectory destDir.toJava

}