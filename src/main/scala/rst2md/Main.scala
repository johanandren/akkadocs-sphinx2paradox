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
case class IncludeCode(name: String, path: String, tag: String, content: Seq[Block] = Seq.empty, options: Options = NoOpt) extends Block with BlockContainer[IncludeCode]
object IncludeCode {
  def apply(spec: String, include: Option[String]): IncludeCode = {
    val (path, hash) = spec.span(_ != '#')
    val tag = include.getOrElse(hash.dropWhile(_ == '#')).replaceAll(".*,", "") // FIXME: include imports
    IncludeCode(new JFile(path).getName, path, tag)
  }
}
case class IncludeCode2(content: Seq[Block], options: Options = NoOpt) extends Block with BlockContainer[IncludeCode2]
case class TocTree(maxDepth: Option[String], toc: Seq[String], content: Seq[Block] = Seq.empty, options: Options = NoOpt) extends Block with BlockContainer[TocTree]

object ApiRef {
  sealed trait Role {
    def name: String = toString.toLowerCase
  }
  case object Class extends Role
  case object Meth extends Role
  case object Obj extends Role
  case object Mod extends Role
  case object Func extends Role
  case object Doc extends Role
  val roles = List(Class, Meth, Obj, Mod, Func, Doc)
}
case class ApiRef(name: String, role: ApiRef.Role, options: Options = NoOpt) extends Span

object ParadoxMarkdown extends RendererFactory[MarkdownWriter] {
  override def fileSuffix: String = "md"
  override def newRenderer(output: Output, root: Element, render: Element => Unit, styles: StyleDeclarationSet): (MarkdownWriter, (Element) => Unit) = {
    val out = new MarkdownWriter(output.asFunction, render, root, ". ")
    (out, renderElement(out))
  }

  private def unwrapBlocks(blocks: Seq[Block]): Seq[Element] = blocks match {
    case SpanSequence(content, _) :: Nil => content
    case Paragraph(content, opt) :: Nil => content
    case ForcedParagraph(content, opt) :: Nil => content
    case other => other
  }

  private def renderElement (out: MarkdownWriter)(elem: Element): Unit = {
    elem match {

      case RootElement(content) =>
        if (content.nonEmpty) out << content.head <<| content.tail

      case Title(content, _) =>
        val (anchor, spans) = content.partition(_.isInstanceOf[InternalLinkTarget])
        out << anchor <<| "# " << spans

      case Header(level, content, _) =>
        val (anchor, spans) = content.partition(_.isInstanceOf[InternalLinkTarget])
        out <<| anchor <<| ("#" * level) << " " << spans

      case Section(header, content, _) =>
        out << header <<| content

      case Paragraph(content, _) =>
        out <|; out << content

      case SpanSequence(content, _) =>
        out << content

      case Emphasized(content, _) =>
        out << "*" << content << "*"

      case Strong(content, _) =>
        out << "**" << content << "**"

      case QuotedBlock(content,attr,_) =>
        content.foreach { block =>
          out <<| ">" << block
        }

      case BulletList(content, _, _) =>
        out << content

      case BulletListItem(content, _, _) =>
        out.indentWith(" * ") { out << unwrapBlocks(content) }

      case EnumList(content,format,start,opt) =>
        //FIXME("ol", opt, ("class", format.enumType.toString.toLowerCase), ("start", noneIfDefault(start,1))) <<|> content <<| "</ol>"
        out << content

      case EnumListItem(content, _, _, _) =>
        // TODO not correct yet
        out.indentWith(" # ") { out << unwrapBlocks(content) }

      case DefinitionList(content, _) =>
        out << content

      case DefinitionListItem(term, definition, _) =>
        //out << s"TERM=[$term] DEF=[$definition]"
        out << term << " " << unwrapBlocks(definition)

      case CodeBlock(language, content, _) =>
        out <<| "```" << language.replace("^none$", "") <<| content <<| "```"

      case LiteralBlock(content, _) =>
        out <<| "```" <<| content <<| "```"

      case Literal(content, _) =>
        out << "`" << content << "`"

      case Text(text, _) =>
        out << text

      case InternalLinkTarget(opt) =>
        out << "<a id=\"" << opt.id.get << "\"></a>"


      case InternalLink(content, ref, _, _) =>
        out << "@ref:[" << content << "](" << ref << ")"

      case ExternalLink(content, url, _, _) =>
        out << "[" << content << "](" << url << ")"

      // Renders note and warnings
      case TitledBlock(title, content, _) =>
        out <<| "**" << title << ":**" << content


      // our custom thingies/not covered by md
      case IncludeCode(name, path, tag, _, _) =>
        out <<| "@@snip [" << name << "](" << path << ") { #" << tag << " }"

      case IncludeCode2(content, _) =>
        // TODO not implemented
        out << "@@snip [Todo.scala](" << content << "{ #todo }"

      case TocTree(maxDepth, toc, _, _) =>
        // FIXME: This needs to do something similar to @@toc but with a list of pages to traverse.
        // out <<| "@@toc" + maxDepth.fold("")(depth => s"{ depth=$depth }")
        out <<| "@@@ index" <|;
        toc foreach { entry => out <<| s"* [$entry]($entry.md)" }
        out <|; out <<| "@@@"

      case ApiRef(name, role, _) =>
        // FIXME: Custom directive or link to ScalaDoc?
        out << "`" << name << "`"

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

class MarkdownWriter(out: String => Unit, render: Element => Unit, root: Element, indentItem: String)
  extends TextWriter(out, render, root, indentItem, newLine = "\n") {

    /**
     * Executes the specified block while temporarily
     * shifting the indentation level. First line uses
     * the provided indent token.
     */
    def indentWith(indent: String)(block: => Unit): Unit = {
       val oldIndent = Indent.current
       <<|(indent)
       Indent.current = "\n" + (" " * indent.length)
       block
       Indent.current = oldIndent
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
    import laika.parse.rst.TextRoles.Parts.{ field => textRoleField }
    import laika.parse.rst.ext._

    val blockDirectives = List(
      BlockDirective("includecode") {
        (argument(withWS = true) ~ optField("include"))(IncludeCode(_, _))
      },
      BlockDirective("toctree") {
        (optField("maxdepth") ~ content[Seq[String]](c => Right(c.split("\n"))))(TocTree(_,_))
      },
      BlockDirective("code-block") {
        (argument(withWS = true) ~ spanContent)(CodeBlock(_, _))
      },
      BlockDirective("includecode2") {blockContent.map(IncludeCode2(_))}
    )

    val textRoles = List(
      TextRole("ref", "")(textRoleField("ref")) { (base, content) =>
        val refMatch = """(.*)( <(.*)>)""".r
        val (text, id) = content match {
          case refMatch(refText, _, refId) => (refText, refId)
          case _ => (content, content)
        }
        InternalLink(List(Text(text)), id, None, NoOpt)
      }
    ) ++ ApiRef.roles.map { role =>
      TextRole(role.name, "")(textRoleField(role.name)) { (_, name) => ApiRef(name, role) }
    }

    ReStructuredText withBlockDirectives(blockDirectives: _*) withTextRoles(textRoles: _*)
  }

  Transform from akkaRst to ParadoxMarkdown fromDirectory srcDir.toJava toDirectory destDir.toJava

}
