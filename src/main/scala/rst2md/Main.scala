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

import scala.collection.mutable.StringBuilder
import scala.io.Codec
import scala.util.Try

// rst extensions
case class IncludeCode(path: String, tag: Seq[String], language: Option[String], content: Seq[Block] = Seq.empty, options: Options = NoOpt) extends Block with BlockContainer[IncludeCode]
object IncludeCode {
  def apply(spec: String, include: Option[String], exclude: Option[String], language: Option[String]): IncludeCode = {
    val (path, hash) = spec.span(_ != '#')
    val tag = include.orElse(Some(hash.dropWhile(_ == '#'))).map(_.replaceAll(".*,", ""))
    IncludeCode(path, tag.toList.flatMap(_.split(",")), language)
  }

  def literal(path: String, language: Option[String]): IncludeCode =
    IncludeCode(path, Seq.empty, language)
}
case class TocTree(maxDepth: Option[String], toc: Seq[String], content: Seq[Block] = Seq.empty, options: Options = NoOpt) extends Block with BlockContainer[TocTree]
case class Figure(path: String, scale: Option[String], align: Option[String], content: Seq[Block] = Seq.empty, options: Options = NoOpt) extends Block with BlockContainer[Figure]

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

object RefsCache {
  val refsDir = new JFile(".refs")

  def put(id: String, title: String) = {
    refsDir.mkdirs
    new java.io.FileWriter(new JFile(refsDir, id)) { write(title); close }
  }

  def get(id: String) = {
    val title = Try {
      scala.io.Source.fromFile(new JFile(refsDir, id)).getLines.mkString
    }
    title getOrElse id
  }
}

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

  private def cacheRefTitle(anchors: Seq[Span], content: Seq[Span]): Unit = anchors.headOption match {
    case Some(InternalLinkTarget(Id(id))) =>
      val title = content match {
        case List(Text(text, _)) => text
      }
      RefsCache.put(id, title)
    case _ => ()
  }

  private def fileName(path: String) = new JFile(path).getName

  private def renderElement (out: MarkdownWriter)(elem: Element): Unit = {
    elem match {

      case RootElement(content) =>
        if (content.nonEmpty) out << content.head <<| content.tail

      case Title(content, _) =>
        val (anchor, spans) = content.partition(_.isInstanceOf[InternalLinkTarget])
        cacheRefTitle(anchor, spans)
        out << anchor
        if (anchor.nonEmpty) { out <| }
        out << "# " << spans

      case Header(level, content, _) =>
        val (anchor, spans) = content.partition(_.isInstanceOf[InternalLinkTarget])
        cacheRefTitle(anchor, spans)
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
        out << content

      case EnumListItem(content, _, num, _) =>
        out.indentWith(s" $num. ") { out << unwrapBlocks(content) }

      case DefinitionList(content, _) =>
        out << content

      case DefinitionListItem(term, definition, _) =>
        out <<| term <<| ": " << unwrapBlocks(definition) <|

      case CodeBlock(language, content, _) =>
        out <<| "```" << language.replace("^none$", "") <<| content <<| "```"

      case LiteralBlock(content, _) =>
        out <<| "```" <<| content <<| "```"

      case ParsedLiteralBlock(content, _) =>
        out <<| "```" <<| content << "```"

      case Literal(content, _) =>
        out << "`" << content << "`"

      case Text(text, _) =>
        out << text

      case InternalLinkTarget(opt) =>
        out << "<a id=\"" << opt.id.get << "\"></a>"


      case CrossLink(content, ref, path, _, _) =>
        val target = path.relative.toString.replace(".rst", ".md")
        val link =  if (ref.isEmpty) target else s"$target#$ref"
        out << "@ref[" << content << "](" << link << ")"

      case InternalLink(content, ref, _, _) =>
        out << "[" << content << "](#" << ref << ")"

      case ExternalLink(content, url, _, _) =>
        out << "[" << content << "](" << url << ")"

      case Image(_, uri, title, _) =>
        out << "![" << fileName(uri.uri) << "](" << uri.uri << ")"

      case Figure(path, scale, align, content, _) =>
        out <<| "![" << fileName(path) << "](" << path << ")"
        // FIXME: Find better way to render caption
        out <<| "> " << content

      case FootnoteLink(ref, label, _) =>
        out << "<a id=\"^" << ref << "\" href=\"#" << ref << "\">[" << label << "]</a>"

      case Footnote(label, content, Id(id)) =>
        out <<| s"""> <a id="$id" href="#^$id">[""" << label << "]</a> " << unwrapBlocks(content)

      case Comment(content, _) =>
        out << "<!-- " << content << " -->"

      // Renders note and warnings
      case TitledBlock(title, content, _) =>
        out <<| "> **" << title << ":**" << content


      case Table(TableHead(headerRows, _), TableBody(rows, _), caption, _, _) =>
        assert(caption.content.isEmpty && headerRows.size <= 1, s"$elem")

        def cellText(cell: Cell): String = {
          val sb = new StringBuilder
          var renderF: Element => Unit = null
          val writer = new MarkdownWriter(s => { sb.append(s); () }, x => renderF(x), cell, ". ")
          renderF = renderElement(writer)
          renderF(cell)
          sb.toString
        }

        val head = headerRows.map(_.content.map(cellText))
        val body = rows.map(_.content.map(cellText))
        val width = body.foldLeft(body.head.map(_ => 0)) { (state, row) =>
          row.map(_.size).zip(state).map(t => math.max(t._1, t._2))
        }

        def renderRow(row: Seq[String]): String =
          row.zip(width).map(t => s"%-${t._2}s".format(t._1)).mkString("|", " | ", "|")

        head.foreach { row =>
          val text = renderRow(row)
          out <<| text <<| text.replaceAll("[^|]", "-")
        }
        body.foreach { row =>
          out <<| renderRow(row)
        }

      case Cell(_, content, _, _, _) =>
        val span = unwrapBlocks(content) map { _ match {
          case Text(content, _) => Text(content.replaceAllLiterally("\n", " "))
          case other => other
        }}
        out << span

      case InvalidSpan(SystemMessage(_, reason, _), fallback, _) =>
        println(s"ERROR: $reason")
        out << "<!-- FIXME: " << reason << " --> " << fallback

      // our custom thingies/not covered by md
      case IncludeCode(path, tags, language, _, _) =>
        val fixme = Set("/akka-http", "/scala/", "directive").forall(path.contains)
        out <<| (if (fixme) "FIXME" else "") << "@@snip [" << fileName(path) << "](" << path << ") {"
        tags foreach { out << " #" << _ }
        language foreach { out << " type=" << _ }
        out << " }"

      case TocTree(maxDepth, toc, _, _) =>
        out <<| "@@toc" << maxDepth.fold("")(depth => s"{ depth=$depth }") <|;
        out <<| "@@@ index" <|;
        toc foreach { entry => out <<| s"* [$entry]($entry.md)" }
        out <|; out <<| "@@@"

      case ApiRef(name, role, _) =>
        // FIXME: Custom directive or link to ScalaDoc?
        out << "`" << name << "`"

      // catchalls
      case unhandled =>
        sys.error(unhandled.toString)
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
        (argument(withWS = true) ~ optField("include") ~ optField("exclude") ~ optField("language"))(IncludeCode(_, _, _, _))
      },
      BlockDirective("includecode2") {
        // :snippet: field is not actually optional
        (argument(withWS = true) ~ optField("snippet"))(IncludeCode(_, _, None, None))
      },
      BlockDirective("literalinclude") {
        (argument(withWS = true) ~ optField("language"))(IncludeCode.literal(_, _))
      },
      BlockDirective("toctree") {
        (optField("maxdepth") ~ content[Seq[String]](c => Right(c.split("\n"))))(TocTree(_,_))
      },
      BlockDirective("code-block") {
        (argument(withWS = true) ~ spanContent)(CodeBlock(_, _))
      },
      BlockDirective("figure") {
        (argument(withWS = true) ~ optField("scale") ~ optField("align") ~ blockContent)(Figure(_, _, _, _))
      },
      BlockDirective("image") {
        (argument(withWS = true) ~ optField("width") ~ optField("align")) { (uri, width, align) =>
          // FIXME: Pass width and align via Styles()?
          Paragraph(Seq(Image("", URI(uri), None, NoOpt)))
        }
      }
    )

    val textRoles = List(
      TextRole("ref", "")(textRoleField("ref")) { (base, content) =>
        val refMatch = """(.*[^ ])[ ]*(<(.*)>)""".r

        def linkReference(text: String, id: String) =
          LinkReference(List(Text(text.replaceAll("^-(.*)(-scala|-java)?-", "$1"))), id.toLowerCase, id)

        content match {
          case refMatch(refText, _, refName) => linkReference(refText, refName)
          case _ => linkReference(RefsCache.get(content), content)
        }
      }
    ) ++ ApiRef.roles.map { role =>
      TextRole(role.name, "")(textRoleField(role.name)) { (_, name) => ApiRef(name, role) }
    }

    ReStructuredText withBlockDirectives(blockDirectives: _*) withTextRoles(textRoles: _*)
  }

  Transform from akkaRst to ParadoxMarkdown fromDirectory srcDir.toJava toDirectory destDir.toJava

}
