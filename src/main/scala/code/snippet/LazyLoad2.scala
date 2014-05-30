package code.snippet

import net.liftweb.util.{Schedule, Helpers}
import net.liftweb.util.Helpers._
import net.liftweb.common._
import net.liftweb.http._
import scala.xml.{Comment, NodeSeq}
import net.liftweb.http.js.{JE, JsCmds, JsCmd}
import net.liftweb.http.js.JsCmds._
import net.liftweb.builtin.snippet._
import net.liftweb.http.js.jquery.JqJE.{JqAppend, JqAppendTo}
import net.liftweb.builtin
import net.liftweb.http.CometCreationInfo
import net.liftweb.common.Full
import scala.xml.Comment
import net.liftweb.http.PerformSetupComet2
import net.liftweb.http.CometCreationInfo
import net.liftweb.http.js.JsCmds.Replace
import net.liftweb.common.Full
import scala.xml.Comment
import net.liftweb.http.PerformSetupComet2
import net.liftweb.http.js.jquery.JqJE

object LazyLoad2 extends DispatchSnippet {
  private object myFuncName extends TransientRequestVar(Helpers.nextFuncName)
  private object myActor extends TransientRequestVar[Box[CometActor]](Empty)

  def dispatch: DispatchIt = {
    case _ => render _
  }

  /**
   * Enclose your snippet like this:
   *
   * <pre name="code" class="xml">
   *   &lt;div class="lift:LazyLoad">
   *     &lt;div class="lift:MyLongRunningSnippet">&lt;/div>
   *   &lt;/div>
   * </pre>
   *
   * You can add the template attribute to the LazyLoad tag and instead of
   * showing the spinning circle, it will render your template.
   *
   *
   * <pre name="code" class="xml">
   *   &lt;div class="lift:LazyLoad?template=&#39;my-nice-wait-message-template&#39;">
   *     &lt;div class="lift:MyLongRunningSnippet">&lt;/div>
   *   &lt;/div>
   * </pre>
   *
   *
   */
  def render(xhtml: NodeSeq): NodeSeq = {
    (for {
      session <- S.session ?~ ("FIXME: Invalid session")
    } yield {

      // if we haven't created the actor yet, register on this
      // thread to create the AsyncRenderComet actor
      if (myActor.isEmpty) {
        LiftRules.cometCreationFactory.request.set(
          (ccinfo: CometCreationInfo) =>
            ccinfo match {
              case CometCreationInfo(theType @ "AsyncRenderComet",
              name,
              defaultXml,
              attributes,
              session) => {
                val ret = new AsyncRenderComet2()
                ret.initCometActor(session,
                  Full(theType),
                  name, defaultXml, attributes)
                ret ! PerformSetupComet2(if (ret.sendInitialReq_?)
                  S.request.map(_.snapshot) else Empty)

                // and save it in the request var
                myActor.set(Full(ret))

                Full(ret)
              }

              case _ => Empty
            })
      }

      val id = Helpers.nextFuncName

      val func: () => JsCmd = {

        LiftRules.noticesToJsCmd =  () => {
          import builtin.snippet.{Msg,Msgs,MsgErrorMeta,MsgNoticeMeta,MsgWarningMeta}

          // A "wrapper" that simply returns the javascript
          val passJs = (in : JsCmd) => in

          // Delegate to Msgs for fadeout and effects
          def noticesFadeOut(noticeType: NoticeType.Value): JsCmd =
            Msgs.noticesFadeOut(noticeType, Noop, passJs)

          def groupEffects(noticeType: NoticeType.Value): JsCmd =
            Msgs.effects(Full(noticeType), noticeType.id, Noop, passJs)

          def idEffects(id : String): JsCmd =
            Msgs.effects(Empty, id, Noop, passJs)

          // Compute the global notices first
          val groupMessages = Msgs.renderNotices() match {
            case NodeSeq.Empty => JsCmds.Noop
            case xml =>
              JqJE.Jq("#" + LiftRules.noticesContainerId) ~> JqAppend(xml) &
              //LiftRules.jsArtifacts.setHtml(LiftRules.noticesContainerId, xml) &
              noticesFadeOut(NoticeType.Notice) &
              noticesFadeOut(NoticeType.Warning) &
              noticesFadeOut(NoticeType.Error) &
              groupEffects(NoticeType.Notice) &
              groupEffects(NoticeType.Warning) &
              groupEffects(NoticeType.Error)
          }

          // We need to determine the full set of IDs that need messages rendered.
          val idSet = (S.idMessages((S.errors)) ++
            S.idMessages((S.warnings)) ++
            S.idMessages((S.notices))).map(_._1).distinct
          // Merge each Id's messages and effects into the JsCmd chain
          idSet.foldLeft(groupMessages) {
            (chain,id) => chain &
              JqJE.Jq(id) ~> JqAppend(Msg.renderIdMsgs(id)) &
              /*LiftRules.jsArtifacts.setHtml(id, Msg.renderIdMsgs(id)) &*/
              idEffects(id)
          }
        }

        session.buildDeferredFunction(() => {

          Replace(id, xhtml) &
          JsCmd.unitToJsCmd(

          )&
          LiftRules.noticesToJsCmd()
        })
      }

      <div id={id}>
        {
        S.attr("template") match {
          case Full(template) => <lift:embed what={template}/>
          case _ => <img src="/images/ajax-loader.gif" alt="Loading"/>
        }
        }
      </div>++ (myActor.is match {
        case Full(actor) => actor ! Ready2(func); NodeSeq.Empty
        case _ => session.setupComet("AsyncRenderComet", Full(myFuncName.is), Ready2(func))
          <tail><lift:comet type="AsyncRenderComet" name={myFuncName.is}/></tail>
      })
    }) match {
      case Full(x) => x
      case Empty => Comment("FIX"+ "ME: session or request are invalid")
      case Failure(msg, _, _) => Comment(msg)
    }

  }

}

private case class Ready2(js: () => JsCmd)
private case class Render2(js: JsCmd)

class AsyncRenderComet2 extends CometActor {

  override def lifespan: Box[TimeSpan] = Full(90 seconds)

  def render = NodeSeq.Empty

  // make this method visible so that we can initialize the actor
  override def initCometActor(theSession: LiftSession,
                              theType: Box[String],
                              name: Box[String],
                              defaultXml: NodeSeq,
                              attributes: Map[String, String]) {
    super.initCometActor(theSession, theType, name, defaultXml,
      attributes)
  }


  override def lowPriority : PartialFunction[Any, Unit] = {
    // farm the request off to another thread
    case Ready2(js) =>
      Schedule.schedule(() => this ! Render2(js()), 0 seconds)

    // render it
    case Render2(js) =>
      partialUpdate(js)
  }
}
