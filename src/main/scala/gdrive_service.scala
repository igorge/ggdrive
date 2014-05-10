package gie.ggdrive

import com.google.api.client.http.javanet.NetHttpTransport
import com.google.api.client.googleapis.auth.oauth2.{GoogleClientSecrets, GoogleCredential, GoogleAuthorizationCodeFlow}
import com.google.api.client.json.jackson2.JacksonFactory
import com.google.api.services.drive.{Drive, DriveScopes}
import java.io._
import java.awt.Desktop
import java.net.URI
import com.google.api.client.auth.oauth2.{StoredCredential, Credential}
import com.google.api.client.json.JsonFactory
import com.google.api.client.http.{GenericUrl, HttpTransport}
import com.typesafe.scalalogging.slf4j.LazyLogging
import javax.swing.JOptionPane
import gie.utils.swing.ui_thread
import scala.concurrent.Await
import scala.concurrent.duration.Duration
import gie.utils.prop.{Configuration, Messages}
import gie.utils.loan
import gie.utils.ImplicitPipe._
import com.google.api.services.drive.model.{File=>GoogleFile}
import scala.collection.mutable.ArrayBuffer
import scala.language.postfixOps
import scala.util.Try


class GDriveException(cause: Throwable = null) extends Exception(cause)
class GDriveCodeExchangeException(cause: Throwable = null) extends GDriveException(cause)
class GDriveNoUserIdException(cause: Throwable = null) extends GDriveException(cause)
class GDriveNoRefreshTokenException(cause: Throwable = null) extends GDriveException(cause)
class GDriveNoTokenResponseException(cause: Throwable = null) extends GDriveException(cause)
class GDriveNoCredentialsException(cause: Throwable = null) extends GDriveException(cause)

case class GDriveCredentials(id: String, secret: String, redirectUri: String)

object GDrive {
  import scala.collection.JavaConverters._



  def authFlow(clientSecrets: GoogleClientSecrets, scopes: Seq[String])(implicit jsonFactory: JsonFactory, httpTransport: HttpTransport): GoogleAuthorizationCodeFlow = {
    val flow =  new GoogleAuthorizationCodeFlow.Builder(httpTransport, jsonFactory, clientSecrets, scopes.asJavaCollection).
      setAccessType("offline").
      setApprovalPrompt("force").
      build()

    flow
  }



  def loadClientSecrets(clientSecretData: InputStream)(implicit jsonFactory: JsonFactory): GoogleClientSecrets = {
    val clientSecretIn = new InputStreamReader(clientSecretData, "utf-8")
    GoogleClientSecrets.load(jsonFactory, clientSecretIn)
  }



  def buildCredentials(clientSecrets: GoogleClientSecrets)(implicit jsonFactory: JsonFactory, httpTransport: HttpTransport) = {
    new GoogleCredential.Builder().
      setClientSecrets(clientSecrets).
      setJsonFactory(jsonFactory).
      setTransport(httpTransport).
      build()
  }



  @throws(classOf[GDriveNoTokenResponseException])
  private def impl_showSwingDialogForResponseToken(title: String, msg: String ): String = try {

    val tokenFut = ui_thread.inUI {
      JOptionPane.showInputDialog(null, msg, title, JOptionPane.PLAIN_MESSAGE)
    }

    val token = Await.result(tokenFut, Duration.Inf)

    if( token eq null ) throw new GDriveNoTokenResponseException( new NullPointerException("token response is null") )
    token

  } catch {
    case e: GDriveNoTokenResponseException => throw e
    case e: Throwable => throw new GDriveNoTokenResponseException(e)
  }



  @throws(classOf[GDriveNoCredentialsException])
  def startNewAuthentication(openBrowserFun: URI=>Any = url=>{Desktop.getDesktop().browse(url)})
                            (implicit jsonFactory: JsonFactory, httpTransport: HttpTransport, messages: Messages, authFlow: GoogleAuthorizationCodeFlow,
                             clientSecrets: GoogleClientSecrets) : Credential = try {


    val redirectUri = clientSecrets.getDetails.getRedirectUris.get(0)

    val url = authFlow.newAuthorizationUrl().setRedirectUri(redirectUri).build()
    openBrowserFun(new URI(url))

    val token = impl_showSwingDialogForResponseToken(messages('authorization), messages('enter_token))

    val response = authFlow.newTokenRequest(token).setRedirectUri(redirectUri).execute()
    val credential = GDrive.buildCredentials(clientSecrets).setFromTokenResponse(response)
    credential.refreshToken()

    credential

  } catch {
    case e:GDriveNoCredentialsException => throw e
    case e:Throwable => throw new GDriveNoCredentialsException(e)
  }



  @throws(classOf[GDriveNoCredentialsException])
  def loadCredentials(loadCredentialsIs: ()=>InputStream)
                     (implicit jsonFactory: JsonFactory, httpTransport: HttpTransport, clientSecrets: GoogleClientSecrets):Credential = try {

    loan.acquire(  loadCredentialsIs() |> (new BufferedInputStream(_)) |> (new ObjectInputStream(_))  ) { is=>

      val storedCredentials = is.readObject().asInstanceOf[StoredCredential]

      val credential = GDrive.buildCredentials(clientSecrets).
            setAccessToken(storedCredentials.getAccessToken).
            setRefreshToken(storedCredentials.getRefreshToken).
            setExpiresInSeconds(storedCredentials.getExpirationTimeMilliseconds)

      val status = credential.refreshToken()
      if(!status) throw new GDriveNoRefreshTokenException()

      credential
    }

  } catch {
    case e:GDriveNoCredentialsException => throw e
    case e:Throwable => throw new GDriveNoCredentialsException(e)
  }



  def storeCredentials(credentials: Credential, storeCredentials: ()=>OutputStream){

    loan.acquire( storeCredentials() |> (new BufferedOutputStream(_)) |> (new ObjectOutputStream(_)) ) { os=>
      val storedCredentials = new StoredCredential(credentials)
      os.writeObject(storedCredentials)
    }

  }

  def files()(implicit drive: Drive): Array[GoogleFile] = try {
    val results = new ArrayBuffer[GoogleFile]()
    val request = drive.files().list()

    do {
      val files = request.execute()
      results ++= files.getItems.asScala
    } while(request.getPageToken() !=null && request.getPageToken().length>0)

    results.toArray

  } catch {
    case ex: Throwable => throw new GDriveException(ex)
  }


  def isDirectory(g: GoogleFile) = g.getMimeType == "application/vnd.google-apps.folder"

  def name(g: GoogleFile):String = Option(g.getTitle) orElse (Some(g.getOriginalFilename)) getOrElse (g.getId)

  def isTrashed(g: GoogleFile) = Option(g.getExplicitlyTrashed()).map(_.booleanValue).getOrElse(false)

  def fileFromId(googleId: String)(implicit drive: Drive): GoogleFile = {
    val request = drive.files().get(googleId)
    request.execute()
  }

  def downloadFile(f: GoogleFile)(implicit drive: Drive): InputStream = {
    val request = drive.getRequestFactory.buildGetRequest( new GenericUrl(f.getDownloadUrl))
    val resp = request.execute()
    assume(resp ne null)
    resp.getContent
  }



}


trait ThreadLocks {

  def withReadLock[T](fun: =>T) = {
    this.synchronized( fun )
  }

  def withWriteLock[T](fun: =>T) = {
    this.synchronized( fun )
  }

}


abstract class GNode(parent: GDirectory, var g: GoogleFile) extends ThreadLocks{

  def valid_? = true
  def threadSafeValid_? = true

  def id = {
    val r = g.getId

    assume( r ne null )
    assume( r.length > 0 )

    r
  }

  def name:String = GDrive.name(g)

}

class GFile(parent: GDirectory, g: GoogleFile) extends GNode(parent, g)

class GDirectory(parent: GDirectory, g: GoogleFile) extends GNode(parent, g){
  val children =  collection.mutable.Map[String, GNode]()

  def threadSafeGet(name: String) = withReadLock{
    children.get(name)
  }
}





class GDrive(appName: String,  openClientSecrets: ()=>InputStream,
              loadCredentials: ()=>InputStream, storeCredentials: ()=>OutputStream)(implicit messages: Messages) extends LazyLogging with ThreadLocks {
  private implicit val jsonFactory = new JacksonFactory
  private implicit val httpTransport = new NetHttpTransport
  private implicit val clientSecrets = loan.acquire( new BufferedInputStream(openClientSecrets()) )( GDrive.loadClientSecrets(_) )
  private implicit val flow = GDrive.authFlow(clientSecrets, DriveScopes.DRIVE::Nil)

  private val credentials = impl_initCredentials()

  private implicit val driveService = new Drive.Builder(httpTransport, jsonFactory, credentials).setApplicationName(appName).build()

  private def impl_initCredentials(): Credential = {

    val credentials = try GDrive.loadCredentials(loadCredentials) catch {
      case e:GDriveNoCredentialsException=>
        GDrive.startNewAuthentication()
    }

    GDrive.storeCredentials(credentials, storeCredentials)

    credentials
  }

  private var m_files = GDrive.files()

  val m_root = new GDirectory(null, null){
    override def name = ""
    override def id = ""
  }

  def root():GDirectory = withReadLock{ m_root }

  def fileFromId(googleId: String) = {
    GDrive.fileFromId(googleId)
  }

  def downloadFile(f: GoogleFile): InputStream = {
    GDrive.downloadFile(f)
  }


  private def impl_makeNodeFrom(parent: GDirectory, g: GoogleFile): GNode = {
    if(GDrive.isDirectory(g)) new GDirectory(parent, g) else new GFile(parent, g)
  }

  private def impl_makeDirectoryFrom(parent: GDirectory, g: GoogleFile): GDirectory = {
    assume(GDrive.isDirectory(g))
    new GDirectory(parent, g)
  }

  private def impl_collectPath(g: GoogleFile, byId: collection.mutable.Map[String, GoogleFile]): IndexedSeq[GoogleFile] = {
    val pathBuffer = new ArrayBuffer[GoogleFile]()

    var currentFileOpt = Option(g)

    def getParent = for(  currentFile <- currentFileOpt;
                          parents     <- Option(currentFile.getParents) if(parents.size()>0);
                          dirId       <- Option( parents.get(0).getId );
                          googleFile  <- Try{ byId(dirId) }.toOption ) yield googleFile

    currentFileOpt = getParent

    while (currentFileOpt.isDefined) {
      pathBuffer += currentFileOpt.get
      currentFileOpt = getParent
    }

    pathBuffer.reverse

  }

  private def impl_isTrashed(g: GoogleFile, path: IndexedSeq[GoogleFile]): Boolean = {

    if(GDrive.isTrashed(g)) true else {
      val reversePath = path.view.reverseIterator
      reversePath.find( GDrive.isTrashed(_) ).fold(false)(_=>true)
    }


  }

  private def impl_buildFileSystem_process(root: GDirectory, g: GoogleFile, byId: collection.mutable.Map[String, GoogleFile]){
    val path = impl_collectPath(g, byId)

    if( !impl_isTrashed(g, path) ) {

      var currentParent:GDirectory = root
      var i = 0

      while(i < path.length){
        val currentNode = path(i)
        val currentNodeName = GDrive.name(currentNode)

        currentParent.children.get(currentNodeName).fold{
          val newNode = impl_makeDirectoryFrom(currentParent, currentNode)
          currentParent.children(currentNodeName) = newNode

          currentParent = newNode
        }{ existedNode=>
          currentParent = existedNode.asInstanceOf[GDirectory]
        }

        i+=1
      }


      currentParent.children.get(GDrive.name(g)).fold{
        val newNode = impl_makeNodeFrom(currentParent, g)
        currentParent.children(newNode.name) = newNode
      }{ existedNode=>
        assume( GDrive.isDirectory(g) )
        assume( GDrive.isDirectory(existedNode.g) )
      }

    }

  }

  private def impl_buildByIdMap(g: Seq[GoogleFile]) = {
    val map = collection.mutable.Map[String, GoogleFile]()
    g.foreach{ v=>

      val id = v.getId

      assume(id ne null)
      assume(id.length>0)

      val r = map.put(v.getId, v)
      assume(r.isEmpty)

    }

    map
  }

  private def impl_buildFileSystem(){
    m_root.children.clear()
    val byIdMap = impl_buildByIdMap(m_files)

    m_files foreach (v=>impl_buildFileSystem_process(m_root, v, byIdMap))
  }


  impl_buildFileSystem()
}