package main

import javafx.geometry.Rectangle2D
import javafx.scene.input.MouseEvent
import javafx.stage.Screen
import javafx.{scene => jfxs}

import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.scene.image.ImageView
import scalafx.scene.text.Font
import scalafx.stage.StageStyle
import scalafxml.core.FXMLLoader

//@Component
//class Thing {
//  @Autowired
//  var connectionToChromeExtension: SocketHandler = null;
//
//}

object GUI extends JFXApp {
  Font.loadFont(GUI.getClass.getResource("/fonts/Roboto-Regular.ttf").toExternalForm(), 12);

//  var connectionToChromeExtension: SocketHandler = null
//
//  def register(connectionToChromeExtension: SocketHandler) = {
//    this.connectionToChromeExtension = connectionToChromeExtension
//  }

  //  val echoText: String = "Hello word\n"
  // write to pipe
  //  pipe.write("play/pause".getBytes)
  // read response
  //  val echoResponse: String = pipe.readLine
  //  System.out.println("Response: " + echoResponse)

  val primaryScreenBounds: Rectangle2D = Screen.getPrimary.getVisualBounds

  val st = new scalafx.scene.control.Label()
  st.setText("Song playing")
  st.setMaxWidth(200)
  st.setId("songTitle")
  st.setFont(Font.font ("Roboto", 16));


  val art = new scalafx.scene.control.Label()
  art.setText("Art")

  val imageView = new ImageView()
  imageView.fitHeight = 90
  imageView.fitWidth = 90

  val posOnRight = false

  val artist = new scalafx.scene.control.Label()
  artist.setText("Artist")
  artist.setMaxWidth(200)
  artist.setId("songArtist")
  artist.setFont(Font.font ("Roboto", 13))

  val sceneLoader = new FXMLLoader(getClass.getResource("/fxml/bigplayer.fxml"), null)

  sceneLoader.load()

  //    val root = loader.getRoot[javafx.scene.Parent]
  val root = sceneLoader.getRoot[jfxs.Parent]


  // Get the controller. We cannot use the controller class itself here,
  // because it is transformed by the macro - but we can use the trait it
  // implements!
   val controller = sceneLoader.getController[BigPlayerInterface]

  val scene = new javafx.scene.Scene(root)
  val sscene = new scalafx.scene.Scene(scene)

  stage = new PrimaryStage {
    title = "ScalaFX Hello World"
    scene = sscene
  }

//    stage.scene = new Scene(root) // if (posOnRight) sceneVertical else sceneHorizontal
  stage.scene.get().getStylesheets.add(getClass().getResource("/css/main.css").toExternalForm())
  stage.setAlwaysOnTop(true)
  stage.initStyle(StageStyle.Undecorated)
//  stage.initStyle(StageStyle.Transparent)

  stage.title = "MiniPlay"

  if (posOnRight) {
    val width = 200
    stage.setWidth(width)
    stage.setX(primaryScreenBounds.getWidth - width)
    stage.setY(primaryScreenBounds.getHeight * 0.1)
  }
  else {
    val height = 100
    val width = 600
    stage.setMaxWidth(width)
    stage.setHeight(height)
    stage.setY(0)
    stage.setX((primaryScreenBounds.getWidth - width) - (primaryScreenBounds.getWidth * 0.1))
  }


  import javafx.event.EventHandler

  private var xOffset = 0.0
  private var yOffset = 0.0

  scene.setOnMousePressed(new EventHandler[MouseEvent]() {
    override def handle(event: MouseEvent): Unit = {
      xOffset = event.getSceneX
      yOffset = event.getSceneY
    }
  })
  root.setOnMouseDragged(new EventHandler[MouseEvent]() {
    override def handle(event: MouseEvent): Unit = {
      val y = Math.min(Math.max(0, event.getScreenY - yOffset), primaryScreenBounds.getHeight - root.getLayoutBounds.getHeight)
      val x = Math.min(Math.max(0, event.getScreenX - xOffset), primaryScreenBounds.getWidth - root.getLayoutBounds.getWidth)
      stage.setX(x)
      stage.setY(y)
    }
  })


  def handleMessageFromChromeExtension(msg: String): Unit = {
    if (controller != null) {
      controller.handleMessage(msg)
    }
    else {
      println("Error, controller not ready: " + msg)
    }
  }
}
