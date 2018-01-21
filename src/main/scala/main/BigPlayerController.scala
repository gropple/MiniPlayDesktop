package main

import json.JsonMem
import svg.SvgLoader
import websocket.SocketHandler
import javafx.beans.property.SimpleBooleanProperty

import scalafx.application.Platform
import scalafx.event.ActionEvent
import scalafx.scene.control.{Button, Label}
import scalafx.scene.image.{Image, ImageView}
import scalafx.scene.layout.HBox
import scalafxml.core.macros.sfxml

trait BigPlayerInterface {
  def handleMessage(msg: String)
}


@sfxml(additionalControls = List("com.jfoenix.controls"))
class BigPlayerController(art: ImageView,
                          title: Label,
                          artist: Label,
                          buttonBar: HBox) extends BigPlayerInterface {

  // Binds us to one chrome extension so not perfect
  private def sh = SocketHandler.connectionToChromeExtension

  private val svgLoader = new SvgLoader()

  private def sendMessage(msg: String): Boolean = {
    val shh = sh
    if (shh == null) {
      println("Error: chrome extension not connected, cannot send: " + msg)
      false
    }
    else {
      shh.broadcastMessage(msg)
      true
    }
  }

  private def createButton(svg: String, onClick: () => Unit): Button = {
    val out = new Button()
    val icon = svgLoader.loadSvg(getClass.getResourceAsStream(svg))
    out.setGraphic(icon)
    out.setOnMouseClicked((v) => onClick())
    out.setPrefHeight(50)
    out.setPrefWidth(50)
    out
  }

  private val buttonPrevSong = createButton("/svg/previousSong.svg", () => sendMessage("""{"action":"prev"}"""))
  private val buttonNextSong = createButton("/svg/nextSong.svg", () => sendMessage("""{"action":"next"}"""))
  private val buttonThumbsUp = createButton("/svg/thumbsUp.svg", () => {
    if (sendMessage("""{"action":"thumbsUp"}""")) {
      // Better model would be to wait for a response from the extension, but in practice that's a little laggy
      // So we make a very-likely-to-be-correct guess about the state we will soon receive
      setState(isPlaying, true, false)
    }
  })
  private val buttonThumbsUpActive = createButton("/svg/thumbsUpActive.svg", () => {
    if (sendMessage("""{"action":"undoThumbsUp"}""")) {
      setState(isPlaying, false, false)
    }
  })
  private val buttonThumbsDown = createButton("/svg/thumbsDown.svg", () => {
    if (sendMessage("""{"action":"thumbsDown"}""")) {
      setState(isPlaying, false, true)
    }
  })
  private val buttonThumbsDownActive = createButton("/svg/thumbsDownActive.svg", () => {
    if (sendMessage("""{"action":"undoThumbsDown"}""")) {
      setState(isPlaying, false, false)
    }
  })
  private val buttonPlay = createButton("/svg/play.svg", () => {
    if (sendMessage("""{"action":"play/pause"}""")) {
      setState(true, isThumbedUp, isThumbedDown)
    }
  })
  private val buttonPause = createButton("/svg/pause.svg", () => {
    if (sendMessage("""{"action":"play/pause"}""")) {
      setState(false, isThumbedUp, isThumbedDown)
    }
  })

  private var isPlaying = false
  private var isThumbedUp = false
  private var isThumbedDown = false
  setState(isPlaying, isThumbedUp, isThumbedUp)


  // Not sure about having lucky.  Clutters interface.
  //  val buttonLucky = createButton("/svg/lucky.svg", () => if (connectionToChromeExtension != null) connectionToChromeExtension.broadcastMessage("""{"action":"lucky"}"""))

  buttonBar.children.add(buttonPrevSong)
  buttonBar.children.add(buttonPlay)
  buttonBar.children.add(buttonPause)
  buttonBar.children.add(buttonNextSong)
  buttonBar.children.add(buttonThumbsUp)
  buttonBar.children.add(buttonThumbsUpActive)
  buttonBar.children.add(buttonThumbsDown)
  buttonBar.children.add(buttonThumbsDownActive)

  private def setState(playing: Boolean, thumbedUp: Boolean, thumbedDown: Boolean): Unit = {
    isPlaying = playing
    isThumbedUp = thumbedUp
    isThumbedDown = thumbedDown

    buttonPlay.setVisible(!playing)
    buttonPlay.setManaged(!playing)
    buttonPause.setVisible(playing)
    buttonPause.setManaged(playing)
    buttonThumbsUp.setVisible(!thumbedUp)
    buttonThumbsUp.setManaged(!thumbedUp)
    buttonThumbsUpActive.setVisible(thumbedUp)
    buttonThumbsUpActive.setManaged(thumbedUp)
    buttonThumbsDown.setVisible(!thumbedDown)
    buttonThumbsDown.setManaged(!thumbedDown)
    buttonThumbsDownActive.setVisible(thumbedDown)
    buttonThumbsDownActive.setManaged(thumbedDown)

  }


  private val firstTime = new SimpleBooleanProperty(true) // Variable to store the focus on stage load
  buttonPrevSong.focusedProperty.addListener((observable, oldValue, newValue) => {
    if (newValue && firstTime.get) {
      GUI.stage.requestFocus // Delegate the focus to container
      firstTime.setValue(false) // Variable value changed for future references
    }
  })

  // event handlers are simple public methods:
  private def onCreate(event: ActionEvent) {
    // ...
  }

  private var lastMsg: String = null

  override def handleMessage(msg: String): Unit = {
    if (msg != lastMsg) {
      val json = JsonMem.read(msg)
      lastMsg = msg

      Platform.runLater(() => {

        title.setText(json.title.toString)
        //        art.setText(json.art.toString)
        artist.setText(json.artist.toString)

        val image = new Image(json.art.toString)
        art.setImage(image)

        setState(json.playing.toString.toBoolean,
          json.isThumbedUp.toString.toBoolean,
          json.isThumbedDown.toString.toBoolean)

      })
      //    }

    }
  }
}
