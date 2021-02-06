import QtQuick 2.0
import Sailfish.Silica 1.0
import Nemo.Notifications 1.0
import EQL5 1.0

import "pages/" as Pages

ApplicationWindow
{
    initialPage: Component { Pages.TickersPage {} }
    cover: Qt.resolvedUrl("cover/CoverPage.qml")
    allowedOrientations: defaultAllowedOrientations

    RemorsePopup { id: remorse }

    //BusyLabel {
    //    id: busy_label
    //    objectName: "busy_label"
    //    text: ""
    //    running: false
    //}

    Rectangle {
        id: busy_rect
        objectName: "busy_rect"
        anchors.fill: parent
        anchors.margins: Theme.paddingLarge
        visible: false

        color: Theme.overlayBackgroundColor
        opacity: Theme.opacityOverlay

        BusyIndicator {
            id: busy_indicator
            objectName: "busy_indicator"
            size: BusyIndicatorSize.Large
            anchors.centerIn: parent
            running: false
        }

        Label {
            id: busy_text
            objectName: "busy_label"
            anchors {
                top: busy_indicator.bottom
                topMargin: Theme.paddingLarge
                horizontalCenter: parent.horizontalCenter
            }
            text: "BusyLabel text stub"
        }
    }

    BackgroundItem {
        id: feedback
        objectName: "feedback"
        anchors.fill: parent
        visible: false

        Rectangle {
            property var portrait: parent.width <= parent.height

            // FIXME read up on Screen.width!: https://sailfishos.org/develop/docs/silica/qml-sailfishsilica-sailfish-silica-screen.html/
            width: parent.width / 1.5
            height: portrait ? parent.width / 1.5 : parent.width / 3
            x: parent.width / 6
            y: portrait ? (parent.height / 2) - (parent.width / 3) :
                          (parent.height / 2) - (parent.width / 6)
            radius: portrait ? parent.width / 16 : parent.height / 16

            color: Theme.overlayBackgroundColor
            opacity: Theme.opacityOverlay

            Icon {
                id: warningIcon
                y: Theme.paddingLarge
                anchors.horizontalCenter: parent.horizontalCenter
                source: "image://theme/icon-l-attention"
            }

            Label {
                id: feedbackLabel
                objectName: "feedbackLabel"

                width: parent.width - 2 * Theme.paddingLarge
                anchors.top: warningIcon.bottom
                x: Theme.paddingLarge

                color: Theme.secondaryHighlightColor
                wrapMode: Text.WordWrap
                text: ""
            }
        }

        onClicked: visible = false
    }

    Notification {
        objectName: "notification"
        appName: "Coinfest"
        summary: ""
        previewSummary: ""
        body: ""
        previewBody: ""
    }

    Timer {
        id: tickerRefreshTimer
        // `*ticker-refresh*` is in seconds
        interval: 1000 * Lisp.call("cf:get-ticker-refresh")
        running: true
        repeat: true
        onTriggered: function() {
            Lisp.call("cf:refresh-tickers")
            setOverviewModelTimer.running = true
        }
    }

    // XXX This hack exists because we cannot update the model in the Lisp
    //     thread where we download new ticker info.
    Timer {
        id: setOverviewModelTimer
        interval: 500
        running: false
        repeat: true
        onTriggered: function() {
            if (Lisp.call("cf:update-model-p")) {
                Lisp.call("cf:set-overview-model")
                setOverviewModelTimer.running = false
            }
        }
    }
}
