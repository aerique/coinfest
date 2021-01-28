import QtQuick 2.0
import Sailfish.Silica 1.0
import EQL5 1.0

Page {
    property string tickerUUID: "stub ticker"
    property int tickerCount: -1

    SilicaFlickable {
        anchors.fill: parent
        contentWidth: parent.width
        contentHeight: column.height

        VerticalScrollDecorator {}

        Column {
            id: column
            spacing: Theme.paddingLarge
            width: parent.width

            PageHeader {
                title: "Ticker Details"
            }

            DetailItem {
                width: parent.width
                label: "Ticker"
                value: Lisp.call("cf:ticker-display-name", tickerUUID)
            }

            DetailItem {
                width: parent.width
                label: "Exchange ID"
                value: Lisp.call("cf:ticker-id", tickerUUID)
            }

            DetailItem {
                width: parent.width
                label: "Exchange"
                value: Lisp.call("cf:ticker-exchange", tickerUUID)
            }

            DetailItem {
                width: parent.width
                label: "Price"
                value: Lisp.call("cf:ticker-price", tickerUUID)
            }

            DetailItem {
                width: parent.width
                label: "Timestamp"
                value: Lisp.call("cf:ticker-timestamp", tickerUUID)
            }

            Label {
                anchors {
                    left: parent.left
                    leftMargin: Theme.paddingLarge
                }
                width: parent.width - (2 * Theme.paddingLarge)
                font.pixelSize: Theme.fontSizeSmall
                color: Theme.highlightColor
                wrapMode: Text.WordWrap
                text: "TBD: graph"  // a graph would be cool
            }
        }
    }

    // XXX Eehhh, we need `cf:next-ticker` or something?
    //ButtonLayout {
    //    y: parent.height - childrenRect.height - Theme.paddingLarge

    //    Button {
    //        color: Theme.highlightColor
    //        text: "<"
    //        onClicked: pageStack.replace(
    //            Qt.resolvedUrl("TickerDetailsPage.qml"),
    //            { tickerIndex: tickerIndex - 1,
    //              tickerCount: tickerCount })
    //        enabled: tickerIndex > 0
    //    }

    //    Button {
    //        color: Theme.highlightColor
    //        text: ">"
    //        onClicked: pageStack.replace(
    //            Qt.resolvedUrl("TickerDetailsPage.qml"),
    //            { tickerIndex: tickerIndex + 1,
    //              tickerCount: tickerCount })
    //        enabled: tickerIndex < (tickerCount - 1)
    //    }
    //}
}
