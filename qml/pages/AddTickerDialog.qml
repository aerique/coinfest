import QtQuick 2.0
import Sailfish.Silica 1.0
import EQL5 1.0

Dialog {
    // This means the top Kraken ticker cannot be accepted right away,
    // but: meh.
    //property bool tickerUsed: false

    Column {
        id: column
        spacing: Theme.paddingLarge
        width: parent.width

        // FIXME Why does this give a warning once here but not in Pusfofefe?!
        DialogHeader {}

        ComboBox {
            id: exchange
            width: parent.width
            description: "exchange"
            currentIndex: 0  // Kraken
            menu: ContextMenu {
                Repeater {
                    model: exchangesModel
                    MenuItem { text: modelData }
                }
                onClicked: Lisp.call("cf:update-tickers-model", exchange.value)
            }
        }

        // Needs a search box.
        ComboBox {
            id: ticker
            width: parent.width
            description: "ticker"
            menu: ContextMenu {
                Repeater {
                    model: tickersModel
                    MenuItem { text: modelData }
                }
            }
            //onClicked: tickerUsed = true
        }

        Label {
            width: parent.width - 2 * Theme.horizontalPageMargin
            x: Theme.horizontalPageMargin
            font.pixelSize: Theme.fontSizeSmall
            color: Theme.highlightColor
            wrapMode: Text.WordWrap
            text: "<br>Pick an exchange first and then ticker you want to " +
                  "add.<br><br>The tickers are pre-populated for Kraken " +
                  "when the app was built. To update them, select Kraken " +
                  "from the dropdown again."
        }
    }

    //canAccept: tickerUsed == true

    onAccepted: { Lisp.call("cf:add-ticker", exchange.value, ticker.value) }
}
