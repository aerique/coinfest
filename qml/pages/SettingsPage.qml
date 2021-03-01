import QtQuick 2.0
import Sailfish.Silica 1.0
import Nemo.KeepAlive 1.2
import EQL5 1.0

Page {
    SilicaFlickable {
        anchors.fill: parent
        contentWidth: parent.width
        contentHeight: column.height

        PullDownMenu {
            MenuItem {
                text: "About"
                onClicked: pageStack.push(Qt.resolvedUrl("AboutPage.qml"))
            }

            MenuItem {
                text: "Reset Settings"
                onClicked: remorse.execute(
                    "Resetting all settings to default",
                    function() { console.log("Reset all settings!") })
            }
        }

        VerticalScrollDecorator {}

        Column {
            id: column
            spacing: Theme.paddingLarge
            width: parent.width

            PageHeader { title: "Settings" }

            //SectionHeader { text: "General" }

            ComboBox {
                id: tickerRefresh
                objectName: "tickerRefresh"
                width: parent.width
                description: "between refreshing all tickers"
                currentIndex: Lisp.call("cf:get-ticker-refresh")
                menu: ContextMenu {
                    // See coinfest.lisp:get-ticker-refresh-for-bgjob
                    //MenuItem { text: "5 minutes" } // too often, sorry
                    MenuItem { text: "15 minutes" }  // 0
                    MenuItem { text: "30 minutes" }  // 1
                    MenuItem { text: "1 hour"     }  // 2
                    MenuItem { text: "4 hours"    }  // 3
                    MenuItem { text: "12 hours"   }  // 4
                    onClicked: function() {
                        Lisp.call("cf:set-ticker-refresh",
                                  tickerRefresh.currentIndex)
                        tickerRefreshTimer.frequency = eval(Lisp.call(
                            "cf:get-ticker-refresh-for-bgjob"))
                    }
                }
            }
        }
    }
}
