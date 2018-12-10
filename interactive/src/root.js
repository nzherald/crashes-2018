import "./base.less"
import "./root.less"
import {Elm} from "./Main.elm"


class Main {
    constructor () {
        document.getElementById('root').innerHTML = '<div id="elm"></div>'
        var app = Elm.Main.init({
            node: document.getElementById('elm')
          });
        this.fadeOut()
    }

    fadeOut (b) {
        sessionStorage.setItem("loading", "done");
        if (typeof($) !== "undefined") {
            $("#loading").fadeTo(600, 0.01, () => {
                $("#loading").remove()
                console.log("Loading screen removed.")
                if (b) b()
            })
        } else {
            var loadingRemove = document.getElementById("loading")
            if (loadingRemove) {
                document.body.removeChild(loadingRemove)
                console.log("Loading screen removed.")
            }
            if (b) b()
        }
    }
}

new Main()
